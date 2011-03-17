#!/usr/bin/ruby

require 'fileutils'

def ffi_type(ptr, type)
  # special cases
  case type
  when "void"
    return "Ptr Word8"       if ptr == 1
    return "Ptr (Ptr Word8)" if ptr == 2
  when "char"
    return "CString"     if ptr == 1
    return "Ptr CString" if ptr == 2
  end

  # standard cases
  types = {
    "void"   => "()",
    "char"   => "CChar",
    "uchar"  => "Word8",
    "short"  => "CShort",
    "ushort" => "CUShort",
    "int"    => "CInt",
    "uint"   => "CUInt",
    "long"   => "CLong",
    "ulong"  => "CULong",
    "size_t" => "CSize",
    "time_t" => "CTime",

    # git_time_t is typedef'd to time_t on every
    # platform that GHC builds on because GHC uses
    # MinGW when building on Windows.
    "git_time_t" => "CTime"
  }

  ffi = types[type] || "<#{type}>"
  return case ptr
    when 0 then ffi
    when 1 then "Ptr #{ffi}"
    when 2 then "Ptr (Ptr #{ffi})"
    else raise "only two pointer indirections supported"
    end
end

def ffi_return(arg)
  if ffi_argument(arg) == "()"
    return "IO ()"
  end
  return "IO (#{ffi_argument(arg)})"
end

def ffi_argument(arg)
  arg.gsub!(/const/, '')
  arg.gsub!(/unsigned char/, 'uchar')
  arg.gsub!(/unsigned short/, 'ushort')
  arg.gsub!(/unsigned int/, 'uint')
  arg.gsub!(/unsigned long/, 'ulong')
  arg.gsub!(/unsigned/, 'uint')
  if (m = arg.match(/^ *([^ ]+) *\*\*/))
    return ffi_type 2, m[1]
  end
  if (m = arg.match(/^ *([^ ]+) *\*/))
    return ffi_type 1, m[1]
  end
  if (m = arg.match(/^ *([^ ]+)/))
    return ffi_type 0, m[1]
  end
  raise "unkown type: #{arg}"
end

def ffi_arguments(arguments)
  converted = []
  arguments.split(",").each{|arg|
    arg.strip!
    converted.push(ffi_argument(arg))
  }
  return converted.join(" -> ")
end

def module_header(header)
  return """
#include <bindings.dsl.h>
#include <git2.h>
module #{module_name(header)} where
#strict_import
"""
end

def transform_enums(contents)
  enums = []
  contents.scan(/typedef enum *\{[^\}]+\} [^;]+;/) {|enum|
    m = enum.match(/\{([^\}]+)\} *([^;]+);/)
    enum_name = m[2].strip
    enum_body = m[1].strip
    enum_definition = ["#integral_t #{enum_name}"]
    enum_body.scan(/^ *[^ ]+ = [^,]+,/) {|e|
      n = e.match(/^ *([^ ]+) = ([^,]+)/)
      enum_definition.push("#num    #{n[1].strip}")
    }
    enums.push({ :transformed => enum_definition.join("\n"),
                 :original => enum
               })
  }
  return enums
end

def module_basename(header)
  header = File.basename(header)
  header.gsub(/^[a-z]|_[a-z]|-[a-z]/) {|a| a.upcase }.gsub(/\.h$/, '').gsub(/_|-/, '')
end

def module_c_helper(header)
  "#{module_path(header)}.helper.c"
end
def c_helpers(headers)
  headers.
    map{|h| module_c_helper(h)}.
    select{|c| File.exists? c}
end
def module_name(header)
  "Bindings.Libgit2.#{module_basename(header)}"
end
def module_path(header)
  "Bindings/Libgit2/#{module_basename(header)}.hsc"
end

def parse_structs(string)
  string.scan(/typedef struct [^ \{]+ ([^ \{]+);/) {|type|
    yield({:opaque => type[0] })
  }

  string.scan(/typedef struct [^;\{]*\{[^}]+\} [^;]+;/) {|struct|
    m = struct.match(/typedef struct [^\{]*\{([^\}]*)\} ([^;]+);/)
    raise "could not parse open struct '#{struct}'" if not m
    yield({:original => struct,
            :name => m[2],
            :body => m[1]})
  }
end

def parse_struct_fields(body)
  body.scan(/^ *[^ ]+ [^;]+;/){|field|
    f = field.match(/^ *([^;]+);/)
    raise "could not parse struct field '#{field}'" if not f
    type = f[1].strip
    fieldname = field.match(/([^* ]+);/)
    arrayname = fieldname[1].match(/([^\[]+)\[/)
    if arrayname
      yield('array_field', type, arrayname[1])
    else
      yield('field', type, fieldname[1])
    end
  }
end

def transform_structs(string)
  string.gsub!(/\/\*[^\/]*\*\//, '')
  structs = []
  parse_structs(string) {|p|
    if p[:opaque]
      structs.push({ :original => p[:original],
                     :transformed => "#opaque_t #{p[:opaque]}"
                   })
    else
      structlist = ["#starttype #{p[:name]}"]
      parse_struct_fields(p[:body]) {|field, type, name|
        structlist.push("##{field}    #{name} , #{ffi_argument(type)}")
      }
      structlist.push("#stoptype")
      structs.push({ :transformed => structlist.join("\n"),
                     :original => p[:original] })
    end
  }
  return structs
end

def transform_typedefs(contents)
  defs = []
  types = {}
  contents.scan(/typedef ([^\s]+) ([^\s]+_t);/) {|m|
    if types[m[1]].nil?
      # git_time_t is handled explicitly in ffi_argument
      if m[1] != "git_time_t"
        defs.push({:transformed => "#integral_t #{m[1]}"})
      end
      types[m[1]] = true
    end
  }
  return defs
end

def transform_consts(contents)
  consts = []
  contents.scan(/#define ([^\s]+) (.+)$/) {|m|
    consts.push({:transformed => "#num    #{m[0]}"})
  }
  return consts
end

def transform_functions(contents)
  functions = []
  contents.scan(/^ *GIT_EXTERN\(([^\)]+)\) ([^\(]+)\(([^\)]*)\)/){|m|
    function_name = m[1]
    return_type = m[0]
    arguments = m[2]

    # The sqlite backend only works if
    # GIT2_SQLITE_BACKEND is defined.
    next if function_name.match(/sqlite/)

    functions.push({:transformed => "#ccall #{function_name} , #{ffi_arguments(arguments)} -> #{ffi_return(return_type)}"})
  }
  return functions
end

def inline_c_helper(fun, ret, args)
  arg_string = args.map{|a| ", #{a}"}.join('')
  ret.strip!
  if (ret == "void")
    "BC_INLINE#{args.length}VOID(#{fun}#{arg_string})"
  else
    "BC_INLINE#{args.length}(#{fun}#{arg_string},#{ret})"
  end
end

def parse_c_type_from_arg(arg)
  arg.strip.gsub(/[a-z_0-9A-Z]+$/, '')
end

def transform_inlines(contents)
  inlines = []
  contents.scan(/^ *GIT_INLINE\(([^\)]+)\) ([^\(]+)\(([^\)]*)\)/){|m|
    function_name = m[1]
    return_type = m[0]
    arguments = m[2]
    inlines.push({ :c_helper => inline_c_helper(function_name,
                                                return_type,
                                                arguments.split(',').map {|t| parse_c_type_from_arg(t)}),
                   :transformed => "#cinline #{function_name} , #{ffi_arguments(arguments)} -> #{ffi_return(return_type)}"})
  }
  return inlines
end

def write_c_helper_for(header, transforms)
  c_helpers = transforms.select{|t| t[:c_helper]}.map{|t| t[:c_helper]}
  return if c_helpers.empty?
  open(header, "r") {|fh|
    contents = fh.read
    open(module_c_helper(header), "w+") {|fh|
      fh << """
#include <bindings.cmacros.h>
#include <git2.h>

#{c_helpers.join("\n")}
"""
    }
  }
end

def scan_includes(contents)
  includes = []
  contents.scan(/#include "([^"]+)"/) {|i|
    includes.push(i[0])
  }
  return includes
end

def transform_headers
  import_headers = []
  Dir.glob('libgit2/include/git2/**/*.h').each {|header|
    next if header.match(/zlib.h/)
    header.strip!

    open(header, "r"){|fh|
      public_structs = []
      public_functions = []

      contents = fh.read
      transforms = [transform_typedefs(contents),
                    transform_consts(contents),
                    transform_enums(contents),
                    transform_structs(contents),
                    transform_functions(contents),
                    transform_inlines(contents)].flatten
      if transforms.length > 0
        includes = scan_includes(contents)
        write_c_helper_for(header, transforms)
        import_headers.push(header)
        open(module_path(header), "w+"){|fh|
          fh << """#{module_header(header)}
#{includes.map{|i| "import #{module_name(i)}"}.join("\n")}
"""
          transforms.each{|t|
            if t[:original]
              fh << "{- #{t[:original]} -}\n"
            end
            fh << "#{t[:transformed]}\n"
          }
        }
      end
    }
  }
  return import_headers
end

def export_modules(headers)
  headers.map{|h| "    module #{module_name(h)}"}.join(",\n")
end

def fill_toplevel(import_headers)
  open("Bindings/Libgit2.hs", "w+") {|fh|
    fh << """
module Bindings.Libgit2 (
#{export_modules(import_headers)}) where
"""
    import_headers.each {|header|
      header.strip!
      fh << "import #{module_name(header)}\n"
    }
  }
  return import_headers
end

def create_directory_structure
  FileUtils.rm_rf("Bindings/Libgit2")
  FileUtils.mkdir_p("Bindings/Libgit2")
end

UNIX_DIRS    = ['libgit2/src/unix']
WIN32_DIRS   = ['libgit2/src/win32']
SOURCE_DIRS  = ['libgit2/src',
                'libgit2/src/block-sha1',
                'libgit2/deps/zlib']
INCLUDE_DIRS = ['libgit2/include'] + SOURCE_DIRS
EXTRA_DIRS   = ['libgit2/include',
                'libgit2/include/git2'] +
                SOURCE_DIRS + UNIX_DIRS + WIN32_DIRS

def paths(pattern, dirs)
  dirs.map{|d| Dir.glob("#{d}/#{pattern}")}.flatten
end

def indent_join(indent, array)
  array.map{|x| "#{indent}#{x}"}.join("\n")
end

def extra_sources()
  paths('*', ['tests/**']) +
  paths('*.h', EXTRA_DIRS)
end

def c_sources()
  paths '*.c', SOURCE_DIRS
end

def c_sources_unix()
  paths '*.c', UNIX_DIRS
end

def c_sources_win32()
  paths '*.c', WIN32_DIRS
end

def fill_cabal(headers)
  open("hlibgit2.cabal", "w+") {|fh|
    fh <<
"""
Name:                hlibgit2
Version:             0.1
Synopsis:            bindings to libgit2
Description:         Generated bindings to libgit2.
License-file:        LICENSE
License:             GPL-2
Author:              Sakari Jokinen, Jacob Stanley
Maintainer:          sakariij@gmail.com
Build-Type:          Simple
Cabal-Version:       >=1.10
Category:            FFI

extra-source-files:
#{indent_join "  ", extra_sources}

Source-repository head
  type: git
  location: git://github.com/sakari/hlibgit2.git

Source-repository this
  type: git
  location: git://github.com/sakari/hlibgit2.git
  tag: hlibgit2-0.1

test-suite smoke
  default-language: Haskell98
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: tests
  build-depends:
    base >=3,
    hlibgit2,
    process

library
  default-language: Haskell98
  default-extensions:
    ForeignFunctionInterface
  build-depends:
    base >= 3 && < 5,
    bindings-DSL >= 1.0.9 && < 1.1
  exposed-modules:
    Bindings.Libgit2
#{headers.map{|h| "    #{module_name(h)}"}.join("\n")}
  c-sources:
#{c_helpers(headers).map{|h| "    #{h}"}.join("\n")}

  include-dirs:
#{indent_join "    ", INCLUDE_DIRS}
  c-sources:
#{indent_join "    ", c_sources}

  if os(windows)
    cc-options: -DGIT_WIN32 -DNO_VIZ
    include-dirs:
#{indent_join "      ", WIN32_DIRS}
    c-sources:
#{indent_join "      ", c_sources_win32}
  else
    include-dirs:
#{indent_join "      ", UNIX_DIRS}
    c-sources:
#{indent_join "      ", c_sources_unix}
"""
  }
end

create_directory_structure
fill_cabal(fill_toplevel(transform_headers))
