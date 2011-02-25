#!/usr/bin/ruby

require 'fileutils'

def ffi_type(type)
  types = {
    "int" => "CInt",
    "char" => "CChar",
    "unsigned" => "CInt",
    "void" => "()",
    "size_t" => "CInt",
    "time_t" => "CInt"
  }
  if types[type]
    return types[type]
  end
  return "<#{type}>"
end

def ffi_return(arg)
  if ffi_argument(arg) == "()"
    return "IO ()"
  end
  return "IO (#{ffi_argument(arg)})"
end

def ffi_argument(arg)
  arg.gsub!(/const/, '')
  arg.gsub!(/unsigned int/, 'unsigned')
  if (m = arg.match(/^ *([^ ]+) *\*\*/))
    return "Ptr (Ptr #{ffi_type(m[1])})" 
  end
  if (m = arg.match(/^ *([^ ]+) *\*/))
    return "Ptr #{ffi_type(m[1])}" 
  end
  if (m = arg.match(/^ *([^ ]+)/))
    return ffi_type(m[1])
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

def module_name(header)
  "Bindings.Libgit2.#{module_basename(header)}"
end
def module_path(header)
  "Bindings/Libgit2/#{module_basename(header)}.hsc"
end

def parse_structs(string)  
  string.scan(/typedef struct [^ \{]+ ([^ \{]+);/) {|type|
    yield({:opaque => type })
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
    fieldname = field.match(/([^* ]+);/)
    yield(f[1].strip, fieldname[1])
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
      parse_struct_fields(p[:body]) {|type, field|
        structlist.push("#field    #{field} , #{ffi_argument(type)}")
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
      defs.push({:transformed => "#integral_t #{m[1]}"})
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
  contents.scan(/^ *GIT_(EXTERN|INLINE)\(([^\)]+)\) ([^\(]+)\(([^\)]*)\)/){|m|
    function_name = m[2]
    return_type = m[1]
    arguments = m[3]
    functions.push({:transformed => "#ccall #{function_name} , #{ffi_arguments(arguments)} -> #{ffi_return(return_type)}"})
  }
  return functions
end

def transform_headers
  import_headers = []
  `find libgit2/src/git2 -name '*.h'`.each {|header|
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
                    transform_functions(contents)].flatten
      if transforms.length > 0
        includes = []
        contents.scan(/#include "([^"]+)"/) {|i| 
          includes.push(i[0])
        }
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
  FileUtils.mkdir_p("Bindings/Libgit2")
end

def fill_cabal(headers)
  open("hlibgit2.cabal", "w+") {|fh|
    fh <<
"""
Name:                hlibgit2
Version:             0.0
Description:         bindings to libgit2
License-file:        LICENSE
Author:              Sakari Jokinen
Maintainer:          sakariij@gmail.com
Build-Type:          Simple
Cabal-Version:       >=1.10

test-suite smoke
  default-language: Haskell98
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: tests
  build-depends:
    base >=3,
    hlibgit2,
    process
  extra-libraries: git2

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
"""
  }
end

create_directory_structure
fill_cabal(fill_toplevel(transform_headers))
