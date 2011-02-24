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
 
def pick_enums(contents)
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
    enums.push({ :enum => enum_definition.join("\n"), 
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

def pick_structs(string)
  string.gsub!(/\/\*[^\/]*\*\//, '')
  structs = []
  parse_structs(string) {|p|
    if p[:opaque]
      structs.push({ :original => p[:origanal],
                     :struct => "#opaque_t #{p[:opaque]}"
                   })
    else
      structlist = ["#starttype #{p[:name]}"]
      parse_struct_fields(p[:body]) {|type, field|
        structlist.push("#field    #{field} , #{ffi_argument(type)}")
      }
      structlist.push("#stoptype")
      structs.push({ :struct => structlist.join("\n"), 
                     :original => p[:original] })
    end
  }
  return structs
end

def pick_typedefs(contents)
  defs = []
  types = {}
  contents.scan(/typedef ([^\s]+) ([^\s]+_t);/) {|m|
    if types[m[1]].nil?
      defs.push({:original => "", :typedef => "#integral_t #{m[1]}"})
      types[m[1]] = true 
    end
  }
  return defs
end

def pick_consts(contents)
  consts = []
  contents.scan(/#define ([^\s]+) ([^\s]+)$/) {|m|
    consts.push({:original => m[0], :const => "#num    #{m[0]}"})
  }
  return consts
end

def fill_prototypes
  import_headers = []
  `find libgit2/src/git2 -name '*.h'`.each {|header|
    header.strip!
    open(header, "r"){|fh|
      public_structs = []
      public_functions = []
      
      contents = fh.read
      public_typedefs = pick_typedefs(contents)
      public_consts = pick_consts(contents)
      public_enums = pick_enums(contents)
      public_structs = pick_structs(contents)
      contents.scan(/^ *GIT_EXTERN[^;]+/) {|prototype|
        m = prototype.match(/GIT_EXTERN\(([^\)]+)\) ([^\(]+)\(([^\)]*)\)/)
        raise "suprisingly formatted prototype '#{prototype}'" if not m    
        public_functions.push(:function_name => m[2],
                              :arguments => m[3],
                              :return_type => m[1],
                              :prototype => prototype)
      }
      if public_functions.length > 0 or public_enums.length > 0 or public_structs.length > 0 or public_consts.length > 0 or public_typedefs.length > 0
        includes = []
        contents.scan(/#include "([^"]+)"/) {|i| 
          includes.push(i[0])
        }
        import_headers.push(header)
        open(module_path(header), "w+"){|fh|
          fh << """#{module_header(header)}
#{includes.map{|i| "import #{module_name(i)}"}.join("\n")}
"""
          public_functions.each{|p|
            fh << """{- #{p[:prototype]} -}
#ccall #{p[:function_name]} , #{ffi_arguments(p[:arguments])} -> #{ffi_return(p[:return_type])}

"""
          }
          public_typedefs.each{|p|
            fh << """{- #{p[:original]} -}
#{p[:typedef]}
"""
          }
          public_consts.each{|p|
            fh << """{- #{p[:original]} -}
#{p[:const]}
"""
          }
          public_enums.each{|p|
            fh << """{- #{p[:original]} -}
#{p[:enum]}
"""
          }
          public_structs.each{|p|
            fh << """{- #{p[:original]} -}
#{p[:struct]}
"""
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
Cabal-Version:       >=1.2

library
  build-depends:
    base >= 3 && < 5,
    bindings-DSL >= 1.0.7 && < 1.1
  exposed-modules:
    Bindings.Libgit2
#{headers.map{|h| "    #{module_name(h)}"}.join("\n")}
  extensions:
    ForeignFunctionInterface
"""
  }
end

create_directory_structure
fill_cabal(fill_toplevel(fill_prototypes))
