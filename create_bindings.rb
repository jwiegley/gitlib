#!/usr/bin/ruby

require 'fileutils'

def ffi_type(type)
  types = {
    "int" => "CInt",
    "char" => "CChar",
    "unsigned" => "CInt",
    "void" => "()",
    "size_t" => "CInt"
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

def fill_types_module
  open("libgit2/src/git2/types.h", "r") {|fh|
    open(module_path("types.h"), "w+") {|mod|
      mod << module_header("types.h")
      fh.read.scan(/typedef struct ([^ ]+)/) {|type|
        mod << "#opaque_t #{type}\n"
      }
    }
  }
end

def module_basename(header)
  header = File.basename(header)
  header.gsub(/^[a-z]|_[a-z]/) {|a| a.upcase }.gsub(/\.h$/, '')
end
def module_name(header)
  "Bindings.Git.#{module_basename(header)}"
end
def module_path(header)
  "Bindings/Git/#{module_basename(header)}.hsc"
end

def fill_prototypes
  import_headers = []
  `find libgit2/src -name '*.h'`.each {|header|
    header.strip!
    open(header, "r"){|fh|
      public_functions = []
      fh.read.scan(/^ *GIT_EXTERN[^;]+/) {|prototype|
        m = prototype.match(/GIT_EXTERN\(([^\)]+)\) ([^\(]+)\(([^\)]*)\)/)
        raise "suprisingly formatted prototype '#{prototype}'" if not m    
        public_functions.push(:function_name => m[2],
                              :arguments => m[3],
                              :return_type => m[1],
                              :prototype => prototype)
      }
      if public_functions.length > 0
        import_headers.push(header)
        open(module_path(header), "w+"){|fh|
          fh << """#{module_header(header)}
import #{module_name("types.h")}

"""
          public_functions.each{|p|
            fh << """-- #{p[:prototype]}
#ccall #{p[:function_name]} , #{ffi_arguments(p[:arguments])} -> #{ffi_return(p[:return_type])}

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
  open("Bindings/Git.hs", "w+") {|fh|
    fh << """
module Bindings.Git (
#{export_modules(import_headers)}) where
"""
    import_headers.each {|header|
      header.strip!
      fh << "import #{module_name(header)}\n"
    }
  }
end

def create_directory_structure
  FileUtils.mkdir_p("Bindings/Git")
end

create_directory_structure
fill_types_module
fill_toplevel(fill_prototypes)


