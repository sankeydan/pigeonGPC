# filediff

filediff = function( in.folder, out.folder){
data.files = list.files( in.folder)
outp.files = list.files( out.folder)
files.diff = setdiff( substr( data.files, 1, nchar(data.files) -4),
                      substr( outp.files, 1, nchar(outp.files) -4))
return(files.diff)
}

