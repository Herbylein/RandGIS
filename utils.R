# handling file names // combine with basename and/or dirname
sub("^([^.]*).*", "\\1", 'filename.extension')
[1] "filename"
sub("^([^.]*).*", "\\1", 'filename.extension.and.more')
[1] "filename"
sub("^([^.]*).*", "\\1", 'filename without extension')

# check if files ase empty
info <- info = file.info(fileNames)
fileNames.notempty = rownames(info[info$size != 0, ])
fileNames.empty = rownames(info[info$size == 0, ])
