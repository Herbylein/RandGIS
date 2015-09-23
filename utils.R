# handling file names // combine with basename and/or dirname
sub("^([^.]*).*", "\\1", 'filename.extension')
[1] "filename"
sub("^([^.]*).*", "\\1", 'filename.extension.and.more')
[1] "filename"
sub("^([^.]*).*", "\\1", 'filename without extension')
