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

# get administrative boders (world) : http://www.gadm.org/
us <- getData("GADM", country="USA", level=1)
# extract states (need to uppercase everything)
nestates <- c("Maine", "Vermont", "Massachusetts", "New Hampshire" ,"Connecticut",
         "Rhode Island","New York","Pennsylvania", "New Jersey",
         "Maryland", "Delaware", "Virginia", "West Virginia")

ne = us[match(toupper(nestates),toupper(us$NAME_1)),]

