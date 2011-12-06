library(XML)


odbAccess("dump.html", "worm", "/tmp/worm.R")
worm(species = "briggsae", list="",  upload="", .url = URI("http://www.wormbase.org"))

# Note the spaces here
# now not needed.
worm(list="AC3.8\240M7\240X\240IV\240III:1000..4000",  feature = "BLASTX Hits", dump = "Plain TEXT", upload=character(), .url = URI("http://www.wormbase.org"))

# To have the chromosomes in a file, place the string
#   AC3.8 M7 X IV III:1000..4000
# in the file Content/chromosomeList and specify that file name as the value
# for upload.
# 
worm(list='', feature = "BLASTX Hits", dump = "Plain TEXT", upload="../Content/chromosomeList", .url = URI("http://www.wormbase.org"))


# Alternatively, if there is no such file we can include the contents directly as the
# string.
worm(list='', feature = "BLASTX Hits", dump = "Plain TEXT", upload="AC3.8 M7 X IV III:1000..4000", .url = URI("http://www.wormbase.org"))
