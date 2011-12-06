a = htmlTreeParse("../test/dump.html", handlers =formElementHandlers())$values()
ee = a$elements[-c(11,12)]
checkFormArgs(ee, list(species="elegans", list="", upload="", feature="UTRs", flank5="0", flank3="0", relative=c("Chromosome", "Query Sequence"), orientation="Relative to feature", dump = "Plain TEXT"), FALSE)
