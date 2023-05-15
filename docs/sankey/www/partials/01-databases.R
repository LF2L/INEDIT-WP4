
# DIT = read.csv2("www/data/DIT.csv") 
# write_json(DIT, "www/data/DIT.json", pretty=TRUE, na = "null")

DIT <- fromJSON("www/data/DIT.json" ) # Options: simplifyDataFrame = TRUE

UL = read.csv2("www/data/DIT-UL.csv") 
UL = merge(DIT[1:2], UL, by="Process")
UL = UL %>% select(Phase, Process, Activity:Practices)


# Data for Consortium Enquetes Word
Data <- yaml.load_file("www/data/Data.yml")