source("dataLoad.R")

outputdir = "data"

#Note that the function dataLoad(..) is defined in the file dataLoad.R
load <- dataLoad(outputdir)
data <- load@alldata
yearsOfInterest <- load@yearsOfInterest
weatherStations <- load@weatherStations
