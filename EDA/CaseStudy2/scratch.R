source("dataLoad.R")

outputdir = "data"


#This will create 3 global variables:
# data - with the main dataset
# yearsOfInterest - with all the years in the dataset
# weatherStations - a list of weatherstations
load <- dataLoad(outputdir)
data <- load@data
yearsOfInterest <- load@yearsOfInterest
weatherStations <- load@weatherStations
