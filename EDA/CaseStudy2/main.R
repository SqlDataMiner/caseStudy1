library(dplyr)
library(tidyverse)

# Class Definitions
setClass("weatherStationData", slots=list(name="character", aboveMeanSeaLevelMeters="numeric",
                                          easterly="character", northerly="character",
                                          lat="numeric", lon="numeric", data="data.frame"))


# Function Definitions
downloadData <- function(weatherStationName){
  downloadVector <- createDownloadVector(weatherStationName)
  url <- downloadVector[1]
  fileName <- downloadVector[2]

  download.file(url, str_glue("data/{fileName}"))
}

formatWeatherStationNameForDownload <- function (weatherStationName) {
  str_replace_all(tolower(weatherStationName), '[^[:alpha:]]', '')
}

# Returns a vector which has the url as first element and filename as second element then
# we can use this in download function to specify where to download from and the filename to download to.
createDownloadVector <- function (weatherStationName) {
  weatherStationsDownloadName <- formatWeatherStationNameForDownload(weatherStationName)
  fileName <- str_glue("{weatherStationsDownloadName}data.txt")
  c(str_glue("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/{fileName}"), fileName)
}

# Reads in a file and parse the data to create an instance of weatherStationData (which is defined by the class)
parseWeatherStationData <- function(fileName) {
  # we need to create a file "Handle" which allows the program to read the file
  # we specify open="r" to say we want to only read the file
  fileHandle <- file(fileName, open="r")
  # Settings warnings to false as readLines expects a terminating end of line character which is not the data format.
  lines <- readLines(fileHandle, warn=FALSE)
  name <- lines[1]
  locationLine <- lines[2]
  close(fileHandle)

  #parse in the data we need for location
  locationVector <- unlist(strsplit(locationLine, split = " "))
  aboveMeanSeaLevelMeters <- as.numeric(str_replace_all(locationVector[8], '[^[:digit:]]', ''))
  easterly <- locationVector[2]
  # Remove comma could regex or substring but this is easy and works
  northerly <- str_replace_all(locationVector[3], ',', '')
  lat <- as.numeric(locationVector[5])
  # Remove comma could regex or substring but this is easy and works
  lon <- as.numeric(str_replace_all(locationVector[7], ',', ''))

  # parse the data we need for records we will:
  # 1) create empty vectors
  # 2) parse the data and populate the vectors
  # 3) create a data table
  years <- c()
  months <-c()
  maxTemp <- c()
  wasMaxTempEstimated <- c()
  minTemp <- c()
  wasMinTempEstimated <- c()
  af <- c()
  wasAfEstimated <- c()
  rain <- c()
  wasRainEstimated <- c()
  sun <- c()
  wasSunMeasuredUsingCambellStokesRecorder <- c()
  wasSunEstimated <- c()
  isProvisionalRecord <- c()

  dataFrame <- data.frame(years, months, maxTemp, wasMaxTempEstimated, minTemp, wasMinTempEstimated, af,
                          wasAfEstimated, rain, wasRainEstimated, sun, wasSunEstimated,
                          wasSunMeasuredUsingCambellStokesRecorder, isProvisionalRecord)

  new("weatherStationData", name=name, aboveMeanSeaLevelMeters=aboveMeanSeaLevelMeters, easterly=easterly,
      northerly=northerly, lat=lat, lon=lon, data=dataFrame)

}

# The program
weatherStations <- c('Sheffield', 'Yeovilton', 'Durham', 'Heathrow', 'Newton Rigg',
                     'Cambridge', 'Bradford', 'Oxford', 'Suttonbonington', 'Waddington', 'Manston', 'Shawbury', 'Ross-on-Wye')

#hide annoying output of lapply function calls by assigning to variable
downloads <- lapply(weatherStations, downloadData)

#apply a file pattern in case we want to store other data in the folder later.
filesToProcess <- list.files(path="data", full.names=TRUE, pattern="^(.*)data.txt$" )


weatherStationDataList <- lapply(filesToProcess, parseWeatherStationData)
