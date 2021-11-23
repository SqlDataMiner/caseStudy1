library(dplyr)
library(tidyverse)

# Class Definitions
setClass("weatherStationData", slots=list(name="character", aboveMeanSeaLevelMeters="numeric",
                                          easterly="character", northerly="character",
                                          lat="numeric", lon="numeric", data="data.frame"))

setClass("dataElement", slots=list(value="numeric", isEstimate="logical",
                                   wasSunMeasuredUsingCambellStokesRecorder="logical"))

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

# this returns a vector of results from an expected numeric element to parse.
# the vector returned is: c(value, isEstimate, wasSunMeasuredUsingCambellStokesRecorder)
# the behaviour is:
#    (1) if the data is missing then all elements are null
#    (2) if the data is not missing and is marked with a # then wasSunMeasuredUsingCambellStokesRecorder=True
#           else wasSunMeasuredUsingCambellStokesRecorder=FALSE
#    (3) if the data is not missing and is marked with a * then isEstimate=TRUE else isEstimate=False
parseExpectedNumericDataElement <- function(element) {
  if(element == '---') {
    #TODO HOW DO WE SET NULLS IN OUR CLASS
    result <- new("dataElement", value = 0, isEstimate = FALSE, wasSunMeasuredUsingCambellStokesRecorder = FALSE)
  } else {
    isEstimate <-  grepl("*", element, fixed=TRUE)
    wasSunMeasuredUsingCambellStokesRecorder <- grepl("#", element, fixed =TRUE)
    value <- as.numeric(str_extract(element, "\\(?[0-9.-]+\\)?"))
    result <-  new("dataElement", value = value, isEstimate = isEstimate,
                   wasSunMeasuredUsingCambellStokesRecorder = wasSunMeasuredUsingCambellStokesRecorder)
  }
  result
}

# Returns a vector which has the url as first element and filename as second element then
# we can use this in download function to specify where to download from and the filename to download to.
createDownloadVector <- function (weatherStationName) {
  weatherStationsDownloadName <- formatWeatherStationNameForDownload(weatherStationName)
  fileName <- str_glue("{weatherStationsDownloadName}data.txt")
  c(str_glue("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/{fileName}"), fileName)
}

# Reads in a file and parse the data to create an instance of weatherStationData (which is defined by the class)
# Note not the most efficent file read is memory intensive byecause we read all the file in in one go
# but this is a small dataset so no point optomising
parseWeatherStationData <- function(fileName) {
  # we need to create a file "Handle" which allows the program to read the file
  # we specify open="r" to say we want to only read the file
  fileHandle <- file(fileName, open="r")
  # Settings warnings to false as readLines expects a terminating end of line character which is not the data format.
  lines <- readLines(fileHandle, warn=FALSE)
  name <- lines[1]
  locationLine <- lines[2]

  #parse in the data we need for location split on whitespace
  locationVector <- unlist(strsplit(locationLine, split = "\\s+"))
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
  numberOfHeaderlines <- 7
  vectorLength <- length(lines) - numberOfHeaderlines
  years <- c(vectorLength)
  months <-c(vectorLength)
  maxTemp <- c(vectorLength)
  wasMaxTempEstimated <- c(vectorLength)
  minTemp <- c(vectorLength)
  wasMinTempEstimated <- c(vectorLength)
  airFrost <- c(vectorLength)
  wasAirFrostEstimated <- c(vectorLength)
  rainfall <- c(vectorLength)
  wasRainfallEstimated <- c(vectorLength)
  sun <- c(vectorLength)
  wasSunMeasuredUsingCambellStokesRecorder <- c(vectorLength)
  wasSunEstimated <- c(vectorLength)
  isProvisionalRecord <- c(vectorLength)

  startAtLine <- numberOfHeaderlines + 1
  for ( i in startAtLine:length(lines)) {
    currentLine <- lines[i]
    #split line on whitespace
    lineVector <- unlist(strsplit(currentLine, split="\\s+"))
    #TODO YEAR DATA IS NOT SET CORRECT AND NOR IS BOOLEAN
    years[i - startAtLine] <-  lineVector[1]
    months[i - startAtLine] <-  lineVector[2]
    tempMax <- parseExpectedNumericDataElement(lineVector[3])
    maxTemp[i - startAtLine] <-  tempMax@value
    wasMaxTempEstimated[i - startAtLine] <-  tempMax@isEstimate
    tempMin <- parseExpectedNumericDataElement(lineVector[4])
    minTemp[i - startAtLine] <- tempMin@value
    wasMinTempEstimated[i - startAtLine] <-  tempMin@isEstimate
    frost <- parseExpectedNumericDataElement(lineVector[5])
    airFrost[i - startAtLine] <- frost@value
    wasAirFrostEstimated[i - startAtLine] <- frost@isEstimate
    rain <- parseExpectedNumericDataElement(lineVector[6])
    rainfall[i - startAtLine] <- rain@value
    wasRainfallEstimated[i - startAtLine] <- rain@isEstimate
    s <- parseExpectedNumericDataElement(lineVector[7])
    sun[i - startAtLine] <-  s@value
    wasSunEstimated[i - startAtLine] <- s@isEstimate
    wasSunMeasuredUsingCambellStokesRecorder[i - startAtLine] <- s@wasSunMeasuredUsingCambellStokesRecorder
    isProvisional <- grepl("Provisional", currentLine, fixed=TRUE)
    isProvisionalRecord[i - startAtLine] <- isProvisional
  }

#name columns with scales
  dataFrame <- data.frame(years=years, months=months, maxTempDegreesC=maxTemp,
                          wasMaxTempEstimated=wasMaxTempEstimated, minTempDegreesC=minTemp,
                          wasMinTempEstimated=wasMinTempEstimated, airFrostDays=airFrost,
                          wasAirFrostEstimated=wasAirFrostEstimated, rainfallmm=rainfall,
                          wasRainfallEstimated=wasRainfallEstimated, sunHours=sun,
                          wasSunEstimated=wasSunEstimated,
                          wasSunMeasuredUsingCambellStokesRecorder=wasSunMeasuredUsingCambellStokesRecorder,
                          isProvisionalRecord=isProvisionalRecord)

  close(fileHandle)

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
