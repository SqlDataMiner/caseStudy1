library(dplyr)
library(tidyverse)

# Class Definitions
# A class to hold each weather stations data
setClass("weatherStationData", slots=list(name="character", aboveMeanSeaLevelMeters="numeric",
                                          easterly="character", northerly="character",
                                          lat="numeric", lon="numeric", data="data.frame"))

# A class representing the result of a parsed element
setClass("dataElement", slots=list(value="numeric", isEstimate="logical",
                                   wasSunMeasuredUsingCambellStokesRecorder="logical"))

# A class representing the download information for a weatherstation
setClass("downloadInfo", slots=list(url="character", fileName="character"))

# Function Definitions
#downloads the data for a given weatherstation name
downloadData <- function(weatherStationName){
  downloadInfo <- createDownloadInfo(weatherStationName)
  download.file(downloadInfo@url, str_glue("data/{downloadInfo@fileName}"))
}

# Strips non-alpha chars out of name and makes lower case.
# Doing this allows us to take more names from user and not have
# to pre-format them which just risks us making a mistake
formatWeatherStationNameForDownload <- function (weatherStationName) {
  str_replace_all(tolower(weatherStationName), '[^[:alpha:]]', '')
}

# This returns a object from an expected numeric element to parse.
# We should never lose information at the parsing stage
# the behaviour is:
#    (1) if the data is missing then all elements are NA (for null vs NA see: https://www.r-bloggers.com/2010/04/r-na-vs-null/)
#         NA can be coerced into a type and null can not; hence we must use NA
#    (2) if the data is not missing and is marked with a # then wasSunMeasuredUsingCambellStokesRecorder=True
#           else wasSunMeasuredUsingCambellStokesRecorder=FALSE
#    (3) if the data is not missing and is marked with a * then isEstimate=TRUE else isEstimate=False
parseExpectedNumericDataElement <- function(element) {
  if(element == '---') {
    result <- new("dataElement", value = NA_real_, isEstimate = NA, wasSunMeasuredUsingCambellStokesRecorder = NA)
  } else {
    isEstimate <-  grepl("*", element, fixed=TRUE)
    wasSunMeasuredUsingCambellStokesRecorder <- grepl("#", element, fixed =TRUE)
    value <- as.numeric(str_extract(element, "\\(?[0-9.-]+\\)?"))
    result <-  new("dataElement", value = value, isEstimate = isEstimate,
                   wasSunMeasuredUsingCambellStokesRecorder = wasSunMeasuredUsingCambellStokesRecorder)
  }
  result
}

# download function to specify where to download from and the filename to download to.
createDownloadInfo <- function (weatherStationName) {
  weatherStationsDownloadName <- formatWeatherStationNameForDownload(weatherStationName)
  fileName <- str_glue("{weatherStationsDownloadName}data.txt")
   new("downloadInfo", url=str_glue("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/{fileName}"), fileName=fileName)
}

# Reads in a file and parse the data to create an instance of weatherStationData (which is defined by the class)
# The file is creatly whitespace seperated and to parse so that we do not lose the estimated values or the sun measuring instrumentation
# we either need to:
#   1) use a regex which can match numbers and the indicators (i.e. '#' and '*') then parse the result using further regexs.
#   2) Split on whitespace, select by ordinal and parse
#  Both work and commerically you would have tests on input rejecting abnormal file formats/or saving for investigation deponding on application.
# We have chosen approach two.
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
  locationName <- rep(name, times=vectorLength-1)
  latitude <- rep(lat, times=vectorLength-1)
  longatude <- rep(lon, times=vectorLength-1)
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

  # ignore header rows and loop over lines in file
  # for each line parse assigning each of the constituent elements value to it's vector ordinal
  startAtLine <- numberOfHeaderlines + 1
  for ( i in startAtLine:length(lines)) {
    currentLine <- lines[i]
    #split line on whitespace but leaves a starting empty string
    lineVector <- unlist(strsplit(currentLine, split="\\s+"))
    # remove empty string which the split produces
    lineVector <- lineVector[lineVector != ""]
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
  dataFrame <- data.frame(name=locationName, lat=latitude, lon=longatude,
                          years=years, months=months, maxTempDegreesC=maxTemp,
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

# The main program which runs the data processing
main <- function(){
}
# The list of weather stations, will format on processing to make extending the list less error prone.
weatherStations <- c('Sheffield', 'Yeovilton', 'Durham', 'Heathrow', 'Newton Rigg',
                     'Cambridge', 'Bradford', 'Oxford', 'Suttonbonington', 'Waddington', 'Manston', 'Shawbury', 'Ross-on-Wye')

#hide annoying output of lapply function calls by assigning to variable
downloads <- lapply(weatherStations, downloadData)

#apply a file pattern in case we want to store other data in the folder later.
filesToProcess <- list.files(path="data", full.names=TRUE, pattern="^(.*)data.txt$" )

#parse the data.
weatherStationDataList <- lapply(filesToProcess, parseWeatherStationData)

#union all the data together

#run the program
