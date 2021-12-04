
library(shiny)
library(tidyverse)
library(hash)

# Cache the data to speed up application startup time.
useRDataIfExists <- TRUE
outputdir = "data"
rDataFile <-  str_glue("{outputdir}/data.RData")

# *** Class Definitions ***
# A class to hold each weather stations data
setClass("weatherStationData", slots=list(name="character", aboveMeanSeaLevelMeters="numeric",
                                          easterly="character", northerly="character",
                                          lat="numeric", lon="numeric", data="data.frame"))

# A class representing the result of a parsed element
setClass("dataElement", slots=list(value="numeric", isEstimate="logical",
                                   wasSunMeasuredUsingCambellStokesRecorder="logical"))

# A class representing the download information for a weatherstation
setClass("downloadInfo", slots=list(url="character", fileName="character"))

# *** Function Definitions ***
#downloads the data for a given weatherstation name
downloadData <- function(weatherStationName){
  downloadInfo <- createDownloadInfo(weatherStationName)
  download.file(downloadInfo@url, str_glue("{outputdir}/{downloadInfo@fileName}"))
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
    years[i - startAtLine] <-  as.integer(lineVector[1])
    months[i - startAtLine] <-  as.integer(lineVector[2])
    tempMax <- parseExpectedNumericDataElement(lineVector[3])
    maxTemp[i - startAtLine] <-  tempMax@value
    wasMaxTempEstimated[i - startAtLine] <-  tempMax@isEstimate
    tempMin <- parseExpectedNumericDataElement(lineVector[4])
    minTemp[i - startAtLine] <- tempMin@value
    wasMinTempEstimated[i - startAtLine] <-  tempMin@isEstimate
    frost <- parseExpectedNumericDataElement(lineVector[5])
    airFrost[i - startAtLine] <- as.integer(frost@value)
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

generatePlot <- function(input, unifiedWeatherDataSet){
  #Not the most extensible code but its simple and suffices for 3 cases
  # if more options would use a map of functions to resolve method.
  if (input$investigationType == "seasons"){

  } else if (input$investigationType == "location"){

  }else {
    #must be investigating by time
    rowsOfInterest <- unifiedWeatherDataSet %>%
      filter(years == as.numeric(input$yearFrom) |
               years == as.numeric(input$yearTo))
    # metricOfInterest <- input$investigationMetric

    plot <- ggplot(rowsOfInterest, aes(monthNames))
    plot + geom_bar(aes(y=rainfallmm))

    plot
  }
}

# create a class for holding all the metric releated information
setClass("metricDefinition", slots=list(metricName="character", aggregation="function", format="function",
                                        scale="character", name="character",
                                        minimumAdjective="character", maximumAdjective="character"))
# hash allows us to map from string to an object.  Saves us having to write branching statements (if else etc) everywhere.
metricDefinitions <- hash()
metricDefinitions[["rainfallmm"]] <- new("metricDefinition", metricName="rainfall", aggregation = mean, format= as.numeric, scale="mm",
                               minimumAdjective="least", maximumAdjective="greatest")
metricDefinitions[["maxTempDegreesC"]] <- new("metricDefinition", metricName="", aggregation = mean, format= as.numeric, scale="degrees celcius",
                                              minimumAdjective="lowest", maximumAdjective="highest")
metricDefinitions[["minTempDegreesC"]] <- new("metricDefinition", metricName="", aggregation = mean, format= as.numeric, scale="degrees celcius",
                                              minimumAdjective="lowest", maximumAdjective="highest")
metricDefinitions[["sunHours"]] <- new("metricDefinition", metricName="", aggregation = sum, format= as.numeric, scale="hours",
                                       minimumAdjective="fewest", maximumAdjective="most")
metricDefinitions[["airFrostDays"]] <-  new("metricDefinition", metricName="", aggregation = sum, format= as.integer, scale="days",
                                            minimumAdjective="fewest", maximumAdjective="most")


#decide on the appropriate aggregation function by metric
aggregationFunction <- function(metric){
  metricDefinitions[[metric]]@aggregation
}

#Decide on the appropriate format function by metric
formatFunction <- function(metric){
  metricDefinitions[[metric]]@format
}

# Functions which create output data
seasonalSummaryTable <- function(data, yearFrom, yearTo, metric) {
  data %>%
    filter(years >= yearFrom) %>%
    filter(years <= yearTo) %>%
    group_by(name, season, years) %>%
    summarise(typical=aggregationFunction(metric)(get(metric))) %>%
    mutate(typical=formatFunction(metric)(typical), years = as.integer(years)) %>%
    pivot_wider(names_from=season, values_from=typical)%>%
    arrange(name, years)
}

seasonalHeaderGenerate <- function(metric) {
  str_glue("<h1>Seasonal {metricDefinitions[[metric]]@metricName}</h1>")
}

seasonalDescriptionGenerate <- function(metric) {
  #typical values will use mean to aggreate identical tests equality on our functions.
  isTypicalValues <- identical(hashAgg[[metric]], mean)
  print(isTypicalValues)
  if (isTypicalValues){
    s <- "Typical daily values for seasons of"
  }else{
    s <- "Total values for seasons of "
  }

  str_glue("{s} {metricDefinitions[[metric]]@metricName} ({metricDefinitions[[metric]]@scale})")
}
seasonalSummaryGenerate <- function(data, metric,
                        yearFrom, yearTo){
  #find min
  x <- data %>%
    filter(years >= yearFrom) %>%
    filter(years <= yearTo) %>%
    group_by(name, season, years) %>%
    summarise(min=min(get(metric)), max=max(get(metric)))
  # find max

  #find how many data points were estimates
  y <- data %>%
    filter(years >= yearFrom) %>%
    filter(years <= yearTo)


  str_glue("some summary goes here {metricDefinitions[[metric]]@metricName}")

}



#*** Data ***
# The list of weather stations, will format on processing to make extending the list less error prone.
weatherStations <- c('Sheffield', 'Yeovilton', 'Durham', 'Heathrow', 'Newton Rigg',
                     'Cambridge', 'Bradford', 'Oxford', 'Suttonbonington', 'Waddington', 'Manston', 'Shawbury', 'Ross-on-Wye')

monthNames <- c("January", "February", "March", "April", "May",
                "June", "July", "August", "September", "October",
                "November", "December")

monthNamesShort <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                     "Sep", "Oct", "Nov", "Dec")

# seasons as defined by met office:   https://www.metoffice.gov.uk/weather/learn-about/met-office-for-schools/other-content/other-resources/our-seasons
seasons <- data.frame(season=c("winter","winter", "spring","spring", "spring",
                               "summer", "summer", "summer", "autumn", "autumn",
                               "autumn", "winter"),
                      months=seq(1, 12, 1),
                      monthNames=monthNames,
                      monthNamesShort=monthNamesShort)


# Define the user interface
# Note the commas are required for arguments to functions.
# Shiny uses a fluent interface style of programming whereby functions
# such as "fluidPage, "sidePanel", mainPanel etc take functions
# as arguments hence the commas can seem misleading at times.
ui <- fluidPage(
  titlePanel("Primary school weather investigation"),

  sidebarPanel(
    # First control: a widget to select the interval type
    selectInput(inputId = "investigationType",
                label = "Investigate weather by:",
                choices = c("Months and seasons" = "seasons",
                            "Location" = "location",
                            "Time" = "time")),

    selectInput(inputId = "investigationMetric",
                label = "What to investigate:",
                choices =c("Rainfall" = "rainfallmm",
                           "Max Temperature" = "maxTempDegreesC",
                           "Min Temperature" = "minTempDegreesC",
                           "Air Frost" = "airFrostDays",
                           "Daylight" = "sunHours")),

    selectizeInput("yearfrom", "year from", choices = c()),
    selectizeInput("yearto", "years to", choices = c())
  ),
  mainPanel(
    tabsetPanel(

      tabPanel("Bar chart and commentry",
               plotOutput("mainPlot")
      ),
      tabPanel("Tables of data",
               htmlOutput("seasonsHeader"),
               htmlOutput("seasonsDescription"),
               tableOutput("seasonsTable"),
               htmlOutput("seasonsSummary"),
      )
    )
  )

)

# Define the "server": where the computations are done/plots generated etc.
# given the input values (input$interval, input$lvl, input$method)
# from the user interface

server <- function(input, output) {
  #Use an RData file if one exists, else download data and create one.
  #If we don't do this the program is really slow on loading and
  # unresponsive
  if (useRDataIfExists && file.exists(rDataFile)) {
    load(rDataFile)
  } else {
    dir.create(outputdir)
    #hide annoying output of lapply function calls by assigning to variable
    downloads <- lapply(weatherStations, downloadData)

    #apply a file pattern in case we want to store other data in the folder later.
    filesToProcess <- list.files(path=outputdir, full.names=TRUE, pattern="^(.*)data.txt$" )

    #parse the data.
    weatherStationDataList <- lapply(filesToProcess, parseWeatherStationData)

    #create a list of dataframes which will allow us to create a unified dataset
    dataTables <- lapply(weatherStationDataList, function(station){station@data})


    #union all the dataframes together using merge function and add season to the rows.
    unifiedWeatherDataSet <-  Reduce(function(x,y) {merge(x,y, all=TRUE)}, dataTables) %>%
      inner_join(seasons, by="months")

    #find the years needed for dropdown
    yearsOfInterest <- unifiedWeatherDataSet %>%
      select(years) %>%
      distinct(years) %>%
      arrange(desc(years))

    weatherStationsOrdered <- sort(weatherStations)

    save(unifiedWeatherDataSet, yearsOfInterest, weatherStationsOrdered, file=rDataFile)

  }

  seasonTableGeneration <- reactive({
    seasonalSummaryTable(unifiedWeatherDataSet, input$yearfrom, input$yearto, input$investigationMetric)
  })

  # update dropdowns with data required
  updateSelectInput(inputId = "yearfrom", choices = yearsOfInterest)
  updateSelectInput(inputId = "yearto", choices = yearsOfInterest)



  output$mainPlot <- renderPlot({
    generatePlot(input, unifiedWeatherDataSet)
  })

  output$seasonsTable <- renderTable({ seasonTableGeneration() })
  output$seasonsHeader <-renderText({  seasonalHeaderGenerate(input$investigationMetric)  })
  output$seasonsDescription <- renderText({seasonalDescriptionGenerate(input$investigationMetric)})
  output$seasonsSummary <- renderText(seasonalSummaryGenerate(unifiedWeatherDataSet, input$investigationMetric,
                                                              input$yearfrom, input$yearto))
}
# Run the application
shinyApp(ui = ui, server = server)

