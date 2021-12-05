#lets make it easy for the teacher and install packages if they don't exist.
if(!(require(shiny))){install.packages('shiny')}
if(!(require(hash))){install.packages('hash')}
if(!(require(tidyverse))){install.packages('tidyverse')}

library(shiny)
library(tidyverse)

# *** DOWNLOAD, EXTRACT, TRANSFORM CODE START *********
# Cache the data to speed up application startup time.
outputdir = "data"
useRDataIfExists <- TRUE

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
                      mm=seq(1, 12, 1),
                      monthName=monthNames,
                      monthNamesShort=monthNamesShort)

#*** Class definitions ***
setClass("LoadData", slots=list(alldata="data.frame", weatherStations="character", weatherStationsProperCase="character", yearsOfInterest="data.frame" ))

# *** Class Definitions ***
# A class to hold each weather stations data
setClass("weatherStationData", slots=list(name="character", aboveMeanSeaLevelMeters="numeric",
                                          easterly="character", northerly="character",
                                          lat="numeric", lon="numeric", data="data.frame"))

# A class representing the result of a parsed element
setClass("dataElement", slots=list(value="numeric", isEstimate="logical",
                                   wasSunMeasuredUsingKippAndZonenRecorder="integer"))

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
    result <- new("dataElement", value = NA_real_, isEstimate = NA, wasSunMeasuredUsingKippAndZonenRecorder = NA_integer_)
  } else {
    isEstimate <-  grepl("*", element, fixed=TRUE)
    if (grepl("#", element, fixed =TRUE)) {
      wasSunMeasuredUsingKippAndZonenRecorder <- as.integer(1)
    } else {
      wasSunMeasuredUsingKippAndZonenRecorder <- as.integer(0)
    }
    value <- as.numeric(str_extract(element, "\\(?[0-9.-]+\\)?"))
    result <-  new("dataElement", value = value, isEstimate = isEstimate,
                   wasSunMeasuredUsingKippAndZonenRecorder = wasSunMeasuredUsingKippAndZonenRecorder)
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
  placeName <- str_replace_all(basename(fileName), 'data.txt', '')
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
  vectorLength <- length(lines) - numberOfHeaderlines + 1
  locationName <- rep(name, times=vectorLength - 1)
  latitude <- rep(lat, times=vectorLength - 1)
  longatude <- rep(lon, times=vectorLength - 1)
  alt <- rep(aboveMeanSeaLevelMeters, times=vectorLength - 1)
  place <- rep(placeName, times=vectorLength - 1)
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
  wasSunMeasuredUsingKippAndZonenRecorder <- c(vectorLength)
  wasSunEstimated <- c(vectorLength)
  isProvisionalRecord <- c(vectorLength)

  # ignore header rows and loop over lines in file
  # for each line parse assigning each of the constituent elements value to it's vector ordinal
  # Note that the flags will be set to zero if not value, which is fine as we are only summing them
  # were we not then we would have to consider this case.
  startAtLine <- numberOfHeaderlines + 1
  for ( i in startAtLine:length(lines)) {
    currentLine <- lines[i]
    #split line on whitespace but leaves a starting empty string
    lineVector <- unlist(strsplit(currentLine, split="\\s+"))
    # remove empty string which the split produces
    lineVector <- lineVector[lineVector != ""]
    offset <- i - startAtLine + 1
    years[offset] <-  as.integer(lineVector[1])
    months[offset] <-  as.integer(lineVector[2])
    tempMax <- parseExpectedNumericDataElement(lineVector[3])
    maxTemp[offset] <-  tempMax@value
    wasMaxTempEstimated[offset] <-  tempMax@isEstimate
    tempMin <- parseExpectedNumericDataElement(lineVector[4])
    minTemp[offset] <- tempMin@value
    wasMinTempEstimated[offset] <-  tempMin@isEstimate
    frost <- parseExpectedNumericDataElement(lineVector[5])
    airFrost[offset] <- as.integer(frost@value)
    wasAirFrostEstimated[offset] <- frost@isEstimate
    rain <- parseExpectedNumericDataElement(lineVector[6])
    rainfall[offset] <- rain@value
    wasRainfallEstimated[offset] <- rain@isEstimate
    s <- parseExpectedNumericDataElement(lineVector[7])
    sun[offset] <-  s@value
    wasSunEstimated[offset] <- s@isEstimate
    wasSunMeasuredUsingKippAndZonenRecorder[offset] <- s@wasSunMeasuredUsingKippAndZonenRecorder
    isProvisional <- grepl("Provisional", currentLine, fixed=TRUE)
    isProvisionalRecord[offset] <- isProvisional
  }

  #name columns with scales
  dataFrame <- data.frame(name=locationName, place=place, lat=latitude, long=longatude, alt = alt,
                          yyyy=years, mm=months, tmax=maxTemp,
                          wasMaxTempEstimated=wasMaxTempEstimated, tmin=minTemp,
                          wasMinTempEstimated=wasMinTempEstimated, af=airFrost,
                          wasAirFrostEstimated=wasAirFrostEstimated, rain=rainfall,
                          wasRainfallEstimated=wasRainfallEstimated, sun=sun,
                          wasSunEstimated=wasSunEstimated,
                          sun_recorder=wasSunMeasuredUsingKippAndZonenRecorder,
                          isProvisionalRecord=isProvisionalRecord)

  close(fileHandle)

  new("weatherStationData", name=name, aboveMeanSeaLevelMeters=aboveMeanSeaLevelMeters, easterly=easterly,
      northerly=northerly, lat=lat, lon=lon, data=dataFrame)

}

dataLoad <- function(outputDir){
  #Use an RData file if one exists, else download data and create one.
  #If we don't do this the program is really slow on loading and
  # unresponsive
  rDataFile <-  str_glue("{outputdir}/data.RData")

  if (useRDataIfExists && file.exists(rDataFile)) {
    load(rDataFile)
  } else {
    if (!dir.exists(outputdir)) {
      dir.create(outputdir)
    }
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
      inner_join(seasons, by="mm")

    #find the years needed for dropdown
    yearsOfInterest <- unifiedWeatherDataSet %>%
      select(yyyy) %>%
      distinct(yyyy) %>%
      arrange(desc(yyyy))

    weatherStationsOrdered <- sort(unlist(lapply(weatherStations, formatWeatherStationNameForDownload)))

    weatherStationsProperCase <- sort(weatherStations)

    save(unifiedWeatherDataSet, yearsOfInterest, weatherStationsOrdered, weatherStationsProperCase, file=rDataFile)
  }

  new("LoadData", alldata = unifiedWeatherDataSet, weatherStations = weatherStationsOrdered,
      weatherStationsProperCase = weatherStationsProperCase, yearsOfInterest = yearsOfInterest)
}

# *** DOWNLOAD, EXTRACT, TRANSFORM CODE END *********

#*** TABLE GENERATION CODE START  ****

# create a class for holding all the metric releated information
setClass("metricDefinition", slots=list(metricName="character", aggregation="function", format="function",
                                        scale="character", name="character",minimumAdjective="character",
                                        maximumAdjective="character", estimatedColumnName="character"
))
# hash allows us to map from string to an object.  Saves us having to write branching statements (if else etc) everywhere.
metricDefinitions <- hash()
metricDefinitions[["rain"]] <- new("metricDefinition", metricName="rainfall", aggregation = mean, format= as.numeric, scale="mm",
                                   minimumAdjective="least", maximumAdjective="greatest", estimatedColumnName="wasRainfallEstimated")
metricDefinitions[["tmax"]] <- new("metricDefinition", metricName="maximum temperature", aggregation = mean, format= as.numeric, scale="degrees celcius",
                                   minimumAdjective="lowest", maximumAdjective="highest", estimatedColumnName="wasMaxTempEstimated")
metricDefinitions[["tmin"]] <- new("metricDefinition", metricName="minimum temperature", aggregation = mean, format= as.numeric, scale="degrees celcius",
                                   minimumAdjective="lowest", maximumAdjective="highest", estimatedColumnName="wasMinTempEstimated")
metricDefinitions[["sun"]] <- new("metricDefinition", metricName="daylight", aggregation = sum, format= as.numeric, scale="hours",
                                  minimumAdjective="fewest", maximumAdjective="most", estimatedColumnName="wasSunEstimated")
metricDefinitions[["af"]] <-  new("metricDefinition", metricName="air frost", aggregation = sum, format= as.integer, scale="days",
                                  minimumAdjective="fewest", maximumAdjective="most", estimatedColumnName="wasAirFrostEstimated")


#decide on the appropriate aggregation function by metric
aggregationFunction <- function(metric){
  metricDefinitions[[metric]]@aggregation
}

#Decide on the appropriate format function by metric
formatFunction <- function(metric){
  metricDefinitions[[metric]]@format
}

# Functions which create output data
seasonalSummaryTable <- function(data, yearFrom, yearTo, metric, placeName) {
  data %>%
    filter(yyyy >= yearFrom) %>%
    filter(yyyy <= yearTo) %>%
   # filter(place == placeName) %>%
    group_by(name, season, yyyy) %>%
    summarise(typical=aggregationFunction(metric)(get(metric)), .groups = "keep") %>%
    mutate(typical=formatFunction(metric)(typical), yyyy = as.integer(yyyy)) %>%
    pivot_wider(names_from=season, values_from=typical)%>%
    arrange(name, yyyy)
}

seasonalHeaderGenerate <- function(metric) {
  str_glue("<h1>Seasonal {metricDefinitions[[metric]]@metricName}</h1>")
}

seasonalDescriptionGenerate <- function(metric) {
  warning <- "<b>WARNING this page ignores the month AND place inputs.</b><br/>"

  #typical values will use mean to aggreate identical tests equality on our functions.
  isTypicalValues <- identical(metricDefinitions[[metric]]@aggregation, mean)
  if (isTypicalValues){
    s <- "Typical daily values for seasons of"
  }else{
    s <- "Total values for seasons of"
  }
  url <- "https://www.metoffice.gov.uk/weather/learn-about/met-office-for-schools/other-content/other-resources/our-seasons"
  t <- str_glue("The definition used for seasons is given by the met office at: <a href='{url}'>{url}</a>")
  str_glue("{warning}{s} {metricDefinitions[[metric]]@metricName} ({metricDefinitions[[metric]]@scale})<br/>{t}")
}
seasonalSummaryGenerate <- function(data, metric,
                                    yearFrom, yearTo){
  estimatedColumnName <- metricDefinitions[[metric]]@estimatedColumnName
  minAdjective <- metricDefinitions[[metric]]@minimumAdjective
  maxAdjective <- metricDefinitions[[metric]]@maximumAdjective
  metricName <- metricDefinitions[[metric]]@metricName
  scale <- metricDefinitions[[metric]]@scale

  seasonVector <- c("spring", "summer", "autumn", "winter")
  summary <- ""

  for(i in 1:length(seasonVector)){
    seasonName <- seasonVector[i]

    x <- data %>%
      filter(yyyy >= yearFrom) %>%
      filter(yyyy <= yearTo) %>%
      filter(season == seasonName)  %>%
      summarise(min=min(get(metric), na.rm = TRUE), max=max(get(metric), na.rm = TRUE),
                estimates=sum(get(estimatedColumnName), na.rm = TRUE), provisional=sum(isProvisionalRecord, na.rm = TRUE),
                mean=mean(get(metric), na.rm = TRUE), count=sum(is.na(get(metric)))
      )

    if (!is.nan(x$mean)) {
      #lets build up the sentences
      a <- str_glue("In {seasonName} the typical value of  {metricName} was {format(round(x$mean, 1), nsmall=1)} ({scale}).")
      b <- str_glue("In {seasonName} the {minAdjective} value of {metricName} was {x$min} ({scale}).")
      c <- str_glue("In {seasonName} the {maxAdjective} value of {metricName} was {x$max} ({scale}).")
      d <- str_glue("In {seasonName} the number of estimated data values were {x$estimates}.")
      e <- str_glue("In {seasonName} the number of provisional data values were {x$provisional}.")
      summary <- str_glue("{summary}{a}<br>{b}<br/>{c}<br/>{d}<br/>{e}<br/><br/>")
    }
  }

  summary
}

monthlySummaryTable <- function(data, yearFrom, yearTo, metric, placeName) {
  x <- data %>%
    filter(yyyy >= yearFrom) %>%
    filter(yyyy <= yearTo) %>%
    #filter(place == placeName) %>%
    group_by(name, monthNamesShort, yyyy) %>%
    summarise(typical=aggregationFunction(metric)(get(metric)), .groups = "keep") %>%
    mutate(typical=formatFunction(metric)(typical), yyyy = as.integer(yyyy)) %>%
    pivot_wider(names_from=monthNamesShort, values_from=typical)%>%
    arrange(name, yyyy)
  #in dyplr if you try to reloacte a column which does not exist then you get an empty dataframe back
  # this is a real issue as were you to redownload the data part way through the current year you
  # would miss the months which had been uploaded.  Therefore we will handle this.
  monthNamesShort <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                       "Sep", "Oct", "Nov", "Dec")

  columns <- colnames(x)
  afterCol <- "yyyy"
  for(i in 1:length(monthNamesShort)) {
    month <- monthNamesShort[i]
    if (month%in%columns){
      x <- x %>%
        relocate(!!as.symbol(month), .after=!!as.symbol(afterCol))
    }
    afterCol<- month
  }

  x
}

monthlyHeaderGenerate <- function(metric) {
  str_glue("<h1>Monthly {metricDefinitions[[metric]]@metricName}</h1>")
}

monthlyDescriptionGenerate <- function(metric) {
  #typical values will use mean to aggreate identical tests equality on our functions.
  isTypicalValues <- identical(metricDefinitions[[metric]]@aggregation, mean)
  if (isTypicalValues){
    s <- "Typical daily values for months of"
  }else{
    s <- "Total values for months of"
  }

  str_glue("{s} {metricDefinitions[[metric]]@metricName} ({metricDefinitions[[metric]]@scale})")
}
monthlySummaryGenerate <- function(data, metric,
                                   yearFrom, yearTo){
  estimatedColumnName <- metricDefinitions[[metric]]@estimatedColumnName
  minAdjective <- metricDefinitions[[metric]]@minimumAdjective
  maxAdjective <- metricDefinitions[[metric]]@maximumAdjective
  metricName <- metricDefinitions[[metric]]@metricName
  scale <- metricDefinitions[[metric]]@scale
  summary <- ""

  for(i in 1:length(monthNames)){
    month <- monthNames[i]

    x <- data %>%
        filter(yyyy >= yearFrom) %>%
        filter(yyyy <= yearTo) %>%
        filter(monthName == month)  %>%
        summarise(min=min(get(metric), na.rm = TRUE), max=max(get(metric), na.rm = TRUE),
                  estimates=sum(get(estimatedColumnName), na.rm = TRUE), provisional=sum(isProvisionalRecord, na.rm = TRUE),
                  mean=mean(get(metric), na.rm = TRUE), count=sum(is.na(get(metric)))
        )

    if (!is.nan(x$mean)) {
      #lets build up the sentences
      a <- str_glue("In {month} the typical value of  {metricName} was {format(round(x$mean, 1), nsmall=1)} ({scale}).")
      b <- str_glue("In {month} the {minAdjective} value of {metricName} was {x$min} ({scale}).")
      c <- str_glue("In {month} the {maxAdjective} value of {metricName} was {x$max} ({scale}).")
      d <- str_glue("In {month} the number of estimated data values were {x$estimates}.")
      e <- str_glue("In {month} the number of provisional data values were {x$provisional}.")
      summary <- str_glue("{summary}{a}<br>{b}<br/>{c}<br/>{d}<br/>{e}<br/><br/>")
    }
  }

  summary
}

#*** TABLE GENERATION CODE END  ****

#*** TIME SERIES PLOT START ****
timeSeriesPlot <- function (alldata, place, month, metric, yearRange){

  data_subset <- alldata[alldata$name == place, ]
  data_subset <- data_subset[data_subset$monthNamesShort == month, ]
  data_subset <- data_subset %>% select(yyyy, metric)

  ggplot(data_subset)+
    aes(x=yyyy, y = !!as.symbol(metric))+
    geom_line(color = 'blue')+ labs(y = metric, x = "Years (CE)") +
    xlim(yearRange)
}


#*** TIME SERIES PLOT END ****


#*** SHINY/UI CODE START ****
# Define the user interface
# Note the commas are required for arguments to functions.
# Shiny uses a fluent interface style of programming whereby functions
# such as "fluidPage, "sidePanel", mainPanel etc take functions
# as arguments hence the commas can seem misleading at times.
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
       tr:nth-child(even) {
            background-color: Lightblue;
        }
    "))
  ),

  titlePanel("Primary school weather investigation"),

  sidebarPanel(
    selectInput(inputId = "investigationMetric",
                label = "Weather Measure",
                choices =c("Rainfall" = "rain",
                           "Max Temperature" = "tmax",
                           "Min Temperature" = "tmin",
                           "Air Frost" = "af",
                           "Daylight" = "sun")),
    selectInput("month", "Month", choices = monthNamesShort),
    selectizeInput("place", "Location", choices = c()),
                   # Input: Slider for the number of bins ---
    sliderInput(inputId = "year_range",
                label = "Year Range:",
                min = 0,
                max = 0,
                value = c(0, 0),
                step=1,
    dragRange = TRUE)
  ),
  mainPanel(
    tabsetPanel(

      tabPanel("Time Series Plot",
               plotOutput("plot")
      ),
      tabPanel("Tables of data",
               htmlOutput("seasonsHeader"),
               htmlOutput("seasonsDescription"),
               tableOutput("seasonsTable"),
               htmlOutput("seasonsSummary"),
               htmlOutput("monthlyHeader"),
               htmlOutput("monthlyDescription"),
               tableOutput("monthlyTable"),
              htmlOutput("monthlySummary")

      )
    )
  )

)

# Define the "server": where the computations are done/plots generated etc.
# given the input values (input$interval, input$lvl, input$method)
# from the user interface

server <- function(input, output) {

  #Load the data and assign to variables
  load <- dataLoad(outputdir)
  alldata <- load@alldata
  yearsOfInterest <- load@yearsOfInterest
  weatherStations <- load@weatherStations
  weatherStationsProperCase <- load@weatherStationsProperCase

  # update dropdowns with data required
  updateSelectInput(inputId = "yearfrom", choices = yearsOfInterest)
  updateSelectInput(inputId = "yearto", choices = yearsOfInterest)
  updateSelectInput(inputId = "place", choices= weatherStationsProperCase)
  minYear <- min(yearsOfInterest)
  maxYear <- max(yearsOfInterest)
  updateSliderInput(inputId="year_range", min=minYear, max=maxYear, value=c(minYear, maxYear))

  #create seasonal tabular data and commentry
  seasonTableGeneration <- reactive({
    seasonalSummaryTable(alldata, input$year_range[1], input$year_range[2], input$investigationMetric, input$place)
  })

  output$seasonsTable <- renderTable({ seasonTableGeneration() })
  output$seasonsHeader <-renderText({  seasonalHeaderGenerate(input$investigationMetric)  })
  output$seasonsDescription <- renderText({seasonalDescriptionGenerate(input$investigationMetric)})
  output$seasonsSummary <- renderText(seasonalSummaryGenerate(alldata, input$investigationMetric,
                                                              input$year_range[1], input$year_range[2]))

  #create monthly tabular data and commentry
  monthlyTableGeneration <- reactive({
    monthlySummaryTable(alldata, input$year_range[1], input$year_range[2], input$investigationMetric, input$place)
  })
  output$monthlyTable <- renderTable({monthlyTableGeneration()})
  output$monthlyHeader <-renderText({monthlyHeaderGenerate(input$investigationMetric)})
  output$monthlyDescription <- renderText({monthlyDescriptionGenerate(input$investigationMetric)})
  output$monthlySummary <- renderText({monthlySummaryGenerate(alldata, input$investigationMetric,
                                                              input$year_range[1], input$year_range[2])})


  timeSeriesPlotGeneration <- reactive({
    timeSeriesPlot(alldata, input$place, input$month, input$investigationMetric, input$year_range)
  })

 output$plot <- renderPlot({ timeSeriesPlotGeneration()  })

}
# Run the application
shinyApp(ui = ui, server = server)

#*** SHINY/UI CODE END ****