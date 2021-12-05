if(!(require(hash))){install.packages('hash')}

library(hash)
#turn off summarise warning messages
dplyr.summarise.inform <- FALSE

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
                                        scale="character", name="character",minimumAdjective="character",
                                        maximumAdjective="character", estimatedColumnName="character"
))
# hash allows us to map from string to an object.  Saves us having to write branching statements (if else etc) everywhere.
metricDefinitions <- hash()
metricDefinitions[["rainfallmm"]] <- new("metricDefinition", metricName="rainfall", aggregation = mean, format= as.numeric, scale="mm",
                                         minimumAdjective="least", maximumAdjective="greatest", estimatedColumnName="wasRainfallEstimated")
metricDefinitions[["maxTempDegreesC"]] <- new("metricDefinition", metricName="maximum temperature", aggregation = mean, format= as.numeric, scale="degrees celcius",
                                              minimumAdjective="lowest", maximumAdjective="highest", estimatedColumnName="wasMaxTempEstimated")
metricDefinitions[["minTempDegreesC"]] <- new("metricDefinition", metricName="minimum temperature", aggregation = mean, format= as.numeric, scale="degrees celcius",
                                              minimumAdjective="lowest", maximumAdjective="highest", estimatedColumnName="wasMinTempEstimated")
metricDefinitions[["sunHours"]] <- new("metricDefinition", metricName="daylight", aggregation = sum, format= as.numeric, scale="hours",
                                       minimumAdjective="fewest", maximumAdjective="most", estimatedColumnName="wasSunEstimated")
metricDefinitions[["airFrostDays"]] <-  new("metricDefinition", metricName="air frost", aggregation = sum, format= as.integer, scale="days",
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
  isTypicalValues <- identical(metricDefinitions[[metric]]@aggregation, mean)
  if (isTypicalValues){
    s <- "Typical daily values for seasons of"
  }else{
    s <- "Total values for seasons of"
  }
  url <- "https://www.metoffice.gov.uk/weather/learn-about/met-office-for-schools/other-content/other-resources/our-seasons"
  t <- str_glue("The definition used for seasons is given by the met office at: <a href='{url}'>{url}</a>")
  str_glue("{s} {metricDefinitions[[metric]]@metricName} ({metricDefinitions[[metric]]@scale})<br/>{t}")
}
seasonalSummaryGenerate <- function(data, metric,
                                    yearFrom, yearTo){
  estimatedColumnName <- metricDefinitions[[metric]]@estimatedColumnName
  minAdjective <- metricDefinitions[[metric]]@minimumAdjective
  maxAdjective <- metricDefinitions[[metric]]@maximumAdjective
  metricName <- metricDefinitions[[metric]]@metricName
  scale <- metricDefinitions[[metric]]@scale

  x <- data %>%
    filter(years >= yearFrom) %>%
    filter(years <= yearTo) %>%
    summarise(min=min(get(metric), na.rm = TRUE), max=max(get(metric), na.rm = TRUE),
              estimates=sum(get(estimatedColumnName), na.rm = TRUE))

  #lets build up the sentences
  a <- str_glue("The {minAdjective} value of {metricName} was {x$min} ({scale}).")
  b <- str_glue("The {maxAdjective} value of {metricName} was {x$max} ({scale}).")
  c <- str_glue("The number of estimated data values were {x$estimates}.")
  str_glue("{a}<br/>{b}<br/>{c}")
}

monthlySummaryTable <- function(data, yearFrom, yearTo, metric) {
  data %>%
    filter(years >= yearFrom) %>%
    filter(years <= yearTo) %>%
    group_by(name, monthNamesShort, years) %>%
    summarise(typical=aggregationFunction(metric)(get(metric))) %>%
    mutate(typical=formatFunction(metric)(typical), years = as.integer(years)) %>%
    pivot_wider(names_from=monthNamesShort, values_from=typical)%>%
    arrange(name, years) %>%
    relocate(Jan, .after=years) %>%
    relocate(Feb, .after=Jan) %>%
    relocate(Mar, .after=Feb) %>%
    relocate(Apr, .after=Mar) %>%
    relocate(May, .after=Apr) %>%
    relocate(Jun, .after=May) %>%
    relocate(Jul, .after=Jun) %>%
    relocate(Aug, .after=Jul) %>%
    relocate(Sep, .after=Aug) %>%
    relocate(Oct, .after=Sep) %>%
    relocate(Nov, .after=Oct)
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

  x <- data %>%
    filter(years >= yearFrom) %>%
    filter(years <= yearTo) %>%
    summarise(min=min(get(metric), na.rm = TRUE), max=max(get(metric), na.rm = TRUE),
              estimates=sum(get(estimatedColumnName), na.rm = TRUE))

  #lets build up the sentences
  a <- str_glue("The {minAdjective} value of {metricName} was {x$min} ({scale}).")
  b <- str_glue("The {maxAdjective} value of {metricName} was {x$max} ({scale}).")
  c <- str_glue("The number of estimated data values were {x$estimates}.")
  str_glue("{a}<br/>{b}<br/>{c}")
}

