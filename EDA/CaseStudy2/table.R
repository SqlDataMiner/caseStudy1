source("dataLoad.R")

#lets make it easy for the teacher and install packages if they don't exist.
if(!(require(shiny))){install.packages('shiny')}
if(!(require(hash))){install.packages('hash')}
if(!(require(tidyverse))){install.packages('tidyverse')}

library(shiny)
library(tidyverse)
library(hash)

outputdir = "data"

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
  data <- load@data
  yearsOfInterest <- load@yearsOfInterest
  weatherStations <- load@weatherStations

  seasonTableGeneration <- reactive({
    seasonalSummaryTable(data, input$yearfrom, input$yearto, input$investigationMetric)
  })

  monthlyTableGeneration <- reactive({
    monthlySummaryTable(data, input$yearfrom, input$yearto, input$investigationMetric)
  })

  # update dropdowns with data required
  updateSelectInput(inputId = "yearfrom", choices = yearsOfInterest)
  updateSelectInput(inputId = "yearto", choices = yearsOfInterest)



  output$mainPlot <- renderPlot({
    generatePlot(input, data)
  })

  #create seasonal tabular data and commentry
  output$seasonsTable <- renderTable({ seasonTableGeneration() })
  output$seasonsHeader <-renderText({  seasonalHeaderGenerate(input$investigationMetric)  })
  output$seasonsDescription <- renderText({seasonalDescriptionGenerate(input$investigationMetric)})
  output$seasonsSummary <- renderText(seasonalSummaryGenerate(data, input$investigationMetric,
                                                              input$yearfrom, input$yearto))

  #create monthly tabular data and commentry
  output$monthlyTable <- renderTable({monthlyTableGeneration()})
  output$monthlyHeader <-renderText({monthlyHeaderGenerate(input$investigationMetric)})
  output$monthlyDescription <- renderText({monthlyDescriptionGenerate(input$investigationMetric)})
  output$monthlySummary <- renderText({monthlySummaryGenerate(data, input$investigationMetric,
                                                              input$yearfrom, input$yearto)})
}
# Run the application
shinyApp(ui = ui, server = server)

