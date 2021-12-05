#lets make it easy for the teacher and install packages if they don't exist.
if(!(require(shiny))){install.packages('shiny')}
if(!(require(hash))){install.packages('hash')}
if(!(require(tidyverse))){install.packages('tidyverse')}

library(shiny)
library(tidyverse)

# pull in functions from other files.
source("dataLoad.R")
source("summaryTable.R")

outputdir = "data"

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

