library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui<- dashboardPage(

  dashboardHeader(title = "Test Application",titleWidth = "400px"),

  dashboardSidebar(
    sidebarMenu(    #Add sidebarMenu here!!!!!
      menuItem("Page 1", tabName = "page1", icon = icon("dashboard")), # You can add icons to your menu if you want
      menuItem("Page 2", tabName = "page2", icon = icon("dashboard")))),

  dashboardBody(
    tabItems(

      tabItem(
        tabName = "page1",
        h1("This is page 1")),

      tabItem( #Add checkboxGroupInput into this tabItem
        tabName = "page2",
        h1("This is page 2"),
        checkboxGroupInput("outcome", "Select Outcome Variable(s):", choices = c("Box 1", "Box 2", "Box 3")),
        selectInput("selectinput", label = "Select:", choices = c("Choice 1", "Choice 2", "Choice 2")))
    )
  ))

server <- function(input,output,session) {

}

shinyApp(ui, server)