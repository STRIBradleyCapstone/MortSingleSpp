library(shiny)

shinyUI(fluidPage(
  titlePanel("Mortality & Growth"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("regionSelect"),
      actionButton("regionAction", label="Load Region", inline = TRUE),
      uiOutput("loadNotification", inline = TRUE),
      uiOutput("speciesSelect"),
      numericInput("dbhNumSelect", label = "Amount of dbh Categories", value = 3, min = 2, max = 100),
      div(style = "display:inline-block; width:47%; float:left", 
          numericInput("dbhBaseNumeric", label = "dbh Base", value = 10, min = 0, max = 10000)),
      div(style = "display:inline-block; width:47%; float:right", 
          numericInput("dbhGapNumeric", label = "dbh Gap", value = 10, min = 1, max = 10000)),
      actionButton("dbhAction", label = "Apply dbh Options"),
      wellPanel(id = "categoryWell", style = "overflow-y:scroll; max-height: 400px", 
                uiOutput("categoryNumerics")),
      radioButtons("calcTypeRadio", label = "Select a function", choices = c("Mortality", "Growth"), selected = "Mortality", inline = TRUE),
      actionButton("action", label = "Calculate"),
      width = 3
    ),
    
    mainPanel(
      
      br(),h3(textOutput("speciesNameTextOut"), style = "position:absolute;left:300px"),
      plotOutput("speciesGraph"),
      uiOutput("surveySlider"),
      uiOutput("yNumeric"),
      uiOutput("xNumeric"),
      actionButton("plotAction", label = "Plot surveys"),
      actionButton("clearAction", label = "Clear graph"),
      actionButton("plotAllAction", label = "Plot all consecutive surveys")
    )
  )
))
