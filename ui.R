library(shiny)

shinyUI(fluidPage(
  titlePanel("Mortality"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("speciesSelect"),
      numericInput("dbhNumSelect", label = "Amount of dbh Categories", value = 3, min = 2, max = 100),
      div(style = "display:inline-block; width:47%; float:left", 
          numericInput("dbhBaseNumeric", label = "dbh Base", value = 10, min = 0, max = 10000)),
      div(style = "display:inline-block; width:47%; float:right", 
          numericInput("dbhGapNumeric", label = "dbh Gap", value = 10, min = 1, max = 10000)),
      actionButton("dbhAction", label = "Apply dbh Options"),
      wellPanel(id = "categoryWell", style = "overflow-y:scroll; max-height: 400px", 
        uiOutput("categoryNumerics")),
      actionButton("action", label = "Apply Changes"),
      width = 3
    ),
    
    mainPanel(
      
      br(),h3(textOutput("speciesNameTextOut"), style = "position:absolute;left:300px"),
      plotOutput("speciesGraph"),
      selectInput("survey1Select", label = "Please select the first survey", choices = c("full1", "full2", "full3", "full4", "full5", "full6"), selected = "full1"),
      selectInput("survey2Select", label = "Please select the second survey", choices = c("full2", "full3", "full4", "full5", "full6", "full7"), selected = "full2"),
      uiOutput("yNumeric"),
      uiOutput("xNumeric"),
      actionButton("plotAction", label = "Plot surveys"),
      actionButton("clearAction", label = "Clear graph"),
      actionButton("plotAllAction", label = "Plot all surveys")
    )
  )
))
