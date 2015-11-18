library(shiny)

shinyUI(fluidPage(
  titlePanel("Mortality"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("speciesSelect"),
      numericInput("dbhNumSelect", label = "Amount of dbh Categories", value = 3, min = 2, max = 10),
      uiOutput("categoryNumerics"),
      actionButton("action", label = "Apply Changes"),
      width = 3
    ),
    
    mainPanel(
      
      br(),h3(textOutput("speciesNameTextOut"), style = "position:absolute;left:300px"),
      plotOutput("speciesGraph"),
      selectInput("survey1Select", label = "Please select the first survey", choices = c("full1", "full2", "full3", "full4", "full5", "full6"), selected = "full1"),
      selectInput("survey2Select", label = "Please select the second survey", choices = c("full2", "full3", "full4", "full5", "full6", "full7"), selected = "full2"),
      numericInput("yNum", label = "Y Axis", value = 0.2, min = 0.005, max = 1, step = 0.05),
      uiOutput("xNumeric"),
      actionButton("plotAction", label = "Plot surveys"),
      actionButton("clearAction", label = "Clear graph"),
      actionButton("plotAllAction", label = "Plot all surveys")
    )
  )
))
