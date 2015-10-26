library(shiny)

shinyUI(fluidPage(
  titlePanel("Mortality"),
  
  sidebarLayout(
    
    sidebarPanel(
      #Species select needs to be reimplemented.  Right now it acquires a list of species from mort.data AFTER calculation.  We need a list from the start.
      #To do this, I think that we can just create a list of species by cycling through one of the surveys.
      #selectInput("speciesSelect", label = "Please enter a species to graph", choices = rownames(mort.data()$N), selected = rownames(mort.data()$N)[1]),
      numericInput("dbhNumSelect", label = "Amount of dbh Categories", value = 3, min = 1, max = 10),
      uiOutput("categoryNumerics"),
      actionButton("action", label = "Apply Changes"),
      width = 3
    ),
    
    mainPanel(
      
      actionButton("speciesAction", label = "Apply Species"),
      br(),h3(textOutput("speciesNameTextOut"), style = "position:absolute;left:300px"),
      plotOutput("speciesGraph"),
      numericInput("yNum", label = "Y Axis", value = 0.2, min = 0.005, max = 1, step = 0.05),
      uiOutput("xNumeric")
    )
  )
))
