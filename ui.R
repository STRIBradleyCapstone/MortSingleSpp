library(shiny)

shinyUI(fluidPage(
  titlePanel("Mortality"),
  
  sidebarLayout(
    
    sidebarPanel(
      #Species select needs to be reimplemented.  Right now it acquires a list of species from mort.data AFTER calculation.  We need a list from the start.
      #To do this, I think that we can just create a list of species by cycling through one of the surveys.
      capture.output(allspp <- sort(unique(bci.full1$sp))),
      selectInput("speciesSelect", label = "Please enter a species to graph", choices = allspp, selected = allspp[1]),
      numericInput("dbhNumSelect", label = "Amount of dbh Categories", value = 3, min = 1, max = 10),
      uiOutput("categoryNumerics"),
      actionButton("action", label = "Apply Changes"),
      width = 3
    ),
    
    mainPanel(
      
      br(),h3(textOutput("speciesNameTextOut"), style = "position:absolute;left:300px"),
      plotOutput("speciesGraph"),
      #plotOutput("table"),
      
      #checkboxGroupInput("surveyPeriodCheckGroup", label = "Choose which survey periods to chart", 
       #                  choices = c("1->2", "1->3", "1->4", "1->5", "1->6", "1->7", "2->3", "2->4", "2->5", "2->6", "2->7", 
      #                               "3->4", "3->5", "3->6", "3->7", "4->5", "4->6", "4->7", "5->6", "5->7", "6->7"), 
      #                   selected = c("1->2", "1->3", "1->4", "1->5", "1->6", "1->7", "2->3", "2->4", "2->5", "2->6", "2->7", 
      #                                "3->4", "3->5", "3->6", "3->7", "4->5", "4->6", "4->7", "5->6", "5->7", "6->7"), inline = T),
      selectInput("survey1Select", label = "Please select the first survey", choices = c("full1", "full2", "full3", "full4", "full5", "full6"), selected = "full1"),
      selectInput("survey2Select", label = "Please select the second survey", choices = c("full2", "full3", "full4", "full5", "full6", "full7"), selected = "full2"),
      numericInput("yNum", label = "Y Axis", value = 0.2, min = 0.005, max = 1, step = 0.05),
      uiOutput("xNumeric"),
      actionButton("plotAction", label = "Plot surveys")
    )
  )
))
