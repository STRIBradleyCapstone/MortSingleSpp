.libPaths("Data")
library(shiny)
library(date)
attach("Data/CTFSRPackage.rdata")
load("Data/full/bci.full1.rdata")
load("Data/full/bci.full2.rdata")
load("Data/full/bci.full3.rdata")
load("Data/full/bci.full4.rdata")
load("Data/full/bci.full5.rdata")
load("Data/full/bci.full6.rdata")
load("Data/full/bci.full7.rdata")
load("Data/Species/bci.spptable.rdata")

shinyServer(function(input, output, session) {
  
  CENSUS_COUNT <- 7
  graphMatrix = matrix("N", nrow = CENSUS_COUNT - 1, ncol = CENSUS_COUNT - 1)
  colMatrix = matrix(c("#8b0000", 0, 0, 0, 0, 0,
                       "#8b2323", "#8b4500", 0, 0, 0, 0,
                       "#a52a2a", "#cd6600", "#8b6508", 0, 0, 0,
                       "#CD3333", "#ee7600", "#cd950c", "#556b2f", 0, 0,
                       "#ee3b3b", "#ff7f00", "#eead0e", "#6e8b3d", "#104e8b", 0,
                       "#ff4040", "#ff8c00", "#ffb90f", "#bcee68", "#1c86ee", "#8968cd"), 
                     nrow = CENSUS_COUNT - 1, ncol = CENSUS_COUNT - 1)
  
  output$categoryNumerics <- renderUI({
    amount = input$dbhNumSelect
    validate(need(amount > 1, "Amount of dbh categories must be > 1"))
    validate(need(amount <= 10, "Amount of dbh categories must be <= 10"))
    lapply (1:amount, function(i){
      numericInput(paste0("category", i, "Num"), paste("category", i, sep=" "), value = 100*i, min = 0, max = 9999)
    })
  })
  
  output$speciesSelect <- renderUI({
    capture.output(allspp <- sort(unique(bci.full1$sp)))
    selectInput("speciesSelect", label = "Please enter a species to graph", choices = allspp, selected = allspp[1])
  })
  
  output$xNumeric <- renderUI({
    numericInput("xNum", "X Axis", value = classes()[as.integer(isolate(dbhCatNum()))]+50, min = 50, max = 9999, step = 10)
  })
  
  #Retrieves category values and stores the values locally.  
  #Runs each time category inputs are changed.
  classes <- eventReactive(input$action,{
    amount = dbhCatNum()
    validate(need(amount > 1, "Amount of dbh categories must be > 1"))
    validate(need(amount <= 10, "Amount of dbh categories must be <= 10"))
    classList = c()
    for(i in 1:amount){
      num = input[[paste0("category", i, "Num")]]
      validate(
        need(num > 0 && num < 10000, "Categories must be >= 0 and <= 9999")
      )
      classList = append(classList,num)
      
    }
    
    classList = sort.int(classList)
    return(classList)
  })
  
  speciesName <- eventReactive(input$action,{
    input$speciesSelect
  })
  
  survey1 <- reactive({
    switch(input$survey1Select, 
           "full1" = {return(1)},
           "full2" = {return(2)},
           "full3" = {return(3)},
           "full4" = {return(4)},
           "full5" = {return(5)},
           "full6" = {return(6)})
  })
  
  survey2 <- reactive({
    slist <- c("full1", "full2", "full3", "full4", "full5", "full6", "full7")
    #match function calculates the indices of each survey input within a list of all surveys
    #Used to ensure that survey1 is less than survey2
    validate(
      need(match(input$survey1Select, slist) < match(input$survey2Select, slist), "First survey must have occurred before the second survey")
    )
    #returns the census number -1 so that using it to subset mort.dataList will yield the correct subset.
    switch(input$survey2Select,
           "full2" = {return(1)},
           "full3" = {return(2)},
           "full4" = {return(3)},
           "full5" = {return(4)},
           "full6" = {return(5)},
           "full7" = {return(6)})
  })
  
  mort.data <- eventReactive(input$action,{
    graphMatrix <<- matrix("N", nrow = CENSUS_COUNT - 1, ncol = CENSUS_COUNT - 1)
    mort.dataList = matrix(list(), nrow = CENSUS_COUNT - 1, ncol = CENSUS_COUNT - 1)
    censusList = c(list(bci.full1[bci.full1$sp == speciesName(), 1:20]),
                   list(bci.full2[bci.full2$sp == speciesName(), 1:20]),
                   list(bci.full3[bci.full3$sp == speciesName(), 1:20]),
                   list(bci.full4[bci.full4$sp == speciesName(), 1:20]),
                   list(bci.full5[bci.full5$sp == speciesName(), 1:20]),
                   list(bci.full6[bci.full6$sp == speciesName(), 1:20]),
                   list(bci.full7[bci.full7$sp == speciesName(), 1:20]))
    for(i in 1:(CENSUS_COUNT - 1)){
      for(j in (i+1):CENSUS_COUNT){
        force(i)
        force(j)
        tempList = mortality.eachspp(censusList[[i]], censusList[[j]], classbreak = classes())
        mort.dataList[[i,j - 1]] <- list(tempList)
      }
      
    }
    return(mort.dataList)
    
  })
  
  observeEvent(input$plotAction, {
    force(survey1())
    force(survey2())
    if(!is.null(mort.data()))
    {
      graphMatrix[[survey1(), survey2()]] <<- "Y"
    }
  })
  
  observeEvent(input$clearAction, {
    graphMatrix <<- matrix("N", nrow = CENSUS_COUNT - 1, ncol = CENSUS_COUNT)
  })
  
  observeEvent(input$plotAllAction, {
    graphMatrix <<- matrix("N", nrow = CENSUS_COUNT - 1, ncol = CENSUS_COUNT)
    for(i in 1:CENSUS_COUNT)
    {
      matrix[[i,i+1]] <- "Y"
    }
  })
  
  dbhCatNum <- eventReactive(input$action, {
    input$dbhNumSelect
  })
  
  output$speciesNameTextOut <- renderText({speciesName()})
  
  output$speciesGraph <- renderPlot({
    input$plotAction
    input$clearAction
    input$plotAllAction
    plot(x=NULL,y=NULL,type='l',xlim=c(0, input$xNum),ylim=c(0, input$yNum),ylab=paste(speciesName(),'mortality'),xlab='dbh')
    if(!is.null(mort.data()) && !is.null(survey2()))
    {
      for(i in 1:(CENSUS_COUNT-1)){
        for(j in i:(CENSUS_COUNT-1)){
          force(i)
          force(j)
          print(graphMatrix[[i,j]])
          if(graphMatrix[[i,j]] == "Y")
          {
            mortdbh.graph(i, j,sp=speciesName(),xrange=c(0, input$xNum),yrange=c(0,input$yNum))
          }
        }
      }
      
    }
  })
  
  mortdbh.graph=function(census1, census2 ,sp,xrange=NULL,yrange=NULL)
  {
    par(col = colMatrix[[census1, census2]])
    data = mort.data()[[census1, census2]]
    y=data[[1]]$rate
    aproblem=is.infinite(y)
    if(is.null(yrange)) yrange=c(0,max(y[!aproblem],na.rm=TRUE))
    if(is.null(xrange)) xrange=c(0,max(data[[1]]$dbhmean,na.rm=TRUE))
    lines(data[[1]]$dbhmean[!aproblem],y[!aproblem],type='l',lwd=2)
  }
  
  
})
