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
  
  #Initializes a list of which census pairs are active.  All set to inactive ("N")
  graphListInit <- function()
  {
    tempList = list()
    for(i in 1:PAIR_COUNT)
    {
      tempList = append(tempList, "N")
    }
    return(tempList)
  }
  
  #Calculates amount of possible census pairs
  calcPairCount <- function()
  {
    return((CENSUS_COUNT * (CENSUS_COUNT -1)) / 2)
  }
  
  CENSUS_COUNT <- 7
  PAIR_COUNT <- calcPairCount()
  graphList = graphListInit()
  plotCount <- 1
  lgndCount <- 1
  cl <- rainbow(PAIR_COUNT)
  lgnd <- vector(mode="character", length=1)
  xMax <- 0
  yMax <- 0
  
  #Species Selector
  output$speciesSelect <- renderUI({
    capture.output(allspp <- sort(unique(bci.full1$sp))) #Prevents output to user
    selectInput("speciesSelect", label = "Please enter a species to graph", choices = allspp, selected = allspp[1])
  })
  
  #Determines starting dbh category
  dbhBase <- eventReactive(input$dbhAction, {
    return(input$dbhBaseNumeric)
  })
  
  #Determines gap between dbh categories
  dbhGap <- eventReactive(input$dbhAction, {
    return(input$dbhGapNumeric)
  })
  
  output$categoryNumerics <- renderUI({
    amount = input$dbhNumSelect
    validate(need(amount > 1, "Amount of dbh categories must be > 1"))
    validate(need(amount <= 100, "Amount of dbh categories must be <= 100"))
    lapply (1:amount, function(i){
      numericInput(paste0("category", i, "Num"), paste("category", i, sep=" "), value = dbhBase() + (dbhGap()*(i-1)), min = 0, max = 9999)
    })
  })
  
  output$xNumeric <- renderUI({
    numericInput("xNum", "X Axis", value = xMax, max = 9999, step = 10)
  })
  
  output$yNumeric <- renderUI({
    numericInput("yNum", "Y Axis", value = yMax, max = 9999, step = 0.05)
  })
  
  #Retrieves category values and stores the values locally.  
  #Runs each time category inputs are changed.
  classes <- eventReactive(input$action,{
    xMax <<- 0
    yMax <<- 0
    amount = input$dbhNumSelect
    validate(need(amount > 1, "Amount of dbh categories must be > 1"))
    validate(need(amount <= 100, "Amount of dbh categories must be <= 100"))
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
           "full2" = {return(2)},
           "full3" = {return(3)},
           "full4" = {return(4)},
           "full5" = {return(5)},
           "full6" = {return(6)},
           "full7" = {return(7)})
  })
  
  mort.data <- eventReactive(input$action,{
    mort.dataList = list()
    censusList = c(list(bci.full1[bci.full1$sp == speciesName(), 1:20]),
                   list(bci.full2[bci.full2$sp == speciesName(), 1:20]),
                   list(bci.full3[bci.full3$sp == speciesName(), 1:20]),
                   list(bci.full4[bci.full4$sp == speciesName(), 1:20]),
                   list(bci.full5[bci.full5$sp == speciesName(), 1:20]),
                   list(bci.full6[bci.full6$sp == speciesName(), 1:20]),
                   list(bci.full7[bci.full7$sp == speciesName(), 1:20]))
    count = 1
    for(i in 1:(CENSUS_COUNT - 1)){
      for(j in (i+1):CENSUS_COUNT){
        force(i)
        force(j)
        tempList = mortality.eachspp(censusList[[i]], censusList[[j]], classbreak = classes())
        print(tempList)
        mort.dataList[[count]] <- list(tempList)
        count = count + 1
      }
      
    }
    return(mort.dataList)
    
  })
  
  observeEvent(input$plotAction, {
    if(!is.null(mort.data()))
    {
      graphList[[calcIndex(survey1(), survey2())]] <<- "Y"
    }
  })
  
  observeEvent(input$clearAction, {
    for(i in 1:PAIR_COUNT)
    {
      graphList[[i]] <<-"N"
    }
    xMax <<- 0
    yMax <<- 0
    updateNumericInput(session, "xNum", value = xMax)
    updateNumericInput(session, "yNum", value = yMax)
    plotCount <<- 1
    lgndCount <<- 1
    lgnd <<- vector(mode="character", length=1)
  })
  
  observeEvent(input$plotAllAction, {
    for(i in 1:PAIR_COUNT)
    {
      graphList[[i]] <<-"N"
    }
    for(i in 1:(CENSUS_COUNT-1))
    {
      force(i)
      graphList[[calcIndex(i, (i+1))]] <<- "Y"
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
    plotCount <<- 1
    lgndCount <<- 1
    lgnd <<- vector(mode="character", length=1)
    if(!is.null(mort.data()) && !is.null(survey2()))
    {
      for(i in 1:(CENSUS_COUNT-1)){
        for(j in (i+1):(CENSUS_COUNT)){
          force(i)
          force(j)
          index = calcIndex(i,j)
          if(graphList[[index]] == "Y")
          {
            mortdbh.graph(i, j,sp=speciesName(),xrange=c(0, input$xNum),yrange=c(0,input$yNum), index = index)
          }
        }
      }
    }
  })
  
  mortdbh.graph=function(census1, census2, sp, xrange=NULL, yrange=NULL, index)
  {
    z <- paste(census1,census2, sep="->")
    lgnd[lgndCount] <<- z
    data = mort.data()[[index]]
    y=data[[1]]$rate
    aproblem=is.infinite(y)
    if(is.null(yrange)) yrange=c(0,max(y[!aproblem],na.rm=TRUE))
    if(is.null(xrange)) xrange=c(0,max(data[[1]]$dbhmean,na.rm=TRUE))
    xTemp = max(data[[1]]$dbhmean[!aproblem], na.rm =TRUE)
    if(xTemp > xMax)
    {
      xMax <<- xTemp
      updateNumericInput(session, "xNum", value = floor(xMax * 1.05))
    }
    yTemp = max(y[!aproblem], na.rm = TRUE)
    if(yTemp > yMax)
    {
      yMax <<- yTemp
      updateNumericInput(session, "yNum", value = round(yMax * 1.1, digits = 2))
    }
    lines(data[[1]]$dbhmean[!aproblem],y[!aproblem],type='l',lwd=2, col = cl[plotCount])
    legend('topright', legend = lgnd , lty=1, col = cl, cex=.75)
    lgndCount <<- lgndCount + 1
    plotCount <<- plotCount + 1
  }
  
  calcIndex <- function(s1, s2)
  {
    result = (s2 - (CENSUS_COUNT))
    for(i in 1:s1)
    {
      force(i)
      result = result + ((CENSUS_COUNT - 1) - (i-1))
    }
    return(result)
  }
})
