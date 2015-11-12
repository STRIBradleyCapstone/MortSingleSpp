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

CENSUS_COUNT <- 7

shinyServer(function(input, output, session) {
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
  
  survey1 <- eventReactive(input$action,{
    switch(input$survey1Select, 
           "full1" = {return(1)},
           "full2" = {return(2)},
           "full3" = {return(3)},
           "full4" = {return(4)},
           "full5" = {return(5)},
           "full6" = {return(6)})
  })
  
  survey2 <- eventReactive(input$plotAction,{
    slist = c("full1", "full2", "full3", "full4", "full5", "full6", "full7")
    #match function calculates the indices of each survey input within a list of all surveys
    #Used to ensure that survey1 is less than survey2
    cat("survey2Validate\n")
    validate(
      need(match(input$survey1Select, slist) < match(input$survey2Select, slist), "First survey must have occurred before the second survey")
    )
    cat("survey2ValidateEnd\n")
    #returns the census number -1 so that using it to subset mort.dataList will yield the correct subset.
    switch(input$survey2Select,
           "full2" = {return(1)},
           "full3" = {return(2)},
           "full4" = {return(3)},
           "full5" = {return(4)},
           "full6" = {return(5)},
           "full7" = {return(6)})
  })
  
  mort.data <- eventReactive(input$plotAction,{
    cat("mort.data\n")
    mort.dataList = matrix(list(), nrow = CENSUS_COUNT - 1, ncol = CENSUS_COUNT)
    cat("buildingCensusList\n")
    censusList = c(list(bci.full1[bci.full1$sp == speciesName(), 1:20]),
                   list(bci.full2[bci.full2$sp == speciesName(), 1:20]),
                   list(bci.full3[bci.full3$sp == speciesName(), 1:20]),
                   list(bci.full4[bci.full4$sp == speciesName(), 1:20]),
                   list(bci.full5[bci.full5$sp == speciesName(), 1:20]),
                   list(bci.full6[bci.full6$sp == speciesName(), 1:20]),
                   list(bci.full7[bci.full7$sp == speciesName(), 1:20]))
    cat("censusListBuilt\n")
    for(i in 1:(CENSUS_COUNT - 1)){
      for(j in (i+1):CENSUS_COUNT){
        force(i)
        force(j)
        tempList = mortality.eachspp(censusList[[i]], censusList[[j]], classbreak = classes())
        #templist["active"] = "N"
        mort.dataList[[i,j - 1]] <- list(tempList)
      }
      
    }
    cat("MortDataEnd\n")
    return(mort.dataList)
    
  })
  
  #observeEvent(input$plotAction, {
  #  mort.data()[[survey1(), survey2()]]$active <- "Y"
  #})
  
  dbhCatNum <- eventReactive(input$action, {
    input$dbhNumSelect
  })
  
  output$speciesNameTextOut <- renderText({speciesName()})
  
  #Graph rendering function must be reimplemented
  output$speciesGraph <- renderPlot({
    cat("plot\n")
    plot(x=NULL,y=NULL,type='l',xlim=c(0, input$xNum),ylim=c(0, input$yNum),ylab=paste(speciesName(),'mortality'),xlab='dbh')
    if(!is.null(mort.data()) && !is.null(survey2()))
    {
      cat("dataNotNull\n")
      mortdbh.graph(survey1(), survey2(),sp=speciesName(),xrange=c(0, input$xNum),yrange=c(0,input$yNum))
    }
    else
    {
      cat("dataNull\n")
    }
    cat("plotEnd\n")
  })
  
  output$table <- renderTable({
    if(!is.null(mort.data()))
    {
      assemble.demography(mort.data()[1,2],type="m",whichdbhcat=num)
    }
  })
  
  #Species graphing function must be reimplemented
  mortdbh.graph=function(census1, census2 ,sp,xrange=NULL,yrange=NULL)
  {
    cat("graphStart\n")
    data = mort.data()[[census1, census2]]
    y=data[[1]]$rate
    aproblem=is.infinite(y)
    if(is.null(yrange)) yrange=c(0,max(y[!aproblem],na.rm=TRUE))
    if(is.null(xrange)) xrange=c(0,max(data[[1]]$dbhmean,na.rm=TRUE))
    cat("graphing...\n")
    lines(data[[1]]$dbhmean[!aproblem],y[!aproblem],type='l')
    cat("graphEnd\n")
  }
  
  
})
