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
  
  output$graphAction <- renderUI({
    
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
  
  mort.data <- eventReactive(input$action,{
    cat("mort.data\n")
    mort.dataList = list()
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
      cat("FirstLoop\n")
      for(j in (i+1):CENSUS_COUNT){
        cat("SecondLoop\n")
        force(i)
        force(j)
        tempList = mortality.eachspp(censusList[[i]], censusList[[j]], classbreak = classes())
        mort.dataList = append(mort.dataList, list(tempList))
        cat("SecondLoopEnd\n")
      }
      
    }
    cat("MortDataEnd\n")
    return(mort.dataList)
    
  })
  
  dbhCatNum <- eventReactive(input$action, {
    input$dbhNumSelect
  })
  
  output$speciesNameTextOut <- renderText({speciesName()})
  
  #Graph rendering function must be reimplemented
  output$speciesGraph <- renderPlot({
    cat("plot\n")
    if(!is.null(mort.data()))
    {
      cat("dataNotNull\n")
      mortdbh.graph(data=mort.data()[[1]],sp=speciesName(),xrange=c(0, input$xNum),yrange=c(0,input$yNum))
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
      assemble.demography(mort.data()[1],type="m",whichdbhcat=num)
    }
  })
  
  #Species graphing function must be reimplemented
  mortdbh.graph=function(data,sp,xrange=NULL,yrange=NULL)
  {
    y=data$rate
    aproblem=is.infinite(y)
    if(is.null(yrange)) yrange=c(0,max(y[!aproblem],na.rm=TRUE))
    if(is.null(xrange)) xrange=c(0,max(data$dbhmean,na.rm=TRUE))
    plot(data$dbhmean[!aproblem],y[!aproblem],type='l',xlim=xrange,ylim=yrange,ylab=paste(sp,'mortality'),xlab='dbh')
  }
  
 
})
