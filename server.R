.libPaths("Data")
library(shiny)
library(date)
attach("Data/CTFSRPackage.rdata")
load("Data/Full/bci.full1.rdata")
load("Data/Full/bci.full2.rdata")
load("Data/Full/bci.full3.rdata")
load("Data/Full/bci.full4.rdata")
load("Data/Full/bci.full5.rdata")
load("Data/Full/bci.full6.rdata")
load("Data/Full/bci.full7.rdata")
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
  
  speciesName <- eventReactive(input$speciesAction,{
    input$speciesSelect
  })
  
  mort.data <- eventReactive(input$action,{
    mort.dataList = list();
    #output.catch(count <<- CENSUS_PAIR_COUNT)
    surveyList = c(bci.full1, bci.full2, bci.full3, bci.full4, bci.full5, bci.full6, bci.full7)
    censusList = c()
    for(i in 1:CENSUS_COUNT)
    {
      append(censusList, surveyList[i][surveyList[i]$sp == speciesName(),])
    }
    for(i in 1:CENSUS_COUNT - 1){
      for(j in i+1:CENSUS_COUNT){
        mort.dataList = append(mort.dataList, mortality.eachspp(censusList[i], censusList[j], classbreak = classes()))
      }
      
    }
    
    return(mort.dataList)
    
  })
  
  dbhCatNum <- eventReactive(input$action, {
    input$dbhNumSelect
  })
  
  output$speciesNameTextOut <- renderText({speciesName()})
  
  #Graph rendering function must be reimplemented
  output$speciesGraph <- renderPlot({
    input$action
    if(!is.null(mort.data()))
    {
      mortdbh.graph(data=mort.data()[1],sp=speciesName(),xrange=c(0, input$xNum),yrange=c(0,input$yNum))
    }
  })
  
  #Species graphing function must be reimplemented
  mortdbh.graph=function(data,sp,xrange=NULL,yrange=NULL)
  {
    y=data$rate
    aproblem=is.infinite(y)
    if(is.null(yrange)) yrange=c(0,max(y[!aproblem],na.rm=TRUE))
    if(is.null(xrange)) xrange=c(0,max(data$dbhmean[sp,],na.rm=TRUE))
    plot(data$dbhmean[sp,!aproblem],y[!aproblem],type='l',xlim=xrange,ylim=yrange,ylab=paste(sp,'mortality'),xlab='dbh')
  }
})
