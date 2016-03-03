library(shiny)
library(date)
attach("../../data/CTFSRPackage.rdata")
#load("../../data/CTFSRPackage/species/bci.spptable.rdata")

shinyServer(function(input, output, session) {
  
  #Initializes a list of which census pairs are active.  All set to inactive ("N")
  #graphListInit <- function()
  #{
  #  tempList = list()
  #  for(i in 1:PAIR_COUNT())
  #  {
  #    tempList = append(tempList, "N")
  #  }
  #  return(tempList)
  #}
  
  #Calculates amount of possible census pairs
  #calcPairCount <- function()
  #{
  #  return((CENSUS_COUNT() * (CENSUS_COUNT() - 1)) / 2)
  #}
  
  calcIndex <- function(s1, s2)
  {
    return ((s2 * (s2 - 1)) / 2 - s1 + 1)
  }
  
  #Region Selector
  output$regionSelect <- renderUI({
    allregions <- lapply(list.dirs(path)[-1], substring, nchar(path)+2)
    selectInput("regionSelect", label = "Select a region", choices = allregions, selected = allregions[1])
  })

  path <- "../../data/CTFSRPackage/full/"
  census_list <- eventReactive(input$regionAction, {
    clist = list()
    for (name in list.files(paste0(path,"/",input$regionSelect))) {
      load(paste0(path,"/",input$regionSelect,"/",name))
      clist = append(clist,list(get(substr(name,0,nchar(name)-6))))
    }
    #CENSUS_COUNT <<- length(clist)
    #PAIR_COUNT <<- (CENSUS_COUNT * (CENSUS_COUNT - 1)) / 2
    validate (need(length(clist) > 1, "Need at least two censuses"))
    return (clist)
    #load("../../data/CTFSRPackage/full/bci.full1.rdata")
    #load("../../data/CTFSRPackage/full/bci.full2.rdata")
    #load("../../data/CTFSRPackage/full/bci.full3.rdata")
    #load("../../data/CTFSRPackage/full/bci.full4.rdata")
    #load("../../data/CTFSRPackage/full/bci.full5.rdata")
    #load("../../data/CTFSRPackage/full/bci.full6.rdata")
    #load("../../data/CTFSRPackage/full/bci.full7.rdata")
    #return (c(list(bci.full1), list(bci.full2), list(bci.full3), list(bci.full4),
    #          list(bci.full5), list(bci.full6), list(bci.full7)))
  })
  #CENSUS_COUNT <- 7
  #PAIR_COUNT <- calcPairCount()
  CENSUS_COUNT <- reactive ({
    return (length(census_list()))
  })
  PAIR_COUNT <- reactive ({
    new_count = ((CENSUS_COUNT() * (CENSUS_COUNT() - 1)) / 2)
    tempList = list()
    for(i in 1:new_count)
    {
      tempList = append(tempList, "N")
    }
    graphList <<- tempList
    return (new_count)
  })
  graphList <- list()
  plotCount <- 1
  lgndCount <- 1
  cl <- reactive ({
    return (rainbow(PAIR_COUNT(), end = .9))
  })
  lgnd <- vector(mode="character", length=1)
  lgndColor <- vector(mode="character", length = 1)
  xMax <- 0
  yMax <- 0

  #Species Selector
  output$speciesSelect <- renderUI({
   #capture.output(allspp <- sort(unique(bci.full1$sp))) #Prevents output to user
    capture.output(allspp <- sort(unique(census_list()[[1]]$sp))) #Prevents output to user
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
  
  output$surveySlider <- renderUI({
    sliderInput("surveySlider", "Please select surveys to plot", min = 1, max = CENSUS_COUNT(), value = c(1,2), step = 1, ticks = FALSE)
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
  
  #Retrieve selected species
  speciesName <- eventReactive(input$action,{
    input$speciesSelect
  })
  
  #Retrieve selected surveys
  surveyVector <- reactive({
    surveyV = c(input$surveySlider[1], input$surveySlider[2])
    
    #If the 2 points on the slider are set to the same value move one
    if(input$surveySlider[1] == input$surveySlider[2])
    {
      if(input$surveySlider[2] == CENSUS_COUNT())
      {
        surveyV[1] = surveyV[1] - 1
        updateSliderInput(session, "surveySlider", value = c(surveyV[1], surveyV[2]))
      }
      else
      {
        surveyV[2] = surveyV[2] + 1
        updateSliderInput(session, "surveySlider", value = c(surveyV[1], surveyV[2]))
      }
    }
    return(surveyV)
  })
  
  func.data <- eventReactive(input$action,{
    cat("MortDataStart")
    func.dataList = list()
    #censusList = c(list(bci.full1[bci.full1$sp == speciesName(), 1:20]),
    #               list(bci.full2[bci.full2$sp == speciesName(), 1:20]),
    #               list(bci.full3[bci.full3$sp == speciesName(), 1:20]),
    #               list(bci.full4[bci.full4$sp == speciesName(), 1:20]),
    #               list(bci.full5[bci.full5$sp == speciesName(), 1:20]),
    #               list(bci.full6[bci.full6$sp == speciesName(), 1:20]),
    #               list(bci.full7[bci.full7$sp == speciesName(), 1:20]))
    censusList = list()
    for(i in 1:CENSUS_COUNT()){
      force(i)
      censusList = append(censusList, (list(census_list()[[i]][census_list()[[i]]$sp == speciesName(), 1:20])))
    }
    count = 1
    cat("MortDataLoop")
    for(i in 1:(CENSUS_COUNT() - 1)){
      for(j in (i+1):CENSUS_COUNT()){
        force(i)
        force(j)
        if (input$calcTypeRadio == "Mortality")
          tempList = mortality.eachspp(censusList[[i]], censusList[[j]], classbreak = classes())
        else
          tempList = growth.eachspp(censusList[[i]], censusList[[j]], classbreak = classes())
        func.dataList[[count]] <- list(tempList)
        count = count + 1
      }
      
    }
    print(func.dataList)
    return(func.dataList)
    
  })
  
  observeEvent(input$plotAction, {
    if(!is.null(func.data()))
    {
      graphList[[calcIndex(surveyVector()[1], surveyVector()[2])]] <<- "Y"
    }
  })
  
  observeEvent(input$clearAction, {
    for(i in 1:PAIR_COUNT())
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
    lgndColor <- vector(mode="character", length = 1)
  })
  
  observeEvent(input$plotAllAction, {
    print(PAIR_COUNT())
    for(i in 1:PAIR_COUNT())
    {
      graphList[[i]] <<-"N"
    }
    for(i in 1:(CENSUS_COUNT() - 1))
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
    cat("renderPlotStart")
    print(func.data())
    input$plotAction
    input$clearAction
    input$plotAllAction
    plot(x=NULL,y=NULL,type='l',xlim=c(0, input$xNum),ylim=c(0, input$yNum),ylab=paste(speciesName(),tolower(input$calcTypeRadio)),xlab='dbh')
    plotCount <<- 1
    lgndCount <<- 1
    lgndColor <- vector(mode="character", length = 1)
    lgnd <<- vector(mode="character", length=1)
    if(!is.null(func.data()) && !is.null(surveyVector()[2]))
    {
      cat("Rendering")
      for(i in 1:(CENSUS_COUNT() - 1)){
        for(j in (i+1):(CENSUS_COUNT())){
          force(i)
          force(j)
          index = calcIndex(i,j)
          if(graphList[[index]] == "Y")
          {
            funcdbh.graph(i, j,sp=speciesName(),xrange=c(0, input$xNum),yrange=c(0,input$yNum), index = index)
          }
        }
      }
    }
  })
  
  funcdbh.graph=function(census1, census2, sp, xrange=NULL, yrange=NULL, index)
  {
    z <- paste(census1,census2, sep="->")
    index = calcIndex(census1, census2)
    lgnd[lgndCount] <<- z
    lgndColor[plotCount] <<- cl()[index]
    data = func.data()[[index]]
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
    lines(data[[1]]$dbhmean[!aproblem],y[!aproblem],type='l',lwd=2, col = cl()[index])#col update
    legend('topright', legend = lgnd , lty=1, col = lgndColor, cex=.75)#col updated
    lgndCount <<- lgndCount + 1
    plotCount <<- plotCount + 1
  }
})
