
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(dplyr)
library(DT)
library(reshape2)
library(lubridate)
shinyServer(function(input, output) {
  readingData<-reactive({
    aux<-data.frame()
    # meses=3:5
    if(input$Opcion0=="MONTHS"){
      path<-"byMonth/"
      vector<-1:12
    }
    else if(input$Opcion0=="WEEK DAYS"){
      path<-"byDayOfWeek/"
      vector<-1:7
    }
    else if(input$Opcion0=="TIME BLOCKS"){
      if(input$Opcion2=="OPERATIONS") {
        path<-"byDepTimeBlock/"
        vector<-c("DEP_0001_0559","DEP_0600_0659","DEP_0700_0759","DEP_0800_0859","DEP_0900_0959","DEP_1000_1059",
                  "DEP_1100_1159","DEP_1200_1259","DEP_1300_1359","DEP_1400_1459","DEP_1500_1559","DEP_1600_1659",
                  "DEP_1700_1759","DEP_1800_1859","DEP_1900_1959","DEP_2000_2059","DEP_2100_2159","DEP_2200_2259",
                  "DEP_2300_2359")
      }
      else if(input$Opcion2=="DEPARTURE DELAYS (MIN)"){
        path<-"byDepTimeBlock/"
        vector<-c("DEP_0001_0559","DEP_0600_0659","DEP_0700_0759","DEP_0800_0859","DEP_0900_0959","DEP_1000_1059",
                  "DEP_1100_1159","DEP_1200_1259","DEP_1300_1359","DEP_1400_1459","DEP_1500_1559","DEP_1600_1659",
                  "DEP_1700_1759","DEP_1800_1859","DEP_1900_1959","DEP_2000_2059","DEP_2100_2159","DEP_2200_2259",
                  "DEP_2300_2359")
      }
      else if(input$Opcion2=="ARRIVAL DELAYS (MIN)"){
        path<-"byArrTimeBlock/"
        vector<-c("ARR_0001_0559","ARR_0600_0659","ARR_0700_0759","ARR_0800_0859","ARR_0900_0959","ARR_1000_1059",
                  "ARR_1100_1159","ARR_1200_1259","ARR_1300_1359","ARR_1400_1459","ARR_1500_1559","ARR_1600_1659",
                  "ARR_1700_1759","ARR_1800_1859","ARR_1900_1959","ARR_2000_2059","ARR_2100_2159","ARR_2200_2259",
                  "ARR_2300_2359")
      }
    }
    if(input$Opcion2=="OPERATIONS") {
      for (i in vector){
        readingData<-read.csv(sprintf("%sdata_nflights%s.csv",path,i))
        aux<-rbind(aux,readingData)
      }
    }
    else if(input$Opcion2=="DEPARTURE DELAYS (MIN)"){
      for (i in vector){
        readingData<-read.csv(sprintf("%sdata_deptimeblk%s.csv",path,i))
        aux<-rbind(aux,readingData)
      }
    }
    else if(input$Opcion2=="ARRIVAL DELAYS (MIN)"){
      for (i in vector){
        readingData<-read.csv(sprintf("%sdata_arrtimeblk%s.csv",path,i))
        aux<-rbind(aux,readingData)
      }
    }
    readingData<-aux
  })
  countData<-reactive({
    if(input$Opcion2=="OPERATIONS") {
      countData<-readingData()$NFLIGHTS
    }
    else if(input$Opcion2=="DEPARTURE DELAYS (MIN)" ){
      countData<-readingData()$COUNT
    }
    else if(input$Opcion2=="ARRIVAL DELAYS (MIN)"){
      countData<-readingData()$COUNT
    }
  })
  timeData<-reactive({
    if(input$Opcion0=="MONTHS"){
      timeData<- month(readingData()$MONTH,label=TRUE,abbr=TRUE)
    }
    else if(input$Opcion0=="WEEK DAYS"){
      timeData<-wday(readingData()$DAY_OF_WEEK,label=TRUE,abbr=TRUE)
    }
    else if(input$Opcion0=="TIME BLOCKS"){
      timeData<-readingData()$TIME_BLOCK#wday(readingData()$DAY_OF_WEEK,label=TRUE,abbr=TRUE)
    }
  })
  handlingData<-reactive({

    if(input$Opcion2=="OPERATIONS"){
      if(input$Opcion1=="TOTAL"){
        aux<-setNames(aggregate(countData(),by=list(timeData(),readingData()$FLIGHT_STATE),FUN=sum),c("TIME","FLIGHT_STATE","COUNT"))
        }
      else if(input$Opcion1=="AIRPORT"){
        aux<-setNames(aggregate(countData(),by=list(timeData(),readingData()$NAME_AIRPORT,readingData()$FLIGHT_STATE),FUN=sum),c("TIME","NAME_AIRPORT","FLIGHT_STATE","COUNT"))
        aux<-filter(aux,NAME_AIRPORT %in% input$i_NombreAeropuerto)
        }
      else if(input$Opcion1=="AIRLINE"){
        aux<-setNames(aggregate(countData(),by=list(timeData(),readingData()$NAME_AIRLINE,readingData()$FLIGHT_STATE),FUN=sum),c("TIME","NAME_AIRLINE","FLIGHT_STATE","COUNT"))
        aux<-filter(aux,NAME_AIRLINE %in% input$i_NombreAerolinea)
        }
    }
    else if(input$Opcion2=="DEPARTURE DELAYS (MIN)"){
      if(input$Opcion1=="TOTAL"){
        aux<-setNames(aggregate(countData(),by=list(timeData()),FUN=sum),c("TIME","COUNT"))
      }
      else if(input$Opcion1=="AIRPORT"){
        aux<-setNames(aggregate(countData(),by=list(timeData(),readingData()$NAME_AIRPORT),FUN=sum),c("TIME","NAME_AIRPORT","COUNT"))
        aux<-filter(aux,NAME_AIRPORT %in% input$i_NombreAeropuerto)
      }
      else if(input$Opcion1=="AIRLINE"){
        aux<-setNames(aggregate(countData(),by=list(timeData(),readingData()$NAME_AIRLINE),FUN=sum),c("TIME","NAME_AIRLINE","COUNT"))
        aux<-filter(aux,NAME_AIRLINE %in% input$i_NombreAerolinea)
      }
    }
    else if(input$Opcion2=="ARRIVAL DELAYS (MIN)"){
      if(input$Opcion1=="TOTAL"){
        aux<-setNames(aggregate(countData(),by=list(timeData()),FUN=sum),c("TIME","COUNT"))
      }
      else if(input$Opcion1=="AIRPORT"){
        aux<-setNames(aggregate(countData(),by=list(timeData(),readingData()$NAME_AIRPORT),FUN=sum),c("TIME","NAME_AIRPORT","COUNT"))
        aux<-filter(aux,NAME_AIRPORT %in% input$i_NombreAeropuerto)
      }
      else if(input$Opcion1=="AIRLINE"){
        aux<-setNames(aggregate(countData(),by=list(timeData(),readingData()$NAME_AIRLINE),FUN=sum),c("TIME","NAME_AIRLINE","COUNT"))
        aux<-filter(aux,NAME_AIRLINE %in% input$i_NombreAerolinea)
      }
    }
    handlingData<-aux
  })
  
  meanData<-reactive({
    if(input$Opcion2=="DEPARTURE DELAYS (MIN)"){
      if(input$Opcion1=="TOTAL"){
        aux<-setNames(aggregate(readingData()$data_deptimeblk_mean.MEAN,by=list(timeData()),FUN=mean, na.rm=TRUE),c("TIME","MEAN"))
      }
      else if(input$Opcion1=="AIRPORT"){
        aux<-setNames(aggregate(readingData()$data_deptimeblk_mean.MEAN,by=list(timeData(),readingData()$NAME_AIRPORT),FUN=mean, na.rm=TRUE),c("TIME","NAME_AIRPORT","MEAN"))
        aux<-filter(aux,NAME_AIRPORT %in% input$i_NombreAeropuerto)
      }
      else if(input$Opcion1=="AIRLINE"){
        aux<-setNames(aggregate(readingData()$data_deptimeblk_mean.MEAN,by=list(timeData(),readingData()$NAME_AIRLINE),FUN=mean, na.rm=TRUE),c("TIME","NAME_AIRLINE","MEAN"))
        aux<-filter(aux,NAME_AIRLINE %in% input$i_NombreAerolinea)
      }
    }
    else if(input$Opcion2=="ARRIVAL DELAYS (MIN)"){
      if(input$Opcion1=="TOTAL"){
        aux<-setNames(aggregate(readingData()$data_arrtimeblk_mean.MEAN,by=list(timeData()),FUN=mean, na.rm=TRUE),c("TIME","MEAN"))
      }
      else if(input$Opcion1=="AIRPORT"){
        aux<-setNames(aggregate(readingData()$data_arrtimeblk_mean.MEAN,by=list(timeData(),readingData()$NAME_AIRPORT),FUN=mean, na.rm=TRUE),c("TIME","NAME_AIRPORT","MEAN"))
        aux<-filter(aux,NAME_AIRPORT %in% input$i_NombreAeropuerto)
      }
      else if(input$Opcion1=="AIRLINE"){
        aux<-setNames(aggregate(readingData()$data_arrtimeblk_mean.MEAN,by=list(timeData(),readingData()$NAME_AIRLINE),FUN=mean, na.rm=TRUE),c("TIME","NAME_AIRLINE","MEAN"))
        aux<-filter(aux,NAME_AIRLINE %in% input$i_NombreAerolinea)
      }
    }
    aux$MEAN<-round(aux$MEAN, digits = 2)
    handlingData<-aux
  })

  output$o_NombreAeropuerto<-renderUI({
    selectInput(inputId="i_NombreAeropuerto", label="Choose airport:", choices=as.character(unique(readingData()$NAME_AIRPORT)), selected = NULL, multiple = FALSE,
                selectize = TRUE)
  })
  
  output$o_NombreAerolinea<-renderUI({
    selectInput(inputId="i_NombreAerolinea", label="Elije la aerolínea:", choices=as.character(unique(readingData()$NAME_AIRLINE)), selected = NULL, multiple = FALSE,
                selectize = TRUE)
  })

  output$distPlot <- renderPlotly({

    if(input$Opcion2=="OPERATIONS"){#month.abb[
      plot_ly(handlingData(),x=handlingData()$TIME,y=handlingData()$COUNT,color=factor(handlingData()$FLIGHT_STATE),colors=c("limegreen","navyblue"), type='bar') %>%  
      layout(title=sprintf("2016 %s",input$Opcion2),xaxis=list(title=input$Opcion0, font=list(size=5)),yaxis=list(title=input$Opcion2),barmode='stack', margin=list(b = 100))
    }
    else if(input$Opcion2=="DEPARTURE DELAYS (MIN)"){
      plot_ly(handlingData(),x=handlingData()$TIME,y=handlingData()$COUNT, type='bar', name = 'Total delay') %>%
      add_trace(meanData(),x=meanData()$TIME, y=meanData()$MEAN, type='scatter', mode='lines', name='Average delay') %>%  
      layout(title=sprintf("2016 %s",input$Opcion2),xaxis=list(title=input$Opcion0),yaxis=list(title=input$Opcion2),barmode='stack' ,margin=list(b = 100))
    }
    else if(input$Opcion2=="ARRIVAL DELAYS (MIN)"){
      plot_ly(handlingData(),x=handlingData()$TIME,y=handlingData()$COUNT, type='bar', name = 'Total delay') %>%
      add_trace(meanData(),x=meanData()$TIME, y=meanData()$MEAN, type='scatter', mode='lines', name='Average delay') %>%  
      layout(title=sprintf("2016 %s",input$Opcion2),xaxis=list(title=input$Opcion0),yaxis=list(title=input$Opcion2),barmode='stack', margin=list(b = 100))
    }
    # if(input$Opcion0=="TIME BLOCKS"){
    #   %>%
    #   layout() 
    # }
    
  })
  output$tablaInfo <- DT::renderDataTable({
    if(input$Opcion2=="OPERATIONS"){
      aux<-dcast(handlingData(),TIME~FLIGHT_STATE,value.var = "COUNT")
      aux[is.na(aux)]<-0
      TOTAL_VALUE<-rowSums(aux[, -1])
      aux<-mutate(aux, TOTAL=TOTAL_VALUE)
      print(aux)
      if("CANCELLED" %in% colnames(aux)){
        aux$CANCELLED<-sprintf("%s / %s%%",as.character(aux$CANCELLED),as.character(100*round(aux$CANCELLED/as.double(aux$TOTAL), 3)))
      }
      if("ON_TIME" %in% colnames(aux)){
        aux$ON_TIME<-sprintf("%s / %s%%",as.character(aux$ON_TIME),as.character(100*round(aux$ON_TIME/as.double(aux$TOTAL), 3)))
      }
      if("DIVERTED" %in% colnames(aux)){
        aux$DIVERTED<-sprintf("%s / %s%%",as.character(aux$DIVERTED),as.character(100*round(aux$DIVERTED/as.double(aux$TOTAL), 3)))
      }
      if("DELAYED" %in% colnames(aux)){
        aux$DELAYED<-sprintf("%s / %s%%",as.character(aux$DELAYED),as.character(100*round(aux$DELAYED/as.double(aux$TOTAL), 3)))
      }
      print(aux)
      if(input$Opcion0=="MONTHS"){ 
        colnames(aux)[1]<-"MONTH"
      }
      else if(input$Opcion0=="WEEK DAYS"){ 
        colnames(aux)[1]<-"DAYS OF THE WEEK"
      }  
      else if(input$Opcion0=="TIME BLOCKS"){ 
        colnames(aux)[1]<-"TIME BLOCKS"
      }
    }
    else if(input$Opcion2=="DEPARTURE DELAYS (MIN)"){
      aux<-setNames(cbind(handlingData(),meanData()$MEAN),c(names(handlingData()),"AVERAGE MIN DELAYED PER FLIGTH"))
      names(aux)[names(aux) == 'COUNT'] <- 'TOTAL MIN DELAYED'
      if(input$Opcion0=="MONTHS"){ 
        colnames(aux)[1]<-"MONTH"
      }
      else if(input$Opcion0=="WEEK DAYS"){ 
        colnames(aux)[1]<-"DAYS OF THE WEEK"
      }
      print(colnames(aux))
      if(input$Opcion1=="AIRPORT"){
        aux$NAME_AIRPORT <- NULL
      }
      else if(input$Opcion1=="AIRLINE"){
        aux$NAME_AIRLINE <- NULL
      }
    }
    else if(input$Opcion2=="ARRIVAL DELAYS (MIN)"){
      aux<-setNames(cbind(handlingData(),meanData()$MEAN),c(names(handlingData()),"AVERAGE MIN DELAYED PER FLIGTH"))
      names(aux)[names(aux) == 'COUNT'] <- 'TOTAL MIN DELAYED'
      if(input$Opcion0=="MONTHS"){ 
        colnames(aux)[1]<-"MONTH"
      }
      else if(input$Opcion0=="WEEK DAYS"){ 
        colnames(aux)[1]<-"DAYS OF THE WEEK"
      }
      if(input$Opcion1=="AIRPORT"){
        aux$NAME_AIRPORT <- NULL
      }
      else if(input$Opcion1=="AIRLINE"){
        aux$NAME_AIRLINE <- NULL
      }
    }
    DT::datatable(aux, class = 'nowrap', selection ="multiple",rownames=FALSE,  
                  options = list(
                    pageLength=12,
                    bPaginat=FALSE,
                    bLengthChange=FALSE,
                    bFilter=FALSE,
                    showNEntries=FALSE,
                    bInfo=FALSE,
                    bAutoWidth=FALSE,
                    
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#2F4F4F', 'color': '#fff'});",
        "}")
    ))
  })

})
