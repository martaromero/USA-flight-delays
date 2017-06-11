library(dplyr)
library(reshape2)
###########################################################################################################
#############################Carga de los CSV#############################################################
names_airlines<-read.csv(file='csv/L_UNIQUE_CARRIERS.csv', sep=',')
names_airports<-read.csv(file='csv/L_AIRPORT_ID.csv', sep=',')
############No se si hay que hacerlo pero esta fallando aqui:
names_airlines$Description<-as.character(names_airlines$Description)
names_airlines$Code<-as.character(names_airlines$Code)
names_airports$Description<-as.character(names_airlines$Description)
names_airports$Code<-as.character(names_airlines$Code)


############################################################################################################
##############################Funciones utiles#############################################################
#A) PARA MESES;
readingMonths<-function(mes){
  if(mes<10){
    path<-sprintf("csv/meses/0%d_16.csv",mes)
  }
  else{
    path<-sprintf("csv/meses/%d_16.csv",mes)
  }
  data<-read.csv(file=path, sep=";")
  return(data)
}

#B) PARA DIAS:
readingDays<-function(dias){
  path<-sprintf("csv/dias/%d.csv",dias)
  data<-read.csv(file=path, sep=",")
  return(data)
}

#C) PARA FRANJAS HORARIAS:
#C.1) A la salida:
readingSlotsDep<-function(franja){
  path<-sprintf("csv/franja_horaria_salida/%s.csv",franja)
  data<-read.csv(file=path, sep=",")
  return(data)
}

#C.2) A la llegada:
readingSlotsArr<-function(franja){
  path<-sprintf("csv/franja_horaria_llegada/%s.csv",franja)
  data<-read.csv(file=path, sep=",")
  return(data)
}
#############################################################################################################
##############################Obtencion de datos:###########################################################
#A) Por meses:
meses<-1:12
for(i in meses){
  data_0<-readingMonths(i)
  # data_nflights<-setNames(aggregate(data_0$UNIQUE_CARRIER,by=list(data_0$MONTH,data_0$AIRLINE,data_0$ORIGIN_AIRPORT,data_0$FLIGHT_STATE),FUN=NROW),c("MONTH","NAME_AIRLINE","NAME_AIRPORT","FLIGHT_STATE","NFLIGHTS"))
  # print(sprintf("Escribiendo el numero de vuelos para el mes %d",i))
  # write.csv(data_nflights,file=sprintf("byMonth/data_nflights%d.csv",i))
  # data_nflights<-read.csv("data_nflights2.csv")
  #Minutos totales de retraso:
  # data_mindelay<-setNames(aggregate(data_0$DEP_DELAY_NEW,by=list(data_0$MONTH,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=sum, na.rm=TRUE),c("MONTH","NAME_AIRLINE","NAME_AIRPORT","DELAYSUM"))
  # print(sprintf("Escribiendo el numero de minutos de retrasos para el mes %d",i))
  # write.csv(data_mindelay,file=sprintf("byMonth/data_mindelay%d.csv",i))
  #Number of departure time block according to time range and flight_state by time, airline and airport
  data_deptimeblk<-setNames(aggregate(data_0$DEP_DELAY_NEW,by=list(data_0$MONTH,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=sum, na.rm=TRUE),c("MONTH","NAME_AIRLINE","NAME_AIRPORT","COUNT"))
  data_deptimeblk_mean<-setNames(aggregate(data_0$DEP_DELAY_NEW,by=list(data_0$MONTH,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=mean, na.rm=TRUE),c("MONTH","NAME_AIRLINE","NAME_AIRPORT","MEAN"))
  data_deptimeblk<-cbind(data_deptimeblk,data_deptimeblk_mean$MEAN)
  # names(data_deptimeblk)[names(data_deptimeblk) == 'data_arrtimeblk_mean.MEAN'] <- 'MEAN'
  print(sprintf("Escribiendo el numero de franjas horarias de retrasos en la salida para el mes %d",i))
  write.csv(data_deptimeblk,file=sprintf("byMonth/data_deptimeblk%d.csv",i))
  #Number of arrive time block according to time range and flight_state by time, airline and airport
  data_arrtimeblk<-setNames(aggregate(data_0$ARR_DELAY_NEW,by=list(data_0$MONTH,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=sum, na.rm=TRUE),c("MONTH","NAME_AIRLINE","NAME_AIRPORT","COUNT"))
  data_arrtimeblk_mean<-setNames(aggregate(data_0$ARR_DELAY_NEW,by=list(data_0$MONTH,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=mean, na.rm=TRUE),c("MONTH","NAME_AIRLINE","NAME_AIRPORT","MEAN"))
  data_arrtimeblk<-cbind(data_arrtimeblk,data_arrtimeblk_mean$MEAN)
  print(sprintf("Escribiendo el numero de franjas horarias de retrasos en la llegada para el mes %d",i))
  write.csv(data_arrtimeblk,file=sprintf("byMonth/data_arrtimeblk%d.csv",i))
}
#B) Por dias de la semana:
dias<-1:7
for(i in dias){
  data_0<-readingDays(i)
  # data_nflights<-setNames(aggregate(data_0$UNIQUE_CARRIER,by=list(data_0$DAY_OF_WEEK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT,data_0$FLIGHT_STATE),FUN=NROW),c("DAY_OF_WEEK","NAME_AIRLINE","NAME_AIRPORT","FLIGHT_STATE","NFLIGHTS"))
  # print(sprintf("Escribiendo el numero de vuelos para el dia %d",i))
  # write.csv(data_nflights,file=sprintf("byDayOfWeek/data_nflights%d.csv",i))
  # data_nflights<-read.csv("data_nflights2.csv")
  #Minutos totales de retraso:
  # data_mindelay<-setNames(aggregate(data_0$ARR_DELAY_NEW,by=list(data_0$DAY_OF_WEEK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=sum, na.rm=TRUE),c("DAY_OF_WEEK","NAME_AIRLINE","NAME_AIRPORT","DELAYSUM"))
  # print(sprintf("Escribiendo el numero de minutos de retrasos para el dia %d",i))
  # write.csv(data_mindelay,file=sprintf("byDayOfWeek/data_mindelay%d.csv",i))
  #Number of departure time block according to time range and flight_state by time, airline and airport
  data_deptimeblk<-setNames(aggregate(data_0$DEP_DELAY_NEW,by=list(data_0$DAY_OF_WEEK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=sum, na.rm=TRUE),c("DAY_OF_WEEK","NAME_AIRLINE","NAME_AIRPORT","COUNT"))
  data_deptimeblk_mean<-setNames(aggregate(data_0$DEP_DELAY_NEW,by=list(data_0$DAY_OF_WEEK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=mean, na.rm=TRUE),c("DAY_OF_WEEK","NAME_AIRLINE","NAME_AIRPORT","MEAN"))
  data_deptimeblk<-cbind(data_deptimeblk,data_deptimeblk_mean$MEAN)
  print(sprintf("Escribiendo el numero de franjas horarias de retrasos en la salida para el dia %d",i))
  write.csv(data_deptimeblk,file=sprintf("byDayOfWeek/data_deptimeblk%d.csv",i))
  #Number of arrive time block according to time range and flight_state by time, airline and airport
  data_arrtimeblk<-setNames(aggregate(data_0$ARR_DELAY_NEW,by=list(data_0$DAY_OF_WEEK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=sum, na.rm=TRUE),c("DAY_OF_WEEK","NAME_AIRLINE","NAME_AIRPORT","COUNT"))
  data_arrtimeblk_mean<-setNames(aggregate(data_0$ARR_DELAY_NEW,by=list(data_0$DAY_OF_WEEK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=mean, na.rm=TRUE),c("DAY_OF_WEEK","NAME_AIRLINE","NAME_AIRPORT","MEAN"))
  data_arrtimeblk<-cbind(data_arrtimeblk,data_arrtimeblk_mean$MEAN)
  print(sprintf("Escribiendo el numero de franjas horarias de retrasos en la llegada para el dia %d",i))
  write.csv(data_arrtimeblk,file=sprintf("byDayOfWeek/data_arrtimeblk%d.csv",i))
}

#C) Por franjas horarias:
#C.1) A la salida:
franjasDep<-c("DEP_0001_0559","DEP_0600_0659","DEP_0700_0759","DEP_0800_0859","DEP_0900_0959","DEP_1000_1059",
           "DEP_1100_1159","DEP_1200_1259","DEP_1300_1359","DEP_1400_1459","DEP_1500_1559","DEP_1600_1659",
           "DEP_1700_1759","DEP_1800_1859","DEP_1900_1959","DEP_2000_2059","DEP_2100_2159","DEP_2200_2259",
           "DEP_2300_2359"
           )

for (i in franjasDep){
  data_0<-readingSlotsDep(i)
  #Operations:
  data_nflights<-setNames(aggregate(data_0$UNIQUE_CARRIER,by=list(data_0$DEP_TIME_BLK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT,data_0$FLIGHT_STATE),FUN=NROW),c("TIME_BLOCK","NAME_AIRLINE","NAME_AIRPORT","FLIGHT_STATE","NFLIGHTS"))
  print(sprintf("Escribiendo el numero de vuelos para la franja horaria %s",i))
  write.csv(data_nflights,file=sprintf("byDepTimeBlock/data_nflights%s.csv",i))
  #Number of departure time block according to time range and flight_state by time, airline and airport
  data_deptimeblk<-setNames(aggregate(data_0$DEP_DELAY_NEW,by=list(data_0$DEP_TIME_BLK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=sum, na.rm=TRUE),c("TIME_BLOCK","NAME_AIRLINE","NAME_AIRPORT","COUNT"))
  data_deptimeblk_mean<-setNames(aggregate(data_0$DEP_DELAY_NEW,by=list(data_0$DEP_TIME_BLK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=mean, na.rm=TRUE),c("TIME_BLOCK","NAME_AIRLINE","NAME_AIRPORT","MEAN"))
  data_deptimeblk<-cbind(data_deptimeblk,data_deptimeblk_mean$MEAN)
  print(sprintf("Escribiendo el numero de franjas horarias de retrasos en la salida para la franja horaria %s",i))
  write.csv(data_deptimeblk,file=sprintf("byDepTimeBlock/data_deptimeblk%s.csv",i))
}

#C.2) A la llegada:
franjasArr<-c("ARR_0001_0559","ARR_0600_0659","ARR_0700_0759","ARR_0800_0859","ARR_0900_0959","ARR_1000_1059",
           "ARR_1100_1159","ARR_1200_1259","ARR_1300_1359","ARR_1400_1459","ARR_1500_1559","ARR_1600_1659",
           "ARR_1700_1759","ARR_1800_1859","ARR_1900_1959","ARR_2000_2059","ARR_2100_2159","ARR_2200_2259",
           "ARR_2300_2359"
)

for (i in franjasArr){
  data_0<-readingSlotsArr(i)
  #Number of arrive time block according to time range and flight_state by time, airline and airport
  data_arrtimeblk<-setNames(aggregate(data_0$ARR_DELAY_NEW,by=list(data_0$ARR_TIME_BLK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=sum, na.rm=TRUE),c("TIME_BLOCK","NAME_AIRLINE","NAME_AIRPORT","COUNT"))
  data_arrtimeblk_mean<-setNames(aggregate(data_0$ARR_DELAY_NEW,by=list(data_0$ARR_TIME_BLK,data_0$AIRLINE,data_0$ORIGIN_AIRPORT),FUN=mean, na.rm=TRUE),c("TIME_BLOCK","NAME_AIRLINE","NAME_AIRPORT","MEAN"))
  data_arrtimeblk<-cbind(data_arrtimeblk,data_arrtimeblk_mean$MEAN)
  print(sprintf("Escribiendo el numero de franjas horarias de retrasos en la llegada para la franja horaria %s",i))
  write.csv(data_arrtimeblk,file=sprintf("byArrTimeBlock/data_arrtimeblk%s.csv",i))
}

##############################################################################################################
################################Pruebas######################################################################
data_nflights$NAME_AIRLINE<-as.character(data_nflights$NAME_AIRLINE)
print(typeof(data_nflights$NAME_AIRLINE))
data_nflights$NAME_AIRPORT<-as.character(data_nflights$NAME_AIRPORT)
names_airlines<-read.csv(file='csv/L_UNIQUE_CARRIERS.csv', sep=',')
print(names(names_airlines))
print(typeof(names_airlines$Code))
print(typeof(names_airlines$Description))
print(names_airlines$Description)
print(names_airlines$Code)
names_airlines$Description<-as.character(names_airlines$Description)

print(names(data_nflights))
resultado<-unlist(strsplit(data_nflights$NAME_AIRPORT, ","))
print(dim(resultado))
print(data_nflights$NAME_AIRLINE)

print(names(data_nflights))
data=data_nflights
data=group_by(FLIGHT_STATE)

aux<-data.frame()
aux1<-data.frame()
print(names(flights))
vector=1:3
for (i in vector){
  aux1<-data_nflights[i,]
  aux<-rbind(aux,aux1)
}
print(aux)
aux<-rbind(data_nflights[1,],data_nflights[2,],data_nflights[3,])
print(aux$FLIGHT_STATE[[1]])
aux$FLIGHT_STATE[[2]]<-"ON_TIME"
aux$NAME_AIRLINE <- NULL
aux$NAME_AIRPORT <- NULL
print(aux)
print(names(aux))
aux<-mutate(aux,TOTAL=)
prueba<-dcast(aux,MONTH~FLIGHT_STATE, value.var = "NFLIGHTS")
print(prueba)

air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
flights <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
print(flights)
print(air)
print(names(names_airports))
print(strsplit(names_airports$Description, ":"))
print(unlist(strsplit(names_airports$Description, ":")))
pepe<-read.csv(file="byMonth/data_deptimeblk9.csv")
print(pepe)
pepe_f<-filter(pepe, pepe$MONTH==1)
a<-read.csv(file="byMonth/data_mindelay3.csv")
b<-read.csv(file="csv/franja_horaria_llegada/ARR_0001_0559.csv", sep=",")
