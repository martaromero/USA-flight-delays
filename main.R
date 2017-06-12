#Librerias a instalar para la app:
if(!require("devtools")){
  install.packages("devtools", dependencies = TRUE)
}

if(!require("shiny")){
  install.packages("shiny")
}

if(!require("shinydashboard")){
  install.packages("shinydashboard")
}

if(!require("plotly")){
  install.packages("plotly")
}

if(!require("dplyr")){
  install.packages("dplyr")
}

if(!require("reshape2")){
  install.packages("reshape2")
}

if(!require("lubridate")){
  install.packages("lubridate")
}

if(!require("DBI")){
  install.packages("DBI")
}

if(!require("DT")){
  install.packages("DT")
}

#Cargamos librerias:
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(reshape2)
library(DT)
library(lubridate)
library(DBI)
runGitHub("USA-flight-delays", "martaromero")
