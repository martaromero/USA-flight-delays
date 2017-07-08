# USA-flight-delays

Se ha hecho público el código utilizado para la implementación de la aplicación Shinny de R con la que se desarrolla una herramienta interactiva para la exploración de los datos correspondientes a los vuelos domésticos de los Estados Unidos durante el año 2016. Esta herramienta está incluida en el Trabajo Fin de Máster: Empleo de Técnicas de Aprendizaje Automático para el Análisis y la Predicción de Retrasos en Vuelos Comerciales.

Ejecutando el siguiente código en RStudio se puede acceder directamente a la aplicación de manera local:

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

PRERREQUISITOS:
- R
- RStudio
