
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)


header<-dashboardHeader(
  title = "2016 USA Flights"
)

sidebar<-dashboardSidebar(
  selectInput(inputId="Opcion2", label="Choose the case of study:", choices=c("OPERATIONS","DEPARTURE DELAYS (MIN)","ARRIVAL DELAYS (MIN)"), selected = NULL, multiple = FALSE,selectize = TRUE),
  selectInput(inputId="Opcion1", label="Choose the scope:", choices=c("TOTAL","AIRPORT", "AIRLINE")),
  conditionalPanel(condition="input.Opcion1=='AIRPORT'",
                   uiOutput(outputId="o_NombreAeropuerto")),
  conditionalPanel(condition="input.Opcion1=='AIRLINE'",
                   uiOutput(outputId="o_NombreAerolinea")),
  selectInput(inputId="Opcion0", label="By:", choices=c("MONTHS","WEEK DAYS","TIME BLOCKS"))
)

body<-dashboardBody(
  fluidRow(
    # div( id = "EEUU",
    #      img(src='/images/EEUU.png')
    # ),
    box(
      div(
        
      )
      
    ),
    box(
     title="RESULTS GRAPH:", width=NULL,
     div(
       plotlyOutput("distPlot")
     )
    ),
    box(width=12,
      title="RESULTS TABLE:",
      DT::dataTableOutput('tablaInfo')
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin="green")
