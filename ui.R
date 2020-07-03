library(data.table)
library(curl)
library(devtools)
library(rio)
library(DT)
library(shiny)

navbarPage("MAPA INTERACTIVO",
           navbarMenu("Inicio",
                      tabPanel("Metadatos"),
                      tabPanel("Grafica",
                               h4("Resumen de los datos:"),
                               plotOutput('grafica1'),)
           ),
           tabPanel("Ver mapa",
                    bootstrapPage(
                      leafletOutput("mapa", width = "100%")
                    ),
           ),
           navbarMenu("Datos",
                      tabPanel("Datos Chiapas",   
                               h2("Visualización de datos"),
                               DT::dataTableOutput("tabla"),
                      ),
                      tabPanel("Datos Marques de Comillas",
                               h2("Visualización de datos"),
                               DT::dataTableOutput("tabla_a"),
                      )
           )
)