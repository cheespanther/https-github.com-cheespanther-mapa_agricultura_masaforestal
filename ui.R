navbarPage("MAPA INTERACTIVO",
           navbarMenu("Inicio",
                      tabPanel("Metadatos"),
                      tabPanel("Grafica",
                               h4("Resumen de los datos:"),
                               plotOutput('grafica1'),)
           ),
           tabPanel("Ver mapa",
                    bootstrapPage(
                      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                      leafletOutput("mapa", width = "100%", height = "100%")),
           ),
           navbarMenu("Datos",
                      tabPanel("Datos Chiapas",   
                               h2("Visualización de datos"),
                               fluidRow(
                                 column(4,
                                        selectInput("Cultivo",
                                                    "Producto:",
                                                    c("Todos",
                                                      unique(as.character(concentrado_comparativo_ac$CULTI_ESPE))))
                                 ),
                                 column(4,
                                        selectInput("Municipio",
                                                    "Municipio:",
                                                    c("Todos",
                                                      unique(as.character(concentrado_comparativo_ac$NOM_MUN))))
                                 ),
                                 column(4,
                                        selectInput("Concat",
                                                    "Clave concatenado/Área de Control:",
                                                    c("Todos",
                                                      unique(as.character(concentrado_comparativo_ac$CVE_CONCAT))))
                                 )
                               ),
                               DT::dataTableOutput("tabla"),
                      ),
                      tabPanel("Selva en Marqués de Comillas",
                               h2("Cambios de superficie con selva"),
                               DT::dataTableOutput("tabla_a"),
                      ),
                      tabPanel("Resumen por producto",
                               h2("Suma de superfice por producto"),
                               DT::dataTableOutput("tabla_b"),
                      ),
                      tabPanel("Resumen por áreas de control",
                               h2("Suma de superficie por área de control"),
                               DT::dataTableOutput("tabla_c"),
                      )
           )
)