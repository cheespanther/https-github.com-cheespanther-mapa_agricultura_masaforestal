navbarPage("MAPA INTERACTIVO",
           navbarMenu("Inicio",
                      tabPanel("Metadatos"),
                      tabPanel("Grafica",
                               h4("Resumen de los datos:"),
                               plotOutput('grafica1'),)
           ),
           tabPanel("Ver mapa",
                    bootstrapPage(
                      
                      leafletOutput("mapa")),
           ),
           navbarMenu("Datos",
                      tabPanel("Datos Chiapas",
                               h2("Visualización de datos"),
                               fluidRow(
                                 column(4,
                                        selectInput("Cultivo",
                                                    "Cultivo:",
                                                    c("Todos",
                                                      unique(as.character(comparado_sum_ac$CULTI_ESPE))))
                                 ),
                                 column(4,
                                        selectInput("Municipio",
                                                    "Municipio:",
                                                    c("Todos",
                                                      unique(as.character(comparado_sum_ac$NOM_MUN_07))))
                                 ),
                                 column(4,
                                        selectInput("Concat",
                                                    "Clave concatenado/Área de Control:",
                                                    c("Todos",
                                                      unique(as.character(comparado_sum_ac$CVE_CONCAT_07))))
                                 )
                               ),
                               DT::dataTableOutput("tabla1"),
                      ),
                      tabPanel("Selva en Marqués de Comillas",
                               h2("Cambios de superficie con selva"),
                               DT::dataTableOutput("tabla2"),
                      )
           )
)