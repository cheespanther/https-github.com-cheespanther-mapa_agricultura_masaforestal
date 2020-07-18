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
                      tabPanel("Producción agrícola 2007",
                               h2("Visualización de datos agrícolas del 2007"),
                               fluidRow(
                                 column(4,
                                        selectInput("CULTI_ESPE",
                                                    "Cultivo:",
                                                    c("Todos",
                                                      unique(as.character(concentrado07$CULTI_ESPE))))
                                 ),
                                 column(4,
                                        selectInput("NOM_MUN_07",
                                                    "Municipio:",
                                                    c("Todos",
                                                      unique(as.character(concentrado07$NOM_MUN_07))))
                                 ),
                                 column(4,
                                        selectInput("CVE_CONCAT_07",
                                                    "Clave concatenado/Área de Control:",
                                                    c("Todos",
                                                      unique(as.character(concentrado07$CVE_CONCAT_07))))
                                 )
                               ),
                               DT::dataTableOutput("tabla1"),
                      ),
                      tabPanel("Producción agrícola 2016",
                               h2("Visualización de datos agrícolas del 2016"),
                               fluidRow(
                                 column(4,
                                        selectInput("CULTI_ESPE",
                                                    "Cultivo:",
                                                    c("Todos",
                                                      unique(as.character(concentrado16$CULTI_ESPE))))
                                 ),
                                 column(4,
                                        selectInput("NOM_MUN_16",
                                                    "Municipio:",
                                                    c("Todos",
                                                      unique(as.character(concentrado16$NOM_MUN_16))))
                                 ),
                                 column(4,
                                        selectInput("CVE_CONCAT_16",
                                                    "Clave concatenado/Área de Control:",
                                                    c("Todos",
                                                      unique(as.character(concentrado16$CVE_CONCAT_16))))
                                 )
                               ),
                               DT::dataTableOutput("tabla2"),
                      )
           )
)