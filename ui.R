navbarPage("MAPA INTERACTIVO",
           navbarMenu("Proyecto",
                      tabPanel("Resumen",
                               includeHTML("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/html/resumen.html")
                      ),
                      tabPanel("Introducción",
                               includeHTML("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/html/introduccion.html")
                               ),
                      tabPanel("Introducción",
                               includeHTML("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/html/index.html")
                      ),
                      tabPanel("Justificación",
                               h4("Justificación:"),
                               plotOutput('grafica1'),)
           ),
           tabPanel("Ver mapa",
                    fluidPage(
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
                      ),
                      
                      tabPanel("Comparación 2007 - 2016",
                               h2("Comparación por área de control"),
                               fluidRow(
                                 column(4,
                                        selectInput("CVE_MUN_07",
                                                    "Municipio:",
                                                    c("Todos",
                                                      unique(as.character(comparado_sum_ac$CVE_MUN_07))))
                                 ),
                                 column(4,
                                        selectInput("CVE_CONCAT_07",
                                                    "Clave concatenado/Área de Control:",
                                                    c("Todos",
                                                      unique(as.character(comparado_sum_ac$CVE_CONCAT_07))))
                                 )
                               ),
                               DT::dataTableOutput("tabla3"),
                      ),
                      
                      tabPanel("Datos para correlaciones",
                               h2("Datos para correlacoines"),
                               DT::dataTableOutput("tabla4"),
                      ),
                      
                      tabPanel("Correlaciones",
                      pageWithSidebar(
                        headerPanel('Gráfica de las variables'),
                        sidebarPanel(
                          selectInput('xcol', 'Variable X', vars),
                          selectInput('ycol', 'Variable Y', vars, selected = vars[[2]])
                          ),
                        mainPanel(
                          plotOutput('plot1')
                        )
                      )
                      )  
           )
           
)