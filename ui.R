bootstrapPage(theme = shinytheme("flatly"),
    navbarPage("Mapa",
      tabPanel("Mapa",
        basicPage("Ver mapa",
                  tags$style(type = "text/css", "html, body {width:100%;height:100%}",
                ".leaflet .legend {
                 line-height: 10px;
                 font-size: 10px;
                 }",
                ".leaflet .legend i{
                width: 10px;
                height: 10px;
                 }"),
                  leafletOutput("mapa", width = "100%", height = 800),
                  absolutePanel(top = 10, right = 10,
                                checkboxInput("leyenda", "Mostrar leyenda", TRUE),
                  absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                              tags$a(href='https://www.centrogeo.edu.mx', tags$img(src='logo_centrogeo_solo.png',height='40',width='80'))),)
      )
      ),
      navbarMenu("Proyecto",
                 tabPanel("Resumen",
                          includeHTML("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/html/resumen.html")
                 ),
                 tabPanel("Introducción",
                          includeHTML("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/html/introduccion.html")
                 ),
                 tabPanel("Justificación",
                          h4("Justificación:"),
                          includeHTML("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/html/justificacion.html"),
                          plotOutput('grafica1'),)
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
                 )
                 

      ),
      
      navbarMenu("Gráficas",
                 tabPanel("Correlaciones",
                          pageWithSidebar(
                            headerPanel('Gráfica de las variables'),
                            sidebarPanel(
                              selectInput('xcol', 'Variable X', vars),
                              selectInput('ycol', 'Variable Y', vars, selected = vars[[2]]),
                              selectInput('tamano', 'Tamaño', vars, selected = vars[[3]])
                            ),
                            mainPanel(
                              plotOutput('plot1')
                            )
                          )
                 )        
                 ),
      
      tabPanel("Acerca de",
                 "Acerca de este sitio",
                          includeHTML("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/html/acerca_de.html")
                 )
    )
)

 