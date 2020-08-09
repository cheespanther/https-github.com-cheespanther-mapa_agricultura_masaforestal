function(input, output, session) {
  
  # GENERAR TABLAS PARA VISUALIZAR DATOS
  # VISUALIZACIÓN DE DATOS 1
  
  output$tabla1 = DT::renderDataTable({
    data <- concentrado07
    DT::datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv')
      ),
      {
        if (input$CULTI_ESPE != "Todos") {
          data <- data[data$CULTI_ESPE == input$CULTI_ESPE,]
        }
        if (input$NOM_MUN_07 != "Todos") {
          data <- data[data$NOM_MUN_07 == input$NOM_MUN_07,]
        }
        if (input$CVE_CONCAT_07 != "Todos") {
          data <- data[data$CVE_CONCAT_07 == input$CVE_CONCAT_07,]
        }
        data
      }
    )
  })
  
  # VISUALIZACIÓN DE DATOS 2
  output$tabla2 = DT::renderDataTable({
    data <- concentrado16
    DT::datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv')
      ),
      {
        if (input$CULTI_ESPE != "Todos") {
          data <- data[data$CULTI_ESPE == input$CULTI_ESPE,]
        }
        if (input$NOM_MUN_07 != "Todos") {
          data <- data[data$NOM_MUN_16 == input$NOM_MUN_16,]
        }
        if (input$CVE_CONCAT_07 != "Todos") {
          data <- data[data$CVE_CONCAT_16 == input$CVE_CONCAT_16,]
        }
        data
      }
    )
  })
  
  # VISUALIZACIÓN DE DATOS 3
  output$tabla3 = DT::renderDataTable({
    data <- comparado_sum_esp
    DT::datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv')
      ),
      {
        if (input$CONCAT_ESPE != "Todos") {
          data <- data[data$CONCAT_ESPE == input$CONCAT_ESPE,]
        }
        data
      }
    )
  })
  
  # VISUALIZACIÓN DE DATOS 3
  output$tabla4 = DT::renderDataTable({
    data <- df_correlacion_mc
    DT::datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv')
      ),
      {
        if (input$CVE_CONCAT != "Todos") {
          data <- data[data$CVE_CONCAT == input$CVE_CONCAT,]
        }
        if (input$NOM_MUN != "Todos") {
          data <- data[data$NOM_MUN == input$NOM_MUN,]
        }
        data
      }
    )
  })
  
  # VISUALIZACIÓN DE DATOS 3
  
  
  output$tabla5 <- DT::renderDataTable(
    DT::datatable(df_correlacion_pearson, options = list(paging = FALSE))
  )
  
  # VISUALIZAR CORRELACIONES
  # Combine the selected variables into a new data frame
  selectedData1 <<- reactive({
    matriz_correlacion[, c(input$xcol)]
  })
  
  selectedData2 <<- reactive({
    matriz_correlacion[, c(input$ycol)]
  })
  
  selectedData3 <<- reactive({
    matriz_correlacion[, c(input$tamano)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$tamano) # NO SE USA
  })
  
  output$plot1 <- renderPlot({
    data <- matriz_correlacion
    ggplot(data, aes(x=selectedData1(), y=selectedData2(), size = selectedData3())) +
      geom_point(alpha=0.7) +
      geom_smooth(method='lm')
  })
  
  # GENERAR GRÁFICAS
  output$grafica1 <- renderPlot({
    scatterplot(matriz_correlacion$`TONELADAS POR HA 2007`, matriz_correlacion$`TONELADAS PRODUCIDAS 2007`)
  })
  
  # GENERAR MAPA
  output$mapa <- renderLeaflet({
    
    m <-leaflet(ac_mapa_mc) %>%
      addMapPane("A", zIndex = 440) %>% #
      addMapPane("B", zIndex = 430) %>% # 
      addMapPane("C", zIndex = 420) %>% # 
      addMapPane("D", zIndex = 410) %>% # 
      addMapPane("E", zIndex = 410) %>% #
      addMapPane("F", zIndex = 400) %>% # 
      addMapPane("G", zIndex = 390) %>% # 
      
      
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")
    
    m <- m %>%  addPolygons(data = ac_mapa_mc, stroke = FALSE, smoothFactor = 0.3,
                            options = pathOptions(pane = "A"),
                            fillOpacity = .7,
                            fillColor = ~pal_1(as.numeric(TERRENOS)),
                            opacity = .3,
                            weight = 1,
                            color = "#4D4D4D",
                            dashArray = "2",
                            highlight = highlightOptions(
                              weight = 1,
                              color = "#4D4D4D",
                              fillOpacity = 0.1,
                              dashArray = "2",
                              bringToFront = TRUE),
                            group = "Terrenos totales",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            popup = ~pop_terrenos)

    # AGREGAR CAPA DE DATOS DE PRODUCCIÓN
    m <- m %>%  addPolygons(data = ac_mapa_mc, stroke = TRUE, smoothFactor = 0.3, 
                            options = pathOptions(pane = "B"),
                            fillOpacity = .7,
                            fillColor = ~pal_2(PCT_FORESTAL),
                            opacity = .3,
                            weight = 1,
                            color = "#4D4D4D",
                            dashArray = "2",
                            highlight = highlightOptions(
                              weight = 1,
                              color = "#4D4D4D",
                              fillOpacity = 0.1,
                              dashArray = "2",
                              bringToFront = TRUE),
                            group = "Actividad forestal",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            popup = ~pop_forestal)
    
    m <- m %>%  addPolygons(data = ac_mapa_mc, stroke = TRUE, smoothFactor = 0.3,
                            options = pathOptions(pane = "C"),
                            fillOpacity = .7,
                            fillColor = ~pal_3(PCT_AGRICOLA),
                            opacity = .3,
                            weight = 1,
                            color = "#4D4D4D",
                            dashArray = "2",
                            highlight = highlightOptions(
                              weight = 1,
                              color = "#4D4D4D",
                              fillOpacity = 0.1,
                              dashArray = "2",
                              bringToFront = TRUE),
                            group = "Actividad agricola",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            popup = ~pop_agricola)

    m <- m %>%  addPolygons(data = ac_mapa_mc, stroke = TRUE, smoothFactor = 0.3, 
                            options = pathOptions(pane = "C"),
                            fillOpacity = .7,
                            fillColor = ~pal_4(PCT_PECUARIO),
                            opacity = .3,
                            weight = 1,
                            color = "#4D4D4D",
                            dashArray = "2",
                            highlight = highlightOptions(
                              weight = 1,
                              color = "#4D4D4D",
                              fillOpacity = 0.1,
                              dashArray = "2",
                              bringToFront = TRUE),
                            group = "Actividad pecuaria",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            popup = ~pop_pecuario)
    
    # CAPA DE AUTOCORRELACIÓN DE LA DEFORESTACIÓN
    m <- m %>%  addPolygons(data = autocorr_1, stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1,
                            options = pathOptions(pane = "D"),
                            fillColor = ~pal_0(ha_1),
                            group = "Autocorrelación deforestación")
    
    # CAPA DE CAMBIOS DE NDVI
    m <- m %>%  addPolygons(data = cambios_usv_forestal, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            options = pathOptions(pane = "E"),
                            fillColor = ~pal_7(as.numeric(gridcode)),
                            group = "Cambios forestal")
    
    # CAPA DE CAMBIOS DE USO DE SUELO
    m <- m %>%  addPolygons(data = cambios_usv, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            options = pathOptions(pane = "F"),
                            fillColor = ~pal_8(as.numeric(gridcode)),
                            group = "Cambios USV")
    
    
    cambios_usv_total
    
    
    # CONTROL DE CAPAS
    m <- m %>% addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("Terrenos totales", "Actividad forestal", "Actividad agricola","Actividad pecuaria",  "Autocorrelación deforestación", "Cambios forestal", "Cambios USV"),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    m
    
  })
  
  # LOS PROXIES PERMITEN ENCENDER Y APAGAR ELEMENTOS EN R LEAFLET
  observe({
    proxy <- leafletProxy("mapa", data = ac_mapa_mc)
    proxy %>% clearControls()
    if (input$leyenda) {
      proxy %>% 
        addLegend("topleft", group = "Terrenos totales", pal = pal_0, values = ~TERRENOS, opacity = 1.0) %>%
        addLegend("topleft", group = "Actividad forestal", pal = pal_2, values = ~PCT_FORESTAL, opacity = 1.0) %>%
        addLegend("topleft", group = "Actividad agricola", pal = pal_3, values = ~PCT_AGRICOLA, opacity = 1.0) %>%
        addLegend("topleft", group = "Actividad pecuaria", pal = pal_4, values = ~PCT_PECUARIO, opacity = 1.0) 
    }
  })
  
  observe({
    proxy <- leafletProxy("mapa", data = cambios_ndvi)
    proxy %>% clearControls()
    if (input$leyenda) {
      proxy %>% 
        addLegend("topleft", group = "Cambios NDVI", pal = pal_7, values = ~as.numeric(gridcode), opacity = 1.0) %>%
        addLegend("topleft", group = "Cambios USV", pal = pal_8, values = ~as.numeric(gridcode), opacity = 1.0)
    }
  })  
}