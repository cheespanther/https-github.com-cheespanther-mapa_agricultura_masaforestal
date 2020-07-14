function(input, output) {
  
  # GENERAR TABLAS PARA VISUALIZAR DATOS
  output$tabla <- DT::renderDataTable(DT::datatable(
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv')
    ),
    {
    data <- concentrados_comp
    if (input$Cultivo != "Todos") {
      data <- data[data$Cultivo == input$Cultivo,]
    }
    if (input$Municipio != "Todos") {
      data <- data[data$Municipio == input$Municipio,]
    }
    if (input$Concat != "Todos") {
      data <- data[data$Concat == input$Concat,]
    }
    data
  }))
  
  output$tabla_a = DT::renderDataTable({
      DT::datatable(
        data <- cambio_usv,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('csv')
        ),
      )
  })
  
  output$tabla_b = DT::renderDataTable({
    concentrados_comp
  })
  
  output$tabla_c = DT::renderDataTable({
    concentrado_comparativo_b
  })
  
  # GENERAR GRÁFICAS
  output$grafica1 <- renderPlot({
    plot(ac_sf16$sup_total, ac_sf16$terrenos)
  })
  
  # GENERAR MAPA
  output$mapa <- renderLeaflet({
    

  })     
  
    m <-leaflet(ac_mapa) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")
    
    m <- m %>%  addPolygons(data = ac_mapa, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            fillColor = ~pal_1(as.numeric(as.character(Terrenos_totales))),
                            group = "Áreas de control",
                            label = ~paste0(cve_concat, ": ", formatC(Terrenos_totales, big.mark = ",")))
    
    # AGREGAR CAPA DE DATOS DE PRODUCCIÓN
    m <- m %>%  addPolygons(data = ac_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal_2(as.numeric(Porcentaje_terrenos_forestal)),
                            opacity = .3,
                            weight = 1,
                            color = "#4D4D4D",
                            dashArray = "2",
                            highlight = highlightOptions(
                              weight = 1,
                              color = "#4D4D4D",
                              fillOpacity = 0.5,
                              dashArray = "2",
                              bringToFront = TRUE),
                            group = "Actividad forestal",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            label = ~paste0(cve_concat, ": ", formatC(as.numeric(Porcentaje_terrenos_forestal), big.mark = ",")))

    m <- m %>%  addPolygons(data = ac_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal_2(as.numeric(Porcentaje_terrenos_agricola)),
                            opacity = .3,
                            weight = 1,
                            color = "#4D4D4D",
                            dashArray = "2",
                            highlight = highlightOptions(
                              weight = 1,
                              color = "#4D4D4D",
                              fillOpacity = 0.5,
                              dashArray = "2",
                              bringToFront = TRUE),
                            group = "Actividad agricola",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            label = ~paste0(cve_concat, ": ", formatC(as.numeric(Porcentaje_terrenos_agricola), big.mark = ",")))
    
    m <- m %>%  addPolygons(data = ac_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal_2(as.numeric(Porcentaje_terrenos_pecuario)),
                            opacity = .3,
                            weight = 1,
                            color = "#4D4D4D",
                            dashArray = "2",
                            highlight = highlightOptions(
                              weight = 1,
                              color = "#4D4D4D",
                              fillOpacity = 0.5,
                              dashArray = "2",
                              bringToFront = TRUE),
                            group = "Actividad pecuaria",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            label = ~paste0(cve_concat, ": ", formatC(as.numeric(Porcentaje_terrenos_pecuario), big.mark = ",")))
    
    m <- m %>%addLegend("bottomleft", pal = pal_1, values = ~Terrenos_totales, opacity = 1.0) %>%
      addLegend("bottomleft", pal = pal, values = ~Superficie_total, opacity = 1.0)
    
    # Layers control
    m <- m %>% addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("Actividad forestal", "Actividad agricola", "Actividad pecuaria", "Áreas de control"),
      options = layersControlOptions(collapsed = TRUE)
    )
    m
}