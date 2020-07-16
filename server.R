function(input, output) {
  
  # GENERAR TABLAS PARA VISUALIZAR DATOS
  output$tabla <- DT::renderDataTable(DT::datatable(
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv')
    ),
    {
    data <- concentrado_comparativo_ac
    if (input$CULTI_ESPE != "Todos") {
      data <- data[data$CULTI_ESPE == input$CULTI_ESPE,]
    }
    if (input$Municipio != "Todos") {
      data <- data[data$NOM_MUN == input$NOM_MUN,]
    }
    if (input$CVE_CONCAT != "Todos") {
      data <- data[data$Concat == input$CVE_CONCAT,]
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
    concentrado_comparativo_b
  })
  
  output$tabla_c = DT::renderDataTable({
    concentrado_comparativo_b_ac
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
                            fillColor = ~pal_1(as.numeric(as.character(TERRENOS))),
                            group = "Áreas de control",
                            label = ~paste0(CVE_CONCAT, ": ", formatC(TERRENOS, big.mark = ",")))
    
    # AGREGAR CAPA DE DATOS DE PRODUCCIÓN
    m <- m %>%  addPolygons(data = ac_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal_2(as.numeric(PCT_FORESTAL)),
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
                            label = ~paste0(CVE_CONCAT, ": ", formatC(as.numeric(PCT_FORESTAL), big.mark = ",")))

    m <- m %>%  addPolygons(data = ac_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal_3(as.numeric(PCT_AGRICOLA)),
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
                            label = ~paste0(CVE_CONCAT, ": ", formatC(as.numeric(PCT_AGRICOLA), big.mark = ",")))
    
    m <- m %>%  addPolygons(data = ac_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal_3(as.numeric(PCT_PECUARIO)),
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
                            label = ~paste0(CVE_CONCAT, ": ", formatC(as.numeric(PCT_PECUARIO), big.mark = ",")))
    
    m <- m %>%addLegend("bottomleft", pal = pal_1, values = ~TERRENOS, opacity = 1.0) %>%
      addLegend("bottomleft", pal = pal_2, values = ~SUP_TOTAL, opacity = 1.0)
    
    # Layers control
    m <- m %>% addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("Actividad forestal", "Actividad agricola", "Actividad pecuaria", "Áreas de control"),
      options = layersControlOptions(collapsed = TRUE)
    )
    m
}