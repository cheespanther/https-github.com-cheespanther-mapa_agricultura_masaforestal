function(input, output) {
  
  # GENERAR TABLAS PARA VISUALIZAR DATOS
  output$tabla = DT::renderDataTable({
    datos
  })
  
  output$tabla_a = DT::renderDataTable({
    datos_a
  })
  
  
  # GENERAR GRÁFICAS
  output$grafica1 <- renderPlot({
    plot(datos_a$sup_tot_ch, datos_a$sup_cal_ha)
  })
  
  # GENERAR MAPA
  
  output$mapa <- renderLeaflet({
    
    m <-leaflet(combine_b) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")
    
    # AGREGAR CAPA DE DATOS DE MAIZ
    m <- m %>%  addPolygons(data = maiz_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal(TERRENOS),
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
                            group = "Maiz",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            label = ~paste0(NOM_MUN, ": ", formatC(TERRENOS, big.mark = ",")))
    
    # AGREGAR CAPA DE DATOS DE MANGO
    m <- m %>%  addPolygons(data = mango_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal(TERRENOS),
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
                            group = "Mango",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            label = ~paste0(NOM_MUN, ": ", formatC(TERRENOS, big.mark = ",")))
    
    # AGREGAR CAPA DE DATOS DE MANGO
    m <- m %>%  addPolygons(data = manzana_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal(TERRENOS),
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
                            group = "Manzana",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            label = ~paste0(NOM_MUN, ": ", formatC(TERRENOS, big.mark = ",")))
    
    m <- m %>%addLegend("bottomleft", pal = pal, values = ~TERRENOS, opacity = 1.0) %>%
      addLegend("bottomleft", pal = pal, values = ~SUP_TOTAL, opacity = 1.0)
    
    # Layers control
    m <- m %>% addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("Maiz", "Mango", "Manzana"),
      options = layersControlOptions(collapsed = TRUE)
    )
    m
  }) 
}