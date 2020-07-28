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
    data <- comparado_sum_ac
    DT::datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv')
      ),
      {
        if (input$CVE_MUN_07 != "Todos") {
          data <- data[data$CVE_MUN_07 == input$CVE_MUN_07,]
        }
        if (input$CVE_CONCAT_07 != "Todos") {
          data <- data[data$CVE_CONCAT_07 == input$CVE_CONCAT_07,]
        }
        data
      }
    )
  })
  
  # VISUALIZACIÓN DE DATOS 3
  
  
  output$tabla4 <- DT::renderDataTable(
    DT::datatable(df_correlacion_mc_b, options = list(paging = FALSE))
  )
  
  # VISUALIZAR CORRELACIONES
  # Combine the selected variables into a new data frame
  selectedData1 <<- reactive({
    df_correlacion_mc_d[, c(input$xcol)]
  })
  
  selectedData2 <<- reactive({
    df_correlacion_mc_d[, c(input$ycol)]
  })
  
  selectedData3 <<- reactive({
    df_correlacion_mc_d[, c(input$tamano)]
  })
  
  
  clusters <- reactive({
    kmeans(selectedData(), input$tamano) # NO SE USA
  })
  
  output$plot1 <- renderPlot({
    data <- df_correlacion_mc_d
    ggplot(data, aes(x=selectedData1(), y=selectedData2(), size = selectedData3())) +
      geom_point(alpha=0.7) +
      geom_smooth(method='lm')
  })
  
  # GENERAR GRÁFICAS
  output$grafica1 <- renderPlot({
    scatterplot(df_correlacion_mc_d$PCT_AGRICOLA, df_correlacion_mc_d$DEFORESTADA)
  })
  
  # GENERAR MAPA
  output$mapa <- renderLeaflet({
    
    m <-leaflet(ac_mapa_mc) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")
    
    m <- m %>%  addPolygons(data = ac_mapa_mc, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
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
                            popup = ~pct_productividad)

    # AGREGAR CAPA DE DATOS DE PRODUCCIÓN
    m <- m %>%  addPolygons(data = ac_mapa_mc, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = .5,
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
                              direction = "auto"))

    m <- m %>%  addPolygons(data = ac_mapa_mc, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = .5,
                            fillColor = ~pal_3(PCT_AGRICOLA.x),
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
                              direction = "auto"))

    m <- m %>%  addPolygons(data = ac_mapa_mc, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = .5,
                            fillColor = ~pal_4(PCT_PECUARIO),
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
                              direction = "auto"))
    
    # AGREGAR CAPA DE DATOS DE AUTOCORRELACIÓN
    m <- m %>%  addPolygons(data = autocorr_1, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = .5,
                            fillColor = ~pal_0(ha_1),
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
                            group = "Autocorr 1",
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"),
                            label = ~paste0(Id, ": ", formatC(ha_1), big.mark = ","))
    
    m <- m %>%  addPolygons(data = serie_3, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            fillColor = ~pal_5(as.numeric(VALOR)),
                            group = "Serie 3")
    
    m <- m %>%  addPolygons(data = serie_6, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            fillColor = ~pal_6(as.numeric(VALOR)),
                            group = "Serie 6")
    
    m <- m %>%  addPolygons(data = cambios_ndvi, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            fillColor = ~pal_6(as.numeric(tipo_camb)),
                            group = "Cambios NDVI")
    
    m <- m %>%  addPolygons(data = cambios_usv, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            fillColor = ~pal_6(as.numeric(camb_tip)),
                            group = "Cambios USV")
    # Layers control
    m <- m %>% addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("Actividad forestal", "Actividad agricola","Actividad pecuaria", "Terrenos totales", "Autocorr 1", "Serie 3", "Serie 6", "Cambios NDVI", "Cambios USV"),
      options = layersControlOptions(collapsed = TRUE)
    )
    
    m
    
  })     
  

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  # LEYENDA EN PROXY QUE PERMITE ENCENDER Y APAGAR EN SHINY
  observe({
    proxy <- leafletProxy("mapa", data = ac_mapa_mc)
    proxy %>% clearControls()
    if (input$leyenda) {
      proxy %>% 
        addLegend("topleft", group = "Terrenos totales", pal = pal_1, values = ~TERRENOS, opacity = 1.0) %>%
        addLegend("topleft", group = "Actividad forestal", pal = pal_2, values = ~PCT_FORESTAL, opacity = 1.0) %>%
        addLegend("topleft", group = "Actividad agricola", pal = pal_3, values = ~PCT_AGRICOLA.x, opacity = 1.0) %>%
        addLegend("topleft", group = "Actividad pecuaria", pal = pal_4, values = ~PCT_PECUARIO, opacity = 1.0)
    }
  })
  
}