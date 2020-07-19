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
    df_correlacion_mc_c[, c(input$xcol)]
  })
  
  selectedData2 <<- reactive({
    df_correlacion_mc_c[, c(input$ycol)]
  })
  
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters) # NO SE USA
  })
  
  output$plot1 <- renderPlot({
    plot(selectedData1(), selectedData2())
  })
  
  # GENERAR GRÁFICAS
  output$grafica1 <- renderPlot({
    corrplot(df_correlacion_pearson, method = "square")
    scatterplot(df_correlacion_mc_c$PCT_AGRICOLA, df_correlacion_mc$DEFORESTADA)
  })
  
  # GENERAR MAPA
  output$mapa <- renderLeaflet({
    
    m <-leaflet(ac_mapa) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")
    
    m <- m %>%  addPolygons(data = ac_mapa, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            fillColor = ~pal_1(as.numeric(TERRENOS)),
                            group = "Terrenos totales",
                            label = ~paste0(CVE_CONCAT, ": ", formatC(as.numeric(as.character(TERRENOS)), big.mark = ",")))
    
    # AGREGAR CAPA DE DATOS DE PRODUCCIÓN
    m <- m %>%  addPolygons(data = ac_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal_2(PCT_FORESTAL),
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
                            label = ~paste0(CVE_CONCAT, ": ", formatC(PCT_FORESTAL), big.mark = ","))
    
    m <- m %>%  addPolygons(data = ac_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal_3(as.numeric(as.character(PCT_AGRICOLA))),
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
                            label = ~paste0(CVE_CONCAT, ": ", formatC(as.numeric(as.character((PCT_AGRICOLA))), big.mark = ",")))
    
    m <- m %>%  addPolygons(data = ac_mapa, stroke = TRUE, smoothFactor = 0.3, 
                            fillOpacity = 0.8,
                            fillColor = ~pal_4(as.numeric(PCT_PECUARIO)),
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
                            label = ~paste0(CVE_CONCAT, ": ", formatC(as.numeric(as.character(PCT_PECUARIO)), big.mark = ",")))
    
    m <- m %>%addLegend("bottomleft", pal = pal_1, values = ~TERRENOS, opacity = 1.0) %>%
      addLegend("bottomleft", pal = pal_2, values = ~PCT_FORESTAL, opacity = 1.0) %>%
      addLegend("bottomleft", pal = pal_3, values = ~PCT_AGRICOLA, opacity = 1.0) %>%
      addLegend("bottomleft", pal = pal_4, values = ~PCT_PECUARIO, opacity = 1.0)
    
    # Layers control
    m <- m %>% addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("Actividad forestal", "Actividad agricola", "Actividad pecuaria", "Terrenos totales"),
      options = layersControlOptions(collapsed = TRUE)
    )
    
    m
    
  })     
  
}
