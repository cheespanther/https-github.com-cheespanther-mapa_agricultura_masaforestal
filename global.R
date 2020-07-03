# LEER TODOS LOS SHAPES DEL MARCO CENSAL 2016 DE UN DIRECTORIO
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/datos/agricola_16/")
archivos <- list.files(pattern=".shp")
dataselect <- lapply(archivos, readOGR)
dataselect_df <- lapply(dataselect, as.data.frame)
combine <- do.call(rbind, dataselect_df)

# LEER TODAS LAS ÁREAS DE CONTROL DEL ESTADO DE CHIAPAS
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/datos/cartografia/ac/")
ac <- readOGR("ac_07.shp")
ac_mapa <- spTransform(ac, CRS("+proj=longlat +datum=WGS84 +no_defs"))
ac_datos <- as.data.frame(ac)

# LEER LOS DATOS DEL CAMBIO DE USO DE SUELO SEGÚN SERIE III Y SERIE VI

# LEER LOS DATOS DE CAMBIOS DE COBERTURA SEGÚN ANÁLISIS DE NDVI

# CONCATENAR INFORMACIÓN CON LAS ÁREAS DE CONTROL

# CALCULAR SUMAS DE SUPERFICES POR ÁREA DE CONTROL

# CALCULAR SUMAS DE SUPERFICIES POR TIPO DE CULTIVO



promedios_generales_especie <<- ddply(combine, .(culti_espe), numcolwise(sum))

promedios_generales_ac <<- ddply(combine, .(CONTROL), numcolwise(sum))

promedios <- group_by(combine, culti_espe) %>% summarise_each(funs(mean))


sup_semb_promedio <<- ddply (combine, "culti_espe", summarise, 
                             producto_pieza = mean(sup_semb))

sup_semb_promedio <<- ddply (combine, "culti_espe", summarise, 
                             "Promedio Superficie Sembrada" = mean(as.numeric(sup_semb)))

sup_carto_promedio <<- ddply (combine, "culti_espe", summarise, 
                              "Promedio Superficie Sembrada" = mean(as.numeric(sup_carto)))

terrenos_promedio <<- ddply (combine, "culti_espe", summarise,
                             "No. de terrenos promedio" = mean(as.numeric(num_terr)))

sup_semb_promedio <<- ddply (combine, "culti_espe", summarise, 
                             "Promedio Superficie Sembrada" = mean(as.numeric(sup_semb)))

presencia_promedio <<- ddply (combine, "culti_espe", summarise, 
                              "Porcentaje de cultivo principal" = count(culti_espe)/length(combine))

sup_semb_ac <<- ddply (combine, "CONTROL", summarise, 
                       "Suma de Superficie Sembrada" = sum(as.numeric(sup_semb)))

sup_carto_ac <<- ddply (combine, "CONTROL", summarise, 
                        "Suma de Superficie Sembrada" = sum(as.numeric(sup_carto)))

sup_carto_ac_maiz <<- ddply ((subset(combine, combine$culti_espe == "MAIZ")), "CONTROL", summarise, 
                             "Suma de Superficie Sembrada" = sum(as.numeric(sup_carto)))