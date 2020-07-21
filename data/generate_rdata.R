# CARGAR LIBRERIAS UTILIZADAS DE R
library(curl)
library(devtools)
library(shiny)
library(rio)
library(DT)
library(leaflet)
library(rgdal)
library(plyr)
library(dplyr)
library(sf)
library(car)
library(corrplot)
library(ggplot2)
library(curl)
library(geojsonio)
library(rmapshaper)


# LECTURA DE SHAPE BASE DE ÁREAS DE CONTROL DE GITHUB
# FUENTE: ACTUALIZACIÓN DEL MARCO SENSAL AGROPECUARIO 2016
ac_mapa <- geojson_read("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/ac_mapa.geojson",  what = "sp")
colnames(ac_mapa@data) <- toupper(colnames(ac_mapa@data)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
ac_mapa@data$CVE_CONCAT <- as.factor(paste(ac_mapa@data$CVE_MUN, ac_mapa@data$CVE_AGEB, ac_mapa@data$CVE_MZA, sep="_"))
ac_mapa <- ms_simplify(ac_mapa, keep = 0.05)
ac_mapa_mc <- subset(ac_mapa, ac_mapa@data$NOM_MUN=="Marqués de Comillas")

autocorr_1 <- geojson_read("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/autocorr",  what = "sp")


# LECTURA DE DATOS DE LA PRODUCCIÓN PECUARIAS DE GITHUB
# FUENTE: ACTUALIZACIÓN DEL MARCO SENSAL AGROPECUARIO 2016
pecuario <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/16_pecuario.csv")
colnames(pecuario) <- toupper(colnames(pecuario)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
col_p <- ncol(pecuario)

# LECTURA DE DATOS DE LA PRODUCCIÓN FORESTAL DE GITHUB
# FUENTE: ACTUALIZACIÓN DEL MARCO SENSAL AGROPECUARIO 2016
forestal <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/16_forestal.csv")
colnames(forestal) <- toupper(colnames(forestal)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
col_f <- ncol(forestal)

# LECTURA DE DATOS DE PRODUCCIÓN AGRÍCOLA DEL 2016 DE GITHUB
# FUENTE: ACTUALIZACIÓN DEL MARCO SENSAL AGROPECUARIO 2016
concentrado16 <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/16_agricola_total.csv")
colnames(concentrado16) <- toupper(colnames(concentrado16)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS

# CAMBIO DE USO DE SUELO EN EL MUNICIPIO DE MARQUÉS DE COMILLAS DE GITHUB
# FUENTE: ELABORACIÓN PROPIA CON DATOS DEL INEGI
datos_cambios <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/cambios_2.csv")
colnames(datos_cambios) <- toupper(colnames(datos_cambios)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS

# LECTURA DE DATOS DE LOS CAMBIOS DE USO DE SUELO Y VEGETACIÓN SERIE III Y SERIE VI
# FUENTE: INEGI
cambio_usv <- import('https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/control_selva.csv')

# UNIFICAR Y AGRUPAR ID'S DE CULTIVOS 
# concentrado16$CULTI_ESPE <- recode(concentrado16$CULTI_ESPE, 'CAFE CIMARRON' = 'CAFE')

# gsub("CAFE CIMARRON" = 'CAFE', concentrado16$CULTI_ESPE)

# PROCESAMIENTO DE LOS DATOS

# CÁLCULO Y CONCATENACIÓN SUMAS POR ÁREA DE CONTROL DE TAMAÑO PROMEDIO DE LOS TERRENOS REGISTRADOS TOTALES Y CON SUPERFICIE SEMBRADA
concentrado16$TERRENO_PROM_TOT <- as.numeric(as.character(concentrado16$SUP_TOTAL))/as.numeric(as.character(concentrado16$TERRENOS))
concentrado16$TERRENO_PROM_SEM <- as.numeric(as.character(concentrado16$SUP_SEMB))/as.numeric(as.character(concentrado16$NUM_TERR))
concentrado16$PCT_SUPSEM <- as.numeric(as.character(concentrado16$SUP_SEMB)) / as.numeric(as.character(concentrado16$SUP_TOTAL))
colnames(concentrado16) <- paste(colnames(concentrado16), "_16", sep="")
concentrado16$CULTI_ESPE <- laply(concentrado16$CULTI_ESPE_16, toupper) # CONVERTIR TODOS LOS VALORES A MAYUSCULAS
concentrado16$CONCAT_ESPE <- paste(concentrado16$CVE_CONCAT, concentrado16$CULTI_ESPE, sep="_")

esp_sum16 <- ddply(concentrado16, .(CONCAT_ESPE), numcolwise(sum))
esp_sum16 <- merge(concentrado16, esp_sum16, by = "CONCAT_ESPE")
ac_sum16 <- ddply(concentrado16, .(CVE_CONCAT_16), numcolwise(sum))
ac_sum07 <- merge(concentrado16, ac_sum16, by.x = "CVE_CONCAT_16", by.y="CVE_CONCAT_16")

# CREACIÓN DE DATA FRAME CON LOS DATOS DE PRODUCCIÓN AGRÍCOLA, PECUARIA Y FORESTAL Y ÁREAS DE CONTROL
dfa <- merge(ac_mapa@data, forestal[,c(10,col_f)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
dfa <- merge(dfa, pecuario[,c(10,col_p)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
dfa <- merge(dfa, ac_sum16[,c(1,10,11,12,15,16,17,18)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT_16", all.y=TRUE, all.x = TRUE)
dfa[is.na(dfa)] <- 0

# CÁLCULO DE PORCENTAJES DE TERRENOS OCUPADOS PARA LA ACTIVIDAD FORESTAL, AGRÍCOLA Y PECAUARIA Y SU SUMA (PCT_OCUPADO)
dfa$PCT_FORESTAL <- dfa$F_TOTAL/as.numeric(as.character(dfa$TERRENOS))
dfa$PCT_PECUARIO <- dfa$P_TOTAL/as.numeric(as.character(dfa$TERRENOS))
dfa$PCT_AGRICOLA <- dfa$NUM_TERR/as.numeric(as.character(dfa$TERRENOS))
dfa$PCT_OCUPADO <- (dfa$PCT_FORESTAL + dfa$PCT_PECUARIO + dfa$PCT_AGRICOLA)

# FILTRADO DE DATOS POR MUNICIPIO
# colnames(dfa) <- c("cve_concat", "AC", "cve_Entidad", "Entidad", "cvd_Municipio", "Municipio", "cve_AGEB", "cve_Manzana", "Terrenos_totales", "Superficie_total", "Terrenos_con_actividad_forestal", "Terrenos_con_actividad_pecuaria", "Terrenos_con_actividad_agricola", "Superfice_carto_agricola", "Superficie_sembrada", "Terreno_promedio_ha_total", "Terreno_promedio_aricola", "Porcentaje_terrenos_forestal", "Porcentaje_terrenos_pecuario", "Porcentaje_terrenos_agricola", "Porcentaje_ocupado")
dfa_mc <- subset(dfa, dfa$NOM_MUN=="Marqués de Comillas")
# write.csv(dfa, file = "df16_final.csv")
# write.csv(dfa_mc, file = "df16_final_mc.csv")

# LECTURA DE DATOS DE LA PRODUCCIÓN AGRÍCOLA DEL 2007 DE GITHUB
# FUENTE: CENSO AGROPECUARIO 2007
concentrado07 <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/07_agricola_total.csv")
colnames(concentrado07) <- toupper(colnames(concentrado07)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
colnames(concentrado07)[colnames(concentrado07) %in% c("MUN", "CULTIVO")] <- c("NOM_MUN", "CULTI_ESPE")
colnames(concentrado07) <- paste(colnames(concentrado07), "_07", sep="")

# UNIFICAR Y AGRUPAR ID'S DE CULTIVOS 
recode(concentrado07$CULTI_ESPE, "MAIZ GRANO" = "MAIZ")

# CÁLCULO Y CONCATENACIÓN SUMAS POR ÁREA DE CONTROL DE TAMAÑO PROMEDIO DE LOS TERRENOS REGISTRADOS TOTALES Y CON SUPERFICIE SEMBRADA
concentrado07$UP_PROM_TOTAL <- as.numeric(as.character(concentrado07$SUP_SC_07))/as.numeric(as.character(concentrado07$UP_07))
concentrado07$UP_PROM_SEMB <- as.numeric(as.character(concentrado07$SUP_SEMB_07))/as.numeric(as.character(concentrado07$UP_07))
concentrado07$CULTI_ESPE<- laply(concentrado07$CULTI_ESPE, toupper) # CONVERTIR TODOS LOS VALORES A MAYUSCULAS
concentrado07$CONCAT_ESPE <- paste(concentrado07$CVE_CONCAT, concentrado07$CULTI_ESPE, sep="_")

esp_sum07 <- ddply(concentrado07, .(CONCAT_ESPE), numcolwise(sum))
esp_sum07 <- merge(concentrado07, esp_sum07, by.x = "CONCAT_ESPE", by.y="CONCAT_ESPE")
ac_sum07 <- ddply(concentrado07, .(CVE_CONCAT_07), numcolwise(sum))
ac_sum07 <- merge(concentrado07, ac_sum07, by.x = "CVE_CONCAT_07", by.y="CVE_CONCAT_07")

# CREACIÓN DE DATA FRAME CON LOS DATOS DE PRODUCCIÓN AGRÍCOLA DEL 2007
dfb <- merge(ac_mapa@data, ac_sum07[,c(1,7,8,9,10,11,12,15,16,17)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT_07")
dfb[is.na(dfb)] <- 0

# CONCATENAR DATOS 2007 Y 2016 PARA ESTIMAR CAMBIOS
comparado_esp <- merge(esp_sum07, esp_sum16, by.x = "CONCAT_ESPE", by.y = "CONCAT_ESPE", all.x = TRUE, all.y = TRUE)
# comparado_b <- comparado[,c(1:8, 10, 36, 37, 10, 13, 14, 16, 20, 16, 26, 27, 36, 37, 45:49, 52,53,54)]

# CONCATENAR CASOS COMPARABLES QUE TENGAN DATOS DE CULTIVO Y ÁREA DE CONTROL QUE COINCIDAN
casos_comparables_esp <- comparado_esp[complete.cases(comparado_esp),]
sum_comparables_esp <- ddply(casos_comparables_esp, .(CONCAT_ESPE), numcolwise(sum))

casos_comparables_esp <- casos_comparables_esp[,c(1, 3, 4, 5, 6, 7, 9, 11, 47, 14, 15, 17, 21, 25, 36, 37, 48, 49, 50, 53, 54, 55)]

# UN "CASO COMPARABLE" ES AQUEL SIN CELDAS VACÍAS
comparado_sum_esp <- ddply(casos_comparables, .(CONCAT_ESPE), numcolwise(sum))
comparado_sum_esp$CAMBIO_AGRICOLA <- as.numeric(as.character(comparado_sum_esp$SUP_SEMB_16)) - as.numeric(as.character(comparado_sum_esp$SUP_SEMB_07))
comparado_sum_esp$CAMBIO_AGRICOLA_RELATIVO <- as.numeric(as.character(comparado_sum_esp$CAMBIO_AGRICOLA)) / as.numeric(as.character(comparado_sum_esp$SUP_SEMB_07))

# SUMA DE CASOS COMPARABLES POR ÁREA DE CONTROL

# CONCATENAR DATOS 2007 Y 2016 PARA ESTIMAR CAMBIOS
comparado_ac <- merge(concentrado07, concentrado16, by.x = "CVE_CONCAT_07", by.y = "CVE_CONCAT_16", all.x = TRUE, all.y = TRUE)
# comparado_b <- comparado[,c(1:8, 10, 36, 37, 10, 13, 14, 16, 20, 16, 26, 27, 36, 37, 45:49, 52,53,54)]

# CONCATENAR CASOS COMPARABLES QUE TENGAN DATOS DE CULTIVO Y ÁREA DE CONTROL QUE COINCIDAN
casos_comparables_ac <- comparado_ac[complete.cases(comparado_ac),]
casos_comparables_ac <- casos_comparables_ac[,c(1, 3, 4, 5, 6, 7, 9, 11, 47, 14, 15, 17, 21, 25, 36, 37, 48, 49, 50, 53, 54, 55)]

# UN "CASO COMPARABLE" ES AQUEL SIN CELDAS VACÍAS
comparado_sum_ac <- ddply(casos_comparables, .(CVE_CONCAT_07), numcolwise(sum))
comparado_sum_ac$CAMBIO_SUP_SEMB_AGRICOLA <- as.numeric(as.character(comparado_sum_ac$SUP_SEMB_16)) - as.numeric(as.character(comparado_sum_ac$SUP_SEMB_07))
comparado_sum_ac$CAMBIO_SUP_SEMB_AGRICOLA_REL <- comparado_sum_ac$CAMBIO_SUP_SEMB_AGRICOLA / comparado_sum_ac$SUP_SEMB_07

ac_mapa_b <- merge(ac_mapa@data, comparado_sum_ac, by.x = "CVE_CONCAT", by.y = "CVE_CONCAT_07", all.y=TRUE, all.x = TRUE)

# CORRELACIÓN DE DATOS
df_correlacion_mc <- merge(dfa, datos_cambios, by.x = "CONTROL" , by.y = "ETIQUETAS DE FILA")
df_correlacion_mc <- merge(df_correlacion_mc, comparado_sum_ac, by.x = "CVE_CONCAT", by.y = "CVE_CONCAT_07", all.x= TRUE)
df_correlacion_mc <- df_correlacion_mc[,c(1:15, 17:31, 36:38, 40, 42, 47, 49, 50)]
  
df_correlacion_mc_b <- df_correlacion_mc[,c(1,2,6,9,16:38)]


df_correlacion_mc_c <- df_correlacion_mc[,c(16:38)]
# colnames(df_correlacion_mc_b) <- c("Tamaño terreno promedio", "Porcentaje Terrenos Forestales", "Porcentaje Terrenos Pecuarios", "Porcentaje Terrenos Agricolas", "Porcentaje Terrenos Ocupados", "Héctareas Productivas", "Héctareas Conservadas", "Héctareas Deforestadas", "Héctareas Degradadas", "Héctareas Reforestadas", "Héctareas Sin Cambio", "Héctareas de Transición", "Héctareas Urbanizadas", "Total General", "Porcentaje Degradado", "Superficie total est", "Superficie Total Cartográfica 07", "Superficie Sembrada 07", "Superficie Total Cartográfica 16", "Superficie Sembrada 16","Cambio Agrícola en Héctareas", "Cambio Agrícola Relativo")

df_correlacion_pearson <- cor(df_correlacion_mc_c, method = "pearson")

# UNIR DATOS ANALÍTICOS CON DATOS GEOESPACIALES

ac_mapa@data = data.frame(ac_mapa@data, dfa[match(ac_mapa@data[,"CVE_CONCAT"], dfa[,"CVE_CONCAT"]),])
ac_mapa_mc@data = data.frame(ac_mapa_mc@data, dfa_mc[match(ac_mapa_mc@data[,"CVE_CONCAT"], dfa_mc[,"CVE_CONCAT"]),])

# ac_mapa@data = data.frame(ac_mapa@data, comparado_sum_ac[match(ac_mapa@data[,"CVE_CONCAT"], comparado_sum_ac[,"CVE_CONCAT_07"]),])

# CREAR ARCHIVOS TIPO RData PARA ALMACENAR LOS RESULTADOS DEL PROCESAMIENTO DE LOS DATOS
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal/data/raw_data")

save(ac_mapa, ac_mapa_mc, file = "carto.RData")
save(dfa, file = "datos.RData")
save(concentrado07, concentrado16, file = "concentrados.RData")
save(comparado, comparado_b, comparado_sum_ac, comparado_sum_esp, file = "comparados.RData")
save(df_correlacion_mc, df_correlacion_mc_b, df_correlacion_mc_c, df_correlacion_pearson, file = "correlaciones.RData")

# REGRESAR AL ENTORNO GENERAL LOCAL
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal")
