# CARGAR LIBRERIAS UTILIZADAS DE R
library(sf)
library(geojsonio)
library(rgdal)
library(rmapshaper)
library(rio)
library(dplyr)
library(plyr)

# LECTURA DE SHAPE BASE DE ÁREAS DE CONTROL DE GITHUB
# FUENTE: ACTUALIZACIÓN DEL MARCO SENSAL AGROPECUARIO 2016
ac_mapa <- geojson_read("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/ac_mapa.geojson",  what = "sp")
colnames(ac_mapa@data) <- toupper(colnames(ac_mapa@data)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
ac_mapa@data$NOM_ENT <- toupper(ac_mapa@data$NOM_ENT) # CONVERTIR TODOS LOS NOMBRES DE ENTIDADES A MAYUSCULAS
ac_mapa@data$NOM_MUN <- toupper(ac_mapa@data$NOM_MUN) # CONVERTIR TODOS LOS NOMBRES DE MUNICIPIO A MAYUSCULAS
ac_mapa@data$CVE_CONCAT <- as.factor(paste(ac_mapa@data$CVE_MUN, ac_mapa@data$CVE_AGEB, ac_mapa@data$CVE_MZA, sep="_"))

ac_mapa <- ms_simplify(ac_mapa, keep = 0.05)
ac_mapa_mc <- subset(ac_mapa, ac_mapa@data$NOM_MUN=="MARQUÉS DE COMILLAS")

autocorr_1 <- geojson_read("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/autocorr_1.geojson",  what = "sp")
serie_3 <- geojson_read("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/serie_3.geojson",  what = "sp")
serie_6 <- geojson_read("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/serie_6.geojson",  what = "sp")
cambios_ndvi <- geojson_read("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/cambios_ndvi.geojson",  what = "sp")
cambios_usv <- geojson_read("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/cambios_usv.geojson",  what = "sp")

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
concentrado16$CULTI_ESPE <- recode(concentrado16$CULTI_ESPE, 'CAFE CIMARRON' = 'CAFE')

# PROCESAMIENTO DE LOS DATOS

# CÁLCULO Y CONCATENACIÓN SUMAS POR ÁREA DE CONTROL DE TAMAÑO PROMEDIO DE LOS TERRENOS REGISTRADOS TOTALES Y CON SUPERFICIE SEMBRADA
concentrado16$TERRENO_PROM_TOT <- as.numeric(as.character(concentrado16$SUP_TOTAL))/as.numeric(as.character(concentrado16$TERRENOS))
concentrado16$TERRENO_PROM_SEM <- as.numeric(as.character(concentrado16$SUP_SEMB))/as.numeric(as.character(concentrado16$NUM_TERR))
concentrado16$PCT_SUPSEM <- as.numeric(as.character(concentrado16$SUP_SEMB)) / as.numeric(as.character(concentrado16$SUP_TOTAL))
colnames(concentrado16) <- paste(colnames(concentrado16), "_16", sep="")
concentrado16$CULTI_ESPE <- laply(concentrado16$CULTI_ESPE_16, toupper) # CONVERTIR TODOS LOS VALORES A MAYUSCULAS
concentrado16$CONCAT_ESPE <- paste(concentrado16$CVE_CONCAT, concentrado16$CULTI_ESPE, sep="_")
concentrado16$PCT_AGRICOLA <- as.numeric(as.character(concentrado16$NUM_TERR_16))/as.numeric(as.character(concentrado16$TERRENOS_16))

esp_sum16 <- ddply(concentrado16, .(CONCAT_ESPE), numcolwise(sum))
esp_sum16$CVE_CONCAT <- substr(esp_sum16$CONCAT_ESPE, 1, 13)
esp_sum16 <- merge(ac_mapa@data, esp_sum16, by = "CVE_CONCAT")
ac_sum16 <- ddply(concentrado16, .(CVE_CONCAT_16), numcolwise(sum))
ac_sum16 <- merge(ac_mapa@data, ac_sum16, by.x = "CVE_CONCAT", by.y = "CVE_CONCAT_16")

# CREACIÓN DE DATA FRAME CON LOS DATOS DE PRODUCCIÓN AGRÍCOLA, PECUARIA Y FORESTAL Y ÁREAS DE CONTROL
df_ac <- merge(ac_mapa@data, forestal[,c(10,col_f)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
df_ac <- merge(df_ac, pecuario[,c(10,col_p)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
df_ac <- merge(df_ac, ac_sum16[,c(1,10,11,12,16,19,20:25)], by = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
df_ac[is.na(df_ac)] <- 0

# CÁLCULO DE PORCENTAJES DE TERRENOS OCUPADOS PARA LA ACTIVIDAD FORESTAL, AGRÍCOLA Y PECAUARIA Y SU SUMA (PCT_OCUPADO)
df_ac$PCT_FORESTAL <- df_ac$F_TOTAL/as.numeric(as.character(df_ac$TERRENOS))
df_ac$PCT_PECUARIO <- df_ac$P_TOTAL/as.numeric(as.character(df_ac$TERRENOS))
df_ac$PCT_OCUPADO <- (df_ac$PCT_FORESTAL + df_ac$PCT_PECUARIO + df_ac$PCT_AGRICOLA)
df_ac[is.na(df_ac)] <- 0


# LECTURA DE DATOS DE LA PRODUCCIÓN AGRÍCOLA DEL 2007 DE GITHUB
# FUENTE: CENSO AGROPECUARIO 2007
concentrado07 <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/07_agricola_total.csv")
colnames(concentrado07) <- toupper(colnames(concentrado07)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
colnames(concentrado07)[colnames(concentrado07) %in% c("MUN", "CULTIVO")] <- c("NOM_MUN", "CULTI_ESPE")
colnames(concentrado07) <- paste(colnames(concentrado07), "_07", sep="")

# CÁLCULO Y CONCATENACIÓN SUMAS POR ÁREA DE CONTROL DE TAMAÑO PROMEDIO DE LOS TERRENOS REGISTRADOS TOTALES Y CON SUPERFICIE SEMBRADA
concentrado07$UP_PROM_TOTAL <- as.numeric(as.character(concentrado07$SUP_SC_07))/as.numeric(as.character(concentrado07$UP_07))
concentrado07$UP_PROM_SEMB <- as.numeric(as.character(concentrado07$SUP_SEMB_07))/as.numeric(as.character(concentrado07$UP_07))
concentrado07$CULTI_ESPE<- laply(concentrado07$CULTI_ESPE_07, toupper) # CONVERTIR TODOS LOS VALORES A MAYUSCULAS

# UNIFICAR Y AGRUPAR ID'S DE CULTIVOS 
concentrado07$CULTI_ESPE <- recode(concentrado07$CULTI_ESPE, 'MAIZ GRANO' = 'MAIZ')
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

# CONCATENAR CASOS COMPARABLES QUE TENGAN DATOS DE CULTIVO Y ÁREA DE CONTROL QUE COINCIDAN
casos_comparables_esp <- comparado_esp[complete.cases(comparado_esp),]
sum_comparables_esp <- ddply(casos_comparables_esp, .(CONCAT_ESPE), numcolwise(sum))
casos_comparables_esp_b <- casos_comparables_esp[,c(1, 41:49, 30:40, 55:65)]

# UN "CASO COMPARABLE" ES AQUEL SIN CELDAS VACÍAS
comparado_sum_esp <- ddply(casos_comparables_esp_b, .(CONCAT_ESPE), numcolwise(sum))
colnames(comparado_sum_esp) <- c("CONCAT_ESPE", "UNIDADES DE PRODUCCIÓN", "SUPERFICE CARTO 07", "SUPERFICIE SEMBRADA 07", "SUPERFICIE COSECHADA 07", "TONELADAS PRODUCIDAS 2007", "TONELADAS POR HA 2007", "SUPERFICIE SEMBRADA % 2007", "SHAPE_LENG_07", "SHAPE_AREA_07", "TERRENO PROMEDIO GENERAL 07", "TERRENO PROMEDIO SEMBRADO 07", "UNIDADES DE PRODUCCION PROMEDIO SEMBRADA", "TERRENOS TOTALES EN LA AC", "SUPERFICIE TOTAL 2016", "TERRENOS SEMBRADOS 2016", "SUPERFICIE CARTO 2016", "SUPERFICIE SEMBRADA 2016", "SHAPE_LENG_16", "SHAPE_AREA_16", "TERRENO PROMEDIO GENERAL 2016", "TERRENO PROMEDIO SEMBRADO 2016", "SUPERFICIE SEMBRADA % 2016" )
comparado_sum_esp$CAMBIO_AGRICOLA <- as.numeric(as.character(comparado_sum_esp$`SUPERFICIE SEMBRADA 2016`)) - as.numeric(as.character(comparado_sum_esp$`SUPERFICIE SEMBRADA 07`))
comparado_sum_esp$CAMBIO_AGRICOLA_RELATIVO <- as.numeric(as.character(comparado_sum_esp$CAMBIO_AGRICOLA)) / as.numeric(as.character(comparado_sum_esp$`SUPERFICIE SEMBRADA 07`))

# SUMA DE CASOS COMPARABLES POR ÁREA DE CONTROL

# CONCATENAR DATOS 2007 Y 2016 PARA ESTIMAR CAMBIOS
comparado_ac <- merge(ac_sum07, ac_sum16, by.x = "CVE_CONCAT_07", by.y = "CVE_CONCAT", all.x = TRUE, all.y = TRUE)
casos_comparables_ac <- comparado_ac[complete.cases(comparado_ac),]
casos_comparables_ac_b <- casos_comparables_ac[,c(1, 41:49, 30:40, 54:64)]


# UN "CASO COMPARABLE" ES AQUEL SIN CELDAS VACÍAS
comparado_sum_ac <- ddply(casos_comparables_ac_b, .(CVE_CONCAT_07), numcolwise(sum))
colnames(comparado_sum_ac) <- c("CVE_CONCAT", "SUPERFICIE TOTAL", "SUPERFICIE CARTO 07", "UNIDADES DE PRODUCCIÓN 07", "SUPERFICIE SEMBRADA 07", "SUPERFICIE COSECHADA 07", "TONELADAS PRODUCIDAS 2007", "TONELADAS POR HA 2007", "SUPERFICIE SEMBRADA % 2007", "SHAPE_LENG_07", "SHAPE_AREA_07", "AREA PROMEDIO GENERAL 07 UP", "AREA PROMEDIO SEMBRADO 07 UP", "TERRENOS TOTALES EN LA AC", "SUPERFICIE TOTAL 2016", "TERRENOS SEMBRADOS 2016", "SUPERFICIE CARTO 2016", "SUPERFICIE SEMBRADA 2016", "SHAPE_LENG_16", "SHAPE_AREA_16", "TERRENO PROMEDIO GENERAL 2016", "TERRENO PROMEDIO SEMBRADO 2016", "SUPERFICIE SEMBRADA % 2016", "PORCENTAJE TERRENOS AGRÍCOLAS 16")
comparado_sum_ac$CAMBIO_SUP_SEMB_AGRICOLA <- as.numeric(as.character(comparado_sum_ac$`SUPERFICIE SEMBRADA 2016`)) - as.numeric(as.character(comparado_sum_ac$`SUPERFICIE SEMBRADA 07`))
comparado_sum_ac$CAMBIO_SUP_SEMB_AGRICOLA_REL <- comparado_sum_ac$CAMBIO_SUP_SEMB_AGRICOLA / comparado_sum_ac$`SUPERFICIE SEMBRADA 07`

ac_mapa_b <- merge(ac_mapa@data, comparado_sum_ac, by = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)


########### CONTROL
# CORRELACIÓN DE DATOS
df_correlacion_mc <- merge(ac_mapa_b, datos_cambios, by.x = "CONTROL" , by.y = "ETIQUETAS DE FILA")
df_correlacion_mc[is.na(df_correlacion_mc)] <- 0

df_correlacion_mc_b <- df_correlacion_mc[,c(1,2,6,9,16:38)]
df_correlacion_mc_c <- df_correlacion_mc[,c(16:38)]
df_correlacion_mc_d <- df_correlacion_mc_c[complete.cases(df_correlacion_mc_c),]
df_correlacion_mc_d <- df_correlacion_mc_c[,c(1, 2, 3, 6:15)]

df_correlacion_pearson <- cor(df_correlacion_mc_d, method = "pearson")

# UNIR DATOS ANALÍTICOS CON DATOS GEOESPACIALES

ac_mapa_mc <- merge(ac_mapa_mc, df_correlacion_mc, by = "CONTROL")

# ac_mapa@data = data.frame(ac_mapa@data, comparado_sum_ac[match(ac_mapa@data[,"CVE_CONCAT"], comparado_sum_ac[,"CVE_CONCAT_07"]),])

# CREAR ARCHIVOS TIPO RData PARA ALMACENAR LOS RESULTADOS DEL PROCESAMIENTO DE LOS DATOS
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal/data/Rdata/")

save(ac_mapa, ac_mapa_mc, serie_3, serie_6, file = "carto.RData")
save(df_ac, file = "datos.RData")
save(concentrado07, concentrado16, file = "concentrados.RData")
save(comparado_ac, comparado_esp, comparado_sum_ac, comparado_sum_esp, file = "comparados.RData")
save(df_correlacion_mc, df_correlacion_mc_b, df_correlacion_mc_c, df_correlacion_mc_d, df_correlacion_pearson, file = "correlaciones.RData")
save(autocorr_1, file = "autocorrelaciones.RData")
save(cambios_ndvi, cambios_usv, file = "cambios.RData")

# REGRESAR AL ENTORNO GENERAL LOCAL
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal")

# AUTOCORRELACIÓN
library(spdep)
contiguidad <- poly2nb(ac_mapa_mc, queen=TRUE)
pesos <- nb2listw(contiguidad, style="W", zero.policy=TRUE)

inc.lag <- lag.listw(pesos, ac_mapa_mc$DEFORESTADA)
plot(inc.lag ~ ac_mapa_mc$DEFORESTADA, pch=16, asp=1)
M1 <- lm(inc.lag ~ ac_mapa_mc$DEFORESTADA)
abline(M1, col="blue")

I <- moran(ac_mapa_mc$DEFORESTADA, pesos, length(contiguidad), Szero(pesos))[1]

grid <- raster(extent(ac_mapa_mc), resolution = c(200,200))
grid <- raster::extend(grid, c(1,1))

# convert to SpatialPolygonsDataFrame
class(gridPolygon)

gridPolygon <- rasterToPolygons(grid)
plot(ac_mapa_mc)
plot(gridPolygon, add = T)

grid <- makegrid(ac_mapa_mc, cellsize = 0.1)
grid <- SpatialPoints(grid)
