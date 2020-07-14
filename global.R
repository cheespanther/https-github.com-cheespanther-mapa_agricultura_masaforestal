# CARGAR LIBRERIAS UTILIZADAS DE R
library(data.table)
library(curl)
library(devtools)
library(rio)
library(shiny)
library(DT)
library(leaflet)
library(rgdal)
library(sf)
library(plyr)
library(DBI)
library(odbc)
library(dplyr)
library(sf)

# LEER LOS DATOS DEL CAMBIO DE USO DE SUELO SEGÚN SERIE III Y SERIE VI
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal/datos/")
ac_mapa <- readOGR("ac_mapa.geojson")
ac_mapa <- spTransform(ac_mapa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
ac_datos <- as.data.frame(ac_mapa)

concentrado_mapa16 <- readOGR("concentrado_mapa16.geojson")
concentrado_mapa16 <- spTransform(concentrado_mapa16, CRS("+proj=longlat +datum=WGS84 +no_defs"))

concentrado_mapa07 <- readOGR("concentrado_mapa07.geojson")
concentrado_mapa07 <- spTransform(concentrado_mapa07, CRS("+proj=longlat +datum=WGS84 +no_defs"))

cambio_usv <- read.csv(file = "control_selva.csv")

# REGRESAR A DIRECTORIO "WORKSPACE" DONDE SE GUARDAN LOS ARCIVOS TEMPORALES
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal/workspace/")

# ESTILOS DEL MAPA
bins <- c(0, 10, 20, 50, 100, 200, 500, Inf)
# PALETA DE COLORES
pal <- colorBin("viridis", domain = ac_sf16$terrenos, bins = bins)

# DATA WRANGLING CON DATOS DEL 2016
concentrado16 <- as.data.frame(concentrado_mapa16)
concentrado16$sup_semb <- as.numeric(as.character(concentrado16$sup_semb))
ac_sf16 <- concentrado_mapa16

concentrado16_limpio <- concentrado16[,c(12, 20, 21, 10, 22, 11, 24)]
terreno_prom_tot <- as.data.frame(concentrado16$sup_total/concentrado16$terrenos)
terreno_prom_prod <- as.data.frame(concentrado16$sup_semb/concentrado16$num_terr)
concentrado16_limpio <- cbind(concentrado16_limpio, terreno_prom_tot, terreno_prom_prod)
colnames(concentrado16_limpio) <- c("Concat", "Actividad", "Cultivo", "Terrenos totales", "Terrenos de cultivo", "Sup total", "Sup sembrada", "Ha promedio terreno", "Ha promedio terreno de cultivo")

# CREAR CAPAS DE PRODUCTOS DEL MARCO CENSAL 2016
maiz_sf16 <- subset(concentrado_mapa16, concentrado_mapa16$culti_espe == "MAIZ")
frijol_sf16 <- subset(concentrado_mapa16, concentrado_mapa16$culti_espe == "FRIJOL")
mango_sf16 <- subset(concentrado_mapa16, concentrado_mapa16$culti_espe == "MANGO")
aguacate_sf16 <- subset(concentrado_mapa16, concentrado_mapa16$culti_espe== "AGUACATE")

# DATA WRANGLING CON DATOS DEL 2007
concentrado07 <- as.data.frame(concentrado_mapa07)
concentrado07$sup_semb <- as.numeric(concentrado07$sup_semb)
ac_sf07 <- concentrado_mapa07

concentrado07_limpio <- concentrado07[,c(8, 11, 14, 13, 16)]
terreno_prom_tot07 <- as.data.frame(concentrado07_limpio$sup_sc/concentrado07_limpio$up)
terreno_prom_prod07 <- as.data.frame(concentrado07_limpio$sup_semb/concentrado07_limpio$up)
concentrado07_limpio <- cbind(concentrado07_limpio, terreno_prom_tot07, terreno_prom_prod07)
concentrado07_limpio <- rbind(concentrado07_limpio, (concentrado07_limpio$sup_total)/as.numeric(concentrado07_limpio$terrenos))

# GENERAR DATAFRAMES DE LOS DATOS CONCENTRADOS POR ÁREA DE CONTROL

sup_semb07 <- as.data.frame(cbind(as.data.frame(concentrado07$cve_concat), concentrado07$sup_sc, concentrado07$sup_semb, concentrado07$cultivo, concentrado07$mun))
sup_semb16 <- as.data.frame(cbind(as.data.frame(concentrado16$cve_concat), concentrado16$sup_total , concentrado16$sup_semb, concentrado16$culti_espe, concentrado16$nom_mun))

colnames(sup_semb16) <- c("Concat", "Superficie Cartográfica Total", "Superficie Sembrada", "Cultivo", "Municipio")
colnames(sup_semb07) <- c("Concat", "Superficie Cartográfica Total", "Superficie Sembrada", "Cultivo", "Municipio")

sup_semb07$Cultivo <- laply(sup_semb07$Cultivo, toupper)
sup_semb16$Cultivo <- laply(sup_semb16$Cultivo, toupper)

sup_semb07$Concat_B <- paste(sup_semb07$Concat, sup_semb07$Cultivo, sep="_")
sup_semb16$Concat_B <- paste(sup_semb16$Concat, sup_semb16$Cultivo, sep="_")

concentrado_comparativo <- merge(sup_semb07, sup_semb16, by = "Concat_B")
concentrado_comparativo_b <- concentrado_comparativo[,c(1, 2, 11, 5, 3, 4, 8, 9)]
colnames(concentrado_comparativo_b) <- c("Concat_B", "Concat", "Municipio", "Cultivo", "Superficie Cartográfica 07", "Superficie Sembrada 07", "Superficie Cartoráfica 16", "Superficie Sembrada 16")
concentrado_comparativo_b <- merge(concentrado_comparativo_b, concentrado16[,c(3,12)], by.x = "Concat", by.y = "cve_concat")

concentrado_comparativo_sum <- ddply(concentrado_comparativo_b, .(Concat), numcolwise(sum))

####

concentrados_comp_todo <- merge(concentrado07, concentrado16, by = "cve_concat")
concentrados_comp <- concentrados_comp_todo[,c(1, 10, 11, 33, 13, 34, 14, 16, 44, 46, 23, 48)]
colnames(concentrados_comp) <- c("Concat", "Municipio", "Cultivo", "Terrenos Totales", "Sup total 07", "Sup total 16", "UP 2007", "Terrenos 2016", "Sup semb 07", "Sup semb 16", "Area Shape 07", "Area Shape 16")

concentrados_comp$pct_semb07 <- as.numeric(as.character((concentrados_comp$`Sup semb 07`)))/as.numeric(as.character(concentrados_comp$`Sup total 07`))
concentrados_comp$pct_semb16 <- as.numeric(as.character((concentrados_comp$`Sup semb 16`)))/as.numeric(as.character(concentrados_comp$`Sup total 16`))

concentrados_comp$cambio_pct <- as.data.frame(as.numeric(unlist(concentrados_comp$`Sup total 16`)) - as.numeric(unlist(concentrados_comp$pct_semb07)))

# pct_semb_cambio <- as.data.frame(as.numeric(unlist(maiz_comp$pct_semb.x)) - as.numeric(unlist(maiz_comp$pct_semb.y)))

####

sup_tot_comp <- as.data.frame(cbind(as.numeric(as.character(concentrados_comp$`Sup total 07`)),as.numeric(as.character(concentrados_comp$`Sup total 16`)))) 
coef_variacion_tot <- sd(sup_tot_comp$V2)/mean(sup_tot_comp$V2)

sup_semb_comp <- as.data.frame(cbind(as.numeric(as.character(concentrados_comp$`Sup semb 07`)),as.numeric(as.character(concentrados_comp$`Sup semb 16`))))
is.na(sup_semb_comp$V2) = 0
coef_variacion_semb <- sd(sup_semb_comp$V2)/mean(sup_semb_comp$V2)

area_shape_comp <- as.data.frame(cbind(as.numeric(as.character(concentrados_comp$`Area Shape 07`)),as.numeric(as.character(concentrados_comp$`Area Shape 16`))))
coef_variacion_shape <- sd(area_shape_comp$V2)/mean(area_shape_comp$V2)



