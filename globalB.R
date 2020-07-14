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


# ESTILOS DEL MAPA
bins_terrenos_tot <- c(0, 10, 20, 50, 100, 200, 500, Inf)
bins_pct <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, Inf)
# PALETA DE COLORES
pal_1 <- colorBin("viridis", domain = as.numeric(as.character(dfa$Terrenos_totales)), bins = bins_terrenos_tot)
pal_2 <- colorBin("heat", domain = as.numeric(as.character(dfa$pct_forestal)), bins = bins_pct)


# LEER LOS DATOS DEL CAMBIO DE USO DE SUELO SEGÚN SERIE III Y SERIE VI
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal/datos/")
ac_mapa <- readOGR("ac_mapa.geojson")
ac_mapa$cve_concat <- as.factor(paste(ac_mapa$CVE_MUN, ac_mapa$CVE_AGEB, ac_mapa$CVE_MZA, sep="_"))
ac_mapa <- spTransform(ac_mapa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
ac_datos <- as.data.frame(ac_mapa)

cambio_usv <- read.csv(file = "control_selva.csv")
forestal <- read.csv(file = "16_forestal.csv")
col_f <- ncol(forestal)
pecuario <- read.csv(file = "16_pecuario.csv")
col_p <- ncol(pecuario)

concentrado16 <- read.csv(file = "16_agricola_total.csv")
datos_forestal <- read.csv(file = "cambios_1.csv")
datos_forestal_2 <- read.csv(file = "cambios_2.csv")

concentrado16$terreno_prom_tot <- as.numeric(as.character(concentrado16$sup_total))/as.numeric(as.character(concentrado16$terrenos))
concentrado16$terreno_prom_prod <- as.numeric(as.character(concentrado16$sup_semb))/as.numeric(as.character(concentrado16$num_terr))
ac_sum16 <- ddply(concentrado16, .(cve_concat), numcolwise(sum))

dfa <- merge(ac_datos, forestal[,c(10,col_f)], by.x = "cve_concat", by.y = "cve_concat", all.y=TRUE, all.x = TRUE)
dfa <- merge(dfa, pecuario[,c(10,col_p)], by.x = "cve_concat", by.y = "cve_concat", all.y=TRUE, all.x = TRUE)
dfa <- merge(dfa, ac_sum16[,c(1,10,11,12,15,16)], by.x = "cve_concat", by.y = "cve_concat", all.y=TRUE, all.x = TRUE)
dfa[is.na(dfa)] <- 0

dfa$pct_forestal <- dfa$f_total/as.numeric(as.character(dfa$TERRENOS))
dfa$pct_pecuario <- dfa$p_total/as.numeric(as.character(dfa$TERRENOS))
dfa$pct_agricola <- dfa$num_terr/as.numeric(as.character(dfa$TERRENOS))
dfa$pct_ocupado <- (dfa$pct_forestal + dfa$pct_pecuario + dfa$pct_agricola)

colnames(dfa) <- c("cve_concat", "AC", "cve_Entidad", "Entidad", "cvd_Municipio", "Municipio", "cve_AGEB", "cve_Manzana", "Terrenos_totales", "Superficie_total", "Terrenos_con_actividad_forestal", "Terrenos_con_actividad_pecuaria", "Terrenos_con_actividad_agricola", "Superfice_carto_agricola", "Superficie_sembrada", "Terreno_promedio_ha_total", "Terreno_promedio_aricola", "Porcentaje_terrenos_forestal", "Porcentaje_terrenos_pecuario", "Porcentaje_terrenos_agricola", "Porcentaje_ocupado")
dfa_mc <- subset(dfa, dfa$Municipio=="Marqués de Comillas")
write.csv(dfa, file = "df16_final.csv")
write.csv(dfa_mc, file = "df16_final_mc.csv")

concentrado07 <- read.csv(file = "07_agricola_total.csv")
concentrado07$up_prom_tot <- as.numeric(as.character(concentrado07$sup_sc))/as.numeric(as.character(concentrado07$up))
concentrado07$up_prom_prod <- as.numeric(as.character(concentrado07$sup_semb))/as.numeric(as.character(concentrado07$up))
ac_sum07 <- ddply(concentrado07, .(cve_concat), numcolwise(sum))

dfb <- merge(ac_datos, ac_sum07[,c(1,7,8,9,10,11,12,15,16,17)], by = "cve_concat")

setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal/workspace/")

sup_semb07 <- as.data.frame(cbind(as.data.frame(concentrado07$cve_concat), concentrado07$sup_sc, concentrado07$sup_semb, concentrado07$cultivo, concentrado07$mun))
sup_semb16 <- as.data.frame(cbind(as.data.frame(concentrado16$cve_concat), concentrado16$sup_total , concentrado16$sup_semb, concentrado16$culti_espe, concentrado16$nom_mun))

colnames(sup_semb16) <- c("Concat", "Superficie Cartográfica Total", "Superficie Sembrada", "Cultivo", "Municipio")
colnames(sup_semb07) <- c("Concat", "Superficie Cartográfica Total", "Superficie Sembrada", "Cultivo", "Municipio")

sup_semb07$Cultivo <- laply(sup_semb07$Cultivo, toupper)
sup_semb16$Cultivo <- laply(sup_semb16$Cultivo, toupper)

sup_semb07$Concat_B <- paste(sup_semb07$Concat, sup_semb07$Cultivo, sep="_")
sup_semb16$Concat_B <- paste(sup_semb16$Concat, sup_semb16$Cultivo, sep="_")

concentrado_comparativo <- merge(sup_semb07, sup_semb16, by="Concat_B", all.y=TRUE, all.x=TRUE)

concentrado_comparativo_b <- merge(ac_datos, concentrado_comparativo, by.x = "cve_concat", by.y = "Concat.x", all.y=TRUE, all.x = TRUE)
concentrado_comparativo_sum <- ddply(concentrado_comparativo_b, .(cve_concat), numcolwise(sum))
concentrado_comparativo_sum$cambio_agricola <- as.numeric(as.character(concentrado_comparativo_sum$`Superficie Cartográfica Total.x`)) - as.numeric(as.character(concentrado_comparativo_sum$`Superficie Cartográfica Total.y`))
concentrado_comparativo_sum$cambio_agricola_relativo <- as.numeric(as.character(concentrado_comparativo_sum$cambio_agricola)) / as.numeric(as.character(concentrado_comparativo_sum$`Superficie Cartográfica Total.x`))

# UNIR DATOS ANALÍTICOS CON DATOS GEOESPACIALES

ac_mapa@data = data.frame(ac_mapa@data, dfa[match(ac_mapa@data[,"cve_concat"], dfa[,"cve_concat"]),])

