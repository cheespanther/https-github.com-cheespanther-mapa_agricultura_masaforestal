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
library(Hmisc)
library(corrplot)
library(car)
library(ggplot2)
library(curl)

cambio_usv <- import('https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/datos/control_selva.csv')

ac_datos <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/datos/ac_07.csv")
ac_mapa <- st_crs(ac_datos)

pecuario <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/datos/16_pecuario.csv")
colnames(pecuario) <- toupper(colnames(pecuario))
col_p <- ncol(pecuario)

forestal <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/datos/16_forestal.csv")
colnames(forestal) <- toupper(colnames(forestal))
col_f <- ncol(forestal)



# LEER LOS DATOS DEL CAMBIO DE USO DE SUELO SEGÚN SERIE III Y SERIE VI
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal/datos/")
ac_mapa <- readOGR("ac_mapa.geojson")
ac_mapa$CVE_CONCAT <- as.factor(paste(ac_mapa$CVE_MUN, ac_mapa$CVE_AGEB, ac_mapa$CVE_MZA, sep="_"))
ac_mapa <- spTransform(ac_mapa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
ac_datos <- as.data.frame(ac_mapa)

cambio_usv <- read.csv(file = "control_selva.csv")
colnames(cambio_usv) <- toupper(colnames(cambio_usv))




concentrado16 <- read.csv(file = "16_agricola_total.csv")
colnames(concentrado16) <- toupper(colnames(concentrado16))
datos_forestal <- read.csv(file = "cambios_1.csv")
colnames(datos_forestal) <- toupper(colnames(datos_forestal))
datos_forestal_2 <- read.csv(file = "cambios_2.csv")
colnames(datos_forestal_2) <- toupper(colnames(datos_forestal_2))
# UNIFICAR Y AGRUPAR ID'S DE CULTIVOS 
# concentrado16$CULTI_ESPE <- recode(concentrado16$CULTI_ESPE, "CAFE CIMARRON" = "CAFE")

concentrado16$TERRENO_PROM_TOT <- as.numeric(as.character(concentrado16$SUP_TOTAL))/as.numeric(as.character(concentrado16$TERRENOS))
concentrado16$TERRENO_PROM_SEM <- as.numeric(as.character(concentrado16$SUP_SEMB))/as.numeric(as.character(concentrado16$NUM_TERR))
ac_sum16 <- ddply(concentrado16, .(CVE_CONCAT), numcolwise(sum))

dfa <- merge(ac_datos, forestal[,c(10,col_f)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
dfa <- merge(dfa, pecuario[,c(10,col_p)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
dfa <- merge(dfa, ac_sum16[,c(1,10,11,12,15,16)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
dfa[is.na(dfa)] <- 0

dfa$PCT_FORESTAL <- dfa$F_TOTAL/as.numeric(as.character(dfa$TERRENOS))
dfa$PCT_PECUARIO <- dfa$P_TOTAL/as.numeric(as.character(dfa$TERRENOS))
dfa$PCT_AGRICOLA <- dfa$NUM_TERR/as.numeric(as.character(dfa$TERRENOS))
dfa$PCT_OCUPADO <- (dfa$PCT_FORESTAL + dfa$PCT_PECUARIO + dfa$PCT_AGRICOLA)

# colnames(dfa) <- c("cve_concat", "AC", "cve_Entidad", "Entidad", "cvd_Municipio", "Municipio", "cve_AGEB", "cve_Manzana", "Terrenos_totales", "Superficie_total", "Terrenos_con_actividad_forestal", "Terrenos_con_actividad_pecuaria", "Terrenos_con_actividad_agricola", "Superfice_carto_agricola", "Superficie_sembrada", "Terreno_promedio_ha_total", "Terreno_promedio_aricola", "Porcentaje_terrenos_forestal", "Porcentaje_terrenos_pecuario", "Porcentaje_terrenos_agricola", "Porcentaje_ocupado")
dfa_mc <- subset(dfa, dfa$Municipio=="Marqués de Comillas")
write.csv(dfa, file = "df16_final.csv")
write.csv(dfa_mc, file = "df16_final_mc.csv")

concentrado07 <- read.csv(file = "07_agricola_total.csv")
colnames(concentrado07) <- toupper(colnames(concentrado07))
colnames(concentrado07)[colnames(concentrado07) %in% c("MUN", "CULTIVO")] <- c("NOM_MUN", "CULTI_ESPE")

# UNIFICAR Y AGRUPAR ID'S DE CULTIVOS 
# concentrado07$CULTI_ESPE <- recode(concentrado07$CULTI_ESPE, "MAIZ GRANO" = "MAIZ")

concentrado07$UP_PROM_TOTAL <- as.numeric(as.character(concentrado07$SUP_SC))/as.numeric(as.character(concentrado07$UP))
concentrado07$UP_PROM_SEMB <- as.numeric(as.character(concentrado07$SUP_SEMB))/as.numeric(as.character(concentrado07$UP))
ac_sum07 <- ddply(concentrado07, .(CVE_CONCAT), numcolwise(sum))

concentrado07$CULTI_ESPE <- laply(concentrado07$CULTI_ESPE, toupper)
concentrado07$CULTI_ESPE <- laply(concentrado07$CULTI_ESPE, toupper)
concentrado16$CULTI_ESPE <- laply(concentrado16$CULTI_ESPE, toupper)

dfb <- merge(ac_datos, ac_sum07[,c(1,7,8,9,10,11,12,15,16,17)], by = "CVE_CONCAT")

# REGRESAR AL ENTORNO GLOBAL DEL PROGRAMA
setwd("/media/iskar/archivosB/PROYECTOS/PROYECTO_ESP_CENTROGEO_3.0/mapa_agricultura_masaforestal/")

sup_semb07 <- as.data.frame(cbind(as.data.frame(concentrado07$CVE_CONCAT), concentrado07$SUP_SC, concentrado07$SUP_SEMB, concentrado07$CULTI_ESPE, concentrado07$NOM_MUN))
sup_semb16 <- as.data.frame(cbind(as.data.frame(concentrado16$CVE_CONCAT), concentrado16$SUP_TOTAL , concentrado16$SUP_SEMB, concentrado16$CULTI_ESPE, concentrado16$NOM_MUN))

colnames(sup_semb16) <- c("CVE_CONCAT", "SUP_TOTAL_CARTO", "SUP_SEMB", "CULTI_ESPE", "NOM_MUN")
colnames(sup_semb07) <- c("CVE_CONCAT", "SUP_TOTAL_CARTO", "SUP_SEMB", "CULTI_ESPE", "NOM_MUN")

sup_semb07$CONCAT_ESPE <- paste(sup_semb07$CVE_CONCAT, sup_semb07$CULTI_ESPE, sep="_")
sup_semb16$CONCAT_ESPE <- paste(sup_semb16$CVE_CONCAT, sup_semb16$CULTI_ESPE, sep="_")

# COMPARACIÓN DE CASOS COMPLETOS ANALIZAR LAS DIFERENCIAS ENTRE TERRENOS POR ESPECIE SOLO CUANDO EXISTAN DATOS COMPARBLES

concentrado_comparativo_esp <- merge(sup_semb07, sup_semb16, by="CONCAT_ESPE", all.y=TRUE, all.x=TRUE)
concentrado_comparativo_b_esp <- concentrado_comparativo_esp[complete.cases(concentrado_comparativo_esp),]

concentrado_comparativo_b_esp <- merge(ac_datos, concentrado_comparativo_b_esp, by.x = "CVE_CONCAT", by.y = "CVE_CONCAT.x", all.y=TRUE, all.x = TRUE)
concentrado_comparativo_sum_esp <- ddply(concentrado_comparativo_b_esp, .(CONCAT_ESPE), numcolwise(sum))
concentrado_comparativo_sum_esp$CAMBIO_AGRICOLA <- as.numeric(as.character(concentrado_comparativo_sum_esp$SUP_SEMB.y)) - as.numeric(as.character(concentrado_comparativo_sum_esp$SUP_SEMB.x))
concentrado_comparativo_sum_esp$CAMBIO_AGRICOLA_RELATIVO <- as.numeric(as.character(concentrado_comparativo_sum_esp$CAMBIO_AGRICOLA)) / as.numeric(as.character(concentrado_comparativo_sum_esp$SUP_SEMB.x))

concentrado_comparativo_ac <- merge(sup_semb07, sup_semb16, by="CVE_CONCAT", all.y=TRUE, all.x=TRUE)
concentrado_comparativo_b_ac <- concentrado_comparativo_ac[complete.cases(concentrado_comparativo_ac),]

concentrado_comparativo_b_ac <- merge(ac_datos, concentrado_comparativo_b_ac, by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
concentrado_comparativo_sum_ac <- ddply(concentrado_comparativo_b_ac, .(CVE_CONCAT), numcolwise(sum))
concentrado_comparativo_sum_ac$CAMBIO_AGRICOLA <- as.numeric(as.character(concentrado_comparativo_sum_ac$SUP_SEMB.y)) - as.numeric(as.character(concentrado_comparativo_sum_ac$SUP_SEMB.x))
concentrado_comparativo_sum_ac$CAMBIO_AGRICOLA_RELATIVO <- as.numeric(as.character(concentrado_comparativo_sum_ac$CAMBIO_AGRICOLA)) / as.numeric(as.character(concentrado_comparativo_sum_ac$SUP_SEMB.x))

concentrado_comparativo_ac_mc <- subset(concentrado_comparativo_b_ac, concentrado_comparativo_b_ac$NOM_MUN.y == "Marqués de Comillas")
concentrado_comparativo_sum_ac_mc <- ddply(concentrado_comparativo_ac_mc, .(CVE_CONCAT), numcolwise(sum))
concentrado_comparativo_sum_ac_mc$CAMBIO_AGRICOLA <- as.numeric(as.character(concentrado_comparativo_sum_ac_mc$SUP_SEMB.y)) - as.numeric(as.character(concentrado_comparativo_sum_ac_mc$SUP_SEMB.x))
concentrado_comparativo_sum_ac_mc$CAMBIO_AGRICOLA_RELATIVO <- as.numeric(as.character(concentrado_comparativo_sum_ac_mc$CAMBIO_AGRICOLA)) / as.numeric(as.character(concentrado_comparativo_sum_ac_mc$SUP_SEMB.x))

df_correlacion_mc <- merge(dfa, datos_forestal_2, by.x = "CONTROL" , by.y = "ETIQUETAS.DE.FILA")
df_correlacion_mc <- merge(df_correlacion_mc, concentrado_comparativo_sum_ac_mc, by = "CVE_CONCAT")
df_correlacion_mc_b <- df_correlacion_mc[,c(17:38)]
colnames(df_correlacion_mc_b) <- c("Tamaño terreno promedio", "Porcentaje Terrenos Forestales", "Porcentaje Terrenos Pecuarios", "Porcentaje Terrenos Agricolas", "Porcentaje Terrenos Ocupados", "Héctareas Productivas", "Héctareas Conservadas", "Héctareas Deforestadas", "Héctareas Degradadas", "Héctareas Reforestadas", "Héctareas Sin Cambio", "Héctareas de Transición", "Héctareas Urbanizadas", "Total General", "Porcentaje Degradado", "Superficie total est", "Superficie Total Cartográfica 07", "Superficie Sembrada 07", "Superficie Total Cartográfica 16", "Superficie Sembrada 16","Cambio Agrícola en Héctareas", "Cambio Agrícola Relativo")

df_correlacion_spearman <- cor(df_correlacion_mc_b)
df_correlacion_pearson <- cor(df_correlacion_mc_b, method = "pearson")
df_correlacion_kendall <- cor(df_correlacion_mc_b, method = "kendall")
df_correlacion_todas <- cor(df_correlacion_mc_b, use = "everything",
    method = c("pearson", "kendall", "spearman"))

corrplot(df_correlacion_todas, method = "square")

x <- df_correlacion_mc_b$`Tamaño terreno promedio`
y <- df_correlacion_mc_b$`Héctareas Deforestadas`

# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Correlación X y Y",
     xlab = "Variable X (agropecuaria)", ylab = "Variable Y (cambio/deforestacion)",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = df_correlacion_mc_b), col = "blue")
cor(x,y)

scatterplot(y ~ x)

# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)

tabla_cor1 <- df_correlacion_mc_b[,c(1,7,8,9,10,11,12,13)]
tabla_cor2 <- df_correlacion_mc_b[,c(2,7,8,9,10,11,12,13)]
tabla_cor3 <- df_correlacion_mc_b[,c(3,7,8,9,10,11,12,13)]
tabla_cor4 <- df_correlacion_mc_b[,c(4,7,8,9,10,11,12,13)]
tabla_cor5 <- df_correlacion_mc_b[,c(5,7,8,9,10,11,12,13)]
tabla_cor6 <- df_correlacion_mc_b[,c(19,7,8,9,10,11,12,13)]
tabla_cor7 <- df_correlacion_mc_b[,c(22,7,8,9,10,11,12,13)]


# Create data 
data <- tabla_cor7

# Check correlations (as scatterplots), distribution and print corrleation coefficient 
ggpairs(data, title="Correlograma Cambios de uso de suelo vs. Cambio relativo agricola 07 a 16")


# UNIR DATOS ANALÍTICOS CON DATOS GEOESPACIALES

ac_mapa@data = data.frame(ac_mapa@data, dfa[match(ac_mapa@data[,"CVE_CONCAT"], dfa[,"CVE_CONCAT"]),])

# ESTILOS DEL MAPA
bins_terrenos_tot <- c(0, 10, 20, 50, 100, 200, 500, Inf)
bins_pct <- seq(0.1:1, by = 0.02)

# PALETA DE COLORES
pal_1 <- colorBin("viridis", domain = as.numeric(as.character(dfa$Terrenos_totales)), bins = bins_terrenos_tot)
pal_2 <- colorBin( palette="magma", domain=ac_mapa@data$NUM_TERR, na.color="transparent", bins=bins_pct)
pal_3 <- colorBin( palette="YlOrBr", domain=ac_mapa@data$NUM_TERR, na.color="transparent", bins=bins_pct)

