# CARGAR LIBRERIAS UTILIZADAS DE R
library(data.table)
library(curl)
library(devtools)
library(rio)
library(shiny)
library(shinythemes)
library(DT)
library(leaflet)
library(rgdal)
library(sf)
library(plyr)
library(DBI)
library(dplyr)
library(sf)
library(corrplot)
library(car)
library(ggplot2)
library(curl)
library(geojsonio)
library(reshape2)
library(ggcorrplot)
library(colorspace)

load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/carto.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/concentrados.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/comparados.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/correlaciones.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/datos.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/autocorrelaciones.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/cambios.RData"))

bins_terrenos_tot <- c(0, 10, 20, 50, 100, 150, 200, Inf)
bins_series <- c(1, 2, 3, 4, 5, 6, 7)
bins_cambios <- c(1, 2, 3, 4)
bins_pct <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
bins_autocorr <- c(0,1,2,3,4)

# PALETA DE COLORES

pal <- colorNumeric(c("white", "red", "blue", "green", "grey"), 0:4)

pal_0a <- colorBin( palette="Set1", domain = autocorr_deforestacion$LISA_CL, bins = bins_autocorr)
pal_0b <- colorBin( palette="Set1", domain = autocorr_deforestacion$LISA_CLdef, bins = bins_autocorr)
pal_0c <- colorBin( palette="Set1", domain = autocorr_deforestacion$LISA_CLdeg, bins = bins_autocorr)

pal_1 <- colorBin( palette="viridis", domain = as.numeric(as.character(ac_mapa@data$TERRENOS)), bins = bins_terrenos_tot)
pal_2 <- colorNumeric( palette= "YlGn", domain=ac_mapa@data$PCT_FORESTAL, na.color="transparent")
pal_3 <- colorBin( palette="YlOrBr", domain=ac_mapa@data$PCT_AGRICOLA, na.color="transparent", bins=bins_pct)
pal_4 <- colorBin( palette="YlOrRd", domain=ac_mapa@data$PCT_PECUARIO, na.color="transparent", bins=bins_pct)
pal_5 <- colorBin( palette="Spectral", domain = as.numeric(as.character(serie_3@data$VALOR)), bins = bins_series)
pal_6 <- colorBin( palette="Spectral", domain = as.numeric(as.character(serie_6@data$VALOR)), bins = bins_series)
pal_7 <- colorBin( palette="Spectral", domain = as.numeric(as.character(cambios_ndvi@data$gridcode)), bins = bins_cambios) 
pal_8 <- colorBin( palette="Spectral", domain = as.numeric(as.character(cambios_usv@data$gridcode)), bins = bins_cambios)

# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- colnames(matriz_correlacion)

# POP UPS

pop_terrenos <- paste0("<b><br/> Área de control: </b>", ac_mapa_mc$CONTROL,
                       "<b><br/> Terrenos totales: </b>", ac_mapa_mc$TERRENOS,
                       "<b><br/> Superficie total: </b>", ac_mapa_mc$SUP_TOTAL, " ha",
                       "<b><br/> Tamaño promedio de terreno: </b>", as.character(round(as.numeric(ac_mapa_mc$`TERRENO PROMEDIO GENERAL 2016`), 2)), " ha",
                       "<b><br/> Tamaño promedio de terreno: </b>", as.character(round(as.numeric(ac_mapa_mc$`TERRENO PROMEDIO SEMBRADO 2016`), 2)), " ha",
                       "<b><br/> Terrenos agrícolas: </b>", as.character(round(as.numeric(ac_mapa_mc$PCT_AGRICOLA), 2)), "%",
                       "<b><br/> Terrenos pecuarios: </b>", as.character(round(as.numeric(ac_mapa_mc$PCT_PECUARIO), 2)), "%",
                       "<b><br/> Terrenos forestales: </b>", as.character(round(as.numeric(ac_mapa_mc$PCT_FORESTAL), 2)), "%")

pop_agricola <- paste0("<b><br/> Terrenos totales: </b>", as.character(ac_mapa_mc$TERRENOS),
                       "<b><br/> Superficie total: </b>", as.character(ac_mapa_mc$SUP_TOTAL), "ha",
                       "<b><br/> Terrenos agrícolas: </b>", as.character(round(as.numeric(ac_mapa_mc$PCT_AGRICOLA), 2)), "%",
                       "<b><br/> Superficie sembrada 2007: </b>", as.character(round(as.numeric(ac_mapa_mc$`SUPERFICIE_SEMBRADA_%_07`), 2)), "%",
                       "<b><br/> Superficie sembrada 2016: </b>", as.character(round(as.numeric(ac_mapa_mc$`SUPERFICIE_SEBRADA_%_16`), 2), "%"))

pop_forestal <- paste0("<b><br/> Terrenos totales: </b>", as.character(ac_mapa_mc$TERRENOS),
                       "<b><br/> Superficie total: </b>", as.character(ac_mapa_mc$SUP_TOTAL), "ha",
                       "<b><br/> Terrenos forestales: </b>", as.character(round(as.numeric(ac_mapa_mc$PCT_FORESTAL), 2)), "%")

pop_pecuario <- paste0("<b><br/> Terrenos totales: </b>", as.character(ac_mapa_mc$TERRENOS),
                       "<b><br/> Superficie total: </b>", as.character(ac_mapa_mc$SUP_TOTAL), "ha",
                       "<b><br/> Terrenos pecuarios: </b>", as.character(round(as.numeric(ac_mapa_mc$PCT_PECUARIO), 2)), "%")
