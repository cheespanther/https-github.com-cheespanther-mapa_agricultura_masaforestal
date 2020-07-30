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
bins_pct <- c(.0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
bins_autocorr <- c(0, 10, 20, 50, 100)

# PALETA DE COLORES

pal <- colorNumeric("viridis", NULL)

pal_0 <- colorBin( palette="magma", domain = as.numeric(as.character(autocorr_1@data$ha_1)), bins = bins_autocorr)
pal_1 <- colorBin( palette="viridis", domain = as.numeric(as.character(ac_mapa@data$TERRENOS.x)), bins = bins_terrenos_tot)
pal_2 <- colorBin( palette= "YlGn", domain=ac_mapa@data$PCT_FORESTAL, na.color="transparent", bins=bins_pct)
pal_3 <- colorBin( palette="inferno", domain=ac_mapa@data$PCT_AGRICOLA, na.color="transparent", bins=bins_pct)
pal_4 <- colorBin( palette=rev(heat.colors(20)), domain=ac_mapa@data$PCT_PECUARIO, na.color="transparent", bins=bins_pct)
pal_5 <- colorBin( palette="Spectral", domain = as.numeric(as.character(serie_3@data$VALOR)), bins = bins_series)
pal_6 <- colorBin( palette="Spectral", domain = as.numeric(as.character(serie_6@data$VALOR)), bins = bins_series)
pal_7 <- colorBin( palette="Spectral", domain = as.numeric(as.character(cambios_ndvi@data$gridcode)), bins = bins_cambios) 
pal_8 <- colorBin( palette="Spectral", domain = as.numeric(as.character(cambios_usv@data$gridcode)), bins = bins_cambios)

# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- colnames(df_correlacion_mc_d)

# POP UPS

pct_productividad <- paste0("<b><br/> Área de control: </b>", ac_mapa_mc$CONTROL,
                            "<b><br/> Terrenos totales: </b>", ac_mapa_mc$TERRENOS.x,
                            "<b><br/> Superficie total: </b>", ac_mapa_mc$SUP_TOTAL.x, " ha",
                            "<b><br/> Tamaño promedio de terreno: </b>", as.character(round(as.numeric(ac_mapa_mc$`TERRENO PROMEDIO SEMBRADO 2016`), 3)), " ha",
                            "<b><br/> Terrenos agrícolas: </b>", as.character(100*round(as.numeric(ac_mapa_mc$PCT_AGRICOLA), 3)), "%",
                            "<b><br/> Terrenos pecuarios: </b>", as.character(100*round(as.numeric(ac_mapa_mc$PCT_PECUARIO), 3)), "%",
                            "<b><br/> Terrenos forestales: </b>", as.character(100*round(as.numeric(ac_mapa_mc$PCT_FORESTAL), 3)), "%")

pop_agricola <- paste0("<b><br/> Terrenos agrícolas: </b>", as.character(100*round(as.numeric(ac_mapa_mc$TERRENOS.1), 3)), "%",
                       "<b><br/> Tamaño promedio terreno sembrado: </b>", as.character(100*round(as.numeric(ac_mapa_mc$TERRENO_PROM_SEM_16), 3)), "%",
                       "<b><br/> Tamaño promedio terreno: </b>", as.character(100*round(as.numeric(ac_mapa_mc$TERRENO_PROM_TOT_16), 3)), "%")


