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
library(geojsonio)

load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/carto.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/concentrados.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/comparados.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/correlaciones.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/datos.RData"))


ac_mapa@data = data.frame(ac_mapa@data, dfa[match(ac_mapa@data[,"CVE_CONCAT"], dfa[,"CVE_CONCAT"]),])


bins_terrenos_tot <- c(0, 10, 20, 50, 100, 150, 200, Inf)
bins_pct <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)

# PALETA DE COLORES
pal_1 <- colorBin( palette="viridis", domain = as.numeric(as.character(ac_mapa@data$TERRENOS)), bins = bins_terrenos_tot)
pal_2 <- colorBin( palette="YlGn", domain=ac_mapa@data$PCT_FORESTAL, na.color="transparent", bins=bins_pct)
pal_3 <- colorBin( palette="YlOrBr", domain=ac_mapa@data$PCT_AGRICOLA, na.color="transparent", bins=bins_pct)
pal_4 <- colorBin( palette="RdPu", domain=ac_mapa@data$PCT_PECUARIO, na.color="transparent", bins=bins_pct)


# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- colnames(df_correlacion_mc_c)

