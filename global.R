# CARGAR LIBRERIAS UTILIZADAS DE R
library(curl)
library(devtools)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(rio)
library(DT)
library(leaflet)
library(rgdal)
library(plyr)
library(dplyr)
library(sf)
library(corrplot)
library(ggplot2)
library(geojsonio)
library(leafpop)
library(raster)
library(diffeR)
library(RColorBrewer)

load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/carto.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/concentrados.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/comparados.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/correlaciones.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/datos.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/Rdata/autocorrelaciones.RData"))

ac_mapa@data = data.frame(ac_mapa@data, df_ac[match(ac_mapa@data[,"CVE_CONCAT"], df_ac[,"CVE_CONCAT"]),])
ac_mapa_mc = subset(ac_mapa, ac_mapa@data$NOM_MUN == "MARQUÉS DE COMILLAS")


bins_terrenos_tot <- c(0, 10, 20, 50, 100, 150, 200, Inf)
bins_series <- c(1, 2, 3, 4, 5, 6, 7)
bins_pct <- c(.0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
bins_autocorr <- c(0, 10, 20, 50, 100)

# PALETA DE COLORES
pal_0 <- colorBin( palette="magma", domain = as.numeric(as.character(autocorr_1@data$ha_1)), bins = bins_autocorr)
pal_1 <- colorBin( palette="viridis", domain = as.numeric(as.character(ac_mapa@data$TERRENOS)), bins = bins_terrenos_tot)
pal_2 <- colorBin( palette= "YlGn", domain=ac_mapa@data$PCT_FORESTAL, na.color="transparent", bins=bins_pct)
pal_3 <- colorBin( palette="inferno", domain=ac_mapa@data$PCT_AGRICOLA, na.color="transparent", bins=bins_pct)
pal_4 <- colorBin( palette=rev(heat.colors(20)), domain=ac_mapa@data$PCT_PECUARIO, na.color="transparent", bins=bins_pct)

pal_5 <- colorBin( palette="Spectral", domain = as.numeric(as.character(serie_3@data$VALOR)), bins = bins_series)
pal_6 <- colorBin( palette="Spectral", domain = as.numeric(as.character(serie_6@data$VALOR)), bins = bins_series)

# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- colnames(df_correlacion_mc_d)

# POP UPS

pct_productividad <- paste0("</strong><br/> Terrenos agrícolas: ", as.character(100*round(ac_mapa_mc$PCT_AGRICOLA.x.1, 3)), "%",
                            "</strong><br/> Terrenos pecuarios: ", as.character(100*round(ac_mapa_mc$PCT_PECUARIO.1, 3)), "%",
                            "</strong><br/> Terrenos forestales: ", as.character(100*round(ac_mapa_mc$PCT_FORESTAL.1, 3)), "%")

pop_agricola <- paste0()




