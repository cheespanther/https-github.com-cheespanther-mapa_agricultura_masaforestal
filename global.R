load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/datos/Rdata/carto.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/datos/Rdata/concentrados.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/datos/Rdata/comparados.RData"))
load(url("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/datos/Rdata/correlaciones.RData"))

bins_terrenos_tot <- c(0, 10, 20, 50, 100, 150, 200, Inf)
bins_pct <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)

# PALETA DE COLORES
pal_1 <- colorBin( palette="viridis", domain = as.numeric(as.character(ac_mapa@data$TERRENOS)), bins = bins_terrenos_tot)
pal_2 <- colorBin( palette="YlGn", domain=ac_mapa@data$PCT_FORESTAL, na.color="transparent", bins=bins_pct)
pal_3 <- colorBin( palette="YlOrBr", domain=ac_mapa@data$PCT_AGRICOLA, na.color="transparent", bins=bins_pct)
pal_4 <- colorBin( palette="RdPu", domain=ac_mapa@data$PCT_PECUARIO, na.color="transparent", bins=bins_pct)

