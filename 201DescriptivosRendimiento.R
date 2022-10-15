library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)

##########Carga info de fincas

load("../Bases de datos/Procesadas/rendimientos_geo.RData")

n.fincas <- n_distinct(fincas.geo$FINCA)

###Gráfico de distribución de rendimientos

grafico.org <- fincas.geo %>% 
  st_drop_geometry() %>% 
  ggplot(aes(x=RENDIMIENTO)) +
  geom_histogram(fill="#70a9a1", color="#40798c", alpha=0.9) +
  ylab("Frecuencia") +
  xlab("Rendimiento") +
  ggtitle("Distribución de rendimiento (kg/ha)")

summary(fincas.geo$RENDIMIENTO)

grafico.org <- fincas.geo %>% 
  st_drop_geometry() %>% 
  filter(RENDIMIENTO<50000) %>% 
  ggplot(aes(x=RENDIMIENTO)) +
  geom_histogram(fill="#70a9a1", color="#40798c", alpha=0.9) +
  ylab("Frecuencia") +
  xlab("Rendimiento") +
  ggtitle("Distribución de rendimiento (kg/ha)")

####Gráfico distribuciones mensuales

fincas.geo <- fincas.geo %>% 
  mutate(MES=month(FECHA_COSECHA))

grafico.box <- fincas.geo %>% 
  st_drop_geometry() %>% 
  filter(RENDIMIENTO<50000) %>% 
  ggplot(aes(x=as.factor(MES), y=RENDIMIENTO)) +
  geom_boxplot(fill="#70a9a1", color="#40798c") +
  ylab("Rendimiento") +
  xlab("Mes") +
  ggtitle("Distribución mensual de rendimiento \n  (kg/ha)")



###Cargar los polígonos de zonas climáticas

load("../Resultados/MapasClasificacion/ClasificacionDef/PoligonosZonasClimaticas.RData")

zonas <- poli1 %>% 
  rbind(poli2) %>% 
  rbind(poli3) %>% 
  rbind(poli4) %>% 
  mutate(Zona=substr(Clust.DBSCAN, 1, 1))

p.zonas <- colorFactor(c("#016339", "#DE8103", "#7400B6", "#006AB6"),
                      zonas$Zona)
####Mapa General

base.out <- fincas.geo %>% 
  filter(RENDIMIENTO<50000) %>% 
  st_transform(4326)

paleta <- colorNumeric(
  palette = "YlGnBu",
  domain = base.out$RENDIMIENTO
)

mapa <- leaflet(base.out) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento",
            opacity = 1
  ) 

#####

etiquetas <- sprintf(
  "<strong>Zona %s</strong>",
  zonas$Zona) %>%
  lapply(htmltools::HTML)

mapa.zonas <- mapa %>% 
  addPolygons(data=zonas, 
              color = "#444444", weight = 1, smoothFactor = 0.5,
              fillColor = ~p.zonas(Zona),
              label = ~etiquetas,
              group = "Zonas climáticas") %>% 
  addLayersControl(
    overlayGroups = c("Zonas climáticas", "Fincas"),
    options = layersControlOptions(collapsed = FALSE)
  )

###Distribuciones espaciales mensuales

fincas.geo <- fincas.geo %>% 
  filter(RENDIMIENTO<50000)

#Enero

finca.mes <- fincas.geo %>% 
  filter(MES==1) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Enero)",
            opacity = 1
  ) 

#Febrero

finca.mes <- fincas.geo %>% 
  filter(MES==2) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Febrero)",
            opacity = 1
  ) 

#Marzo

finca.mes <- fincas.geo %>% 
  filter(MES==3) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Marzo)",
            opacity = 1
  )

#Abril

finca.mes <- fincas.geo %>% 
  filter(MES==4) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Abril)",
            opacity = 1
  )

#Mayo

finca.mes <- fincas.geo %>% 
  filter(MES==5) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Mayo)",
            opacity = 1
  )

#Junio

finca.mes <- fincas.geo %>% 
  filter(MES==6) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Junio)",
            opacity = 1
  )

#Julio

finca.mes <- fincas.geo %>% 
  filter(MES==7) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Julio)",
            opacity = 1
  )

#Agosto

finca.mes <- fincas.geo %>% 
  filter(MES==8) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Agosto)",
            opacity = 1
  )

#Septiembre

finca.mes <- fincas.geo %>% 
  filter(MES==9) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Septiembre)",
            opacity = 1
  )

#Octubre

finca.mes <- fincas.geo %>% 
  filter(MES==10) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Octubre)",
            opacity = 1
  )

#Noviembre

finca.mes <- fincas.geo %>% 
  filter(MES==11) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Noviembre)",
            opacity = 1
  )

#Diciembre

finca.mes <- fincas.geo %>% 
  filter(MES==12) %>% 
  st_transform(4326)

mapa <- leaflet(finca.mes) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addCircleMarkers(
    radius = 3,
    color = ~paleta(RENDIMIENTO),
    stroke = FALSE, fillOpacity = 0.7,
    group = "Fincas"
  ) %>% 
  addLegend("bottomright", pal = paleta, values = ~RENDIMIENTO,
            title = "Rendimiento (Diciembre)",
            opacity = 1
  )

