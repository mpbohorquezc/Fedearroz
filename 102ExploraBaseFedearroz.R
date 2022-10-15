library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(leaflet)
library(automap)
library(tidyr)
library(rgdal)

##Carga coordenadas de las fincas

coords.fincas <- read_xlsx("../Bases de datos/Originales/Coordenadas_AMTEC_Hist_2020.xlsx") %>% 
  dplyr::select(ID_TEMP, LONG, LAT) %>% 
  mutate(LONG=as.numeric(LONG), LAT=as.numeric(LAT))

##Carga info de las fincas

info.fincas <- read_xlsx("../Bases de datos/Originales/CONSOLIDADO_SACFA_Sel_Variables.xlsx", sheet="Hoja1") 

fincas <- info.fincas %>% 
  left_join(coords.fincas, by=c("ID_TEMP"="ID_TEMP"))

n.fincas <- unique(info.fincas$FINCA)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

fincas <- fincas %>% 
  filter(!is.na(LAT) & !is.na(LONG))

fincas.sf <- st_as_sf(fincas, coords=c("LONG", "LAT"),
                      crs=projcrs)

n.fincas.geo <- unique(fincas.sf$FINCA)

plot(st_geometry(fincas.sf))

ufincas <- fincas.sf %>% 
  distinct(FINCA, .keep_all = TRUE) %>% 
  mutate(fig="finca")

plot(st_geometry(ufincas))

####Creación de los mapas

iconos <- iconList(
  finca = makeIcon(
    iconUrl = "https://img.icons8.com/external-flatart-icons-lineal-color-flatarticons/64/000000/external-agriculture-agriculture-flatart-icons-lineal-color-flatarticons.png",
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 15, iconAnchorY = 15
  ))

mapa <-  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>% 
  addMarkers(data=ufincas %>%
               st_transform(4326), icon=~iconos[fig],
             group="Fincas")


mapa <- mapa %>% 
  addLayersControl(overlayGroups = c("Fincas", "Estaciones", "Buffer"),
                   options = layersControlOptions(collapsed = FALSE))

###Carga de estaciones con info

estaciones.sf <- read_sf("../Resultados/capaEstacionesIDEAM", 
                         layer="capaEstacionesIDEAM")


##¿Hasta qué distancia hay correlación espacial?

# n.var <- baseActiva %>% 
#   group_by(Variable) %>% 
#   summarise()
# 
# 

###Creación de buffers de 100km alrededor de las fincas

buffer.fin <- st_buffer(ufincas, dist=100000)


mapaBuffer <-  mapa %>% 
  addPolygons(data=buffer.fin %>% 
                st_transform(4326), color="#606c38", 
              fillColor ="#606c38", fillOpacity = 0.2, weight=0.2,
              group="Buffer") %>%
  addCircleMarkers(data=estaciones.sf %>%
                     st_transform(4326), radius=1, color="#ae2012",
                   group="Estaciones")

##Estaciones de Arauca y Guaviare adicionales a las de los buffers

departamentos <-read_sf("../Bases de datos/Originales/MGN2020_DPTO_POLITICO",
                        layer="MGN_DPTO_POLITICO")

dep <- departamentos %>% 
  filter(DPTO_CNMBR %in% c("GUAVIARE", "ARAUCA"))

int <- st_intersects(estaciones.sf, dep)


for(i in 1:nrow(estaciones.sf)){
  
  
  estaciones.sf$est.ad[i] <- case_when(length(int[[i]])>0 ~ 1,
                            TRUE ~ 0)
  
  
}


###Estaciones dentro de los buffers

interseccion <- st_intersects(estaciones.sf, buffer.fin)


for(i in 1:nrow(estaciones.sf)){
  
  estaciones.sf$est.arroz[i] <- case_when(length(interseccion[[i]])>0 ~ 1,
                            TRUE ~ 0)
  
}

###Shapefile de estaciones de la zona arrocera

est.arroz.sf <- estaciones.sf %>%
  filter(est.arroz==1 | est.ad==1)

mapaEstArroz <-  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%
  addCircleMarkers(data=est.arroz.sf %>%
                     st_transform(4326), radius=1, color="#023047",
                   group="Estaciones")

st_write(est.arroz.sf, dsn="../Resultados/capaEstacionesZonaArrocera/capaEstacionesZonaArrocera.shp", 
         layer="capaEstacionesZonaArrocera.shp")

#################################################################################
#######Creación de base de datos con la información de las estaciones arroceras##
##################################################################################

###Carga base de toda la información IDEAM

load("../Bases de datos/Procesadas/baseIdeamGeorreferenciada.RData") 

###Pegar la columna que indica si es una estación arrocera

baseIndArroz <- baseActiva %>% 
  left_join(estaciones.sf %>% 
              st_drop_geometry() %>% 
              select(Estacion, est.ad, est.arroz), by=c("Estacion"="Estacion")) 

rm(baseActiva)  
gc()

###Base con registros de las estaciones arroceras

baseEstarroz <- baseIndArroz %>% 
  filter(est.arroz==1 | est.ad==1)

#save(baseEstarroz, file="../Bases de datos/Procesadas/baseIdeamEstacionesArroz.RData")


load("../Bases de datos/Procesadas/baseIdeamEstacionesArroz.RData")

baseEstarroz <- baseEstarroz %>% 
  distinct(Fecha, Variable, Estacion, .keep_all = TRUE)

##Resumen total de datos por variable y mínimo de fecha y máximo fecha

# tot_estaciones <-  baseIndArroz %>% 
#   filter(est.arroz==1) %>% 
#   group_by(Estacion) %>% 
#   summarise(Tot=n()) %>% 
#   nrow()


resumenEstArroz <- baseEstarroz %>% 
  group_by(Variable) %>% 
  summarise(Total=n(), Fecha_ini=min(Fecha), Fecha_max=max(Fecha), Tot_dias=Fecha_max-Fecha_ini,
            Esperado=Tot_dias*tot_estaciones, N_estaciones=n_distinct(Estacion)) %>% 
  mutate(Porcentaje=(1-(Total/as.numeric(Esperado)))*100)


####Base de datos para mapa de calor: días con registros completos

variables <- unique(baseEstarroz$Variable)

for(i in 1:length(variables)){
  
  baseVar <- baseEstarroz %>% 
    filter(Variable==paste0(variables[i])) %>% 
    mutate(Mes=format(Fecha, "%Y-%m"), Dia=as.Date(paste0(Mes, "-", 01), format="%Y-%m-%d")) %>% 
    group_by(Estacion, Dia) %>% 
    summarise(Tot_obs=n()) %>% 
    arrange(Estacion, Dia)
  
  grafico <- ggplot(data=baseVar, aes(x=Estacion, y=Dia, fill=Tot_obs)) + 
    geom_tile() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank()) +
    xlab("Estaciones") +
    ylab("Meses") +
    ggtitle(paste0(variables[i])) +
    labs(fill="Total registros mensuales")
  
  ggsave(filename = paste0("../Resultados/DescriptivosPreviaImputacion/NumeroDatosMes_", variables[i], ".png"),
         width=10, height=5.18)
  
}

rm(baseVar)
gc()

######Dejar registros asociados a precipitación y temperatura y desde 1980

baseFiltArroz <- baseEstarroz %>% 
  filter(Año>=1980, Variable %in% c("PTPG_TT_D", "PTPM_CON", "TMX_CON", "TMN_CON"))

save(baseFiltArroz, file="../Bases de datos/Procesadas/baseFiltradaArroz.RData")

tot_estaciones <-  baseFiltArroz %>%
  group_by(Estacion) %>%
  summarise(Tot=n()) %>%
  nrow()

####Nuevo resumen

resumenEstArroz <- baseFiltArroz %>% 
  group_by(Variable) %>% 
  summarise(Total=n(), Fecha_ini=min(Fecha), Fecha_max=max(Fecha), Tot_dias=Fecha_max-Fecha_ini,
            Esperado=Tot_dias*tot_estaciones, N_estaciones=n_distinct(Estacion)) %>% 
  mutate(Porcentaje=(1-(Total/as.numeric(Esperado)))*100)
