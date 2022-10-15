library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)

##############################################
##Pega info de radiación solar y cubrimiento##
##############################################

load("../Bases de datos/Procesadas/BaseModeloRelacionClima.RData")
load("../Bases de datos/Procesadas/RadiacionFincas.RData")
load("../Bases de datos/Procesadas/CubrimientoFincas.RData")

#####Id de tiempos de medición####

base.modelo <- base.modelo %>% 
  group_by(FINCA, Cosecha, Variable) %>% 
  mutate(Tiempo=seq(from=1, to=n(), by=1)) %>% 
  ungroup()

base.prec <- base.modelo %>% 
  filter(Variable=="PTPM_CON")

base.tmp <- base.modelo %>% 
  filter(Variable!="PTPM_CON")

base.w <- base.tmp %>% 
  pivot_wider(id_cols=c(Fecha, Dia, FECHA_SIEMBRA, FECHA_COSECHA, Tiempo, FINCA, Cosecha),
              names_from = Variable, values_from = Valor) %>% 
  left_join(rad.fincas %>% 
              pivot_wider(id_cols=c(Fecha, FINCA), 
                          names_from=Variable, values_from=Valor)) %>% 
  left_join(v.fincas %>% 
              pivot_wider(id_cols=c(Fecha, FINCA), 
                          names_from=Variable, values_from=Valor)) %>% 
  pivot_longer(TMN_CON:CUBRIMIENTO, names_to = "Variable", values_to = "Valor") %>% 
  filter(!is.na(Valor)) %>% 
  bind_rows(base.prec)

tot.obs <- base.modelo %>% 
  group_by(FINCA, Cosecha, Variable) %>% 
  summarise(Total=n())


###Pegar el cluster de la zona más cercana

estaciones <- read_xlsx("../Bases de datos/Originales/DatosMeteoIDEAM/Cat_logo_Nacional_de_Estaciones_del_IDEAM.xlsx")

estaciones <- estaciones %>%
  filter(!is.na(Ubicacion)) %>% 
  mutate(Codigo=as.character(Codigo), Ubicacion=gsub("\\(", "", Ubicacion)) %>%
  mutate(Ubicacion=gsub("\\)", "", Ubicacion)) %>%
  mutate(LAT=as.numeric(gsub(",.*$", "", Ubicacion)), LONG=as.numeric(gsub('.*,\\s*', '', Ubicacion))) %>%
  dplyr::select(Codigo, Nombre, Estado, Altitud, LAT, LONG)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

grupos <- read_xlsx("C:/Users/mpdue/OneDrive/Documents/Extensión UNAL/Fedearroz/Resultados/MapasClasificacion/ClasificacionDef/ClasificacionEstaciones.xlsx") %>%
  dplyr::select(-c(LONG, LAT)) %>% 
  left_join(estaciones %>% 
              dplyr::select(Codigo, LONG, LAT), by=c("Estacion"="Codigo")) %>% 
  st_as_sf(coords=c("LONG", "LAT"),
           crs=projcrs)

####Pegar la estación más cercana

load("../Bases de datos/Procesadas/rendimientos_geo.RData")

clase <- st_distance(fincas.geo, grupos) %>% 
  apply(1, which.min) 

fincas.geo$Clase <- grupos$Clase[clase]

base.modelo <- base.w %>% 
  left_join(fincas.geo %>% 
              dplyr::select(FINCA, Clase) %>% 
              distinct(FINCA, .keep_all=TRUE), by=c("FINCA"="FINCA"))

tot <- base.modelo %>% 
  group_by(FINCA, Cosecha, Variable) %>% 
  summarise(Total=n())


save(base.modelo, file="../Bases de datos/Procesadas/BaseModeloRelacionClimaComp.RData")


