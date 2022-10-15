library(R.utils)
library(dplyr)
library(ncdf4)
library(lubridate)
library(tibble)
library(stringr)
library(sf)

###Extraer las fechas de los periodos de cultivo

load("../Bases de datos/Procesadas/BaseModeloRelacionClima.RData")

fechas <- unique(base.modelo$Fecha)

#######Lectura de archivos

setwd("C:/Users/mpdue/OneDrive/Documents/RadiacionSolar/Resultados")

archivos <- list.files(path="..", full.names=TRUE, recursive = TRUE)

####Extraer los archivos de las fechas necesarias

fechas.rad <- str_match(archivos, "AgERA5_\\s*(.*?)\\s*_final-v1_")[,2]

fechas.rad <- as.Date(paste0(substr(fechas.rad,1,4),"-",substr(fechas.rad,5,6),"-",substr(fechas.rad,7,8)),
                      format = "%Y-%m-%d")


archivos.op <- archivos[which(fechas.rad %in% fechas)]

datosRad <- NULL

for(i in 1:2000)
{
  
  tryCatch({
    
    Datanc <- nc_open(archivos.op[i])
    
    ######Extracción de la información
    
    n <- length(Datanc$dim$lat$vals)*length(Datanc$dim$lon$vals)
    
    nx <- length(Datanc$dim$lon$vals)
    ny <- length(Datanc$dim$lat$vals)
    
    info <- data.frame("LON"=rep(Datanc$dim$lon$vals, ny),
                       "LAT"=rep(Datanc$dim$lat$vals, each=nx),
                       "Fecha"=rep(as.Date(Datanc$dim$time$vals, origin="1900-01-01"), n),
                       "Valor"=c(ncvar_get(Datanc,varid = "Solar_Radiation_Flux")/86400),
                       "Variable"=rep("RadiacionSolar", n)
    ) %>%
      filter(!is.na(Valor))
    
    datosRad <- datosRad %>%
      bind_rows(info)
    
    rm(Datanc, info)
    gc()
    
    print(i) }, error = function(e) { closeAllConnections() })
  closeAllConnections()
  
}

save(datosRad,
     file="C:/Users/mpdue/OneDrive/Documents/Extensión UNAL/Fedearroz/Bases de datos/Procesadas/datosRadiacionDiaria3.RData")

####Reiniciar R y cargar los datos faltantes

load("C:/Users/mpdue/OneDrive/Documents/Extensión UNAL/Fedearroz/Bases de datos/Procesadas/datosRadiacionDiaria3.RData")

load("../Bases de datos/Procesadas/BaseModeloRelacionClima.RData")

fechas <- unique(base.modelo$Fecha)

#######Lectura de archivos

setwd("C:/Users/mpdue/OneDrive/Documents/RadiacionSolar/Resultados")

archivos <- list.files(path="..", full.names=TRUE, recursive = TRUE)

####Extraer los archivos de las fechas necesarias

fechas.rad <- str_match(archivos, "AgERA5_\\s*(.*?)\\s*_final-v1_")[,2]

fechas.rad <- as.Date(paste0(substr(fechas.rad,1,4),"-",substr(fechas.rad,5,6),"-",substr(fechas.rad,7,8)),
                      format = "%Y-%m-%d")


archivos.op <- archivos[which(fechas.rad %in% fechas)]


for(i in 2001:length(fechas))
{
  
  tryCatch({
    
    Datanc <- nc_open(archivos.op[i])
    
    ######Extracción de la información
    
    n <- length(Datanc$dim$lat$vals)*length(Datanc$dim$lon$vals)
    
    nx <- length(Datanc$dim$lon$vals)
    ny <- length(Datanc$dim$lat$vals)
    
    info <- data.frame("LON"=rep(Datanc$dim$lon$vals, ny),
                       "LAT"=rep(Datanc$dim$lat$vals, each=nx),
                       "Fecha"=rep(as.Date(Datanc$dim$time$vals, origin="1900-01-01"), n),
                       "Valor"=c(ncvar_get(Datanc,varid = "Solar_Radiation_Flux")/86400),
                       "Variable"=rep("RadiacionSolar", n)
    ) %>%
      filter(!is.na(Valor))
    
    datosRad <- datosRad %>%
      bind_rows(info)
    
    rm(Datanc, info)
    gc()
    
    print(i) }, error = function(e) { closeAllConnections() })
  closeAllConnections()
  
}

save(datosRad,
     file="C:/Users/mpdue/OneDrive/Documents/Extensión UNAL/Fedearroz/Bases de datos/Procesadas/datosRadiacionDiaria3.RData")


################################
#####Pegar la info a las fincas

##Traer coordenadas de las fincas con datos

setwd("~/Extensión UNAL/Fedearroz/Scripts")
load("../Bases de datos/Procesadas/rendimientos_geo.RData")
load("../Bases de datos/Procesadas/datosRadiacionDiaria3.RData")
load("../Bases de datos/Procesadas/BaseModeloRelacionClima.RData")

fechas <- unique(base.modelo$Fecha)

fincas.mod <- base.modelo %>% 
  distinct(Fecha, FINCA) 

fincas.geo <- fincas.geo %>% 
  dplyr::select(FINCA, FECHA_SIEMBRA, FECHA_COSECHA, RENDIMIENTO) %>% 
  filter(year(FECHA_COSECHA)<2020) %>% 
  distinct(FINCA, FECHA_SIEMBRA, FECHA_COSECHA, .keep_all = TRUE)

coords <- data.frame("FINCA"=fincas.geo$FINCA,
                     st_coordinates(fincas.geo)) %>% 
  distinct(FINCA, .keep_all=TRUE)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

####Fincas y fechas únicas

fincas.mod <- fincas.mod %>% 
  left_join(coords) %>% 
  st_as_sf(coords=c("X", "Y"),
           crs=projcrs)

###Pegar la información del punto más cercano a cada finca en cada fecha

rad.fincas <- NULL

for(i in 1:length(fechas)){
  
  rad.fecha <- datosRad %>% 
    filter(Fecha==fechas[i]) %>% 
    st_as_sf(coords=c("LON", "LAT"),
             crs=projcrs)
  
  fincas.fecha <- fincas.mod %>% 
    filter(Fecha==fechas[i])
  
  distancias <- st_distance(fincas.fecha, rad.fecha)
  valor <- rad.fecha$Valor[apply(distancias, 1, which.min)]
  
  info.temp <- fincas.fecha%>% 
    bind_cols(data.frame("Variable"=rep("RADIACION", nrow(fincas.fecha)),
                         "Valor"=valor)) %>% 
    st_drop_geometry()
  
  rad.fincas <- rad.fincas %>% 
    bind_rows(info.temp)
  
  print(paste0("i",i))
}

save(rad.fincas, file="../Bases de datos/Procesadas/RadiacionFincas.RData")