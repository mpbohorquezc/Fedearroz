library(dplyr)
library(sf)
library(lubridate)
library(tidyr)
library(writexl)
library(automap)
library(gstat)

####Carga info IDEAM

# load("../Bases de datos/Procesadas/baseFiltradaArroz.RData")
# 
# ####Calcular porcentaje de faltantes
# 
# periodo  <- seq(from=as.Date("1980-01-01", format="%Y-%m-%d"),
#                 to=as.Date("2019-12-31", format="%Y-%m-%d"),
#                 by="day")
# 
# 
# b.ancha <- baseFiltArroz %>% 
#   dplyr::select(Estacion, Fecha, Variable, Valor) %>% 
#   pivot_wider(names_from = "Variable", values_from = "Valor") %>% 
#   pivot_longer(cols=PTPG_TT_D:TMX_CON, names_to = "Variable", values_to = "Valor")
# 
# faltantes <- b.ancha %>% 
#   group_by(Variable) %>% 
#   summarise(Total=n(), Faltantes=sum(is.na(Valor)), P.Falt=(Faltantes/Total)*100)
# 
# write_xlsx(faltantes, "../Resultados/PorcentajeFaltantesVariablesOriginales.xlsx")

######Imputación 

load("../Bases de datos/Procesadas/baseFiltradaArroz.RData")


base.pt <- baseFiltArroz %>% 
filter(Variable=="PTPM_CON", Altitud<1163) %>% 
    dplyr::select(Estacion, Fecha, Valor, LONG, LAT)


###Estaciones únicas

estaciones <- base.pt %>% 
  distinct(Estacion, .keep_all = TRUE)

crs.p <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

shp.estaciones <- estaciones %>% 
  st_as_sf(coords=c("LONG", "LAT"),
           crs=crs.p)

##identificar datos faltantes

periodo  <- seq(from=as.Date("1980-01-01", format="%Y-%m-%d"),
                to=as.Date("2019-12-31", format="%Y-%m-%d"),
                by="day")


tt <- data.frame("Fecha"=rep(periodo, each=nrow(estaciones)),
                "Estacion"=rep(estaciones$Estacion, length(periodo))
)


##Cargar municipios

municipios <- st_read("../Bases de datos/Originales/MGN2020_MPIO_POLITICO", 
                      layer="MGN_MPIO_POLITICO") %>% 
  dplyr::select(DPTO_CCDGO, MPIO_CCDGO, DPTO_CNMBR, MPIO_CCNCT, MPIO_CNMBR) %>% 
  st_transform(4326)

est.muni <- st_intersection(shp.estaciones, municipios)

####Departamentos con arroz

dep.arroz <- est.muni %>% 
  distinct(DPTO_CNMBR)

llanos <- c("VICHADA", "CASANARE", "GUAVIARE", "CAQUETÁ", "ARAUCA", "META")

centro <- c("QUINDIO", "RISARALDA", "ANTIOQUIA", "CALDAS",  "CUNDINAMARCA", "BOYACÁ", "BOGOTÁ, D.C.")

tolima_huila <- c("CAUCA", "VALLE DEL CAUCA"," HUILA", "TOLIMA")

caribe <- c("BOLÍVAR", "CESAR", "LA GUAJIRA", "MAGDALENA", "CÓRDOBA", "ATLÁNTICO", "SUCRE", "NORTE DE SANTANDER",
            "SANTANDER")

#####Pegar la región a la base de estaciones

est.muni <- est.muni %>% 
  mutate(Zona=case_when(DPTO_CNMBR %in% llanos ~ "Llanos",
                        DPTO_CNMBR %in% centro ~ "Centro",
                        DPTO_CNMBR %in% tolima_huila ~ "Tolima y Huila",
                        TRUE ~ "Norte"))

#########Identificar datos faltantes

base.pt <- tt %>% 
  left_join(base.pt %>% 
              dplyr::select(-c(LAT, LONG)), by=c("Fecha"="Fecha", "Estacion"="Estacion")) %>% 
  left_join(estaciones %>% 
              dplyr::select(Estacion, LAT, LONG), by=c("Estacion"="Estacion"))


n.falt <- sum(is.na(base.pt$Valor))/nrow(base.pt)

save(base.pt, est.muni, file="../Bases de datos/Procesadas/basePrecipitacion.RData")

######Imputación de precipitación

#base.predicciones <-  NULL

load("../Bases de datos/Procesadas/ImputacionPrecipitacionDiaria.RData")

#11319

for(i in 11319:length(periodo)){
  
  base.comp <- base.pt %>% 
    filter(Fecha==periodo[i] & !is.na(Valor)) 
  coordinates(base.comp) <- ~LONG+LAT
  
  base.pred <- base.pt %>% 
    filter(Fecha==periodo[i] & is.na(Valor)) 
  coordinates(base.pred) <- ~LONG+LAT
  
  skip_to_next <- FALSE
  
  tryCatch(krig <-  autoKrige(Valor~1, base.comp, base.pred),  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  
    prediccion <- data.frame(krig$krige_output, 
                             "Fecha"=rep(periodo[i], nrow(base.pred)),
                                         "Estacion"= base.pred$Estacion)
    
    base.predicciones <- base.predicciones %>% 
      bind_rows(prediccion)
    
    print(paste0(periodo[i]))
  
}

  save(base.predicciones, file="../Bases de datos/Procesadas/ImputacionPrecipitacionDiaria.RData")


