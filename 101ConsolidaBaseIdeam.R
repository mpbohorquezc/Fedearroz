library(stringr)
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(sf)
library(writexl)
library(ggplot2)

#  lista <- list.files(path="../Bases de datos/Originales/DATOS_20219050138092_1")
# 
# baseIdeam <- NULL
# 
# for(i in 1:length(lista))
# {
# 
# baseIdeam <- baseIdeam %>%
#   bind_rows(read.table(paste0("../Bases de datos/Originales/DATOS_20219050138092_1/",lista[i]), sep="|", header=TRUE) %>%
#               mutate(Fecha=as.Date(Fecha),
#                      Año=year(Fecha),
#                      Mes=month(Fecha),
#                      Variable=sub("@.* ", "", paste0(lista[i])),
#                      Estacion=str_match(paste0(lista[i]), "@\\s*(.*?)\\s*.data")[2]))
# print(i)
# 
# }
# 
# baseIdeam <-  baseIdeam %>%
#   mutate(Variable=sub("@.*", "", Variable))
# 
# save(baseIdeam, file="../Bases de datos/Procesadas/baseIdeamN.RData")
# 
# ###
# 
# lista2 <- list.files(path="../Bases de datos/Originales/DATOS_20219050138092_2")
# 
# baseIdeam2 <- NULL
# 
# for(i in 1:length(lista2))
# {
#   
#   baseIdeam2 <- baseIdeam2 %>%
#     bind_rows(read.table(paste0("../Bases de datos/Originales/DATOS_20219050138092_2/",lista2[i]), sep="|", header=TRUE) %>%
#                 mutate(Fecha=as.Date(Fecha),
#                        Año=year(Fecha),
#                        Mes=month(Fecha),
#                        Variable=sub("@.* ", "", paste0(lista[i])),
#                        Estacion=str_match(paste0(lista[i]), "@\\s*(.*?)\\s*.data")[2]))
#   print(i)
#   
# }
# 
# baseIdeam2 <-  baseIdeam2 %>%
#   mutate(Variable=sub("@.*", "", Variable))
# 
# save(baseIdeam2, file="../Bases de datos/Procesadas/baseIdeamN2.RData")


load("../Bases de datos/Procesadas/baseIdeam.RData")
# 
# tot.estaciones <- baseIdeam %>% 
#   group_by(Estacion) %>% 
#   summarise(Tot=n())
# 
# tot.variables <- baseIdeam %>% 
#   group_by(Variable) %>% 
#   summarise(Tot=n())
# 
# tot.fechas <- baseIdeam %>% 
#   group_by(Fecha) %>% 
#   summarise(Tot=n())
# 



# baseIdeam <- baseIdeam %>%
#   filter(Variable %in% c("BSHG_TT_D", "HRHG_MEDIA_D", "PT_10_TT_D", "PTPG_TT_D", "TMX_CON",
#                          "TMN_CON", "TSTG_MEDIA_D", "PTPM_CON", "HRA2_MEDIA_D"))
# 
# save(baseIdeam, file="../Bases de datos/Procesadas/baseIdeamFiltrada.RData")
# 
# tot.estaciones <- baseIdeam %>%
#   group_by(Estacion) %>%
#   summarise(Tot=n())


###Esta base sólo incluye las variables de interés para el estudio

load("../Bases de datos/Procesadas/baseIdeamFiltrada.RData") 

# variables <- baseActiva %>% 
#   group_by(Variable) %>% 
#   summarise(Total=n())

# humedad <- baseIdeam %>% 
#   filter(Variable=="HRA2_MEDIA_D")

baseActiva <- baseIdeam %>% 
  filter(!(Variable=="TMX_CON" & Valor>45)) %>% 
  filter(!(Variable=="TMN_CON" & Valor>40)) %>% 
  filter(Año>=1980, Variable %in% c("PTPG_TT_D", "PTPM_CON", "TMX_CON", "TMN_CON"))

rm(baseIdeam)
gc()

tot_var <- baseIdeam %>%
  group_by(Estacion, Variable) %>%
  summarise(Total_Obs=n()) %>%
  pivot_wider(id_cols=Estacion, names_from = Variable, values_from = Total_Obs, values_fill = 0)

#Variable brillo solar

hist_Brillo <- ggplot(tot_var, aes(x=BSHG_TT_D)) +
  geom_histogram(fill="#4F942A", alpha=0.6) +
  xlab("Número de datos en la variable Brillo Solar") +
  ylab("Número de estaciones") +
  ggtitle("Datos disponibles por estación")

hist_Humedad <- ggplot(tot_var, aes(x=HRHG_MEDIA_D)) +
  geom_histogram(fill="#4F942A", alpha=0.6) +
  xlab("Número de datos en la variable Humedad relativa") +
  ylab("Número de estaciones") +
  ggtitle("Datos disponibles por estación")

hist_Precipitacion10 <- ggplot(tot_var, aes(x=PT_10_TT_D)) +
  geom_histogram(fill="#4F942A", alpha=0.6) +
  xlab("Número de datos en la variable precipitación 10") +
  ylab("Número de estaciones") +
  ggtitle("Datos disponibles por estación")

hist_PrecipitacionPG <- ggplot(tot_var, aes(x=PTPG_TT_D)) +
  geom_histogram(fill="#4F942A", alpha=0.6) +
  xlab("Número de datos en la variable precipitación PG") +
  ylab("Número de estaciones") +
  ggtitle("Datos disponibles por estación")

hist_TempMax <- ggplot(tot_var, aes(x=TMX_CON)) +
  geom_histogram(fill="#4F942A", alpha=0.6) +
  xlab("Número de datos en la variable temperatura máxima") +
  ylab("Número de estaciones") +
  ggtitle("Datos disponibles por estación")

hist_TempMin <- ggplot(tot_var, aes(x=TMN_CON)) +
  geom_histogram(fill="#4F942A", alpha=0.6) +
  xlab("Número de datos en la variable temperatura mínima") +
  ylab("Número de estaciones") +
  ggtitle("Datos disponibles por estación")

hist_TempMed <- ggplot(tot_var, aes(x=TSTG_MEDIA_D)) +
  geom_histogram(fill="#4F942A", alpha=0.6) +
  xlab("Número de datos en la variable temperatura media") +
  ylab("Número de estaciones") +
  ggtitle("Datos disponibles por estación")




write_xlsx(tot_var, "../Resultados/totalObs_Estacion_Var.xlsx")

# base.fechas <- baseIdeam %>% 
#   select(Fecha, Variable, Estacion, Valor) %>% 
#   pivot_wider(id_cols=c(Estacion, Variable), names_from = Fecha, values_from = Valor)

estaciones <- read_xlsx("../Bases de datos/Originales/DatosMeteoIDEAM/Cat_logo_Nacional_de_Estaciones_del_IDEAM.xlsx")

estaciones <- estaciones %>%
  filter(!is.na(Ubicacion)) %>% 
  mutate(Codigo=as.character(Codigo), Ubicacion=gsub("\\(", "", Ubicacion)) %>%
  mutate(Ubicacion=gsub("\\)", "", Ubicacion)) %>%
  mutate(LAT=as.numeric(gsub(",.*$", "", Ubicacion)), LONG=as.numeric(gsub('.*,\\s*', '', Ubicacion))) %>%
  dplyr::select(Codigo, Nombre, Estado, Altitud, LAT, LONG)
  

baseActiva <- baseActiva %>%
  left_join(estaciones, by=c("Estacion"="Codigo")) %>%
  filter(!is.na(Estado))

baseActiva <- baseActiva %>% 
  distinct(Fecha, Variable, Estacion, .keep_all = TRUE)


length(levels(as.factor((baseActiva$Estacion))))

length(levels(as.factor((baseActiva$Variable))))

estacionesAct <- baseActiva %>%
  distinct(Estacion, .keep_all=TRUE)

###Guardar la info de las estaciones georreferenciadas

save(baseActiva, file="../Bases de datos/Procesadas/baseIdeamGeorreferenciada.RData")

###Creación de objeto sf

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

estaciones.sf <- st_as_sf(estacionesAct, coords=c("LONG", "LAT"),
                          crs=projcrs)

plot(st_geometry(estaciones.sf))

###Exportar el shape de estaciones meteorológicas

st_write(estaciones.sf, dsn="../Resultados/capaEstacionesIDEAM/capaEstacionesIDEAM.shp", 
         layer="capaEstacionesIDEAM.shp",
         layer_options = "OVERWRITE=true")

######Crear una base para cada variable

# save(baseActiva, estacionesAct, estaciones.sf, 
#      file="../Bases de datos/Procesadas/info_estActivas.RData")


