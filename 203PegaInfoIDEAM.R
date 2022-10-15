library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(fda)
library(ggplot2)
library(stringr)
library(writexl)

###Carga rendimientos

load("../Bases de datos/Procesadas/rendimientos_geo.RData")

fincas.geo <- fincas.geo %>% 
  dplyr::select(FINCA, FECHA_SIEMBRA, FECHA_COSECHA, RENDIMIENTO) %>% 
  filter(year(FECHA_COSECHA)<2020) %>% 
  distinct(FINCA, FECHA_SIEMBRA, FECHA_COSECHA, .keep_all = TRUE)

n.fincas <- fincas.geo %>% 
  st_drop_geometry() %>% 
  distinct(FINCA)
  
###Cargar base diaria IDEAM
  
load("../Bases de datos/Procesadas/baseFiltradaArroz.RData")
  
base.diaria <- baseFiltArroz %>% 
    filter(Año>=2008)
  
rm(baseFiltArroz)
gc()

###Se dejan las que tienen altitud menor a los 1163 metros

base.def <- base.diaria %>% 
  filter(Altitud<=1163 & Variable !="PTPG_TT_D") %>% 
  dplyr::select(Fecha, Variable, Valor, Estacion, LAT, LONG, Altitud)

rm(base.diaria)
gc()

coords.est <- base.def %>% 
  dplyr::select(Estacion, LAT, LONG) %>% 
  distinct()

##################################
####Asignar información IDEAM#####
##################################

fincas.df <- fincas.geo %>% 
  mutate(dias.periodo=as.numeric(FECHA_COSECHA-FECHA_SIEMBRA)+1) %>% 
  filter(dias.periodo>0 & dias.periodo<240)

variables <- unique(base.def$Variable)

info.finca <- fincas.df %>% 
  st_drop_geometry()


info.modelos <- NULL
info.modelos.ll <- NULL

for(i in 1:nrow(fincas.df)){
  
  fecha.ini.p <- fincas.df$FECHA_SIEMBRA[i]-days(60)
  fecha.ini <- fincas.df$FECHA_SIEMBRA[i]
  fecha.fin <- fincas.df$FECHA_COSECHA[i]
  
  dias <- as.Date(seq(from=fecha.ini, to=fecha.fin, by="day"))
  dias.p <- as.Date(seq(from=fecha.ini.p, to=fecha.fin, by="day"))
  
  #Base con información del periodo de interés
  clima.periodo <- base.def %>% 
    filter(Fecha %in% dias, Variable!="PTPM_CON")
  
  clima.lluvia <- base.def %>% 
    filter(Fecha %in% dias.p, Variable=="PTPM_CON")
  
  #No de datos disponibles por variable en el periodo
  clima.temp <- clima.periodo %>% 
    group_by(Estacion, Variable) %>% 
    summarise(tot.dias=n()) %>% 
    pivot_wider(names_from = Variable, values_from = tot.dias) %>% 
    na.omit() 
  
  clima.temp.ll <-  clima.lluvia %>% 
    group_by(Estacion, Variable) %>% 
    summarise(tot.dias=n()) %>% 
    pivot_wider(names_from = Variable, values_from = tot.dias) %>% 
    na.omit() 
  
  #Georreferenciaicón de estaciones meteorológicas
  
  projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  clima.temp.sf <- clima.temp %>% 
    left_join(coords.est) %>% 
    st_as_sf(coords=c("LONG", "LAT"),
             crs=projcrs) %>% 
    mutate(across(TMN_CON:TMX_CON, ~ .x/as.numeric(fincas.df$dias.periodo[i]))) %>% 
    filter(across(TMN_CON:TMX_CON, ~.>0.6))
  
  clima.ll.sf <- clima.temp.ll %>% 
    left_join(coords.est) %>% 
    st_as_sf(coords=c("LONG", "LAT"),
             crs=projcrs) %>% 
    mutate(PTPM_CON=PTPM_CON/(as.numeric(fecha.fin-fecha.ini.p))) %>% 
    filter(PTPM_CON>0.6)
  
  
  distancias <- st_distance(fincas.df[i,], clima.temp.sf)
  dist.ll <- st_distance(fincas.df[i,], clima.ll.sf)
  
  y <-  data.frame("Fecha"=rep(dias, 2),
                   "Variable"=rep(c("TMN_CON","TMX_CON"), each=length(dias)))
  y.ll <- data.frame("Fecha"=rep(dias.p, 1), 
                     "Variable"=rep("PTPM_CON", length(dias.p)))
  
  temp <- y %>% 
    left_join(clima.periodo %>% 
                filter(Estacion==clima.temp.sf[which.min(distancias),]$Estacion) %>% 
                dplyr::select(-c(Estacion, LAT, LONG, Altitud)) %>% 
                pivot_wider(names_from = Variable, values_from = Valor) %>%
                arrange(Fecha) %>% 
                pivot_longer(TMN_CON:TMX_CON, names_to = "Variable", values_to = "Valor") %>% 
                arrange(Variable, Fecha),
              by=c("Fecha"="Fecha", "Variable"="Variable"))
  
  
  ###Imputar con el valor más cercano en el tiempo
  
  for(k in 1:nrow(temp)){
    
    if(is.na(temp$Valor[k])){
      
      j <- k 
      h <- k 
      
      if(j<nrow(temp)){
        while(is.na(temp$Valor[j]) & temp$Variable[j+1]==temp$Variable[j]){
          j <- j+1
          if(j==nrow(temp)){break}
        }} else{
          j <- k
        }
      
      
      if(h>1){
        while(is.na(temp$Valor[h]) & temp$Variable[h-1]==temp$Variable[h]){
          h <- h-1
          if(h==1){break}
        }} else{h <- k}
      
      disth <- abs(k-h)
      distj <- abs(k-j)
      
      if(disth==0){
        temp$Valor[k] <- temp$Valor[j]
      } else if (distj==0){
        temp$Valor[k] <- temp$Valor[h]
      } else if(distj !=0 & disth !=0){
        
        ind <- ifelse(which.min(c(distj,disth))==1, j, h)
        temp$Valor[k] <- temp$Valor[ind]
      }
      
    }
  }
  
  ###Tomar la base de percentiles
  
  percentiles <- round(quantile(seq(from=1, to=as.numeric(fincas.df$dias.periodo[i])), seq(from=0, to=1, by=0.02)))
  
  base.finca <- temp %>% 
    pivot_wider(id_cols=c(Fecha), names_from = Variable, values_from = Valor) %>% 
    mutate(Dia=seq(from=1, to=length(dias))) %>% 
    filter(Dia %in% percentiles)
  
  base.finca <- base.finca %>% 
    bind_cols(data.frame("FINCA"=rep(info.finca$FINCA[i], nrow(base.finca)),
                         "FECHA_SIEMBRA"=rep(info.finca$FECHA_SIEMBRA[i], nrow(base.finca)),
                         "FECHA_COSECHA"=rep(info.finca$FECHA_COSECHA[i], nrow(base.finca))
    )
    )
  
  ##Pegar la info de cada finca
  
  info.modelos <- info.modelos %>% 
    bind_rows(base.finca)
  
  
  rm(temp)
  
  ######Completar la información de la precipitación
  
  temp <- y.ll %>% 
    left_join(clima.lluvia %>% 
                filter(Estacion==clima.temp.ll[which.min(distancias),]$Estacion) %>% 
                dplyr::select(-c(Estacion, LAT, LONG, Altitud)) %>% 
                pivot_wider(names_from = Variable, values_from = Valor) %>%
                arrange(Fecha) %>% 
                pivot_longer(PTPM_CON, names_to = "Variable", values_to = "Valor") %>% 
                arrange(Variable, Fecha),
              by=c("Fecha"="Fecha", "Variable"="Variable"))
  
  
  ###Imputar con el valor más cercano en el tiempo
  
  for(k in 1:nrow(temp)){
    
    if(is.na(temp$Valor[k])){
      
      j <- k 
      h <- k 
      
      if(j<nrow(temp)){
        while(is.na(temp$Valor[j]) & temp$Variable[j+1]==temp$Variable[j]){
          j <- j+1
          if(j==nrow(temp)){break}
        }} else{
          j <- k
        }
      
      
      if(h>1){
        while(is.na(temp$Valor[h]) & temp$Variable[h-1]==temp$Variable[h]){
          h <- h-1
          if(h==1){break}
        }} else{h <- k}
      
      disth <- abs(k-h)
      distj <- abs(k-j)
      
      if(disth==0){
        temp$Valor[k] <- temp$Valor[j]
      } else if (distj==0){
        temp$Valor[k] <- temp$Valor[h]
      } else if(distj !=0 & disth !=0){
        
        ind <- ifelse(which.min(c(distj,disth))==1, j, h)
        temp$Valor[k] <- temp$Valor[ind]
      }
      
    }
  }
  
  ###Tomar la base de percentiles
  
  per.ll <- round(quantile(seq(from=1, to=as.numeric(fincas.df$dias.periodo[i])+60), seq(from=0, to=1, by=0.02)))
  
  base.finca.ll <- temp %>% 
    pivot_wider(id_cols=c(Fecha), names_from = Variable, values_from = Valor) %>% 
    mutate(Dia=seq(from=1, to=length(dias.p))) %>% 
    filter(Dia %in% percentiles)
  
  base.finca.ll <- base.finca.ll %>% 
    bind_cols(data.frame("FINCA"=rep(info.finca$FINCA[i], nrow(base.finca)),
                         "FECHA_SIEMBRA"=rep(info.finca$FECHA_SIEMBRA[i], nrow(base.finca)),
                         "FECHA_COSECHA"=rep(info.finca$FECHA_COSECHA[i], nrow(base.finca))
    )
    )
  
  ##Pegar la info de cada finca
  
  info.modelos.ll <- info.modelos.ll %>% 
    bind_rows(base.finca.ll)
  
  print(i)
  
}

base.modelo <- info.modelos %>% 
  pivot_longer(cols=TMN_CON:TMX_CON, names_to = "Variable", values_to = "Valor")

base.lluvia <- info.modelos.ll %>% 
  pivot_longer(cols=PTPM_CON, names_to = "Variable", values_to = "Valor")


base.modelo <- base.modelo %>% 
  bind_rows(base.lluvia)


cosechas <- base.modelo %>% 
  distinct(FINCA, FECHA_SIEMBRA, FECHA_COSECHA) %>% 
  group_by(FINCA) %>% 
  mutate("Cosecha"=seq(1:n()))

base.modelo <- base.modelo %>% 
  left_join(cosechas)

save(base.modelo, file="../Bases de datos/Procesadas/BaseModeloRelacionClima.RData")

