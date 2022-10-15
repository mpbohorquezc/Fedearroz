library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(tidyverse)
library(fda)
library(leaflet)
library(mapview)

####Carga de base

load("../Bases de datos/Procesadas/baseIdeamGeorreferenciada.RData")

baseActiva <- baseActiva %>% 
  filter(Altitud<1163)

base.pt <- baseActiva %>%
  filter(Variable=="PTPM_CON") %>%
  mutate(Valor=case_when(
    Valor>350 ~ 350,
    TRUE ~ Valor
  ),
    Dia_Ll=case_when(
    Valor>=5 ~ 1,
    TRUE ~ 0
  ),
  Dia_Ll_L=case_when(
    Valor>=5 & Valor<=20 ~ 1,
    TRUE ~ 0
  ),
  Dia_Ll_M=case_when(
    Valor>20 & Valor<=350 ~ 1,
    TRUE ~ 0
  )
    )

rm(baseActiva)
gc()

####Crear las rachas

rachas <- base.pt %>%
  group_by(Año, Mes, Estacion) %>%
  mutate(lagged=lag(Dia_Ll),
         lagged_L=lag(Dia_Ll_L),
         lagged_M=lag(Dia_Ll_M)) %>%
  ungroup()

rachas <- rachas %>%
  mutate(start=(Dia_Ll != lagged),
         start_L=(Dia_Ll_L != lagged_L),
         start_M=(Dia_Ll_M != lagged_M)
         )

rachas$start <- case_when(is.na(rachas$start) ~ TRUE,
                          TRUE ~ rachas$start)
rachas$start_L <- case_when(is.na(rachas$start_L) ~ TRUE,
                          TRUE ~ rachas$start_L)
rachas$start_M <- case_when(is.na(rachas$start_M) ~ TRUE,
                            TRUE ~ rachas$start_M)



rachas.t <- rachas %>%
  group_by(Año, Mes, Estacion) %>%
  mutate(racha_id=cumsum(start),
         racha_id_L=cumsum(start_L),
         racha_id_M=cumsum(start_M)) %>%
  ungroup()

r.ori <- rachas.t %>%
  group_by(Año, Mes, Estacion, racha_id) %>%
  summarise(Longitud=n(), T_racha=max(Dia_Ll)) %>% 
  mutate(Tipo="Lluvia") 

r.L <- rachas.t %>%
  group_by(Año, Mes, Estacion, racha_id_L) %>% 
  summarise(Longitud=n(), T_racha=max(Dia_Ll_L)) %>% 
  mutate(Tipo="L_Leve") 

r.M <- rachas.t %>%
  group_by(Año, Mes, Estacion, racha_id_M) %>% 
  summarise(Longitud=n(), T_racha=max(Dia_Ll_M)) %>% 
  mutate(Tipo="L_Moderada") 

rachas <- r.ori %>% 
  bind_rows(r.L) %>% 
  bind_rows(r.M)

#######Guardar base de rachas

rachas.imp <- rachas %>%
  filter(Longitud>1)

rachas.imp <- rachas.imp %>% 
  dplyr::select(Año, Mes, Estacion, Longitud, T_racha, Tipo)

save(rachas.imp, file="../Bases de datos/Procesadas/rachasPrecipitacion.RData")

gc()

#################

load("../Bases de datos/Procesadas/rachasPrecipitacion.RData")

est.arroz <- st_read("../Resultados/capaEstacionesZonaArrocera",
                     layer="capaEstacionesZonaArrocera") %>%
  dplyr::select(Estacion) %>%
  mutate(Est_arroz=1)

###Distribuciones de las rachas

frec.rachas <-  ranchas.imp %>%
  group_by(T_racha, Longitud) %>%
  summarise(Total=n())

gdensidad <-  ggplot(frec.rachas, aes(x=Longitud, y=Total, fill=as.factor(T_racha))) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Longitud de la racha") +
  ylab("Número de rachas") +
  scale_fill_discrete(name = "Tipo", labels = c("Seco", "Lluvioso"))

####Estadísticas de rachas

  max.min <- ranchas.imp %>% 
    group_by(Año, Mes, Estacion, T_racha) %>% 
    summarise(Max_l=max(Longitud), Min_l=min(Longitud)) %>% 
    ungroup() %>% 
    mutate(Fecha=as.Date(paste0(Año,"-",Mes, "-01"))) 
  
  base.g <- max.min %>% 
    group_by(Max_l, T_racha) %>% 
    summarise(Total=n())
  
  gdensidad <-  ggplot(base.g, aes(x=Max_l, y=Total, fill=as.factor(T_racha))) +
    geom_bar(position="dodge", stat="identity") +
    xlab("Longitud de la racha más larga del mes") +
    ylab("Número de rachas") +
    scale_fill_discrete(name = "Tipo", labels = c("Seco", "Lluvioso"))
  
  ##Mínimo
  
  base.g <- max.min %>% 
    group_by(Min_l, T_racha) %>% 
    summarise(Total=n())
  
  gdensidad <-  ggplot(base.g, aes(x=Min_l, y=Total, fill=as.factor(T_racha))) +
    geom_bar(position="dodge", stat="identity") +
    xlab("Longitud de la racha más corta del mes") +
    ylab("Número de rachas") +
    scale_fill_discrete(name = "Tipo", labels = c("Seco", "Lluvioso"))
  
####Calcular el promedio
  
  promedio <- ranchas.imp %>% 
    left_join(est.arroz %>% 
                st_drop_geometry()) %>% 
    group_by(Año, Mes, T_racha, Est_arroz) %>% 
    summarise(Promedio=mean(Longitud), Est_arroz=max(Est_arroz))
  
  ###Lluvia

  p.grafico.l <-  promedio %>% 
    filter(T_racha==1) %>% 
    mutate(Fecha=as.Date(paste0(Año,"-",Mes, "-01"))) 
  
  grafico <- p.grafico.l %>%
    ggplot( aes(x=Fecha, y=Promedio, group=as.factor(Est_arroz), color=as.factor(Est_arroz))) +
    geom_line() +
    ggtitle("Rachas de lluvia") +
    xlab("Longitud promedio de las rachas por zona") +
    ylab("Longitud promedio") +
    scale_color_discrete(name = "Zona", labels = c("Arrocera", "No arrocera"))
  
  ##Seco
  
  p.grafico.l <-  promedio %>% 
    filter(T_racha==0) %>% 
    mutate(Fecha=as.Date(paste0(Año,"-",Mes, "-01"))) 
  
  grafico <- p.grafico.l %>%
    ggplot( aes(x=Fecha, y=Promedio, group=as.factor(Est_arroz), color=as.factor(Est_arroz))) +
    geom_line() +
    ggtitle("Rachas de días secos") +
    xlab("Longitud promedio de las rachas por zona") +
    ylab("Longitud promedio") +
    scale_color_discrete(name = "Zona", labels = c("Arrocera", "No arrocera"))
  
###Promedios mensuales
  
  prom.mens <- ranchas.imp %>% 
    left_join(est.arroz %>% 
                st_drop_geometry()) %>% 
    group_by(Mes, T_racha, Est_arroz) %>% 
    summarise(Promedio=mean(Longitud)) 
  
  p.grafico.l <-  prom.mens %>% 
    filter(T_racha==1)
  
  grafico <- p.grafico.l %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(Est_arroz), color=as.factor(Est_arroz))) +
    geom_line() +
    ggtitle("Rachas de días lluviosos") +
    xlab("Longitud promedio de las rachas por zona") +
    ylab("Mes") +
    scale_color_discrete(name = "Zona", labels = c("Arrocera", "No arrocera"))
  
  p.grafico.l <-  prom.mens %>% 
    filter(T_racha==0)
  
  grafico <- p.grafico.l %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(Est_arroz), color=as.factor(Est_arroz))) +
    geom_line() +
    ggtitle("Rachas de días secos") +
    xlab("Longitud promedio de las rachas por zona") +
    ylab("Mes") +
    scale_color_discrete(name = "Zona", labels = c("Arrocera", "No arrocera"))
  
  ###Mapas mensuales promedio racha 
  
  base.mapa <- ranchas.imp %>% 
    left_join(est.arroz %>% 
                st_drop_geometry()) %>% 
    group_by(Mes, T_racha, Estacion, Est_arroz) %>% 
    summarise(Promedio=mean(Longitud)) %>% 
    ungroup() %>% 
    mutate(Est_arroz=case_when(is.na(Est_arroz) ~ 0,
                               TRUE ~ Est_arroz))
  
  estaciones <- st_read("../Resultados/capaEstacionesIDEAM",
                        layer="capaEstacionesIDEAM") 
  
  
  for(i in 1:12){
    
    base.mes.l <- base.mapa %>% 
      filter(Mes==i, T_racha==1)
      
    est.mapa <- estaciones %>% 
      dplyr::select(Estacion) %>% 
      left_join(base.mes.l, by=c("Estacion"="Estacion")) %>% 
      filter(!is.na(Promedio)) %>% 
      mutate(Fig=case_when(Est_arroz==1 ~ "arroz",
                           TRUE ~ "n.arroz"))

    paleta <- pal <- colorNumeric(
      palette = "PuBu",
      domain = est.mapa$Promedio
    )
    
    iconos <- iconList(
      arroz = makeIcon(
        iconUrl ="https://img.icons8.com/plasticine/16/000000/filled-circle.png",
        iconWidth = 6, iconHeight = 6,
        iconAnchorX = 6, iconAnchorY = 6
      ),
      n.arroz = makeIcon(
        iconUrl = "https://img.icons8.com/ios/16/000000/filled-circle.png",
        iconWidth = 6, iconHeight = 6,
        iconAnchorX = 6, iconAnchorY = 6
      )
      )
   mapa.l <-  leaflet(est.mapa) %>%
     addProviderTiles(providers$CartoDB.Positron) %>% 
     setView(-72.958,4.095,6) %>% 
      addCircleMarkers(
        radius = 6,
        color = ~paleta(Promedio),
        stroke = FALSE, fillOpacity = 0.7
      ) %>% 
     addMarkers(icon=~iconos[Fig], group="Zonas") %>% 
     addLayersControl(overlayGroups = c("Zonas"),
                      options = layersControlOptions(collapsed = FALSE)) %>% 
     addLegend("bottomright", pal=paleta,
               values =~Promedio,
               title = paste0("Promedio mes ",i),
               opacity = 1
     )
   
   mapshot(mapa.l, url = paste0("../Resultados/rachasLluvia_",i,".html"))
   
   ###mapa rachas días secos
    
   base.mes.l <- base.mapa %>% 
     filter(Mes==i, T_racha==0)
   
   est.mapa <- estaciones %>% 
     dplyr::select(Estacion) %>% 
     left_join(base.mes.l, by=c("Estacion"="Estacion")) %>% 
     filter(!is.na(Promedio)) %>% 
     mutate(Fig=case_when(Est_arroz==1 ~ "arroz",
                          TRUE ~ "n.arroz"))
   
   paleta <- pal <- colorNumeric(
     palette = "YlOrRd",
     domain = est.mapa$Promedio
   )
   
   iconos <- iconList(
     arroz = makeIcon(
       iconUrl ="https://img.icons8.com/plasticine/16/000000/filled-circle.png",
       iconWidth = 6, iconHeight = 6,
       iconAnchorX = 6, iconAnchorY = 6
     ),
     n.arroz = makeIcon(
       iconUrl = "https://img.icons8.com/ios/16/000000/filled-circle.png",
       iconWidth = 6, iconHeight = 6,
       iconAnchorX = 6, iconAnchorY = 6
     )
   )
   
   mapa.s <-  leaflet(est.mapa) %>%
     addProviderTiles(providers$CartoDB.Positron) %>% 
     setView(-72.958,4.095,6) %>% 
     addCircleMarkers(
       radius = 6,
       color = ~paleta(Promedio),
       stroke = FALSE, fillOpacity = 0.7
     ) %>% 
     addMarkers(icon=~iconos[Fig], group="Zonas") %>% 
     addLayersControl(overlayGroups = c("Zonas"),
                      options = layersControlOptions(collapsed = FALSE)) %>% 
     addLegend("bottomright", pal=paleta,
               values =~Promedio,
               title = paste0("Promedio mes ",i),
               opacity = 1
     )
   
   mapshot(mapa.s, url = paste0("../Resultados/rachasSeco_",i,".html"))
   
   
  }
  