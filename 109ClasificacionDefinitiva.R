library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(leaflet)
library(lubridate)
library(mapview)
library(GWmodel)
library(RColorBrewer)
library(factoextra)
library(ppclust)
library(writexl)

######Cargar la base de proyecciones

load("../Bases de datos/Procesadas/BaseProyeccionesAleatorias.RData")

proyecciones <- proyecciones %>% 
  mutate(Estacion=rownames(proyecciones))

est.arroz <- st_read("../Resultados/capaEstacionesZonaArrocera",
                     layer="capaEstacionesZonaArrocera")

est.arroz <- est.arroz %>% 
  filter(Altitud<=1163)



###Carga de curvas mensuales

load("../Bases de datos/Procesadas/ImputacionTodasVariables.RData")

var.completas.mens <- var.completas

temp <- var.completas.mens %>%
  mutate(Valor_Def=case_when(!is.na(Valor) ~ Valor,
                             !is.na(var1.pred) ~ var1.pred,
                             TRUE ~ Valor)) %>%
  dplyr::select(Año, Mes, Anio_Mes, estaciones, Altitud, X, Y, Est_arroz, Variable, Valor_Def) %>%
  pivot_wider(names_from = Variable, values_from = Valor_Def)

summary(temp)

p999.temp <- temp %>%
  dplyr::select(TMN_P) %>%
  summarise(quantile(TMN_P, 0.999)) %>%
  as.numeric()

p001.temp <- temp %>%
  dplyr::select(TMN_P) %>%
  summarise(quantile(TMN_P, 0.001)) %>%
  as.numeric()

p995.temp.m <- temp %>%
  dplyr::select(TMX_P) %>%
  summarise(quantile(TMX_P, 0.995)) %>%
  as.numeric()

p003.temp.m <- temp %>%
  dplyr::select(TMX_P) %>%
  summarise(quantile(TMX_P, 0.003)) %>%
  as.numeric()

var.completas.mens <- var.completas.mens %>%
  mutate(Valor_Def=case_when(!is.na(Valor) ~ Valor,
                             !is.na(var1.pred) ~ var1.pred,
                             TRUE ~ Valor)) %>%
  mutate(Valor_Def=ifelse(Variable %in% c( "Max_R_L_Leve_1","Max_R_L_Moderada_1","Max_R_Lluvia_0","Max_R_Lluvia_1",
                                           "Min_R_L_Leve_1","Min_R_L_Moderada_1","Min_R_Lluvia_1","N_dias_ll",
                                           "N_dias_ll_L","N_dias_ll_M","N_dias_T_23","N_dias_T_34",
                                           "Prom_R_L_Leve_1","Prom_R_L_Moderada_1","Prom_R_Lluvia_0","Prom_R_Lluvia_1") & Valor_Def>31,30,Valor_Def),
         Valor_Def=ifelse(Variable %in% c( "Max_R_L_Leve_1","Max_R_L_Moderada_1","Max_R_Lluvia_0","Max_R_Lluvia_1",
                                           "Min_R_L_Leve_1","Min_R_L_Moderada_1","Min_R_Lluvia_1","N_dias_ll",
                                           "N_dias_ll_L","N_dias_ll_M","N_dias_T_23","N_dias_T_34",
                                           "Prom_R_L_Leve_1","Prom_R_L_Moderada_1","Prom_R_Lluvia_0","Prom_R_Lluvia_1") & Valor_Def<0,0,Valor_Def),
         Valor_Def=ifelse(Variable %in% c( "Max_R_L_Leve_1","Max_R_L_Moderada_1","Max_R_Lluvia_0","Max_R_Lluvia_1",
                                           "Min_R_L_Leve_1","Min_R_L_Moderada_1","Min_R_Lluvia_1","N_dias_ll",
                                           "N_dias_ll_L","N_dias_ll_M","N_dias_T_23","N_dias_T_34"),round(Valor_Def,0),Valor_Def),
         Valor_Def=ifelse(Variable %in% c("TMN_P") & Valor_Def>60, p999.temp, Valor_Def),
         Valor_Def=ifelse(Variable %in% c("TMN_P") & Valor_Def< -5, p001.temp, Valor_Def),
         Valor_Def=ifelse(Variable %in% c("TMX_P") & Valor_Def>60, p995.temp.m, Valor_Def),
         Valor_Def=ifelse(Variable %in% c("TMX_P") & Valor_Def< -5, p003.temp.m, Valor_Def),
  )

rm(temp)

##################

sf.estaciones <- est.arroz %>% 
  left_join(proyecciones) %>% 
  filter(!is.na(N_dias_ll))

base.clasificar <- sf.estaciones %>% 
  dplyr::select(N_dias_ll:Prom_R_Lluvia_1) %>% 
  st_drop_geometry()

#####################
###GWPA##############
#####################

# base.escalada <- proyecciones %>%
#   left_join(sf.estaciones %>%
#               st_drop_geometry() %>%
#               select(Estacion, Altitud), by=c("Estacion"="Estacion")) %>%
#   select(-Estacion) %>%
#   scale()

base.escalada <- scale(as.matrix(base.clasificar))

#Crear un sp de coordenadas

coords1 <- data.frame(st_coordinates(sf.estaciones))
d1s <- SpatialPointsDataFrame(coords1,as.data.frame(base.escalada))

pca.gw <- gwpca(d1s,vars=colnames(d1s@data),bw=1000000,k=10, scores=TRUE)

scores.pca <- data.frame(pca.gw$gwpca.scores[[1]][,1:4])

####Número de clusters 

K <- 4

res.fcm <- fcm(scores.pca, centers=K)

prob.clust <- as.data.frame(res.fcm$u) 

max.prob <- prob.clust %>% 
  rowwise() %>% 
  mutate(Prob=max(c_across('Cluster 1':'Cluster 4')),
         Clase=which.max(c_across('Cluster 1':'Cluster 4'))) %>% 
  bind_cols(sf.estaciones %>% 
              dplyr::select(Estacion) %>% 
              st_drop_geometry())


#####Mapa de clasificación

estaciones.fuzzy <-sf.estaciones %>% 
  left_join(max.prob) %>% 
  filter(!is.na(Clase))

clust1 <- estaciones.fuzzy %>% 
  filter(Clase==1)
pal1 <- colorNumeric("Greens",
                     domain=clust1$Prob)

clust2 <- estaciones.fuzzy %>% 
  filter(Clase==2)
pal2 <- colorNumeric("Oranges",
                     domain=clust2$Prob)

clust3 <- estaciones.fuzzy %>% 
  filter(Clase==3)
pal3 <- colorNumeric("Purples",
                     domain=clust3$Prob)

clust4 <- estaciones.fuzzy %>%
  filter(Clase==4)
pal4 <- colorNumeric("Blues",
                     domain=clust4$Prob)

#####

mapaFuzzy <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=clust1 %>%
                     st_transform(4326), radius=1, color=~pal1(Prob), group = "Clase 1") %>% 
  addCircleMarkers(data=clust2 %>%
                     st_transform(4326), radius=1, color=~pal2(Prob), group = "Clase 2") %>% 
  addCircleMarkers(data=clust3 %>%
                     st_transform(4326), radius=1, color=~pal3(Prob), group = "Clase 3") %>% 
  addLayersControl(overlayGroups = c("Clase 1", "Clase 2", "Clase 3", "Clase 4"),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addCircleMarkers(data=clust4 %>%
                     st_transform(4326), radius=1, color=~pal4(Prob), group = "Clase 4")  

# %>%
#   addCircleMarkers(data=clust4 %>%
#                      st_transform(4326), radius=1, color=~pal4(Prob))

mapshot(mapaFuzzy, url = paste0("../Resultados/MapasClasificacion//ClasificacionDef/Proyecciones_FuzzyGeo_3_1500.html"))

#########################
####Caracterización######
#########################

var.clases <-  var.completas.mens %>% 
  left_join(estaciones.fuzzy %>% 
              dplyr::select(Estacion, Clase), by=c("estaciones"="Estacion"))


variables <- c(unique(var.clases$Variable))

for(j in 1:length(variables)){
  
  base.g <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d"))
  # 
  # grafico <- base.g %>%
  #   ggplot(aes(x=as.factor(Fecha), y=Valor_Def, group=as.factor(estaciones), color=as.factor(Clase))) +
  #   geom_line() +
  #   scale_color_manual(values=c("#016339", "#DE8103", "#7400B6", "#006AB6")) +
  #   ggtitle(paste0("Curvas", variables[j])) +
  #   labs(color = "Clase", x="Mes", y="Valor")
  # 
  # ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/Curvas_Estaciones_FuzzyGeo_3", variables[j], ".jpeg"))
  # 
  # base.prom <- var.clases %>%
  #   filter(Variable==variables[j]) %>%
  #   mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>%
  #   group_by(Fecha, Clase) %>%
  #   summarise(Promedio=mean(Valor_Def))
  # 
  # grafico <- base.prom %>%
  #   ggplot( aes(x=as.factor(Fecha), y=Promedio, group=as.factor(Clase), color=as.factor(Clase))) +
  #   geom_line() +
  #   scale_color_manual(values=c("#016339", "#DE8103", "#7400B6", "#006AB6")) +
  #   ggtitle(paste0("Curvas promedio ", variables[j])) +
  #   labs(color = "Clase", x="Mes", y="Valor")
  # 
  # ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/CurvasPromedio_FuzzyGeo_3", variables[j], ".jpeg"))
  # 
  
  
  base.prom <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>%
    group_by(Mes, Clase) %>%
    summarise(Promedio=mean(Valor_Def))
  
  grafico <- base.prom %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(Clase), color=as.factor(Clase))) +
    geom_line() +
    scale_color_manual(values=c("#016339", "#DE8103", "#7400B6", "#006AB6")) +
    ggtitle(paste0("Curvas promedio ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/CurvasPromedioMes_FuzzyGeo_3", variables[j], ".jpeg"))
  
  base.per.mens <- base.g %>%
    filter(Variable==variables[j]) %>%
    group_by(estaciones, Mes) %>%
    summarise(Promedio=mean(Valor_Def), Clase=max(Clase))
  
  grafico <- base.per.mens %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(estaciones), color=as.factor(Clase))) +
    geom_line() +
    scale_color_manual(values=c("#016339", "#DE8103", "#7400B6", "#006AB6")) +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/CurvasPromedioMensual_FuzzyGeo_Estaciones_3", variables[j], ".jpeg"))
  
  
}

########################
###Altitud#############
#######################

altitud.clases <- var.clases %>% 
  group_by(estaciones) %>% 
  summarise(Altitud=max(Altitud), Clase=max(Clase)) %>% 
  group_by(Clase) %>% 
  summarise(Maxima=max(Altitud), Minima=min(Altitud), Mediana=median(Altitud), Promedio=mean(Altitud))

##########################################
####Abrir áreas geográficamente distantes#
###########################################

library(dbscan)

#----------------------
####Cluster 1
#----------------------

grupos1 <-  dbscan(clust1 %>% 
                     st_coordinates() %>% 
                     data.frame(), eps = 0.7, minPts = 5)

clust1.g <- clust1 %>% 
  bind_cols(data.frame("Clust.DBSCAN"=paste0("1_",grupos1$cluster))) %>% 
  filter(Clust.DBSCAN!="1_0")

pal1 <- colorFactor(c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"),
                    domain=clust1.g$Clust.DBSCAN)


mapaGrupos1 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=clust1.g %>%
                     st_transform(4326), radius=1, color=~pal1(Clust.DBSCAN))

#####Caracterización

carac1 <- clust1.g %>% 
  dplyr::select(Estacion, Clust.DBSCAN)

var.completas1 <-var.clases %>% 
  left_join(carac1, by=c("estaciones"="Estacion")) %>% 
  filter(!is.na(Clust.DBSCAN))

carac1 <- var.completas1 %>% 
  group_by(Mes, Variable, Clust.DBSCAN) %>% 
  summarise(Promedio=mean(Valor_Def)) 

for(j in 1:length(variables)){
  
  base.per.mens <- carac1 %>%
    filter(Variable==variables[j])
  
  grafico <- base.per.mens %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, color=as.factor(Clust.DBSCAN), group=as.factor(Clust.DBSCAN))) +
    geom_line() +
    scale_colour_brewer(palette="Set1") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Subclase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/Subzonas1", variables[j],".jpeg"))
  
  
}



#----------------------
####Cluster 2
#----------------------

grupos2 <-  dbscan(clust2 %>% 
                     st_coordinates() %>% 
                     data.frame(), eps = 0.5, minPts = 5)

clust2.g <- clust2 %>% 
  bind_cols(data.frame("Clust.DBSCAN"=paste0("2_",grupos2$cluster))) %>% 
  filter(Clust.DBSCAN!="2_0")

pal2 <- colorFactor("Set2",
                    domain=clust2.g$Clust.DBSCAN)

mapaGrupos2 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=clust2.g %>%
                     st_transform(4326), radius=1, color=~pal2(Clust.DBSCAN))

#####Caracterización

carac2 <- clust2.g %>% 
  dplyr::select(Estacion, Clust.DBSCAN)

var.completas2 <- var.clases %>% 
  left_join(carac2, by=c("estaciones"="Estacion")) %>% 
  filter(!is.na(Clust.DBSCAN))

carac2 <- var.completas2 %>% 
  group_by(Mes, Variable, Clust.DBSCAN) %>% 
  summarise(Promedio=mean(Valor_Def)) 

for(j in 1:length(variables)){
  
  base.per.mens <- carac2 %>%
    filter(Variable==variables[j])
  
  grafico <- base.per.mens %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, color=as.factor(Clust.DBSCAN), group=as.factor(Clust.DBSCAN))) +
    geom_line() +
    scale_colour_brewer(palette="Set1") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Subclase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/Subzonas2", variables[j],".jpeg"))
  
  
}

#--------------
####Cluster 3
#--------------

grupos3 <-  dbscan(clust3 %>% 
                     st_coordinates() %>% 
                     data.frame(), eps = 0.7, minPts = 10)

clust3.g <- clust3 %>% 
  bind_cols(data.frame("Clust.DBSCAN"=paste0("3_",grupos3$cluster))) %>% 
  filter(Clust.DBSCAN!="3_0")

pal3 <- colorFactor("Set2",
                    domain=clust3.g$Clust.DBSCAN)

mapaGrupos3 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=clust3.g %>%
                     st_transform(4326), radius=1, color=~pal3(Clust.DBSCAN))

#####Caracterización

carac3 <- clust3.g %>% 
  dplyr::select(Estacion, Clust.DBSCAN)

var.completas3 <- var.clases %>% 
  left_join(carac3, by=c("estaciones"="Estacion")) %>% 
  filter(!is.na(Clust.DBSCAN))

carac3 <- var.completas3 %>% 
  group_by(Mes, Variable, Clust.DBSCAN) %>% 
  summarise(Promedio=mean(Valor_Def)) 

for(j in 1:length(variables)){
  
  base.per.mens <- carac3 %>%
    filter(Variable==variables[j])
  
  grafico <- base.per.mens %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, color=as.factor(Clust.DBSCAN), group=as.factor(Clust.DBSCAN))) +
    geom_line() +
    scale_colour_brewer(palette="Set1") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Subclase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/Subzonas3", variables[j],".jpeg"))
  
  
}

#--------------
####Cluster 4
#--------------

grupos4 <-  dbscan(clust4 %>% 
                     st_coordinates() %>% 
                     data.frame(), eps = 0.7, minPts = 5)

clust4.g <- clust4 %>% 
  bind_cols(data.frame("Clust.DBSCAN"=paste0("4_",grupos4$cluster))) %>% 
  filter(Clust.DBSCAN!="4_0")

pal4 <- colorFactor(c("#66c2a5", "#fc8d62"),
                    domain=clust4.g$Clust.DBSCAN)

mapaGrupos4 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=clust4.g %>%
                     st_transform(4326), radius=1, color=~pal4(Clust.DBSCAN))

#####Caracterización

carac4 <- clust4.g %>%
  dplyr::select(Estacion, Clust.DBSCAN)

var.completas4 <- var.clases %>%
  left_join(carac4, by=c("estaciones"="Estacion")) %>%
  filter(!is.na(Clust.DBSCAN))

carac4 <- var.completas4 %>%
  group_by(Mes, Variable, Clust.DBSCAN) %>%
  summarise(Promedio=mean(Valor_Def))

for(j in 1:length(variables)){
  
  base.per.mens <- carac4 %>%
    filter(Variable==variables[j])
  
  grafico <- base.per.mens %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, color=as.factor(Clust.DBSCAN), group=as.factor(Clust.DBSCAN))) +
    geom_line() +
    scale_colour_brewer(palette="Set2") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Subclase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/Subzonas4", variables[j],".jpeg"))
  
  
}

######Base definitiva

grupos.def <- clust1.g %>% 
  bind_rows(clust2.g) %>% 
  bind_rows(clust3.g) 

######Creación de polígonos

###Cluster1

df1 <- clust1.g %>% 
  st_coordinates() %>%
  data.frame() %>% 
  bind_cols(clust1.g %>% 
              dplyr::select(Clust.DBSCAN)) 

poli1 <- df1 %>% 
  group_by(Clust.DBSCAN) %>% 
  slice(chull(X,Y))

p1 <- df1 %>%
  ggplot(aes(x = X,
             y = Y))+
  geom_point() +
  geom_polygon(data = poli1,
               aes(fill = Clust.DBSCAN,
                   colour = Clust.DBSCAN),
               alpha = 0.3,
               show.legend = FALSE)

poli1 <- poli1 %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  group_by(Clust.DBSCAN) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

###Cluster 2

df2 <- clust2.g %>% 
  st_coordinates() %>%
  data.frame() %>% 
  bind_cols(clust2.g %>% 
              dplyr::select(Clust.DBSCAN)) 

poli2 <- df2 %>%
  group_by(Clust.DBSCAN) %>% 
  slice(chull(X,Y))

p2 <- df2 %>%
  ggplot(aes(x = X,
             y = Y))+
  geom_point() +
  geom_polygon(data = poli2,
               aes(fill = Clust.DBSCAN,
                   colour = Clust.DBSCAN),
               alpha = 0.3,
               show.legend = FALSE)

poli2 <- poli2 %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  group_by(Clust.DBSCAN) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

###Cluster 3

df3 <- clust3.g %>% 
  st_coordinates() %>%
  data.frame() %>% 
  bind_cols(clust3.g %>% 
              dplyr::select(Clust.DBSCAN)
  )

poli3 <- df3 %>%
  group_by(Clust.DBSCAN) %>% 
  slice(chull(X,Y))


p3 <- df3 %>%
  ggplot(aes(x = X,
             y = Y))+
  geom_point() +
  geom_polygon(data = poli3,
               aes(fill = Clust.DBSCAN,
                   colour = Clust.DBSCAN),
               alpha = 0.3,
               show.legend = FALSE)

poli3 <- poli3 %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  group_by(Clust.DBSCAN) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

###Cluster 4

df4 <- clust4.g %>%
  st_coordinates() %>%
  data.frame() %>%
  bind_cols(clust4.g %>%
              dplyr::select(Clust.DBSCAN)
  )

poli4 <- df4 %>%
  group_by(Clust.DBSCAN) %>%
  slice(chull(X,Y))

p4 <- df4 %>%
  ggplot(aes(x = X,
             y = Y))+
  geom_point() +
  geom_polygon(data = poli4,
               aes(fill = Clust.DBSCAN,
                   colour = Clust.DBSCAN),
               alpha = 0.3,
               show.legend = FALSE)

poli4 <- poli4 %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  group_by(Clust.DBSCAN) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")


save(poli1, poli2, poli3, poli4, 
     file="../Resultados/MapasClasificacion/ClasificacionDef/PoligonosZonasClimaticas.RData")


###################################
##Mapa de estaciones y polígonos##
##################################

pal1 <- colorNumeric("Greens",
                     domain=clust1$Prob)

pal2 <- colorNumeric("Oranges",
                     domain=clust2$Prob)

pal3 <- colorNumeric("Purples",
                     domain=clust3$Prob)

pal4 <- colorNumeric("Blues",
                     domain=clust4$Prob)


mapaZonas <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=clust1.g %>%
                     st_transform(4326), radius=1, color=~pal1(Prob)) %>%
  addPolygons(data=poli1 %>%
                st_transform(4326), fillColor = "#016339", color="#016339",
              weight=1, opacity=0.3) %>%
  addCircleMarkers(data=clust2.g %>%
                     st_transform(4326), radius=1, color=~pal2(Prob)) %>%
  addPolygons(data=poli2 %>%
                st_transform(4326), fillColor = "#DE8103", color="#DE8103",
              weight=1, opacity=0.3) %>%
  addCircleMarkers(data=clust3.g %>%
                     st_transform(4326), radius=1, color=~pal3(Prob)) %>% 
  addCircleMarkers(data=clust4.g %>%
                     st_transform(4326), radius=1, color=~pal4(Prob)) %>% 
  addPolygons(data=poli3 %>%
                st_transform(4326), fillColor = "#7400B6", color="#7400B6",
              weight=1, opacity=0.3) %>% 
  addPolygons(data=poli4 %>%
                st_transform(4326), fillColor = "#0E6BBA", color="#0E6BBA",
              weight=1, opacity=0.3)

##########

  tabla.final <- clust1.g %>%
  bind_rows(clust2.g) %>% 
  bind_rows(clust3.g) %>% 
  bind_rows(clust4.g)

tabla.final <- tabla.final %>% 
  mutate("LONG"=st_coordinates(tabla.final)[1], "LAT"=st_coordinates(tabla.final)[2]) %>% 
  dplyr::select(Estacion, Nombre, 'Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                Prob, Clase, Clust.DBSCAN, LONG, LAT) %>% 
  rename_with(~gsub(" ", "", .x, fixed = TRUE))



write_xlsx(tabla.final, "../Resultados/MapasClasificacion/ClasificacionDef/ClasificacionEstaciones.xlsx")

st_write(tabla.final, dsn="../Resultados/MapasClasificacion/ClasificacionDef/ClasificacionEstaciones_Shp/
         ClasificacionEstaciones_SF.shp",
         layer="ClasificacionEstaciones_SF.shp")
