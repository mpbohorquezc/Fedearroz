library(dplyr)
library(tidyr)
library(factoextra)
library(cluster)
library(FactoMineR)
library(sf)
library(ggplot2)
library(leaflet)
library(lubridate)
library(mapview)
library(ClustGeo)
library(robustbase)
library(mvoutlier)
library(GWmodel)
library(RColorBrewer)
library(ppclust)

###Cargar base de proyecciones

load("../Bases de datos/Procesadas/BaseProyeccionesAleatorias.RData")

proyecciones <- proyecciones %>% 
  mutate(Estacion=rownames(proyecciones))

est.arroz <- st_read("../Resultados/capaEstacionesZonaArrocera",
                     layer="capaEstacionesZonaArrocera")

base.clasificar <- proyecciones %>% 
  dplyr::select(-Estacion)

###Carga de curvas mensuales
load("../Bases de datos/Procesadas/ImputacionSeisVariables.RData")
base.6var <- var.completas

load("../Bases de datos/Procesadas/ImputacionCuatroVariables.RData")
base.4var <- var.completas

rm(var.completas)

var.completas.mens <- base.6var %>% 
  bind_rows(base.4var)

rm(base.6var, base.4var)

##################
####Clara#########
##################

n.clust <- fviz_nbclust(base.clasificar, FUN=clara)

##Entre 2 y 3 grupos

clasificacion <- clara(base.clasificar, k=3, samples = 50, pamLike = TRUE,correct.d=TRUE)

clases <- data.frame("Clases"=clasificacion$clustering,
                     "Estacion"=names(clasificacion$clustering))

##mapa de la clasificación


sf.clases <- est.arroz %>% 
  left_join(clases, by=c("Estacion"="Estacion")) %>% 
  filter(!is.na(Clases))

paleta <- colorFactor("Set2",
                      domain=as.factor(sf.clases$Clases))

mapaClases <-  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=sf.clases %>%
                     st_transform(4326), radius=1, color=~paleta(Clases)) %>% 
  addLegend("bottomright", pal = paleta, values = sf.clases$Clases, opacity = 1)

mapshot(mapaClases, url = paste0("../Resultados/MapasClasificacion/Proyecciones_Clara.html"))

###Caracterización basada en proyecciones

caracteris <- proyecciones %>% 
  left_join(clases)

prom.proyecciones <- caracteris %>% 
  pivot_longer(cols = N_dias_ll:Prom_R_1, names_to = "Variable", values_to = "Proyeccion") %>% 
  group_by(Variable, Clases) %>% 
  summarise(Promedio=mean(Proyeccion))

# grafico.prom <- ggplot(prom.proyecciones, aes(x=Variable, y=Promedio, fill=as.factor(Clases))) +
#   geom_bar(position="dodge", stat="identity") 


#####Comparación de curvas mensuales

var.completas <- var.completas.mens %>% 
  mutate(Valor_Def=case_when(!is.na(Valor) ~ Valor, 
                             !is.na(var1.pred) ~ var1.pred,
                             TRUE ~ Valor)) %>% 
  left_join(clases, by=c("estaciones"="Estacion"))

variables <- unique(var.completas$Variable)

for(j in 1:length(variables)){

base.g <- var.completas %>%
  filter(Variable==variables[j]) %>%
  mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d"))

grafico <- base.g %>%
  ggplot( aes(x=as.factor(Fecha), y=Valor_Def, group=as.factor(estaciones), color=as.factor(Clases))) +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  ggtitle(paste0("Curvas ", variables[j])) +
  labs(color = "Clase", x="Mes", y="Valor")

ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/Curvas_Proyecciones_Clara_Estaciones", variables[j], ".jpeg"))

base.per.mens <- base.g %>%
  filter(Variable==variables[j]) %>%
  group_by(estaciones, Mes) %>%
  summarise(Promedio=mean(Valor_Def), Clases=max(Clases))

grafico <- base.per.mens %>%
  ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(estaciones), color=as.factor(Clases))) +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  ggtitle(paste0("Curvas ", variables[j])) +
  labs(color = "Clase", x="Mes", y="Valor")

ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedioMensual_Proyecciones_Clara_Estaciones", variables[j], ".jpeg"))

base.prom <- var.completas %>%
  filter(Variable==variables[j]) %>%
  mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>%
  group_by(Fecha, Clases) %>%
  summarise(Promedio=mean(Valor_Def))

grafico <- base.prom %>%
  ggplot( aes(x=as.factor(Fecha), y=Promedio, group=as.factor(Clases), color=as.factor(Clases))) +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  ggtitle(paste0("Curvas promedio ", variables[j])) +
  labs(color = "Clase", x="Mes", y="Valor")

ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedio_Proyecciones_Clara ", variables[j], ".jpeg"))

base.prom.mes <- var.completas %>% 
  filter(Variable==variables[j]) %>% 
  group_by(Mes, Clases) %>% 
  summarise(Promedio=mean(Valor_Def))

grafico <- base.prom.mes %>%
  ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(Clases), color=as.factor(Clases))) +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  ggtitle(paste0("Curvas promedio ", variables[j])) +
  labs(color = "Clase", x="Mes", y="Valor")

ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedioMensual_Proyecciones_Clara", variables[j], ".jpeg"))


}

########################
####Clara con altitud###
########################

base.alt <- proyecciones %>% 
  left_join(est.arroz %>% 
              st_drop_geometry() %>% 
              select(Estacion, Altitud)) 

n.clust <- fviz_nbclust(base.alt %>% 
                          select(-Estacion), FUN=clara)

clasificacion <- clara(base.alt, k=3, samples = 50, pamLike = TRUE,correct.d=TRUE)

clases <- data.frame("Clases"=clasificacion$clustering,
                     "Estacion"=base.alt$Estacion)

##mapa de la clasificación

sf.clases <- est.arroz %>% 
  left_join(clases, by=c("Estacion"="Estacion")) %>% 
  filter(!is.na(Clases))

paleta <- colorFactor("Set2",
                      domain=as.factor(sf.clases$Clases))

mapaClases <-  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=sf.clases %>%
                     st_transform(4326), radius=1, color=~paleta(Clases)) %>% 
  addLegend("bottomright", pal = paleta, values = sf.clases$Clases, opacity = 1)

mapshot(mapaClases, url = paste0("../Resultados/MapasClasificacion/ProyeccionesAltitud_Clara.html"))

###Gráficos de caracterización

variables <- c(unique(var.completas$Variable), "Altitud")

for(j in 1:length(variables)){
  
  base.g <- var.completas %>% 
    filter(Variable==variables[j]) %>% 
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d"))
  
  grafico <- base.g %>%
    ggplot( aes(x=as.factor(Fecha), y=Valor_Def, group=as.factor(estaciones), color=as.factor(Clases))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/Curvas_Proyecciones_ClaraAltitud", variables[j], ".jpeg"))
  
  
  base.prom <- var.completas %>% 
    filter(Variable==variables[j]) %>% 
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>% 
    group_by(Fecha, Clases) %>% 
    summarise(Promedio=mean(Valor_Def))
  
  grafico <- base.prom %>%
    ggplot( aes(x=as.factor(Fecha), y=Promedio, group=as.factor(Clases), color=as.factor(Clases))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas promedio ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedio_Proyecciones_ClaraAltitud", variables[j], ".jpeg"))
  
}

grafico <- base.alt %>%
  left_join(clases) %>% 
   ggplot(aes(x=Altitud, y= ..scaled.., color=as.factor(Clases))) + 
  geom_density() +
  scale_color_brewer(palette = "Set2") +
  labs(color="Clase")

ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/Altitud_Proyecciones_ClaraAltitud", variables[j], ".jpeg"))


####################
###Jerárquico#######
####################

n.clust <- fviz_nbclust(base.clasificar, FUN=hcut)

distance_mat <- dist(base.clasificar, method = 'euclidean')
distance_mat
  
set.seed(240)  # Setting seed
Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl  
plot(Hierar_cl)

fit <- cutree(Hierar_cl, k = 3)

clases <- data.frame("Clases"=fit,
                     "Estacion"=base.alt$Estacion)

sf.clases <- est.arroz %>% 
  left_join(clases, by=c("Estacion"="Estacion")) %>% 
  filter(!is.na(Clases))

paleta <- colorFactor("Set2",
                      domain=as.factor(sf.clases$Clases))

mapaClases <-  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=sf.clases %>%
                     st_transform(4326), radius=1, color=~paleta(Clases)) %>% 
  addLegend("bottomright", pal = paleta, values = sf.clases$Clases, opacity = 1)

mapshot(mapaClases, url = paste0("../Resultados/MapasClasificacion/Proyecciones_Jerarquico.html"))

#########################
###cluster espacial######
#########################

sf.estaciones <- est.arroz %>% 
  left_join(proyecciones) %>% 
  filter(!is.na(N_dias_ll))

#Distancia entre estaciones
D1 <- st_distance(sf.estaciones, sf.estaciones)
D1 <- as.dist(D1)

#Distancia entre variables
D0 <- dist(base.clasificar)

tree <- hclustgeo(D0)

plot(tree,hang = -1, label = FALSE, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

P5 <- cutree(tree,5)

range.alpha <- seq(0,1,0.1)
K <- 5

cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q

plot(cr)

tree <- hclustgeo(D0,D1,alpha=0.4)
plot(tree)
P5bis <- cutree(tree,5)

base.clases <- sf.estaciones %>% 
  bind_cols(data.frame("Clase.Esp"=P5bis))

####Mapa de las clases

paleta <- colorFactor("Set2",
                      domain=as.factor(base.clases$Clase.Esp))

mapaClases <-  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=base.clases %>%
                     st_transform(4326), radius=1, color=~paleta(Clase.Esp)) %>% 
  addLegend("bottomright", pal = paleta, values = base.clases$Clase.Esp, opacity = 1)

mapshot(mapaClases, url = paste0("../Resultados/MapasClasificacion/Proyecciones_HclustEspacial.html"))


#####Caracterización con curvas mensuales

var.clases <-  var.completas.mens %>% 
  mutate(Valor_Def=case_when(!is.na(Valor) ~ Valor, 
                             !is.na(var1.pred) ~ var1.pred,
                             TRUE ~ Valor)) %>% 
  left_join(base.clases %>% 
              select(Estacion, Clase.Esp), by=c("estaciones"="Estacion"))


variables <- c(unique(var.clases$Variable))

for(j in 1:length(variables)){
  
  base.g <- var.clases %>% 
    filter(Variable==variables[j]) %>% 
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d"))
  
  grafico <- base.g %>%
    ggplot( aes(x=as.factor(Fecha), y=Valor_Def, group=as.factor(estaciones), color=as.factor(Clase.Esp))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/Curvas_Proyecciones_HclustEspacial", variables[j], ".jpeg"))
  
  base.prom <- var.clases %>% 
    filter(Variable==variables[j]) %>% 
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>% 
    group_by(Fecha, Clase.Esp) %>% 
    summarise(Promedio=mean(Valor_Def))
  
  grafico <- base.prom %>%
    ggplot( aes(x=as.factor(Fecha), y=Promedio, group=as.factor(Clase.Esp), color=as.factor(Clase.Esp))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas promedio ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedio_Proyecciones_HclustEspacial", variables[j], ".jpeg"))
  
  
  base.per.mens <- base.g %>%
    filter(Variable==variables[j]) %>%
    group_by(estaciones, Mes) %>%
    summarise(Promedio=mean(Valor_Def), Clases=max(Clases))
  
  grafico <- base.per.mens %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(estaciones), color=as.factor(Clases))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedioMensual_Proyecciones_Clara_Estaciones", variables[j], ".jpeg"))
  
  
}


#################################
###Cluster espacial con altitud##
#################################

###Se calcula distancia con altitud

D0 <- dist(base.alt %>% 
             select(-Estacion))

tree <- hclustgeo(D0)

range.alpha <- seq(0,1,0.1)
K <- 4

cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Qnorm

plot(cr)

tree <- hclustgeo(D0,D1,alpha=1)
plot(tree)
P5bis <- cutree(tree,K)

base.clases <- sf.estaciones %>% 
  bind_cols(data.frame("Clase.Esp"=P5bis))

####Mapa de las clases

paleta <- colorFactor("Set2",
                      domain=as.factor(base.clases$Clase.Esp))

mapaClases <-  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=base.clases %>%
                     st_transform(4326), radius=1, color=~paleta(Clase.Esp)) %>% 
  addLegend("bottomright", pal = paleta, values = base.clases$Clase.Esp, opacity = 1)

mapshot(mapaClases, url = paste0("../Resultados/MapasClasificacion/Proyecciones_HclustEspacialAltitud.html"))

#####Caracterización con curvas mensuales

var.clases <-  var.completas.mens %>% 
  mutate(Valor_Def=case_when(!is.na(Valor) ~ Valor, 
                             !is.na(var1.pred) ~ var1.pred,
                             TRUE ~ Valor)) %>% 
  left_join(base.clases %>% 
              select(Estacion, Clase.Esp), by=c("estaciones"="Estacion"))


variables <- c(unique(var.clases$Variable))

for(j in 1:length(variables)){
  
  base.g <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d"))
  # 
  # grafico <- base.g %>%
  #   ggplot( aes(x=as.factor(Fecha), y=Valor_Def, group=as.factor(estaciones), color=as.factor(Clase.Esp))) +
  #   geom_line() +
  #   scale_color_brewer(palette = "Set2") +
  #   ggtitle(paste0("Curvas ", variables[j])) +
  #   labs(color = "Clase", x="Mes", y="Valor")
  # 
  # # ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/Curvas_Proyecciones_HclustEspacial", variables[j], ".jpeg"))
  # # 
  base.prom <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>%
    group_by(Mes, Clase.Esp) %>%
    summarise(Promedio=mean(Valor_Def))

  grafico <- base.prom %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(Clase.Esp), color=as.factor(Clase.Esp))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas promedio ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")

  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedioMes_Proyecciones_HclustEspacial", variables[j], ".jpeg"))

  base.per.mens <- base.g %>%
    filter(Variable==variables[j]) %>%
    group_by(estaciones, Mes) %>%
    summarise(Promedio=mean(Valor_Def), Clases=max(Clase.Esp))
  
  grafico <- base.per.mens %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(estaciones), color=as.factor(Clases))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedioMensual_Proyecciones_HclustEspacial_Estaciones", variables[j], ".jpeg"))
  
  
}


##################################
###fuzzy clustering##
###################################

res.fcm <- fcm(base.clasificar, centers=3)

prob.clust <- as.data.frame(res.fcm$u) 

summary(res.fcm)

max.prob <- prob.clust %>% 
  rowwise() %>% 
  mutate(Prob=max(c_across('Cluster 1':'Cluster 3')),
         Clase=which.max(c_across('Cluster 1':'Cluster 3'))) %>% 
  bind_cols(proyecciones %>% 
              select(Estacion))

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

#####

mapaFuzzy <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=clust1 %>%
                     st_transform(4326), radius=1, color=~pal1(Prob)) %>% 
  addCircleMarkers(data=clust2 %>%
                     st_transform(4326), radius=1, color=~pal2(Prob)) %>% 
  addCircleMarkers(data=clust3 %>%
                     st_transform(4326), radius=1, color=~pal3(Prob))

mapshot(mapaFuzzy, url = paste0("../Resultados/MapasClasificacion/Proyecciones_Fuzzy.html"))

#####Caracterización con curvas mensuales

var.clases <-  var.completas.mens %>% 
  mutate(Valor_Def=case_when(!is.na(Valor) ~ Valor, 
                             !is.na(var1.pred) ~ var1.pred,
                             TRUE ~ Valor)) %>% 
  left_join(estaciones.fuzzy %>% 
              select(Estacion, Clase), by=c("estaciones"="Estacion"))


variables <- c(unique(var.clases$Variable))

for(j in 1:length(variables)){
  
  base.g <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d"))

  grafico <- base.g %>%
    ggplot(aes(x=as.factor(Fecha), y=Valor_Def, group=as.factor(estaciones), color=as.factor(Clase))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")

  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/Curvas_Proyecciones_Fuzzy", variables[j], ".jpeg"))

  base.prom <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>%
    group_by(Fecha, Clase) %>%
    summarise(Promedio=mean(Valor_Def))
  
  grafico <- base.prom %>%
    ggplot( aes(x=as.factor(Fecha), y=Promedio, group=as.factor(Clase), color=as.factor(Clase))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas promedio ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedio_Proyecciones_Fuzzy", variables[j], ".jpeg"))
  
  
  
  base.prom <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>%
    group_by(Mes, Clase) %>%
    summarise(Promedio=mean(Valor_Def))
  
  grafico <- base.prom %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(Clase), color=as.factor(Clase))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas promedio ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedioMes_Proyecciones_Fuzzy", variables[j], ".jpeg"))
  
  base.per.mens <- base.g %>%
    filter(Variable==variables[j]) %>%
    group_by(estaciones, Mes) %>%
    summarise(Promedio=mean(Valor_Def), Clase=max(Clase))
  
  grafico <- base.per.mens %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(estaciones), color=as.factor(Clase))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedioMensual_Proyecciones_Fuzzy_Estaciones", variables[j], ".jpeg"))
  
  
}

#####################
#Fuzzy geographical##
####################

base.escalada <- scale(as.matrix(base.clasificar))


#Crear un sp de coordenadas

coords1 <- data.frame(st_coordinates(sf.estaciones))
d1s <- SpatialPointsDataFrame(coords1,as.data.frame(base.escalada))

pca.gw <- gwpca(d1s,vars=colnames(d1s@data),bw=1000000,k=10, scores=TRUE)

scores.pca <- data.frame(pca.gw$gwpca.scores[[1]][,1:2])

res.fcm <- fcm(scores.pca, centers=3)

prob.clust <- as.data.frame(res.fcm$u) 


max.prob <- prob.clust %>% 
  rowwise() %>% 
  mutate(Prob=max(c_across('Cluster 1':'Cluster 3')),
         Clase=which.max(c_across('Cluster 1':'Cluster 3'))) %>% 
  bind_cols(proyecciones %>% 
              select(Estacion))

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

#####

mapaFuzzy <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addCircleMarkers(data=clust1 %>%
                     st_transform(4326), radius=1, color=~pal1(Prob)) %>% 
  addCircleMarkers(data=clust2 %>%
                     st_transform(4326), radius=1, color=~pal2(Prob)) %>% 
  addCircleMarkers(data=clust3 %>%
                     st_transform(4326), radius=1, color=~pal3(Prob))

mapshot(mapaFuzzy, url = paste0("../Resultados/MapasClasificacion/Proyecciones_Fuzzy.html"))

#####Caracterización con curvas mensuales

var.clases <-  var.completas.mens %>% 
  mutate(Valor_Def=case_when(!is.na(Valor) ~ Valor, 
                             !is.na(var1.pred) ~ var1.pred,
                             TRUE ~ Valor)) %>% 
  left_join(estaciones.fuzzy %>% 
              select(Estacion, Clase), by=c("estaciones"="Estacion"))


variables <- c(unique(var.clases$Variable))

for(j in 1:length(variables)){
  
  base.g <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d"))
  
  grafico <- base.g %>%
    ggplot(aes(x=as.factor(Fecha), y=Valor_Def, group=as.factor(estaciones), color=as.factor(Clase))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/Curvas_Proyecciones_FuzzyGeo", variables[j], ".jpeg"))
  
  base.prom <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>%
    group_by(Fecha, Clase) %>%
    summarise(Promedio=mean(Valor_Def))
  
  grafico <- base.prom %>%
    ggplot( aes(x=as.factor(Fecha), y=Promedio, group=as.factor(Clase), color=as.factor(Clase))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas promedio ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedio_Proyecciones_FuzzyGeo", variables[j], ".jpeg"))
  
  
  
  base.prom <- var.clases %>%
    filter(Variable==variables[j]) %>%
    mutate(Fecha=as.Date(paste0(Año,"-", Mes, "-01"), format="%Y-%m-%d")) %>%
    group_by(Mes, Clase) %>%
    summarise(Promedio=mean(Valor_Def))
  
  grafico <- base.prom %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(Clase), color=as.factor(Clase))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas promedio ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedioMes_Proyecciones_FuzzyGeo", variables[j], ".jpeg"))
  
  base.per.mens <- base.g %>%
    filter(Variable==variables[j]) %>%
    group_by(estaciones, Mes) %>%
    summarise(Promedio=mean(Valor_Def), Clase=max(Clase))
  
  grafico <- base.per.mens %>%
    ggplot( aes(x=as.factor(Mes), y=Promedio, group=as.factor(estaciones), color=as.factor(Clase))) +
    geom_line() +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Curvas ", variables[j])) +
    labs(color = "Clase", x="Mes", y="Valor")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/CurvasPromedioMensual_Proyecciones_FuzzyGeo_Estaciones", variables[j], ".jpeg"))
  
  
}
