library(dplyr)
library(fda)
library(tidyr)
library(readxl)
library(DepthProc)
library(sf)
library(leaflet)
library(mapview)

####Carga y depuración de datos

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
rm(var.completas)
gc()

#####Cargar clasificación

clases <- read_xlsx("../Resultados/MapasClasificacion/ClasificacionDef/ClasificacionEstaciones.xlsx")


var.clases <-  var.completas.mens %>% 
  left_join(clases %>% 
              dplyr::select(Estacion, Clase, Clust.DBSCAN),
            by=c("estaciones"="Estacion")) %>% 
  filter(!is.na(Clase))

#####Creación de los boxplots

variables <- unique(var.clases$Variable)


for(i in 1:length(variables)){

  df <-  NULL
  
  base.var <- var.clases %>% 
    filter(Variable==variables[i]) %>%
    arrange(Año, Mes) %>% 
    group_by(estaciones, Variable, Mes) %>% 
    summarise(Valor=mean(Valor_Def), Clase=unique(Clase), Clust.DBSCAN=unique(Clust.DBSCAN)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = c(estaciones, Clase, Clust.DBSCAN), 
                names_from = Mes, values_from = Valor)
  
  for(j in 1:4){
  
      base.c <- base.var %>% 
        filter(Clase==j) 
      
      m.base <- base.c %>% 
        dplyr::select(-c(estaciones, Clase, Clust.DBSCAN)) %>% 
        as.matrix()
      
      prof <- fncDepth(m.base)
      
      cdf.prof <- ecdf(prof)(prof)
      
      top75curves <- t(m.base)[,(cdf.prof > 0.25)]
      top75ranges <- apply(top75curves, 1, range)
      
      
      ###Grafico
      
      df <- df %>% 
        bind_rows(data.frame("Fecha"=as.factor(seq(from=1, to=ncol(m.base), by=1)),
                       "bottom"=top75ranges[1,],
                       "top"=top75ranges[2,],
                       "median"=top75curves[,1],
                       "Clase"=rep(j, ncol(m.base))
                       ))
      
      
  }
  
  grafico <- ggplot(data=df, aes(x=Fecha, y=bottom, group=as.factor(Clase))) +
              geom_line(aes(color=as.factor(Clase)), linetype="dashed") +
              geom_line(aes(y=top, color=as.factor(Clase)), linetype="dashed") +
              geom_line(aes(y=median, color=as.factor(Clase)),
                        size=1.01) +
              geom_ribbon(aes(ymin=bottom, ymax=top,
                              fill=as.factor(Clase)), alpha = 0.3) +
              scale_color_manual(values=c("#016339", "#DE8103", "#7400B6", "#006AB6")) +
              scale_fill_manual(values=c("#016339", "#DE8103", "#7400B6", "#006AB6")) +
              ggtitle(paste0("Boxplot 75% - ", variables[i])) +
              labs(color="Zona", fill="Zona")
  
  ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/BoxplotFunc_", variables[i], ".jpeg"))

}

#####Boxplots funcionales de subzonas

for(i in 1:length(variables)){
  
  base.var <- var.clases %>% 
    filter(Variable==variables[i]) %>%
    arrange(Año, Mes) %>% 
    group_by(estaciones, Variable, Mes) %>% 
    summarise(Valor=mean(Valor_Def), Clase=unique(Clase), Clust.DBSCAN=unique(Clust.DBSCAN)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = c(estaciones, Clase, Clust.DBSCAN), 
                names_from = Mes, values_from = Valor)
  
  
  for(j in 1:4){
    
    df <-  NULL
    
    base.c <- base.var %>% 
      filter(Clase==j) %>% 
      mutate("Subzona"=substr(Clust.DBSCAN, 3, 3))
    
    n.sub <- length(unique(base.c$Subzona))
    
    for(k in 1:n.sub){
      
      m.base <- base.c %>% 
        filter(Subzona==k) %>% 
        dplyr::select(-c(estaciones, Clase, Clust.DBSCAN, Subzona)) %>% 
        as.matrix()
      
      prof <- fncDepth(m.base)
      
      cdf.prof <- ecdf(prof)(prof)
      
      top75curves <- t(m.base)[,(cdf.prof > 0.25)]
      top75ranges <- apply(top75curves, 1, range)
      
      
      ###Grafico
      
      df <- df %>% 
        bind_rows(data.frame("Fecha"=as.factor(seq(from=1, to=ncol(m.base), by=1)),
                             "bottom"=top75ranges[1,],
                             "top"=top75ranges[2,],
                             "median"=top75curves[,1],
                             "Clase"=rep(j, ncol(m.base)),
                             "Subzona"=rep(k, ncol(m.base)))
        )

    }
    
    grafico <- ggplot(data=df, aes(x=Fecha, y=bottom, group=as.factor(Subzona))) +
      geom_line(aes(color=as.factor(Subzona)), linetype="dashed") +
      geom_line(aes(y=top, color=as.factor(Subzona)), linetype="dashed") +
      geom_line(aes(y=median, color=as.factor(Subzona)),
                size=1.01) +
      geom_ribbon(aes(ymin=bottom, ymax=top,
                      fill=as.factor(Subzona)), alpha = 0.3) +
      scale_color_brewer(palette="Set2") +
      scale_fill_brewer(palette="Set2") +
      ggtitle(paste0("Boxplot 75% - ", variables[i]," Zona ", j)) +
      labs(color="Subzona", fill="Subzona")
    
    ggsave(grafico, filename=paste0("../Resultados/MapasClasificacion/ClasificacionDef/Zona ",k, " BoxplotFunc_", variables[i],".jpeg"))
    
    
  }
  
}


########################################
########Mapas de subzonas###############
########################################

load("../Resultados/MapasClasificacion/ClasificacionDef/PoligonosZonasClimaticas.RData")


pal1 <- colorFactor("Set2",
                    domain=poli1$Clust.DBSCAN)

pal2 <- colorFactor("Set2",
                    domain=poli2$Clust.DBSCAN)

pal3 <- colorFactor("Set2",
                    domain=poli3$Clust.DBSCAN)

pal4 <- colorFactor("Set2",
                    domain=poli4$Clust.DBSCAN)


###

mapaGrupos1 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addPolygons(data=poli1 %>%
                     st_transform(4326), color=~pal1(Clust.DBSCAN))

mapshot(mapaGrupos1, url = paste0("../Resultados/MapasClasificacion//ClasificacionDef/Mapa_Subzonas_Zona1.html"))

mapaGrupos2 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addPolygons(data=poli2 %>%
                st_transform(4326), color=~pal2(Clust.DBSCAN))

mapshot(mapaGrupos2, url = paste0("../Resultados/MapasClasificacion//ClasificacionDef/Mapa_Subzonas_Zona2.html"))

mapaGrupos3 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addPolygons(data=poli3 %>%
                st_transform(4326), color=~pal3(Clust.DBSCAN))

mapshot(mapaGrupos3, url = paste0("../Resultados/MapasClasificacion//ClasificacionDef/Mapa_Subzonas_Zona3.html"))

mapaGrupos4 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-72.958,4.095,6) %>%  
  addPolygons(data=poli4 %>%
                st_transform(4326), color=~pal4(Clust.DBSCAN))

mapshot(mapaGrupos4, url = paste0("../Resultados/MapasClasificacion//ClasificacionDef/Mapa_Subzonas_Zona4.html"))
