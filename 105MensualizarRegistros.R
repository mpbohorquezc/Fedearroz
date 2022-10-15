library(dplyr)
library(sf)
library(tidyr)
library(writexl)

####Carga de base de datos 

load("../Bases de datos/Procesadas/baseIdeamGeorreferenciada.RData")

baseActiva <- baseActiva %>% 
  filter(Altitud<1163)

gc()

###Mensualizar los registros que no dependen de las rachas

base.ancha <- baseActiva %>% 
  pivot_wider(names_from = Variable, values_from=Valor)

gc()
rm(baseActiva)
gc()

suma <-  function(x) if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)

base.est <- base.ancha %>% 
  dplyr::select(-PTPG_TT_D) %>% 
  mutate(PTPM_CON=ifelse(PTPM_CON>350, 350, PTPM_CON),
    Dia_ll=case_when(PTPM_CON>5 ~ 1,
                          is.na(PTPM_CON) ~ PTPM_CON,
                          TRUE ~ 0),
         Dia_ll_L=case_when((PTPM_CON>=5) & (PTPM_CON<=20) ~ 1,
                            is.na(PTPM_CON) ~ PTPM_CON,
                            TRUE ~ 0),
         Dia_ll_M=case_when((PTPM_CON>20) & (PTPM_CON<=350) ~ 1,
                            is.na(PTPM_CON) ~ PTPM_CON,
                            TRUE ~ 0),
        Dia_T_34=case_when(TMX_CON>34 ~ 1,
                           is.na(TMX_CON) ~ TMX_CON,
                           TRUE ~ 0),
        Dia_T_23=case_when(TMN_CON>23 ~ 1,
                           is.na(TMN_CON) ~ TMN_CON,
                           TRUE ~ 0)
    ) %>% 
  group_by(Año, Mes, Estacion) %>% 
  summarise(N_dias_ll=suma(Dia_ll),
            N_dias_ll_L=suma(Dia_ll_L),
            N_dias_ll_M=suma(Dia_ll_M),
            Tot_prec=suma(PTPM_CON),
            N_dias_T_34=suma(Dia_T_34),
            N_dias_T_23=suma(Dia_T_23),
            TMX_P=mean(TMX_CON, na.rm=TRUE),
            TMN_P=mean(TMN_CON, na.rm=TRUE)
            )

rm(base.ancha)
gc()

####Cargar la base de rachas

load("../Bases de datos/Procesadas/rachasPrecipitacion.RData")

rachas.est <- rachas.imp %>% 
  group_by(Tipo, Año, Mes, Estacion, T_racha) %>% 
  summarise(Max_R=max(Longitud, na.rm=TRUE),
            Min_R=min(Longitud, na.rm=TRUE),
            Prom_R=mean(Longitud, na.rm=TRUE)) %>% 
  pivot_wider(id_cols = c(Año, Mes, Estacion), names_from=c(Tipo, T_racha),
              values_from=c(Max_R, Min_R, Prom_R),
              values_fill=0)

base.estadisticas <- base.est %>% 
  left_join(rachas.est)

gc()
rm(rachas.est)
rm(base.est)
gc()

#########Crear un vector de fechas

meses <-  data.frame("Año"=rep(seq(from=1980, to=2019, by=1), 12),
                     Mes=rep(1:12, each = 40))

n.estaciones <- length(unique(base.estadisticas$Estacion))

estaciones <- rep(as.character(unique(base.estadisticas$Estacion)), each=nrow(meses))
  
meses <- do.call("rbind", replicate(n.estaciones, meses, simplify = FALSE))

meses <- cbind(meses, estaciones)

#####Crear base con fechas

base.completa <- meses %>% 
  left_join(base.estadisticas, by=c("Año"="Año", "Mes"="Mes", "estaciones"="Estacion"))  


#####Contar NAs

tot.na <- base.completa %>% 
  group_by(estaciones) %>% 
  summarise(across(.cols=N_dias_ll:Prom_R_Lluvia_1, ~sum(is.na(.x))))

n <-  nrow(tot.na)
tot.reg.est <- 12*40*n

tot.na.var <- tot.na %>%  
  summarise(across(.cols=N_dias_ll:Prom_R_Lluvia_1, ~sum(.x))) %>% 
  mutate(across(.cols=N_dias_ll:Prom_R_Lluvia_1, ~(.x/tot.reg.est)*100))
  
#as.Date("2019-12-17", format="%Y-%m-%d")-as.Date("1980-01-01", format="%Y-%m-%d")

tot.reg <- 12*40

tot.na <- tot.na %>% 
  mutate(across(.cols=N_dias_ll:Prom_R_Lluvia_1, ~(.x/tot.reg)*100))

write_xlsx(tot.na, "../Resultados/PorcentajeCompletosIndicadores.xlsx")


####Pegar info de la estación

estaciones<- st_read("../Resultados/capaEstacionesIDEAM",
                     layer="capaEstacionesIDEAM") 

estaciones <- estaciones %>% 
  dplyr::select(Estacion, Estado, Altitud) %>% 
  bind_cols(data.frame(st_coordinates(estaciones)))


base.completa.info <- base.completa %>% 
  left_join(estaciones, by=c("estaciones"="Estacion")) 

rm(base.completa)

####Estaciones arroceras

est.arroz <- st_read("../Resultados/capaEstacionesZonaArrocera",
                                  layer="capaEstacionesZonaArrocera") %>%
 dplyr::select(Estacion) %>%
  mutate(Est_arroz=1)

base.completa.info <- base.completa.info %>% 
  left_join(est.arroz, by=c("estaciones"="Estacion")) 

base.completa.info <- base.completa.info %>% 
  dplyr::select(-c(geometry.x, geometry.y)) %>% 
  mutate(Est_arroz=case_when(is.na(Est_arroz) ~ 0,
                             TRUE ~ Est_arroz))

save(base.completa.info, file="../Bases de datos/Procesadas/baseMensualizadaIndicadores.RData")
