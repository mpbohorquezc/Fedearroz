library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(zoo)

# ####cargar imputación de precipitación
# 
# load("../Bases de datos/Procesadas/ImputacionPrecipitacionDiaria.RData")
#load("../Bases de datos/Procesadas/basePrecipitacion.RData")
# 
# ###Crear base para rachas
# 
# base.rachas <- base.pt %>% 
#   left_join(base.predicciones %>% 
#               dplyr::select(-c(LONG, LAT)), by=c("Fecha"="Fecha", "Estacion"="Estacion")) %>% 
#   mutate(Pred=case_when(is.na(Valor) ~ 0, 
#                         TRUE ~1),
#          Valor_D=case_when(is.na(Valor) ~ var1.pred,
#                            TRUE ~ Valor)) %>% 
#   dplyr::select(Fecha, Estacion, Valor_D, LAT, LONG)
# 
# 
# base.rachas <- base.rachas %>% 
#   distinct(Fecha, Estacion, .keep_all=TRUE)
# 
# ###Observaciones por estación
# 
# n.est <- base.rachas %>% 
#   group_by(Estacion) %>% 
#   summarise(tot=n())
# 
# not_all_na <- function(x) any(!is.na(x))
# 
# info.est <- base.rachas %>% 
#   pivot_wider(id_cols = c("Estacion","LAT", "LONG"),
#               names_from = "Fecha",
#               values_from = "Valor_D") %>% 
#   dplyr::select(where(not_all_na))
# 
# ####Imputar con el más cercano lo que no se logró imputar con kriging
# 
# base.impt <- info.est %>% 
#   pivot_longer(cols=4:ncol(info.est), names_to="Fecha", values_to = "Valor_D") 
# 
# for(k in 1:nrow(base.impt)){
#   
#   if(is.na(base.impt$Valor_D[k])){
#     
#     j <- k 
#     h <- k 
#     
#     if(j<nrow(base.impt)){
#       while(is.na(base.impt$Valor_D[j])){
#         j <- j+1
#         if(j==nrow(base.impt)){break}
#       }} else{
#         j <- k
#       }
#     
#     
#     if(h>1){
#       while(is.na(base.impt$Valor_D[h])){
#         h <- h-1
#         if(h==1){break}
#       }} else{h <- k}
#     
#     disth <- abs(k-h)
#     distj <- abs(k-j)
#     
#     if(disth==0){
#       base.impt$Valor_D[k] <- base.impt$Valor_D[j]
#     } else if (distj==0){
#       base.impt$Valor_D[k] <- base.impt$Valor_D[h]
#     } else if(distj !=0 & disth !=0){
#       
#       ind <- ifelse(which.min(c(distj,disth))==1, j, h)
#       base.impt$Valor_D[k] <- base.impt$Valor_D[ind]
#     }
#     
#   }
#   
#   print(k)
# }
# 
# tot.na <- sum(is.na(base.impt$Valor_D))
# 
# save(base.impt, file="../Bases de datos/Procesadas/CompPrecipitacionDiaria.RData")
# 
# 
# ########Calculas las estadísticas
# 
# gc()
# 
# rm(base.rachas, base.pt, base.predicciones)

load("../Bases de datos/Procesadas/CompPrecipitacionDiaria.RData")

base.calc <- base.impt %>% 
  mutate(Valor_D=case_when(
    Valor_D>350 ~ 350,
    TRUE ~ Valor_D
  ),
  Dia_Ll2=case_when(
    Valor_D>=2 ~ 1,
    TRUE ~ 0
  ),
  Dia_Ll5=case_when(
    Valor_D>=5 ~ 1,
    TRUE ~ 0
  ),
  Dia_S2=case_when(
    Valor_D<2 ~ 1,
    TRUE ~ 0
  ),
  Dia_S5=case_when(
    Valor_D<5 ~ 1,
    TRUE ~ 0
  )
  ) %>% 
  pivot_longer(cols=c("Dia_Ll2", "Dia_Ll5", "Dia_S2", "Dia_S5"),
               names_to = "Tipo_Dia",
               values_to = "Ind")

rm(base.impt)

##Calcular rachas

rachas <- base.calc %>% 
  arrange(Tipo_Dia, Estacion, Fecha) %>% 
  group_by(Tipo_Dia, Estacion) %>% 
  mutate(lagged=lag(Ind)) %>% 
  ungroup() %>% 
  mutate(start=(Ind != lagged)) %>% 
  mutate(start=case_when(is.na(start) ~ TRUE,
                          TRUE ~ start)) %>% 
  group_by(Estacion) %>%
  mutate(racha_id=cumsum(start),
         id_lag=lag(racha_id)) %>% 
  ungroup() %>% 
  group_by(Tipo_Dia, Estacion, id_lag) %>% 
  mutate(Dacum=case_when(lagged==0 ~ 0,
                          TRUE ~ cumsum(lagged))) %>% 
  ungroup() %>% 
  group_by(Tipo_Dia, Estacion) %>% 
  mutate(PM7=c(rep(NA, 6), rollmean(Dacum, 7)),
         D7=c(rep(NA, 6), rollsum(Ind, 7)),
         PM10=c(rep(NA, 9), rollmean(Dacum, 10)),
         D10=c(rep(NA, 9), rollsum(Ind, 10)),
         PM20=c(rep(NA, 19), rollmean(Dacum, 20)),
         D20=c(rep(NA, 19), rollsum(Ind, 20)),
         PM30=c(rep(NA, 29), rollmean(Dacum, 30)),
         D30=c(rep(NA, 29), rollsum(Ind, 30)),
         PM45=c(rep(NA, 44), rollmean(Dacum, 45)),
         D45=c(rep(NA, 44), rollsum(Ind, 45))
                 )


save(rachas, file="../Bases de datos/Procesadas/promediosRachas.RData")
