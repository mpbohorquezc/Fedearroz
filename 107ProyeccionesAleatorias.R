library(dplyr)
library(fda.usc)
library(fda)
library(BSDA)
library(PairedData)
library(PASWR)
library(tidyr)

###Cargar base imputada

load("../Bases de datos/Procesadas/ImputacionTodasVariables.RData")

variables <- unique(var.completas$Variable)

var.completas.mens <- var.completas %>% 
     mutate(Valor_Def=case_when(!is.na(Valor) ~ Valor,
                                !is.na(var1.pred) ~ var1.pred,
                                TRUE ~ Valor))
#
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

var.completas <- var.completas.mens %>%
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

# rm(temp)
#
# gc()

###proyecciones aleatorias

n.estaciones <- unique(var.completas$estaciones)
meses <-unique(var.completas$Anio_Mes)

proyecciones <- matrix(NA, nrow=length(n.estaciones), ncol=length(variables))


for(k in 1:length(variables)){
  
  base.var <- var.completas %>% 
    filter(Variable==variables[k]) %>% 
    dplyr::select(estaciones, Anio_Mes, Valor_Def) %>% 
    pivot_wider(names_from = Anio_Mes, values_from = Valor_Def)
  
  
  l <- ncol(base.var)-1
  
  ####Crear el movimiento
  
  z <- rnorm (length(meses), 0, 1)
  v <- rep (0, (length(meses)+1))
  for (i in 2:(length(meses)+1))
  {
    v[i] <- v[i-1]+rnorm(1,0,2)
  }
  v <- v[-1]
  
  #plot(t,v, type="l", ylab=expression(nu(t)), xlab="t") 
  
  t <- seq(1, l, length=l)  
  
 for(j in 1:nrow(base.var)){
   
   curva_t <- as.numeric(base.var[j,-1])  
   
   #plot(t, v*curva_t, lty=1, type="l", ylab=expression(nu(t)*X(t)))
   #summary(v*curva_t)
   
   prod=v*curva_t
   fdataobj<-fdata(prod,t)     # Convierte la curva \nu(t)\times X(t) en un datao funcional
   x<-int.simpson(fdataobj)    # Integra num?ricamente el dato funcional
   
   
   proyecciones[j,k] <- as.numeric(x)  
   
 }
  
  
}

colnames(proyecciones) <- variables
rownames(proyecciones) <- n.estaciones

proyecciones <- as.data.frame(proyecciones)    

save(proyecciones, file="../Bases de datos/Procesadas/BaseProyeccionesAleatorias.RData")
