library(gstat)
library(dplyr)
library(automap)
library(tidyr)
library(sf)
library(writexl)

####Cargar la base de datos mensuales

load("../Bases de datos/Procesadas/baseMensualizadaIndicadores.RData")


####Crear base para imputar

base.imputar <- base.completa.info %>% 
  pivot_longer(cols=N_dias_ll:Prom_R_Lluvia_1 ,names_to = "Variable", values_to = "Valor") %>% 
  mutate(Anio_Mes=paste0(Año, "_", Mes))

rm(base.completa.info)

##Imputación con kriging

meses <- unique(base.imputar$Anio_Mes)
variables<- unique(base.imputar$Variable)

variables <- c("N_dias_ll","N_dias_ll_L","N_dias_ll_M","Tot_prec",
               "N_dias_T_34","N_dias_T_23","TMX_P","TMN_P",
               "Max_R_L_Leve_1","Max_R_L_Moderada_1", "Max_R_Lluvia_1", "Min_R_L_Leve_1" ,
               "Max_R_Lluvia_0", "Min_R_L_Moderada_1", "Min_R_Lluvia_1", "Prom_R_L_Leve_1",
               "Prom_R_L_Moderada_1", "Prom_R_Lluvia_0", "Prom_R_Lluvia_1")

###Validacion cruzada con algunas fechas

meses.val <- sample(meses, 20)

promedios <- NULL
base.predicciones <- NULL

for(i in 1:length(variables)){

  for(j in 1:length(meses.val)){

    base.obs.val <- base.imputar %>%
      filter(Variable==variables[i] & Anio_Mes==meses.val[j] & !is.na(Valor))

    muestra <- sample(base.obs.val$estaciones, round(nrow(base.obs.val)*0.1))

    base.pred.val <- base.imputar %>%
      filter(Variable==variables[i] & Anio_Mes==meses.val[j] & is.na(Valor))
    coordinates(base.pred.val) <- ~X+Y

    base.obs.krig <- base.obs.val 
    coordinates(base.obs.krig) <- ~X+Y

    krig.val <- if(variables[i] %in% c("TMX_P","TMN_P")){
      
      krig.val.uni <- autoKrige(Valor~Altitud, base.obs.krig, base.pred.val)
      
    } else{
      
      krig.val.uni <-autoKrige(Valor~1, base.obs.krig, base.pred.val)
      
    }
      

    pred.var <- data.frame("Modelo.Ord"=krig.val$var_model$model[2],
                                     "Nug.Ord"=krig.val$var_model$psill[1], 
                                     "Sill.Ord"=krig.val$var_model$psill[2], 
                                     "Range.Ord"=krig.val$var_model$range[2],
                                    "Variable"=variables[i],
                                    "Mes"=meses.val[j]
                                     )

    base.predicciones <- base.predicciones %>%
      bind_rows(pred.var) 

  }

}

write_xlsx(base.predicciones, "../Resultados/ModelosImputacion.xlsx")

#######Imputación de datos

base.predicciones <-  NULL

for(i in 1:length(variables))
{

  for(j in 1:length(meses))
  {
    base.krig <- base.imputar %>% 
      filter(Variable==variables[i] & Anio_Mes==meses[j])
    
    base.obs <- base.krig %>%                         
      filter(!is.na(Valor))   
    coordinates(base.obs) <- ~X+Y
    
    base.pred <- base.krig %>% 
      filter(Est_arroz==1 & is.na(Valor))
    coordinates(base.pred) <- ~X+Y
    
    if(variables[i] %in% c("TMX_P", "TMN_P")){
      
      krig <-  try(autoKrige(Valor~Altitud, base.obs, base.pred))
      
      
    } else{
      
      krig <-  try(autoKrige(Valor~1, base.obs, base.pred))
      if(class(krig) == "try-error"){
          krig <-  try(autoKrige(Valor~Altitud, base.obs, base.pred))
      }
      
    }
    
    prediccion <- data.frame(krig$krige_output, "Variable"=variables[i], 
                             "Anio_Mes"=meses[j], "Estacion"=base.pred$estaciones)
    
    base.predicciones <- base.predicciones %>% 
      bind_rows(prediccion)
    
    print(paste0(i,"_",j))
    
  }
  
}

#save(base.predicciones, file="../Bases de datos/Procesadas/ImputacionZonaArrozCuatroVar.RData")

save(base.predicciones, file="../Bases de datos/Procesadas/ImputacionZonaArroz.RData") 

####Preparar base completa para proyecciones aleatorias

load("../Bases de datos/Procesadas/ImputacionZonaArroz.RData")

base.proyecciones <- base.imputar %>% 
  filter(Est_arroz==1) 

pred.arroz <- base.predicciones %>% 
  dplyr::select(var1.pred, Variable, Anio_Mes, Estacion)

base.proyecciones <- base.proyecciones %>% 
  left_join(pred.arroz, by=c("Anio_Mes"="Anio_Mes", "estaciones"="Estacion", "Variable"="Variable"))


faltantes <- base.proyecciones %>% 
  mutate(Valor_def=case_when(!is.na(Valor) ~ Valor, 
                             !is.na(var1.pred) ~ var1.pred,
                             TRUE ~ Valor)) %>% 
  filter(is.na(Valor_def)) %>% 
  group_by(Variable) %>% 
  summarise(Total_falt=n(), tot_estaciones=n_distinct(estaciones))

tot_estaciones <- base.proyecciones %>% 
  summarise(tot=n_distinct(estaciones))


var.completas <- base.proyecciones %>% 
  filter(!Variable %in% faltantes$Variable)

save(var.completas, file="../Bases de datos/Procesadas/ImputacionTodasVariables.RData")

######Algunos modelos

