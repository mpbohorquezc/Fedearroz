library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

###Carga base de completa ideam

load("../Bases de datos/Procesadas/baseIdeamGeorreferenciada.RData") 

baseActiva <- baseActiva %>% 
  filter(Año>=1980, Variable %in% c("PTPG_TT_D", "PTPM_CON", "TMX_CON", "TMN_CON"))

###Análisis de precipitación

basePrec <- baseActiva %>% 
  filter(Variable %in% c("PTPG_TT_D", "PTPM_CON")) %>% 
  distinct(Fecha, Variable, Estacion, .keep_all = TRUE)

rm(baseActiva)

resumen <- summary(basePrec %>% 
                     filter(Variable=="PTPM_CON") %>% 
  select(Valor))

##Gráfico de densidad

gdensidad <- ggplot(basePrec, aes(x=Valor, y= ..scaled.., color=Variable)) + 
  geom_density() +
  xlim(-3, 5) +
  ylab("Densidad") +
  ggtitle("Distribuciones de precipitación diaria")

##Coeficiente de correlación

coeficientes <- basePrec %>% 
  pivot_wider(names_from = Variable, values_from = Valor) 

cor(coeficientes$PTPG_TT_D, coeficientes$PTPM_CON, use="pairwise.complete.obs")

diferencias <- coeficientes %>% 
  mutate(Dif=abs(PTPM_CON-PTPG_TT_D))

summary(diferencias$Dif)

promdif <- diferencias %>%  
  group_by(Estacion) %>% 
  summarise(Prom_Dif=mean(Dif, na.rm=TRUE)) %>% 
  filter(!is.na(Prom_Dif))

summary(promdif$Prom_Dif)

###Mapa de diferencias promedio

estaciones <- st_read("../Resultados/capaEstacionesIDEAM", layer="capaEstacionesIDEAM") %>% 
  left_join(promdif) %>% 
  filter(!is.na(Prom_Dif))

mapa.dif <- ggplot() +
  geom_sf(data=estaciones, aes(colour=Prom_Dif)) +
  labs(colour="Promedio \ndiferencias \nabsolutas") +
  ggtitle("Promedio diferencias absolutas \nentre variables de precipitación")

rm(coeficientes, diferencias)
gc()

####Mensualizar la precipitación

prec.mens <- basePrec %>% 
  group_by(Variable, Estacion, Año, Mes) %>% 
  summarise(Total_Prec=sum(Valor), Observaciones=n())

summary(prec.mens$Observaciones)

##Gráfico de densidad

gdensidad <- ggplot(prec.mens, aes(x=Total_Prec, y= ..scaled.., color=Variable)) + 
  geom_density() +
  xlim(-3, 5) +
  ylab("Densidad") +
  ggtitle("Distribuciones total de precipitación mensual")

###Solo las estaciones con las dos variables

prec.mens.comp <- prec.mens %>% 
  select(-Observaciones) %>% 
  pivot_wider(names_from=Variable, values_from = Total_Prec) %>% 
  drop_na() %>% 
  pivot_longer(cols=PTPG_TT_D:PTPM_CON, names_to = "Variable", values_to = "Valor")

gdensidad <- ggplot(prec.mens.comp, aes(x=Valor, y= ..scaled.., color=Variable)) + 
  geom_density() +
  xlim(-3, 5) +
  ylab("Densidad") +
  ggtitle("Distribuciones total de precipitación mensual \n(observaciones completas)")


length(levels(as.factor(prec.mens.comp$Estacion)))

prec.mens <- prec.mens %>% 
  select(-Observaciones) %>% 
  pivot_wider(names_from=Variable, values_from = Total_Prec)

cor(prec.mens$PTPG_TT_D, prec.mens$PTPM_CON, use="pairwise.complete.obs")

rm(prec.mens, prec.mens.comp)

#######Cálculo de otras estadísticas

prec.mens <- basePrec %>% 
  mutate(Dia_lluvia=case_when(Valor>=1 ~ 1,
                              TRUE ~ 0),
         Dia_lluviaL=case_when(Valor>=1 & Valor<5 ~ 1,
                               TRUE ~ 0),
         Dia_lluviaM=case_when(Valor>=5 & Valor<15 ~ 1,
                               TRUE ~ 0),
         Dia_lluviaF=case_when(Valor>=15 ~ 1,
                               TRUE ~ 0))
  group_by(Variable, Estacion, Año, Mes) %>% 
  summarise(Total_Dias=sum(Valor), Observaciones=n())
  
resumen.est.prec <- prec.mens %>% 
  group_by(Año, Mes, Estacion, Variable) %>% 
  summarise(Tot_lluvia=sum(Dia_lluvia), Tot_lluviaL=sum(Dia_lluviaL), 
Tot_lluviaM=sum(Dia_lluviaM), Tot_lluviaF=sum(Dia_lluviaF))
  
#####Comparación de densidades

gdensidad <- ggplot(resumen.est.prec, aes(x=Tot_lluvia, y= ..scaled.., color=Variable)) + 
  geom_density(adjust = 1.5) +
  ylab("Densidad") +
  xlab("Días de lluvia") +
  ggtitle("Distribuciones días de lluvia mensuales")

gdensidad <- ggplot(resumen.est.prec, aes(x=Tot_lluviaL, y= ..scaled.., color=Variable)) + 
  geom_density(adjust = 3) +
  ylab("Densidad") +
  xlab("Días de lluvia leve") +
  ggtitle("Distribuciones días de lluvia leve mensuales")

gdensidad <- ggplot(resumen.est.prec, aes(x=Tot_lluviaM, y= ..scaled.., color=Variable)) + 
  geom_density(adjust = 3) +
  ylab("Densidad") +
  xlab("Días de lluvia moderada") +
  ggtitle("Distribuciones días de lluvia moderada mensuales")

gdensidad <- ggplot(resumen.est.prec, aes(x=Tot_lluviaF, y= ..scaled.., color=Variable)) + 
  geom_density(adjust = 3) +
  ylab("Densidad") +
  xlab("Días de lluvia fuerte") +
  ggtitle("Distribuciones días de lluvia fuerte mensuales")
  
####Gráficos de densidad con observaciones completas

prec.mens.comp <- resumen.est.prec %>% 
  pivot_longer(cols=Tot_lluvia:Tot_lluviaF, names_to="Estadistica", values_to = "Valor") %>% 
  pivot_wider(names_from = c(Variable,Estadistica), names_sep=".",values_from = Valor) 

basecompleta <-  prec.mens.comp  %>% 
  drop_na() %>% 
  pivot_longer(cols=PTPM_CON.Tot_lluvia:PTPG_TT_D.Tot_lluviaF,
               names_to = "Estadistica", values_to= "Valor") %>% 
  mutate(Variable=sub("\\..*", "", Estadistica), Estadistica=sub(".*\\.", "", Estadistica))

##Gráficos adicionales de densidad con observaciones completas

tot_lluvia <- basecompleta %>% 
  filter(Estadistica=="Tot_lluvia")

gdensidad <- ggplot(tot_lluvia, aes(x=Valor, y= ..scaled.., color=Variable)) + 
  geom_density(adjust = 1.5) +
  ylab("Densidad") +
  xlab("Días de lluvia") +
  ggtitle("Distribuciones días de lluvia mensuales \n (observaciones completas)")

tot_lluvia <- basecompleta %>% 
  filter(Estadistica=="Tot_lluviaL")

gdensidad <- ggplot(tot_lluvia, aes(x=Valor, y= ..scaled.., color=Variable)) + 
  geom_density(adjust = 3) +
  ylab("Densidad") +
  xlab("Días de lluvia leve") +
  ggtitle("Distribuciones días de lluvia leve mensuales\n (observaciones completas)")

tot_lluvia <- basecompleta %>% 
  filter(Estadistica=="Tot_lluviaM")

gdensidad <- ggplot(tot_lluvia, aes(x=Valor, y= ..scaled.., color=Variable)) + 
  geom_density(adjust = 3) +
  ylab("Densidad") +
  xlab("Días de lluvia moderada") +
  ggtitle("Distribuciones días de lluvia moderada mensuales \n (observaciones completas)")

tot_lluvia <- basecompleta %>% 
  filter(Estadistica=="Tot_lluviaF")

gdensidad <- ggplot(tot_lluvia, aes(x=Valor, y= ..scaled.., color=Variable)) + 
  geom_density(adjust = 3) +
  ylab("Densidad") +
  xlab("Días de lluvia fuerte") +
  ggtitle("Distribuciones días de lluvia fuerte mensuales \n (observaciones completas)")

###Coeficientes de correlación entre estadísticas

cor(prec.mens.comp$PTPM_CON.Tot_lluvia, prec.mens.comp$PTPG_TT_D.Tot_lluvia,use="pairwise.complete.obs")

cor(prec.mens.comp$PTPM_CON.Tot_lluviaL, prec.mens.comp$PTPG_TT_D.Tot_lluviaL, use="pairwise.complete.obs")

cor(prec.mens.comp$PTPM_CON.Tot_lluviaM, prec.mens.comp$PTPG_TT_D.Tot_lluviaM, use="pairwise.complete.obs")

cor(prec.mens.comp$PTPM_CON.Tot_lluviaF, prec.mens.comp$PTPG_TT_D.Tot_lluviaF, use="pairwise.complete.obs")
