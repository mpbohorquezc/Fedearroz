library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(plotly)
library(lubridate)
library(writexl)

#########Carga base de rachas####################

load("../Bases de datos/Procesadas/promediosRachas.RData")

load("../Bases de datos/Procesadas/basePrecipitacion.RData")
rm(base.pt)
gc()

regiones <- est.muni %>% 
  st_drop_geometry() %>% 
  distinct(Zona)

indicadores <- data.frame("Indicador"=c("Dia_Ll2", "Dia_Ll5", "Dia_S2", "Dia_S5"),
                          "Tipo"=c("1", "2", "1", "2"))

nom.ind <- c("Días con más o menos de 2mm de precipitación",
             "Días con más o menos de 5mm de precipitación")

tipos <- indicadores %>% 
  distinct(Tipo)

estadisticas <- colnames(rachas)[13:ncol(rachas)]

nom.est <- c("Promedio móvil de días seguidos con o sin lluvia en los últimos 7 días",
             "Promedio móvil de días con o sin lluvia en los últimos 7 días",
             "Promedio móvil de días seguidos con o sin lluvia en los últimos 10 días",
             "Promedio móvil de días con o sin lluvia en los últimos 10 días",
             "Promedio móvil de días seguidos con o sin lluvia en los últimos 20 días",
             "Promedio móvil de días con o sin lluvia en los últimos 20 días",
             "Promedio móvil de días seguidos con o sin lluvia en los últimos 30 días",
             "Promedio móvil de días con o sin lluvia en los últimos 30 días",
             "Promedio móvil de días seguidos con o sin lluvia en los últimos 45 días",
             "Promedio móvil de días con o sin lluvia en los últimos 45 días")

nombres <- c("PM7","D7", "PM10", "D10", "PM20", "D20",
             "PM30", "D30", "PM45", "D45")

for(i in 1:nrow(regiones)){ 
  
  for(j in 1:nrow(tipos)){ 
    
    estaciones <- est.muni$Estacion[est.muni$Zona==regiones[i,1]]
    
    ind <- indicadores %>% 
      filter(Tipo==tipos[j,1])
    
    temp.r <- rachas %>% 
      filter(Estacion %in% estaciones, Tipo_Dia %in% ind$Indicador) %>% 
      group_by(Fecha, Tipo_Dia) %>% 
      summarise(across(MA_7:TD_45,  list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))),
                ) %>% 
      ungroup()
     
    for(k in 1:length(estadisticas)){
      
      tabla <- temp.r %>% 
       dplyr::select(Fecha, Tipo_Dia, starts_with(paste0(estadisticas[k]))) %>% 
        mutate(BI=.[[3]]-1.96*.[[4]],
               BS=.[[3]]+1.96*.[[4]])

      tabla <- tabla %>% 
        pivot_wider(id_cols=Fecha, names_from = Tipo_Dia, values_from = 3:6) %>% 
        mutate(Temp_lluvia=case_when(.[[6]]>=.[[9]] ~ 1,
                                     TRUE ~ 0)) %>% 
        mutate(PM_Lluvia=.[[2]]*1,
               PM_Seco=.[[3]],
               BI_Lluvia=.[[6]],
               BS_Lluvia=.[[8]],
               BI_Seco=.[[7]],
               BS_Seco=.[[9]]) %>% 
        mutate(Fecha=as.Date(Fecha, format="%Y-%m-%d"),
               Temp_lluvia=case_when(PM_Lluvia>PM_Seco ~ 1,
                                     TRUE ~ 0),
               lagged=lag(Temp_lluvia),
               Marca_T=(Temp_lluvia != lagged)) %>% 
        dplyr::select(Fecha, PM_Lluvia, PM_Seco, BI_Lluvia, BS_Lluvia, BI_Seco, BS_Seco, Marca_T) 

      
      write_xlsx(tabla, paste0("../Resultados/Determinacion temporada lluvia/",
                               "Tipo", indicadores$Tipo[j],
                               "_", nombres[k],
                               "_region", regiones[i,1],
                               ".xlsx"))
      
      
      grafico2 <- plot_ly(tabla, x = ~Fecha, y=~PM_Lluvia, type = 'scatter', mode = 'lines',
                          line = list(color = '#0081a7', width = 1),
                          name="Días de lluvia")  %>% 
        add_trace(y = ~PM_Seco, line = list(color = '#f07167', width = 1),
                  name="Días secos") %>% 
        # add_ribbons(ymin = ~BI_Lluvia,
        #             ymax = ~BS_Lluvia,
        #             line = list(color = '#7FD9F4'),
        #             fillcolor = '#7FD9F4',
        #             name = 'Banda 95% de confianza lluvia',
        #             opacity = 0.6) %>% 
        # add_ribbons(ymin = ~BI_Seco,
        #             ymax = ~BS_Seco,
        #             line = list(color = '#fed9b7'),
        #             fillcolor = '#fed9b7',
        #             name = 'Banda 95% de confianza seco',
        #             opacity = 0.6) %>% 
        layout(title=paste0(nom.est[k]," (",nombres[k],") ", nom.ind[j]," ",regiones[i,1], sep="\n"),
                   xaxis = list(title = "Fecha"),
                   yaxis = list(title = "Promedio de estaciones")
               )
    
      
      htmlwidgets::saveWidget(as_widget(grafico2), 
                              paste0("../Resultados/Determinacion temporada lluvia/",
                                     "Tipo", indicadores$Tipo[j],
                                "_", nombres[k],
                                "_region", regiones[i,1],
                                ".html"))
      
    }
    
  }
  
}
