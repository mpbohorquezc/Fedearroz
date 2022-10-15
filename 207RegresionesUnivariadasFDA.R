library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(fda)
library(ggplot2)
library(stringr)
library(writexl)
library(DepthProc)

###Cargar rendimientos

load("../Bases de datos/Procesadas/rendimientos_geo.RData")

fincas.uni <- fincas.geo %>% 
  dplyr::select(FINCA, FECHA_SIEMBRA, FECHA_COSECHA, RENDIMIENTO, SISTEMA) %>% 
  filter(year(FECHA_COSECHA)<2020) %>% 
  st_drop_geometry() %>% 
  distinct(FINCA, FECHA_SIEMBRA, FECHA_COSECHA, RENDIMIENTO, SISTEMA)


####Carga base modelo

load("../Bases de datos/Procesadas/BaseModeloRelacionClimaComp.RData")


# total.obs <- base.modelo %>% 
#   group_by(FINCA, Variable, Cosecha) %>% 
#   summarise(Tot=n())

base.modelo <- base.modelo %>% 
  mutate(id_cosecha=paste0(FINCA, "_", Cosecha)) %>% 
  left_join(fincas.uni %>% 
              dplyr::select(FINCA, FECHA_SIEMBRA, FECHA_COSECHA, SISTEMA, RENDIMIENTO)) %>% 
  distinct(FINCA, Cosecha, Tiempo, Variable, .keep_all = TRUE)

tot <- base.modelo %>% 
  group_by(FINCA, Cosecha, Variable) %>% 
  summarise(Total=n())

variables <- unique(base.modelo$Variable)

id_cosecha <- unique(base.modelo$id_cosecha) 
clases <- seq(1:4)
sistemas <- c("RIEGO", "SECANO")

##########Ajuste de modelos#############################

info.reg <- NULL
l.resid <- NULL


for(i in 1:length(variables)){
  for(j in 1:4){
    for(k in 1:2){
    
    base.temp <- base.modelo %>% 
      filter(Variable==variables[i] & Clase==clases[j] & SISTEMA==sistemas[k])
    
    tt <- unique(base.temp$Tiempo)
    base.temp$dia_mod <- paste0("t", base.temp$Tiempo)
    
    base.temp <- base.temp %>% 
      pivot_wider(id_cols=c(FINCA, id_cosecha, FECHA_SIEMBRA, FECHA_COSECHA, Clase, SISTEMA, RENDIMIENTO), 
                  names_from = dia_mod,
                  values_from = Valor)
    
    ##Convierte la covariable funcional en un objeto fda 
    
    basis1  <-  create.bspline.basis(rangeval = c(0, max(tt)), nbasis = 17)
    cv.fd <-  smooth.basis(tt, t(as.matrix(base.temp %>% 
                                             dplyr::select(t1:t51))), basis1)
    
    
    prof <- fncDepth(t(eval.fd(tt,cv.fd$fd)))  
    
    cdf.prof <- ecdf(prof)(prof)
    
    top75curves <- eval.fd(tt,cv.fd$fd)[,(cdf.prof > 0.25)]
    top75ranges <- apply(top75curves, 1, range)
    
    
    ###Grafico
    
    covariable <- data.frame("Tiempo"=rep(tt, nrow(base.temp)),
                             "bottom"=top75ranges[1,],
                             "top"=top75ranges[2,],
                             "median"=top75curves[,1],
                             "Zona"=paste0("Zona ",rep(clases[j], length(c(eval.fd(tt,cv.fd$fd))))),
                             "Finca"=rep(base.temp$FINCA, each=length(tt))
    )
    
    color.g <- case_when(clases[j]==1 ~ "#016339",
                         clases[j]==2 ~ "#DE8103",
                         clases[j]==3 ~ "#7400B6",
                         TRUE ~ "#006AB6")
    
    
    grafico <- ggplot(data=covariable, aes(x=Tiempo, y=bottom, group=Zona)) +
      geom_line(aes(color=Zona), linetype="dashed") +
      geom_line(aes(y=top, color=Zona), linetype="dashed") +
      geom_line(aes(y=median, color=Zona),
                size=1.01) +
      geom_ribbon(aes(ymin=bottom, ymax=top,
                      fill=Zona), alpha = 0.3) +
      scale_color_manual(values=c(paste0(color.g))) +
      scale_fill_manual(values=c(paste0(color.g))) +
      labs(x="Tiempo (percentil)", y=paste0(str_to_title(variables[i]))) +
      ggtitle(paste0("Comportamiento ", str_to_title(variables[i]), " en la zona ", j, " - ", sistemas[k]))
    
    
    ggsave(paste0("../Resultados/Modelo relacion clima rendimiento/boxplotCovariable_",variables[i], 
                  "_zona_",j,"-",sistemas[k], ".jpeg"),
           grafico,
           width = 30,
           height = 20,
           units = "cm")
    
    ##Extraer la variable respuesta
    
    rend.df <- base.temp 
    
    rend <- as.numeric(as.character(rend.df$RENDIMIENTO))
    
    ####Preparar los datos para el modelo
    
    conbasis2 <-  create.constant.basis(c(0, max(tt)))
    betabasis2 <- create.bspline.basis(c(0, max(tt)),7)
    
    ##Lista de la covariable
    
    templist2 <- vector("list",2)
    templist2[[1]] <-  rep(1,nrow(base.temp))  ##Hay 35 individuos
    templist2[[2]] <-  cv.fd$fd  #
    
    ##Lista de la función beta
    
    betalist2 <-  vector("list",2)
    betalist2[[1]] <-  conbasis2
    betalist2[[2]] <-  betabasis2
    
    reg0 <-  fRegress(rend,templist2,betalist2)
    
    coef(reg0$betaestlist[[1]]) 
    
    plot(reg0$betaestlist[[2]]$fd) 
    
    ####Residuales
    
    resid <-  rend - reg0$yhatfdobj
    SigmaE <-  sum(resid^2)/(nrow(base.temp)-reg0$df)
    SigmaE <-  SigmaE*diag(rep(1,nrow(base.temp)))
    y2cMap  <-  cv.fd$y2cMap
    stderrList <-  fRegress.stderr(reg0, y2cMap,SigmaE)
    
    betafdPar  <-  reg0$betaestlist[[2]]
    betafd  <-  betafdPar$fd
    betastderrList  <-  stderrList$betastderrlist
    betastderrfd  <-  betastderrList[[2]]
    
    
    beta.est <- data.frame("rango"=seq(from=0, to=100, by=2),
                           "beta"=eval.fd(tt,betafd),
                           "ls"=eval.fd(tt,betafd+1.96*betastderrfd),
                           "li"=eval.fd(tt,betafd-1.96*betastderrfd)
    )
    
    grafico <- ggplot(beta.est, aes(x=rango, y=beta)) +
      geom_line() +
      geom_hline(aes(yintercept=0), linetype='dotted') +
      geom_line(aes(y=ls, colour="Bandas de confianza"), linetype="dashed") +
      geom_line(aes(y=li, colour="Bandas de confianza"), linetype="dashed") +
      scale_color_manual(name = "", values = c("Bandas de confianza" = "red")) +
      labs(x="Tiempo (percentil)", y=expression(hat(beta)(t))) +
      ggtitle(bquote(hat(beta)(t) ~ "para la variable" ~ 
                       .(str_to_title(variables[i])) ~ "en la zona climática" ~ .(j) ~ " - " ~ .(sistemas[k])))
    
    ggsave(paste0("../Resultados/Modelo relacion clima rendimiento/beta_",variables[i], 
                  "_zona_",j,"-", sistemas[k],".jpeg"),
           grafico,
           width = 30,
           height = 20,
           units = "cm")
    
    ###R cuadrado
    
    SSE1.1 <- sum(resid^2)
    SSE0  <-  sum((rend - mean(rend))^2)
    
    RSQ1 = (SSE0-SSE1.1)/SSE0
    
    
    info.reg <- info.reg %>% 
      bind_rows(data.frame("Variable"=variables[i],
                           "Clase"=clases[j],
                           "Sistema"=sistemas[k],
                           "R2A"=RSQ1,
                           "beta.0"=coef(reg0$betaestlist[[1]]),
                           "F"=((SSE0-SSE1.1)/7)/(SSE1.1/(nrow(base.temp)-7-1)),
                           "GL.N"=7,
                           "GL.D"=nrow(base.temp)-7-1)
      )
    
    l.resid <- l.resid %>% 
      bind_rows(data.frame("Finca"=rend.df$FINCA,
                           "Ajustados"=reg0$yhatfdobj,
                           "Valor.Ori"=rend.df$RENDIMIENTO,
                           "Residual"=resid,
                           "Variable"=rep(variables[i], length(resid)),
                           "Clase"=rep(clases[j], length(resid)),
                           "Sistema"=rep(sistemas[k], length(resid))
      ))
    
    }
    }
  }

write_xlsx(l.resid, "../Resultados/Modelo relacion clima rendimiento/residualesZonas.xlsx")
write_xlsx(info.reg , "../Resultados/Modelo relacion clima rendimiento/infoModelos.xlsx")
