library(devtools)
library(ggplot2)
library(tidyverse) #requerido para la funcion gather
library(bbplot) #requerido para bbc style
library(plyr) #requerido para hacer gr?ficos de pir?mide
library(dplyr) #requerido para usar la funcion mutate
library(tidyr) #requerido para usar la funcion gather
library(stringr)#requerida para usar str_replace_all
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

#Prepare datos
sucesosmil_cuando <- sucesos %>%
  select(mil,prensa,ocurrencia_muerte) %>%
  filter(mil == "Si")

sucesosmil_cuando


sucesosmil_cuando_sel <- sucesos %>%
  select(mil,prensa,ocurrencia_muerte) %>%
  filter(mil == "Si"& !mil %in% c(NA, "NA") &
           !ocurrencia_muerte %in% c(NA, "NA"))

sucesosmil_cuando_sel

prensa_sucesosmil_cuando_sel <- length(unique(sucesosmil_cuando_sel[["prensa"]]))
prensa_sucesosmil_cuando_sel

sucesosmil_cuando_sel$prensa<- NULL

#Poniendo acentos en la leyenda
sucesosmil_cuando_sel$ocurrencia_muerte <- str_replace_all(sucesosmil_cuando_sel$ocurrencia_muerte,
                                                      "[^[:alnum:]]"," ")

sucesosmil_cuando_sel$ocurrencia_muerte <- str_replace_all(sucesosmil_cuando_sel$ocurrencia_muerte,
                                                      "D a de la semana","Día de la semana")
sucesosmil_cuando_sel$ocurrencia_muerte <- str_replace_all(sucesosmil_cuando_sel$ocurrencia_muerte,
                                                      "D a de la semana en el d a","Día de la semana en el día")

sucesosmil_cuando_sel$ocurrencia_muerte <- str_replace_all(sucesosmil_cuando_sel$ocurrencia_muerte,
                                                      "D a de la semana en la noche","Día de la semana en la noche")
sucesosmil_cuando_sel$ocurrencia_muerte <- str_replace_all(sucesosmil_cuando_sel$ocurrencia_muerte,
                                                      "D a de la semana en la madrugada","Día de la semana en la madrugada")
sucesosmil_cuando_sel$ocurrencia_muerte <- str_replace_all(sucesosmil_cuando_sel$ocurrencia_muerte,
                                                      "Fin de semana en el d a","Fin de semana en el día")
sucesosmil_cuando_sel$ocurrencia_muerte <- str_replace_all(sucesosmil_cuando_sel$ocurrencia_muerte,
                                                           "Día de la semana en el d a","Día de la semana en el día")



# Data transformation
porcent_grup_sucesosmilcuando <- sucesosmil_cuando_sel %>%
  group_by(ocurrencia_muerte) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesosmilcuando


#graficando

sucesosmilcuando_barras <- ggplot(porcent_grup_sucesosmilcuando,
                                  aes(x = reorder(ocurrencia_muerte, freq), y = freq))+
  geom_col(aes(fill = ocurrencia_muerte), position = 'identity') +
  #geom_bar(aes(fill = organanismo_seguridad_1), position = "identity")+
  #geom_text(aes(label = porcentaje), position = position_fill(vjust = 1))+
  theme_classic()+ #scale_fill_brewer(palette = "GnBu")+coord_flip()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.justification = "left",
        axis.text = element_text(size= 10,color='black',
                                 angle=0, vjust = 0.5),
        legend.text = element_text(size = 7 ,
                                   color='black',
                                   angle=0,
                                   vjust = 0.5))+
  coord_flip()+
  scale_y_continuous( limits=c(0, 190),
                      breaks=seq(0,190,20))+
  geom_label(aes(x = ocurrencia_muerte, y = freq, label = porcentaje),
             hjust = -0.1,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesosmil_cuando)} ({sum(is.na(sucesosmil_cuando$ocurrencia_muerte)| sucesosmil_cuando$ocurrencia_muerte == 'NA')} casos perdidos por información faltante) en {prensa_sucesosmil_cuando_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesosmilcuando_barras
