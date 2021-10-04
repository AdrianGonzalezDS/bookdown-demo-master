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
sucesosmil_donde <- sucesos %>%
  select(mil,prensa,donde_muerte) %>%
  filter(mil == "Si")

sucesosmil_donde

sucesosmil_donde_sel <- sucesos %>%
  select(mil,prensa,donde_muerte) %>%
  filter(mil == "Si" & !mil %in% c(NA, "NA") &
           !donde_muerte %in% c(NA, "NA"))

sucesosmil_donde_sel

prensa_sucesosmil_donde_sel <- length(unique(sucesosmil_donde_sel[["prensa"]]))
prensa_sucesosmil_donde_sel

sucesosmil_donde_sel$prensa<- NULL

#Poniendo acentos en la leyenda
sucesosmil_donde_sel$donde_muerte <- str_replace_all(sucesosmil_donde_sel$donde_muerte,
                                                      "[^[:alnum:]]"," ")

sucesosmil_donde_sel$donde_muerte <- str_replace_all(sucesosmil_donde_sel$donde_muerte,
                                                      "Entornos de atenci n institucional","Entornos de atención institucional")
sucesosmil_donde_sel$donde_muerte <- str_replace_all(sucesosmil_donde_sel$donde_muerte,
                                                      "Hogar de la victima","Hogar de la víctima")


# Data transformation
porcent_sucesosmildonde <- sucesosmil_donde_sel %>%
  group_by(donde_muerte) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_sucesosmildonde

#graficando

sucesosmildonde_barras <- ggplot(porcent_sucesosmildonde,
                                 aes(x = reorder(donde_muerte, freq), y = freq))+
  geom_col(aes(fill = donde_muerte), position = 'identity') +
  #geom_bar(aes(fill = organanismo_seguridad_1), position = "identity")+
  #geom_text(aes(label = porcentaje), position = position_fill(vjust = 1))+
  theme_classic()+ #scale_fill_brewer(palette = "GnBu")+coord_flip()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.justification = "left",
        axis.text = element_text(size= 7,color='black',
                                 angle=0, vjust = 0.5),
        legend.text = element_text(size = 7 ,
                                   color='black',
                                   angle=0,
                                   vjust = 0.5))+
  coord_flip()+
  scale_y_continuous( limits=c(0, 390),
                      breaks=seq(0,390,20))+
  geom_label(aes(x = donde_muerte, y = freq, label = porcentaje),
             hjust = -0.0001,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 2.5)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesosmil_donde)} ({sum(is.na(sucesosmil_donde$donde_muerte)| sucesosmil_donde$donde_muerte == 'NA')} casos perdidos por información faltante) en {prensa_sucesosmil_donde_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesosmildonde_barras
