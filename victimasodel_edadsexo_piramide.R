library(devtools)
library(ggplot2)
library(tidyverse)
library(bbplot)
library(plyr) #requerido para hacer gr?ficos de pir?mide
library(dplyr) #requerido para usar la funcion mutate
library(tidyr) #requerido para usar la funcion gather
library(stringr)#requerida para usar str_replace_all
library(writexl)#requerido para exportar df a excel
library(ggrepel)#requerido para usar geom_text_repel
#library(reshape2)#requerido para el paquete melt
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

#Prepare datos
victimasodel <- victimasdelito %>%
  select(edad__victima_2, sexo_victima_2, prensa, infodelito2, tipo_delito) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional"))

victimasodel

victimasodel_sel <- victimasdelito %>%
  select(edad__victima_2, sexo_victima_2, prensa, infodelito2, tipo_delito) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional") &
           !edad__victima_2 %in% c(99, "No informa", NA, "NA") &
           !sexo_victima_2 %in% c("No informa", NA, "NA"))

victimasodel_sel
# victimasodel_sel$Edad = as.numeric(victimasodel_sel$Edad)
# victimasodel_sel

prensa_victimasodel_sel <- length(unique(victimasodel_sel[["prensa"]]))
prensa_victimasodel_sel


#graficando
victimasodel_piramide <- ggplot(data=victimasodel_sel,aes(x=cut(edad__victima_2,
                                                                  breaks=c(-1,seq(0,100,5))),
                                                            fill=sexo_victima_2)) +
  geom_bar(data=subset(victimasodel_sel,sexo_victima_2=="Femenino")) +
  geom_bar(data=subset(victimasodel_sel,sexo_victima_2=="Masculino"),aes(y=..count..*(-1))) +
  scale_x_discrete(labels=c("< 1",paste0(seq(1,91,5),"-",seq(5,100,5))), drop=T) +
  scale_y_continuous(breaks=seq(-70,70,10),labels=abs(seq(-70,70,10)))+
  xlab("Edad (años)") + ylab("Número de víctimas") +
  coord_flip()+
  #bbc_style()+
  theme_classic( )+
  #labs(x = "Edad (años)", y = "Número de víctimas", size = 2)+
  scale_fill_manual(values = c("red3","dodgerblue4"))+
  theme(strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9 ,color='black',
                                   angle=0,vjust = 0.5),
        #axis.title.y = element_text(color='black', angle=90, vjust = 0.5),
        #axis.title.x = element_text(size= 7,color='black', angle=0, vjust = 0.5),
        axis.text = element_text(size= 11,color='black', angle=0, vjust = 0.5),
        strip.text = element_blank())+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasodel)} ({sum(is.na(victimasodel$sexo_victima_2) | is.na(victimasodel$edad__victima_2) |victimasodel$edad__victima_2 == 99 | victimasodel$sexo_victima_2 == 'No informa')} casos perdidos por edad y sexo faltante) en {prensa_victimasodel_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))

victimasodel_piramide


