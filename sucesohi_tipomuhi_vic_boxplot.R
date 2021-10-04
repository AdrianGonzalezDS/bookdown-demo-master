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
sucesoshi_tipomuhi_vic <- sucesos %>%
  select(infodelito2, prensa,tipo_delito,tipo_muerte,numero_victimas_2) %>%
  filter(infodelito2 == "Si" &
           tipo_delito == "Homicidio intencional")
sucesoshi_tipomuhi_vic

sucesoshi_tipomuhi_vic$numero_victimas_2[sucesoshi_tipomuhi_vic$numero_victimas_2 == 98] <- 10

sucesoshi_tipomuhi_vic


sucesoshi_tipomuhi_vic_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,tipo_muerte,numero_victimas_2) %>%
  filter(infodelito2 == "Si" &
           tipo_delito == "Homicidio intencional"&
           !tipo_muerte %in% c(NA, "NA")&
           !numero_victimas_2 %in% c(NA, "NA",99))
sucesoshi_tipomuhi_vic_sel

sucesoshi_tipomuhi_vic_sel$numero_victimas_2[sucesoshi_tipomuhi_vic_sel$numero_victimas_2 == 98] <- 10

sucesoshi_tipomuhi_vic_sel

prensa_sucesoshi_tipomuhi_vic_sel <- length(unique(sucesoshi_tipomuhi_vic_sel[["prensa"]]))
prensa_sucesoshi_tipomuhi_vic_sel

sucesoshi_tipomuhi_vic_sel$prensa<- NULL



#Poniendo acentos en la leyenda
sucesoshi_tipomuhi_vic_sel$tipo_muerte <- str_replace_all(sucesoshi_tipomuhi_vic_sel$tipo_muerte,
                                                                                 "[^[:alnum:]]"," ")

sucesoshi_tipomuhi_vic_sel$tipo_muerte <- str_replace_all(sucesoshi_tipomuhi_vic_sel$tipo_muerte,
                                                                                 "Agresi n grave mortal","Agresión grave mortal")
sucesoshi_tipomuhi_vic_sel$tipo_muerte <- str_replace_all(sucesoshi_tipomuhi_vic_sel$tipo_muerte,
                                                                                 "Ejecuci n extrajudicial","Ejecución extrajudicial")






sucesoshi_tipomuhi_vic_boxplot <- ggplot(sucesoshi_tipomuhi_vic_sel,
                                aes(x = reorder(tipo_muerte, numero_victimas_2),
                                    y = numero_victimas_2))+
  geom_boxplot(outlier.colour = "blue", outlier.shape = 1)+
  stat_boxplot(geom ='errorbar', width = 0.6)+
  geom_point(alpha=0.4, color="tomato", position = "jitter")+
  stat_summary(fun= mean, geom="point", shape=8, size=2)+
  theme_classic()+

  scale_y_continuous( limits=c(0, 10),
                      breaks=seq(0,10,1),labels = c("0", "1", "2","3", "4",
                                                    "5","6", "7", "8", "9", "\u2265 10")) +
  xlab("") +
  ylab("Número de víctimas por suceso")+
  expand_limits(y = 0)+
  coord_flip()+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesoshi_tipomuhi_vic)} ({sum(is.na(sucesoshi_tipomuhi_vic$numero_victimas_2)| sucesoshi_tipomuhi_vic$numero_victimas_2 == 'NA'| sucesoshi_tipomuhi_vic$numero_victimas_2 == 99)} casos perdidos por información faltante) en {prensa_sucesoshi_tipomuhi_vic_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))


sucesoshi_tipomuhi_vic_boxplot
