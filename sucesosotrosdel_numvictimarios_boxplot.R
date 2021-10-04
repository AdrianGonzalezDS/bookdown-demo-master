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
sucesosotrosdelnumvictimarios <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,numero_victimarios) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional"))
sucesosotrosdelnumvictimarios

sucesosotrosdelnumvictimarios$numero_victimarios[sucesosotrosdelnumvictimarios$numero_victimarios == 98] <- 10

sucesosotrosdelnumvictimarios


sucesosotrosdelnumvictimarios_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,numero_victimarios) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional")&
           !tipo_delito %in% c(NA, "NA")&
           !infodelito2 %in% c(NA, "NA")&
           !numero_victimarios %in% c(NA, "NA",99))
sucesosotrosdelnumvictimarios_sel

sucesosotrosdelnumvictimarios_sel$numero_victimarios[sucesosotrosdelnumvictimarios_sel$numero_victimarios == 98] <- 10

sucesosotrosdelnumvictimarios_sel

prensa_sucesosotrosdelnumvictimarios_sel <- length(unique(sucesosotrosdelnumvictimarios_sel[["prensa"]]))
prensa_sucesosotrosdelnumvictimarios_sel

#Poniendo acentos en la leyenda
sucesosotrosdelnumvictimarios_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimarios_sel$tipo_delito,
                                                              "[^[:alnum:]]"," ")

sucesosotrosdelnumvictimarios_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimarios_sel$tipo_delito,
                                                              "Coacci n  incluye extorsi n","Coacción  incluye extorsión")
sucesosotrosdelnumvictimarios_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimarios_sel$tipo_delito,
                                                              "Agresi n  incluye lesiones graves o leves  etc ","Agresión, lesiones graves o leves")
sucesosotrosdelnumvictimarios_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimarios_sel$tipo_delito,
                                                              "Violaci n sexual","Violación sexual")
sucesosotrosdelnumvictimarios_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimarios_sel$tipo_delito,
                                                              "Desaparici n forzada","Desaparición forzada")
sucesosotrosdelnumvictimarios_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimarios_sel$tipo_delito,
                                                              "Amenaza de agresi n","Amenaza de agresión")







sucesosotrosdelnumvictimarios_boxplot <- ggplot(sucesosotrosdelnumvictimarios_sel,
                                             aes(x = reorder(tipo_delito, numero_victimarios),
                                                 y = numero_victimarios))+
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
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesosotrosdelnumvictimarios)} ({sum(is.na(sucesosotrosdelnumvictimarios$numero_victimarios)| sucesosotrosdelnumvictimarios$numero_victimarios == 'NA'| sucesosotrosdelnumvictimarios$numero_victimarios == 99)} casos perdidos por información faltante) en {prensa_sucesosotrosdelnumvictimarios_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))


sucesosotrosdelnumvictimarios_boxplot

