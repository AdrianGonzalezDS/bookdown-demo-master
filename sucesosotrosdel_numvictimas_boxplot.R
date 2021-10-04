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
sucesosotrosdelnumvictimas <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,numero_victimas_2) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional"))
sucesosotrosdelnumvictimas

sucesosotrosdelnumvictimas$numero_victimas_2[sucesosotrosdelnumvictimas$numero_victimas_2 == 98] <- 10

sucesosotrosdelnumvictimas


sucesosotrosdelnumvictimas_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,numero_victimas_2) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional")&
           !tipo_delito %in% c(NA, "NA")&
           !infodelito2 %in% c(NA, "NA")&
           !numero_victimas_2 %in% c(NA, "NA",99))
sucesosotrosdelnumvictimas_sel

sucesosotrosdelnumvictimas_sel$numero_victimas_2[sucesosotrosdelnumvictimas_sel$numero_victimas_2 == 98] <- 10

sucesosotrosdelnumvictimas_sel

prensa_sucesosotrosdelnumvictimas_sel <- length(unique(sucesosotrosdelnumvictimas_sel[["prensa"]]))
prensa_sucesosotrosdelnumvictimas_sel

#Poniendo acentos en la leyenda
sucesosotrosdelnumvictimas_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimas_sel$tipo_delito,
                                                          "[^[:alnum:]]"," ")

sucesosotrosdelnumvictimas_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimas_sel$tipo_delito,
                                                          "Coacci n  incluye extorsi n","Coacción  incluye extorsión")
sucesosotrosdelnumvictimas_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimas_sel$tipo_delito,
                                                          "Agresi n  incluye lesiones graves o leves  etc ","Agresión, lesiones graves o leves")
sucesosotrosdelnumvictimas_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimas_sel$tipo_delito,
                                                              "Violaci n sexual","Violación sexual")
sucesosotrosdelnumvictimas_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimas_sel$tipo_delito,
                                                              "Desaparici n forzada","Desaparición forzada")
sucesosotrosdelnumvictimas_sel$tipo_delito <- str_replace_all(sucesosotrosdelnumvictimas_sel$tipo_delito,
                                                              "Amenaza de agresi n","Amenaza de agresión")







sucesosotrosdelnumvictimas_boxplot <- ggplot(sucesosotrosdelnumvictimas_sel,
                                         aes(x = reorder(tipo_delito, numero_victimas_2),
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
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesosotrosdelnumvictimas)} ({sum(is.na(sucesosotrosdelnumvictimas$numero_victimas_2)| sucesosotrosdelnumvictimas$numero_victimas_2 == 'NA'| sucesosotrosdelnumvictimas$numero_victimas_2 == 99)} casos perdidos por información faltante) en {prensa_sucesosotrosdelnumvictimas_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))


sucesosotrosdelnumvictimas_boxplot

