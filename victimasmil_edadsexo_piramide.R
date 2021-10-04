library(devtools)
library(ggplot2)
library(tidyverse) #requerido para la funcion gather
library(bbplot) #requerido para bbc style
library(plyr) #requerido para hacer gr?ficos de pir?mide
library(dplyr) #requerido para usar la funcion mutate
library(tidyr) #requerido para usar la funcion gather
library(stringr)#requerida para usar str_replace_all
library(reshape2)
library(XML)
library(plotrix)
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

#Prepare datos
victimasmilsexoedad <- victimasmil %>%
  select(mil,edad__victima_1, sexo_victima_1,prensa,numero_victimas_1) %>%
  filter(mil == "Si")

victimasmilsexoedad

victimasmilsexoedad_sel <- victimasmil %>%
  select(mil,edad__victima_1, sexo_victima_1,prensa, numero_victimas_1) %>%
  filter(mil == "Si" &
           !edad__victima_1 %in% c(99, "No informa", NA, "NA") &
           !sexo_victima_1 %in% c("No informa", NA, "NA"))

victimasmilsexoedad_sel

victimasmilsexoedad_sel$numero_victimas_1[victimasmilsexoedad_sel$numero_victimas_1 == 98] <- 10

victimasmilsexoedad_sel
# piramide_hom
# piramide_hom$Edad = as.numeric(piramide_hom$Edad)
# piramide_hom

prensa_victimasmilsexoedad_sel <- length(unique(victimasmilsexoedad_sel[["prensa"]]))
prensa_victimasmilsexoedad_sel



#graficando
a <- ggplot(victimasmilsexoedad_sel,
            mapping = aes(x = edad__victima_1, fill = sexo_victima_1))+
  # female histogram
  geom_histogram(data = victimasmilsexoedad_sel %>% filter(sexo_victima_1 == "Femenino"),
                 breaks = seq(0,85,5),
                 colour = "white")+
  # male histogram (values converted to negative)
  geom_histogram(data = victimasmilsexoedad_sel %>% filter(sexo_victima_1 == "Masculino"),
                 breaks = seq(0,85,5),
                 mapping = aes(y = ..count..*(-1)),
                 colour = "white")+
  # flip the X and Y axes
  coord_flip() +
  # adjust counts-axis scale
  scale_x_continuous(limits = c(0, 70),
                     breaks = seq(0,70,10),
                     labels = abs(seq(0,70, 10)))+
  scale_y_continuous(limits = c(-70, 5),
                     breaks = seq(-70,10,5),
                     labels = abs(seq(-70, 10, 5)))+
  theme_classic( )+ #scale_fill_brewer(palette = "GnBu")+coord_flip()+
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y= element_line(color="#cbcbcb"),
        legend.title = element_blank(),
        legend.justification = "center",
        axis.text = element_text(size= 11,color='black',
                                 angle=0, vjust = 0.5),
        legend.text = element_text(size = 9 ,
                                   color='black',
                                   angle=0,
                                   vjust = 0.5))+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasmilsexoedad)} ({sum(is.na(victimasmilsexoedad$sexo_victima_1) | is.na(victimasmilsexoedad$edad__victima_1) |victimasmilsexoedad$edad__victima_1 == 99 | victimasmilsexoedad$sexo_victima_1 == 'No informa')} casos perdidos por edad y sexo faltante) en {prensa_victimasmilsexoedad_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  scale_fill_manual(values = c("red3","dodgerblue4"))+
  xlab("Edad (años)") +
  ylab("Número de víctimas")


a

#este grafico que sigue me esta dando problemas, el numero de victimas
# segun la banda etarea no se corresponde con la realidad
#
victimasmil_piramide <- ggplot(data=victimasmilsexoedad_sel,aes(x=cut(edad__victima_1,
                                                                breaks=c(-1,seq(0,100,5))),
                                                          fill=sexo_victima_1)) +
  geom_bar(data=subset(victimasmilsexoedad_sel,sexo_victima_1=="Femenino")) +
  geom_bar(data=subset(victimasmilsexoedad_sel,sexo_victima_1=="Masculino"),aes(y=..count..*(-1))) +
  scale_x_discrete(labels=c("< 1",paste0(seq(1,91,5),"-",seq(5,100,5))), drop=T) +
  scale_y_continuous(breaks=seq(-70,70,10),labels=abs(seq(-70,70,10)))+
  xlab("Edad (a?os)") + ylab("N?mero de v?ctimas") +
  coord_flip()+
  #bbc_style()+
  theme_classic( )+
  #labs(x = "Edad (a?os)", y = "N?mero de v?ctimas", size = 2)+
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

victimasmil_piramide

# victimasmilsexoedad_piramide <- ggplot(data=victimasmilsexoedad_sel,
#                                        aes(x=cut(edad__victima_1,
#                                                  breaks=c(-1,seq(0,100,5))),
#                                            fill=sexo_victima_1)) +
#   geom_bar(data=subset(victimasmilsexoedad_sel,sexo_victima_1=="Femenino")) +
#   geom_bar(data=subset(victimasmilsexoedad_sel,sexo_victima_1=="Masculino"),aes(y=..count..*(-1))) +
#   scale_x_discrete(labels=c("< 1",paste0(seq(1,91,15),"-",seq(5,100,15))), drop=T) +
#   scale_y_continuous(breaks=seq(-50,50,10),labels=abs(seq(-50,50,10)))+
#   xlab("Edad (a?os)") + ylab("N?mero de v?ctimas") +
#   #coord_flip()+
#   #bbc_style()+
#   #labs(x = "Edad (a?os)", y = "N?mero de v?ctimas", size = 2)+
#   scale_fill_manual(values = c("red3","dodgerblue4"))+
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         legend.text = element_text(size = 9 ,color='black',
#                                    angle=0,vjust = 0.5),
#         #axis.title.y = element_text(color='black', angle=90, vjust = 0.5),
#         #axis.title.x = element_text(size= 7,color='black', angle=0, vjust = 0.5),
#         axis.text = element_text(size= 11,color='black', angle=0, vjust = 0.5),
#         strip.text = element_blank())
#
# victimasmilsexoedad_piramide
#
#
# p <- ggplot(data= victimasmilsexoedad_sel,aes(x=as.factor(edad__victima_1),
#                                               fill=sexo_victima_1)) +
#   geom_bar(data=subset(victimasmilsexoedad_sel,sexo_victima_1=="Femenino")) +
#   geom_bar(data=subset(victimasmilsexoedad_sel,sexo_victima_1=="Masculino"),aes(y=..count..*(-1))) +
#   scale_y_continuous(breaks=seq(-50,50,1),labels=abs(seq(-50,50,1))) +
#   coord_flip()+
#   xlab("Edad (a?os)") +
#   ylab("N?mero de v?ctimas")+
#   theme_classic()+ #scale_fill_brewer(palette = "GnBu")+coord_flip()+
#   theme(legend.position = "right",
#         legend.title = element_blank(),
#         legend.justification = "left",
#         axis.text = element_text(size= 8,color='black',
#                                  angle=0, vjust = 0.5),
#         legend.text = element_text(size = 9 ,
#                                    color='black',
#                                    angle=0,
#                                    vjust = 0.5))+
#   scale_fill_manual(values = c("red3","dodgerblue4"))
# p
#
# ggplot(data=test,aes(x=as.factor(v),fill=g)) +
#   geom_bar(data=subset(test,g=="F")) +
#   geom_bar(data=subset(test,g=="M"),aes(y=..count..*(-1))) +
#   scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) +
#   coord_flip()
#
# n1 <- ggplot(victimasmilsexoedad_sel, aes(x = edad__victima_1, y = numero_victimas_1, fill = sexo_victima_1)) +
#   geom_bar(subset = .(sexo_victima_1 == "Female"), stat = "identity") +
#   geom_bar(subset = .(sexo_victima_1 == "Male"), stat = "identity") +
#   scale_y_continuous(breaks = seq(-40, 40, 5),
#                      labels = paste0(as.character(c(seq(40, 0, -5), seq(5, 40, 5))))) +
#   coord_flip() +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw()
#
# n1
#
# n1 <- ggplot(nigeria, aes(x = Age, y = Population, fill = Gender)) +
#   geom_bar(subset = .(Gender == "Female"), stat = "identity") +
#   geom_bar(subset = .(Gender == "Male"), stat = "identity") +
#   scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000),
#                      labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) +
#   coord_flip() +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw()
#
# n1
#
#
#
#
# #graficando
# pir_h2 <- ggplot(data=piramide_hom,aes(x=cut(Edad, breaks=c(-1,seq(0,100,5))), fill=Sexo)) +
#   geom_bar(data=subset(piramide_hom,Sexo=="Femenino")) +
#   geom_bar(data=subset(piramide_hom,Sexo=="Masculino"),aes(y=..count..*(-1))) +
#   scale_x_discrete(labels=c("< 1",paste0(seq(1,91,5),"-",seq(5,100,5))), drop=T) +
#   scale_y_continuous(breaks=seq(-70,70,10),labels=abs(seq(-70,70,10)))+
#   xlab("Edad (a?os)") + ylab("N?mero de v?ctimas") +
#   coord_flip()+
#   bbc_style()+
#   #labs(x = "Edad (a?os)", y = "N?mero de v?ctimas", size = 2)+
#   scale_fill_manual(values = c("red3","dodgerblue4"))+
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         legend.text = element_text(size = 9 ,color='black',
#                                    angle=0,vjust = 0.5),
#         #axis.title.y = element_text(color='black', angle=90, vjust = 0.5),
#         #axis.title.x = element_text(size= 7,color='black', angle=0, vjust = 0.5),
#         axis.text = element_text(size= 11,color='black', angle=0, vjust = 0.5),
#         strip.text = element_blank())
# pir_h2
#
# a <- ggplot(victimasmilsexoedad_sel,
#             mapping = aes(x = edad__victima_1, fill = sexo_victima_1))+
#   # female histogram
#   geom_histogram(data = victimasmilsexoedad_sel %>% filter(sexo_victima_1 == "Femenino"),
#                  breaks = seq(0,85,5),
#                  colour = "white")+
#   # male histogram (values converted to negative)
#   geom_histogram(data = victimasmilsexoedad_sel %>% filter(sexo_victima_1 == "Masculino"),
#                  breaks = seq(0,85,5),
#                  mapping = aes(y = ..count..*(-1)),
#                  colour = "white")+
#   # flip the X and Y axes
#   coord_flip() +
#   # adjust counts-axis scale
#   scale_x_continuous(limits = c(0, 70),
#                      breaks = seq(0,70,10),
#                      labels = abs(seq(0,70, 10)))+
#   scale_y_continuous(limits = c(-70, 5),
#                      breaks = seq(-70,10,5),
#                      labels = abs(seq(-70, 10, 5)))+
#   theme_classic( )+ #scale_fill_brewer(palette = "GnBu")+coord_flip()+
#   theme(legend.position = "right",
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y= element_line(color="#cbcbcb"),
#         legend.title = element_blank(),
#         legend.justification = "left",
#         axis.text = element_text(size= 11,color='black',
#                                  angle=0, vjust = 0.5),
#         legend.text = element_text(size = 9 ,
#                                    color='black',
#                                    angle=0,
#                                    vjust = 0.5))+
#   scale_fill_manual(values = c("red3","dodgerblue4"))+
#   xlab("Edad (a?os)") +
#   ylab("N?mero de v?ctimas")
#
# a
#
#   # begin ggplot
#   ggplot(mapping = aes(x = age, fill = gender)) +
#
#   # female histogram
#   geom_histogram(data = linelist %>% filter(gender == "f"),
#                  breaks = seq(0,85,5),
#                  colour = "white") +
#
#   # male histogram (values converted to negative)
#   geom_histogram(data = linelist %>% filter(gender == "m"),
#                  breaks = seq(0,85,5),
#                  mapping = aes(y = ..count..*(-1)),
#                  colour = "white") +
#
#   # flip the X and Y axes
#   coord_flip() +
#
#   # adjust counts-axis scale
#   scale_y_continuous(limits = c(-600, 900),
#                      breaks = seq(-600,900,100),
#                      labels = abs(seq(-600, 900, 100)))
