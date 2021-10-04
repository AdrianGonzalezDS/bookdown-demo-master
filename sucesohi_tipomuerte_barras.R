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
library(scales)
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))


#Prepare datos
victimasdelhitipomuhi <- sucesos %>%
  select(infodelito2, prensa, tipo_delito, tipo_muerte) %>%
  filter(infodelito2 == "Si" &
           tipo_delito == "Homicidio intencional")
victimasdelhitipomuhi

victimasdelhitipomuhi_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito, tipo_muerte) %>%
  filter(infodelito2 == "Si" &
           tipo_delito == "Homicidio intencional"&
           !tipo_muerte %in% c(NA, "NA"))

victimasdelhitipomuhi_sel

prensa_victimasdelhitipomuhi_sel <- length(unique(victimasdelhitipomuhi_sel[["prensa"]]))
prensa_victimasdelhitipomuhi_sel

victimasdelhitipomuhi_sel$prensa<- NULL


victimasdelhitipomuhint <- data.frame(victimasdelhitipomuhi_sel$tipo_muerte)
victimasdelhitipomuhint

# conyugal$victimasmilcoyugal_sel.conyugal_victima_1 <- str_replace_all(conyugal$victimasmilcoyugal_sel.conyugal_victima_1,
#                                                                       "[^[:alnum:]]", " ")
victimasdelhitipomuhint$victimasdelhitipomuhi_sel.tipo_muerte <- str_replace_all(victimasdelhitipomuhint$victimasdelhitipomuhi_sel.tipo_muerte,
                                                                                 "[^[:alnum:]]"," ")

victimasdelhitipomuhint$victimasdelhitipomuhi_sel.tipo_muerte <- str_replace_all(victimasdelhitipomuhint$victimasdelhitipomuhi_sel.tipo_muerte,
                                                                          "Agresi n grave mortal","Agresión grave mortal")
victimasdelhitipomuhint$victimasdelhitipomuhi_sel.tipo_muerte <- str_replace_all(victimasdelhitipomuhint$victimasdelhitipomuhi_sel.tipo_muerte,
                                                                          "Ejecuci n extrajudicial","Ejecución extrajudicial")




# Data transformation
victimasdelhitipomuhi_porcent <- victimasdelhitipomuhint %>%
  group_by(victimasdelhitipomuhi_sel.tipo_muerte) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(perc) %>%
  mutate(porcentaje = scales::percent(perc))

victimasdelhitipomuhi_porcent

tabla_tipomuerte <- victimasdelhitipomuhint %>%
  group_by(victimasdelhitipomuhi_sel.tipo_muerte) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(perc) %>%
  mutate(porcentaje = scales::percent(perc))

tabla_tipomuerte

names(tabla_tipomuerte)[names(tabla_tipomuerte) ==
                         "victimasdelhitipomuhi_sel.tipo_muerte"] <- "Tipo de muerte"
names(tabla_tipomuerte)[names(tabla_tipomuerte) ==
                         "freq"] <- "No. Sucesos"
tabla_tipomuerte$perc<- NULL
tabla_tipomuerte$label_pos<- NULL
tabla_tipomuerte
#graficando

victimasdelhitipomuhi_barras <- ggplot(victimasdelhitipomuhi_porcent,
                                  aes(x = reorder(victimasdelhitipomuhi_sel.tipo_muerte, freq), y = freq))+
  geom_col(aes(fill = victimasdelhitipomuhi_sel.tipo_muerte), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 450),
                      breaks=seq(0,450,30))+
  geom_label(aes(x = victimasdelhitipomuhi_sel.tipo_muerte, y = freq, label = porcentaje),
             hjust = -0.1,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasdelhitipomuhi)} ({sum(is.na(victimasdelhitipomuhi$tipo_muerte)| victimasdelhitipomuhi$tipo_muerte == 'NA')} casos perdidos por información faltante) en {prensa_victimasdelhitipomuhi_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

victimasdelhitipomuhi_barras
