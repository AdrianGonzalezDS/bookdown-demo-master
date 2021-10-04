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
victimasodelactividad <- victimasdelito %>%
  select(infodelito2, prensa, tipo_delito, victima_era_2) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional"))
victimasodelactividad

victimasodelactividadi_sel <- victimasdelito %>%
  select(infodelito2, prensa, tipo_delito, victima_era_2) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional")&
           !victima_era_2 %in% c(NA, "NA"))

victimasodelactividadi_sel

prensa_victimasodelactividadi_sel <- length(unique(victimasodelactividadi_sel[["prensa"]]))
prensa_victimasodelactividadi_sel


victimasodelactividadi <- data.frame(victimasodelactividadi_sel$victima_era_2)
victimasodelactividadi



# Data transformation
victimasodelactividadi_porcent <- victimasodelactividadi %>%
  group_by(victimasodelactividadi_sel.victima_era_2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(perc) %>%
  # mutate(porcentaje = scales::percent(perc))
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

victimasodelactividadi_porcent

tabla_actividad <- victimasodelactividadi %>%
  group_by(victimasodelactividadi_sel.victima_era_2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(perc)) %>%
  mutate(label_pos = cumsum(perc) - perc / 2,
         Porcentaje = paste0(round(perc * 100,1),"%"))

tabla_actividad

names(tabla_actividad)[names(tabla_actividad) ==
                         "victimasodelactividadi_sel.victima_era_2"] <- "Ocupación"
names(tabla_actividad)[names(tabla_actividad) ==
                         "freq"] <- "Víctimas"
tabla_actividad$perc<- NULL
tabla_actividad$label_pos<- NULL

tabla_actividad

#graficando
# Barplot
victimasodel_actividad_donagraf <- ggplot(data = victimasodelactividadi_porcent,
                                          aes(x = 2, y = perc,
                                              fill = victimasodelactividadi_sel.victima_era_2))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Dark2") +
  #scale_fill_viridis_d(direction = 1)+
  #scale_fill_brewer(palette = "Dark")+
  xlim(1,2.5) + guides(fill=guide_legend(title=''))+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasodelactividad)} ({sum(is.na(victimasodelactividad$victima_era_2)| victimasodelactividad$victima_era_2 == 'NA')} casos perdidos por información faltante) en {prensa_victimasodelactividadi_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  geom_text_repel(aes(label = porcentaje, x = 1.95),
                  position = position_stack(vjust = 0.6),
                  color = c("white", "white", "white", "white", "white", "white","white"))

victimasodel_actividad_donagraf




