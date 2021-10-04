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
victimasodelvictidel <- victimasdelito %>%
  select(infodelito2, prensa, tipo_delito, victi_deli2) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional"))
victimasodelvictidel

victimasodelvictideli_sel <- victimasdelito %>%
  select(infodelito2, prensa, tipo_delito, victi_deli2) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional")&
           !victi_deli2 %in% c(NA, "NA"))

victimasodelvictideli_sel

prensa_victimasodelvictideli_sel <- length(unique(victimasodelvictideli_sel[["prensa"]]))
prensa_victimasodelvictideli_sel


victimasodelvictideli <- data.frame(victimasodelvictideli_sel$victi_deli2)
victimasodelvictideli



# Data transformation
victimasodelvictideli_porcent <- victimasodelvictideli %>%
  group_by(victimasodelvictideli_sel.victi_deli2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(perc) %>%
  mutate(porcentaje = scales::percent(perc))

victimasodelvictideli_porcent
#graficando
# Barplot
victimasodel_victideli_donagraf <- ggplot(data = victimasodelvictideli_porcent,
                                         aes(x = 2, y = perc,
                                             fill = victimasodelvictideli_sel.victi_deli2))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Dark2") +
  #scale_fill_viridis_d(direction = 1)+
  #scale_fill_brewer(palette = "Dark")+
  xlim(1,2.5) + guides(fill=guide_legend(title=''))+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasodelvictidel)} ({sum(is.na(victimasodelvictidel$victi_deli2)| victimasodelvictidel$victi_deli2 == 'NA')} casos perdidos por información faltante) en {prensa_victimasodelvictideli_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  geom_text_repel(aes(label = porcentaje, x = 1.95),
                  position = position_stack(vjust = 0.6),
                  color = c("white", "white", "white"))

victimasodel_victideli_donagraf




