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
victimasdelhicondi <- victimasdelito %>%
  select(infodelito2,prensa,tipo_delito,condicion_victima_2) %>%
  filter(infodelito2 == "Si" &
           tipo_delito == "Homicidio intencional")
victimasdelhicondi

victimasdelhicondicion_sel <- victimasdelito %>%
  select(infodelito2,prensa,tipo_delito,condicion_victima_2) %>%
  filter(infodelito2 == "Si" &
           tipo_delito == "Homicidio intencional"&
           !condicion_victima_2 %in% c(NA, "NA"))

victimasdelhicondicion_sel

prensa_victimasdelhicondicion_sel <- length(unique(victimasdelhicondicion_sel[["prensa"]]))
prensa_victimasdelhicondicion_sel


victimasdelhicondicion <- data.frame(victimasdelhicondicion_sel$condicion_victima_2)
victimasdelhicondicion

#acortando nombres de delitos
# conyugal$victimasmilcoyugal_sel.conyugal_victima_1 <- str_replace_all(conyugal$victimasmilcoyugal_sel.conyugal_victima_1,
#                                                                       "[^[:alnum:]]", " ")
# conyugal$victimasmilcoyugal_sel.conyugal_victima_1 <- str_replace_all(conyugal$victimasmilcoyugal_sel.conyugal_victima_1,
#                                                                       "S lo","S?lo")

# Data transformation
victimasdelhicondicion_porcent <- victimasdelhicondicion %>%
  group_by(victimasdelhicondicion_sel.condicion_victima_2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  #arrange(desc(perc)) %>%
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

victimasdelhicondicion_porcent

victimasdelhicondicion_donagraf <- ggplot(data = victimasdelhicondicion_porcent,
                                         aes(x =2, y = perc,
                                             fill = victimasdelhicondicion_sel.condicion_victima_2))+
  #geom_col(width = 1, color = "black") +
  geom_bar(stat = "identity")+
  coord_polar("y", start = 90) +
  theme_void() +
  scale_fill_viridis_d(direction = 1)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasdelhicondi)} ({sum(is.na(victimasdelhicondi$condicion_victima_2)| victimasdelhicondi$condicion_victima_2 == 'NA')} casos perdidos por información faltante) en {prensa_victimasdelhicondicion_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  #scale_fill_brewer(palette = "Dark")+
  xlim(1,2.5) + guides(fill=guide_legend(title=''))+
  geom_text_repel(aes(label = porcentaje, x = 1.95),
                  position = position_stack(vjust = 0.5),
                  color = c("white", "white", "blue"))

victimasdelhicondicion_donagraf
# scale_fill_distiller()
# scale_fill_identity()

#Graficando

# pievictimasmil_conyugal <- ggplot(conyugal_porcent, aes(x = 2, y = perc,
#                                          fill = victimasmilcoyugal_sel.conyugal_victima_1)) +
#   geom_col(width = 1, color = "black") +
#   coord_polar( "y", start = 0)+
#   #guides(fill = guide_legend(title = "Delito"))+
#   theme_void()+  bbc_style()+
#   theme(legend.position="right", axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid=element_blank(),
#         axis.ticks = element_blank(),
#         plot.title=element_blank(),
#         legend.text = element_text(size = 10 ,color='black', angle=0,
#                                    vjust = 0.5))+
#   scale_fill_viridis_d(direction = -1)+
#   geom_text_repel(aes(label = porcentaje, x = -1),
#                   position = position_stack(vjust = 1),
#                   color = c(5, 5, 5))
#
# pievictimasmil_conyugal


