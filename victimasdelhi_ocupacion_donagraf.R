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
victimasdelocupa <- victimasdelito %>%
  select(infodelito2,prensa,ocupacion_victima_2, tipo_delito) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional")

victimasdelocupa

victimasdelocupacion_sel <- victimasdelito %>%
  select(infodelito2,prensa,ocupacion_victima_2, tipo_delito) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional"&
           !ocupacion_victima_2 %in% c(NA, "NA"))

victimasdelocupacion_sel

prensa_victimasdelocupacion_sel <- length(unique(victimasdelocupacion_sel[["prensa"]]))
prensa_victimasdelocupacion_sel

victimasdelocupacion <- data.frame(victimasdelocupacion_sel$ocupacion_victima_2 )
victimasdelocupacion

#acortando nombres de delitos
# conyugal$victimasmilcoyugal_sel.conyugal_victima_1 <- str_replace_all(conyugal$victimasmilcoyugal_sel.conyugal_victima_1,
#                                                                       "[^[:alnum:]]", " ")
# conyugal$victimasmilcoyugal_sel.conyugal_victima_1 <- str_replace_all(conyugal$victimasmilcoyugal_sel.conyugal_victima_1,
#                                                                       "S lo","S?lo")

#acortando nombres de leyenda
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                       "[^[:alnum:]]", " ")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                          "Trabajadores de los servicios y vendedores de comercios y mercados","Trabajadores servicios")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Agricultores y trabajadores calificados agropecuarios  forestales y pesqueros","Agricultores")

victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Miembros del poder ejecutivo y de los cuerpos legislativos y personal directivo de la administraci n p blica y de empresas","Ejecutivos")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Oficiales  operarios y artesanos de artes mecanicas y de otros oficios","Artes mecánicas")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Conductores de veh culos y operadores de equipos pesados m viles","Conductores")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Operadores de instalaciones y maquinas  y montadores","Operadores máquinas")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Tecnicos y profesionales de nivel medio","Técnicos")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Profesionales cientificos e intelectuales","Profesionales")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Personal de los servicios de proteccion","Protección")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Empleados administrativos de oficina","Oficina")
victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2 <- str_replace_all(victimasdelocupacion$victimasdelocupacion_sel.ocupacion_victima_2,
                                                                                     "Trabajadores no calificados","Trab. no calificados")

victimasdelocupacion


# Data transformation
victimasdelocupacion_porcent <- victimasdelocupacion %>%
  group_by(victimasdelocupacion_sel.ocupacion_victima_2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  #arrange(desc(perc)) %>%
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

victimasdelocupacion_porcent

tabla_ocupacion <- victimasdelocupacion %>%
  group_by(victimasdelocupacion_sel.ocupacion_victima_2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(perc)) %>%
  mutate(label_pos = cumsum(perc) - perc / 2,
         Porcentaje = paste0(round(perc * 100,1),"%"))

tabla_ocupacion

names(tabla_ocupacion)[names(tabla_ocupacion) ==
             "victimasdelocupacion_sel.ocupacion_victima_2"] <- "Ocupación"
names(tabla_ocupacion)[names(tabla_ocupacion) ==
                         "freq"] <- "Víctimas"
tabla_ocupacion$perc<- NULL
tabla_ocupacion$label_pos<- NULL

victimasdelocupacion_donagraf <- ggplot(data = victimasdelocupacion_porcent,
                                        aes(x = 2, y = perc,
                                            fill = victimasdelocupacion_sel.ocupacion_victima_2))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 90) +
  theme_void() +
  #scale_fill_brewer(palette = "Dark2", direction = 1)+
  scale_fill_viridis_d(direction = -1)+
  #scale_fill_brewer(palette = "Dark")+
  xlim(1,2.5) + guides(fill=guide_legend(title=''))+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasdelocupa)} ({sum(is.na(victimasdelocupa$ocupacion_victima_2)| victimasdelocupa$ocupacion_victima_2 == 'NA')} casos perdidos por información faltante) en {prensa_victimasdelocupacion_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  geom_text_repel(aes(label = porcentaje, x = 1.95),
                  position = position_stack(vjust = 0.5),
                  color = c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white"))

victimasdelocupacion_donagraf
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


