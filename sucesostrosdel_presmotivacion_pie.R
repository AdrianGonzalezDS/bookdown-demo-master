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
suodelpresmotivacion1 <- sucesos %>%
  select(infodelito2, prensa, tipo_delito, motivacion, presuncion_codificador_3) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional"))
suodelpresmotivacion1

suodelpresmotivacion_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito, motivacion, presuncion_codificador_3) %>%
  filter(infodelito2 == "Si" &
           !tipo_delito %in% c("Homicidio intencional") &
           !infodelito2 %in% c(NA, "NA") &
           !presuncion_codificador_3 %in% c(NA, "NA"))

suodelpresmotivacion_sel

prensa_suodelpresmotivacion_sel <- length(unique(suodelpresmotivacion_sel[["prensa"]]))
prensa_suodelpresmotivacion_sel

suodelpresmotivacion <- data.frame(suodelpresmotivacion_sel$presuncion_codificador_3)
suodelpresmotivacion



# Data transformation
suodelpresmotivacion_porcent <- suodelpresmotivacion %>%
  group_by(suodelpresmotivacion_sel.presuncion_codificador_3) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(perc) %>%
  mutate(porcentaje = scales::percent(perc))

suodelpresmotivacion_porcent
#graficando
# Barplot
suodelpresmotivacion_pie<- ggplot(suodelpresmotivacion_porcent,
                               aes(x="", y= suodelpresmotivacion_sel.presuncion_codificador_3,
                                   fill= porcentaje))+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  geom_text(aes(label = porcentaje),
            position = position_stack(vjust = 0.5))+
  theme_void()+
  #bbc_style()+
  scale_fill_manual(labels=c('Presunción codificador', 'Estipulado en el artículo'),
                    values = c("#1380A1", "#FAAB18"))+
  theme(legend.position="bottom", axis.title.x = element_blank(),
        legend.text = element_text(size = 9 ,color='black',angle=0,vjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_blank())+
  guides(fill=guide_legend(title=''))+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(suodelpresmotivacion1)} ({sum(is.na(suodelpresmotivacion1$presuncion_codificador_3)| suodelpresmotivacion1$presuncion_codificador_3 == 'NA')} casos perdidos por información faltante) en {prensa_suodelpresmotivacion_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))

suodelpresmotivacion_pie




