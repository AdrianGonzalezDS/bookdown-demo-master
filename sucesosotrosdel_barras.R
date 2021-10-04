library(devtools)
library(ggplot2)
library(tidyverse) #requerido para la funcion gather
library(bbplot) #requerido para bbc style
library(plyr) #requerido para hacer gr?ficos de pir?mide
library(dplyr) #requerido para usar la funcion mutate
library(tidyr) #requerido para usar la funcion gather
library(stringr)#requerida para usar str_replace_all
library(janitor)
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

#Prepare datos
sucesosotrosdel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional") )

sucesosotrosdel


sucesosotrosdel_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional")& !infodelito2 %in% c(NA, "NA"))

sucesosotrosdel_sel

prensa_sucesosotrosdel_sel <- length(unique(sucesosotrosdel_sel[["prensa"]]))
prensa_sucesosotrosdel_sel


#Poniendo acentos en la leyenda
sucesosotrosdel_sel$tipo_delito <- str_replace_all(sucesosotrosdel_sel$tipo_delito,
                                                         "[^[:alnum:]]"," ")

sucesosotrosdel_sel$tipo_delito <- str_replace_all(sucesosotrosdel_sel$tipo_delito,
                                                         "Agresi n  incluye lesiones graves o leves  etc ","Agresión lesiones graves o leves")
sucesosotrosdel_sel$tipo_delito <- str_replace_all(sucesosotrosdel_sel$tipo_delito,
                                                         "Violaci n sexual","Violación sexual")

sucesosotrosdel_sel$tipo_delito <- str_replace_all(sucesosotrosdel_sel$tipo_delito,
                                                         "Coacci n  incluye extorsi n","Coacción  incluye extorsión")
sucesosotrosdel_sel$tipo_delito <- str_replace_all(sucesosotrosdel_sel$tipo_delito,
                                                         "Amenaza de agresi n","Amenaza de agresión")
sucesosotrosdel_sel$tipo_delito <- str_replace_all(sucesosotrosdel_sel$tipo_delito,
                                                         "Desaparici n forzada","Desaparición forzada")
# sucesosotrosdel_sel$tipo_delito <- str_replace_all(sucesosotrosdel_sel$tipo_delito,
#                                                       "Día de la semana en el d a","Día de la semana en el día")

sucesosotrosdel_sel$prensa<- NULL
sucesosotrosdel_sel

# Data transformation
porcent_grup_sucesosotrosdel <- sucesosotrosdel_sel %>%
  group_by(tipo_delito) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesosotrosdel



#graficando

sucesosotrosdel_barras <- ggplot(porcent_grup_sucesosotrosdel,
                                       aes(x = reorder(tipo_delito, freq), y = freq))+
  geom_col(aes(fill = tipo_delito), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 540),
                      breaks=seq(0,540,40))+
  geom_label(aes(x = tipo_delito, y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesosotrosdel)} ({sum(is.na(sucesosotrosdel$tipo_delito)| sucesosotrosdel$tipo_delito == 'NA'|is.na(sucesosotrosdel$infodelito2)|sucesosotrosdel$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_sucesosotrosdel_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesosotrosdel_barras

