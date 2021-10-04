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
suodelvictimariocseguridad <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,organismo_seguridad_2 ) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional"))

suodelvictimariocseguridad


suodelvictimariocseguridad_sel <- sucesos%>%
  select(infodelito2, prensa, tipo_delito,organismo_seguridad_2 ) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional")&
           !infodelito2 %in% c(NA, "NA")&
           !organismo_seguridad_2  %in% c(NA, "NA"))

suodelvictimariocseguridad_sel

prensa_suodelvictimariocseguridad_sel <- length(unique(suodelvictimariocseguridad_sel[["prensa"]]))
prensa_suodelvictimariocseguridad_sel


#Poniendo acentos en la leyenda
suodelvictimariocseguridad_sel$organismo_seguridad_2  <- str_replace_all(suodelvictimariocseguridad_sel$organismo_seguridad_2 ,
                                                                     "[^[:alnum:]]"," ")

suodelvictimariocseguridad_sel$organismo_seguridad_2  <- str_replace_all(suodelvictimariocseguridad_sel$organismo_seguridad_2 ,
                                                                     "Polic a Estadal","Policía Estadal")
suodelvictimariocseguridad_sel$organismo_seguridad_2  <- str_replace_all(suodelvictimariocseguridad_sel$organismo_seguridad_2 ,
                                                                     "Polic a municipal","Policía municipal")

suodelvictimariocseguridad_sel$organismo_seguridad_2  <- str_replace_all(suodelvictimariocseguridad_sel$organismo_seguridad_2 ,
                                                                     "Relaci n laboral colegas","Relación laboral colegas")
suodelvictimariocseguridad_sel$organismo_seguridad_2  <- str_replace_all(suodelvictimariocseguridad_sel$organismo_seguridad_2 ,
                                                                     "Relaci n de autoridad o cuidado  doctor enfermero policia  etc","Relación de autoridad o cuidado")
suodelvictimariocseguridad_sel$organismo_seguridad_2  <- str_replace_all(suodelvictimariocseguridad_sel$organismo_seguridad_2 ,
                                                                     "Desaparici n forzada","Desaparición forzada")
suodelvictimariocseguridad_sel$organismo_seguridad_2  <- str_replace_all(suodelvictimariocseguridad_sel$organismo_seguridad_2 ,
                                                                     "D a de la semana en la madrugada","Día de la semana en la madrugada")
suodelvictimariocseguridad_sel$organismo_seguridad_2  <- str_replace_all(suodelvictimariocseguridad_sel$organismo_seguridad_2 ,
                                                                     "Día de la semana en el d a","Día de la semana en el día")
suodelvictimariocseguridad_sel$organismo_seguridad_2  <- str_replace_all(suodelvictimariocseguridad_sel$organismo_seguridad_2 ,
                                                                     "Fin de semaan en la noche","Fin de semana en la noche")

suodelvictimariocseguridad_sel$prensa<- NULL
suodelvictimariocseguridad_sel$tipo_delito <- NULL
suodelvictimariocseguridad_sel

# Data transformation
porcent_grup_suodelvictimariocseguridad <- suodelvictimariocseguridad_sel %>%
  group_by(organismo_seguridad_2 ) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_suodelvictimariocseguridad



#graficando

suodelvictimariocseguridad_barras <- ggplot(porcent_grup_suodelvictimariocseguridad,
                                          aes(x = reorder(organismo_seguridad_2 , freq), y = freq))+
  geom_col(aes(fill = organismo_seguridad_2 ), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 23),
                      breaks=seq(0,23,5))+
  geom_label(aes(x = organismo_seguridad_2 , y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(suodelvictimariocseguridad)} ({sum(is.na(suodelvictimariocseguridad$organismo_seguridad_2 )| suodelvictimariocseguridad$organismo_seguridad_2  == 'NA'|is.na(suodelvictimariocseguridad$infodelito2)|suodelvictimariocseguridad$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_suodelvictimariocseguridad_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

suodelvictimariocseguridad_barras

