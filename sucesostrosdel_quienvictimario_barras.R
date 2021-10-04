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
suodelquienvictimario <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,relacion_victimario ) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional"))

suodelquienvictimario


suodelquienvictimario_sel <- sucesos%>%
  select(infodelito2, prensa, tipo_delito,relacion_victimario ) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional")&
           !infodelito2 %in% c(NA, "NA")&
           !relacion_victimario  %in% c(NA, "NA"))

suodelquienvictimario_sel

prensa_suodelquienvictimario_sel <- length(unique(suodelquienvictimario_sel[["prensa"]]))
prensa_suodelquienvictimario_sel


#Poniendo acentos en la leyenda
suodelquienvictimario_sel$relacion_victimario  <- str_replace_all(suodelquienvictimario_sel$relacion_victimario ,
                                                    "[^[:alnum:]]"," ")

suodelquienvictimario_sel$relacion_victimario  <- str_replace_all(suodelquienvictimario_sel$relacion_victimario ,
                                                    "Delincuencia com n","Delincuencia común")
suodelquienvictimario_sel$relacion_victimario  <- str_replace_all(suodelquienvictimario_sel$relacion_victimario ,
                                                    "Da de la semana en el d a","Día de la semana en el día")

suodelquienvictimario_sel$relacion_victimario  <- str_replace_all(suodelquienvictimario_sel$relacion_victimario ,
                                                    "D a de la semana en la noche","Día de la semana en la noche")
suodelquienvictimario_sel$relacion_victimario  <- str_replace_all(suodelquienvictimario_sel$relacion_victimario ,
                                                    "Fin de semana en el d a","Fin de semana en el día")
suodelquienvictimario_sel$relacion_victimario  <- str_replace_all(suodelquienvictimario_sel$relacion_victimario ,
                                                    "Desaparici n forzada","Desaparición forzada")
suodelquienvictimario_sel$relacion_victimario  <- str_replace_all(suodelquienvictimario_sel$relacion_victimario ,
                                                    "D a de la semana en la madrugada","Día de la semana en la madrugada")
suodelquienvictimario_sel$relacion_victimario  <- str_replace_all(suodelquienvictimario_sel$relacion_victimario ,
                                                    "Día de la semana en el d a","Día de la semana en el día")
suodelquienvictimario_sel$relacion_victimario  <- str_replace_all(suodelquienvictimario_sel$relacion_victimario ,
                                                    "Fin de semaan en la noche","Fin de semana en la noche")

suodelquienvictimario_sel$prensa<- NULL
suodelquienvictimario_sel$tipo_delito <- NULL
suodelquienvictimario_sel

# Data transformation
porcent_grup_suodelquienvictimario <- suodelquienvictimario_sel %>%
  group_by(relacion_victimario ) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_suodelquienvictimario



#graficando

suodelquienvictimario_barras <- ggplot(porcent_grup_suodelquienvictimario,
                                aes(x = reorder(relacion_victimario , freq), y = freq))+
  geom_col(aes(fill = relacion_victimario ), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 640),
                      breaks=seq(0,640,40))+
  geom_label(aes(x = relacion_victimario , y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(suodelquienvictimario)} ({sum(is.na(suodelquienvictimario$relacion_victimario )| suodelquienvictimario$relacion_victimario  == 'NA'|is.na(suodelquienvictimario$infodelito2)|suodelquienvictimario$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_suodelquienvictimario_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

suodelquienvictimario_barras

