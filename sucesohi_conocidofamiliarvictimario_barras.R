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
sucesoshi_conocidofamiliarvictimario <- sucesos %>%
  select(infodelito2, prensa, tipo_delito, victimario_conocido) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional")

sucesoshi_conocidofamiliarvictimario


sucesoshi_conocidofamiliarvictimario_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,victimario_conocido) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional"& !infodelito2 %in% c(NA, "NA") &
           !victimario_conocido %in% c(NA, "NA"))

sucesoshi_conocidofamiliarvictimario_sel

prensa_sucesoshi_conocidofamiliarvictimario_sel <- length(unique(sucesoshi_conocidofamiliarvictimario_sel[["prensa"]]))
prensa_sucesoshi_conocidofamiliarvictimario_sel

sucesoshi_conocidofamiliarvictimario_sel$prensa<- NULL


#Poniendo acentos en la leyenda
sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido <- str_replace_all(sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido,
                                                                     "[^[:alnum:]]"," ")

sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido <- str_replace_all(sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido,
                                                                     "Otro transgresor conocido por la victima","Conocido por la vcítima")
sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido <- str_replace_all(sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido,
                                                                     "Relaci n laboral colegas","Relación laboral colegas")

 sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido <- str_replace_all(sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido,
                                                       "Relaci n de autoridad o cuidado  doctor enfermero policia  etc","Relación de autoridad")
# sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido <- str_replace_all(sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido,
#                                                       "D a de la semana en la madrugada","D?a de la semana en la madrugada")
# sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido <- str_replace_all(sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido,
#                                                       "Fin de semana en el d a","Fin de semana en el d?a")
# sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido <- str_replace_all(sucesoshi_conocidofamiliarvictimario_sel$victimario_conocido,
#                                                       "D?a de la semana en el d a","D?a de la semana en el d?a")


# Data transformation
porcent_grup_sucesoshiconocidofamiliarvictimario <- sucesoshi_conocidofamiliarvictimario_sel %>%
  group_by(victimario_conocido) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesoshiconocidofamiliarvictimario



#graficando

sucesoshiconocidofamiliarvictimario_barras <- ggplot(porcent_grup_sucesoshiconocidofamiliarvictimario,
                                          aes(x = reorder(victimario_conocido, freq), y = freq))+
  geom_col(aes(fill = victimario_conocido), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 40),
                      breaks=seq(0,40,10))+
  geom_label(aes(x = victimario_conocido, y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesoshi_conocidofamiliarvictimario)} ({sum(is.na(sucesoshi_conocidofamiliarvictimario$victimario_conocido)| sucesoshi_conocidofamiliarvictimario$victimario_conocido == 'NA'|is.na(sucesoshi_conocidofamiliarvictimario$infodelito2)|sucesoshi_conocidofamiliarvictimario$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_sucesoshi_conocidofamiliarvictimario_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesoshiconocidofamiliarvictimario_barras
