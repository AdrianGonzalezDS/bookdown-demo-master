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
sucesosotrosdelcuando <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,cuando_delito) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional"))

sucesosotrosdelcuando


sucesosotrosdelcuando_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,cuando_delito) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional")&
           !infodelito2 %in% c(NA, "NA")&
           !cuando_delito %in% c(NA, "NA"))

sucesosotrosdelcuando_sel

prensa_sucesosotrosdelcuando_sel <- length(unique(sucesosotrosdelcuando_sel[["prensa"]]))
prensa_sucesosotrosdelcuando_sel


#Poniendo acentos en la leyenda
sucesosotrosdelcuando_sel$cuando_delito <- str_replace_all(sucesosotrosdelcuando_sel$cuando_delito,
                                                   "[^[:alnum:]]"," ")

sucesosotrosdelcuando_sel$cuando_delito <- str_replace_all(sucesosotrosdelcuando_sel$cuando_delito,
                                                   "D a de la semana","Día de la semana")
sucesosotrosdelcuando_sel$cuando_delito <- str_replace_all(sucesosotrosdelcuando_sel$cuando_delito,
                                                   "Da de la semana en el d a","Día de la semana en el día")

sucesosotrosdelcuando_sel$cuando_delito <- str_replace_all(sucesosotrosdelcuando_sel$cuando_delito,
                                                   "D a de la semana en la noche","Día de la semana en la noche")
sucesosotrosdelcuando_sel$cuando_delito <- str_replace_all(sucesosotrosdelcuando_sel$cuando_delito,
                                                   "Fin de semana en el d a","Fin de semana en el día")
sucesosotrosdelcuando_sel$cuando_delito <- str_replace_all(sucesosotrosdelcuando_sel$cuando_delito,
                                                   "Desaparici n forzada","Desaparición forzada")
 sucesosotrosdelcuando_sel$cuando_delito <- str_replace_all(sucesosotrosdelcuando_sel$cuando_delito,
                                                       "D a de la semana en la madrugada","Día de la semana en la madrugada")
 sucesosotrosdelcuando_sel$cuando_delito <- str_replace_all(sucesosotrosdelcuando_sel$cuando_delito,
                                                            "Día de la semana en el d a","Día de la semana en el día")
 sucesosotrosdelcuando_sel$cuando_delito <- str_replace_all(sucesosotrosdelcuando_sel$cuando_delito,
                                                            "Fin de semaan en la noche","Fin de semana en la noche")

sucesosotrosdelcuando_sel$prensa<- NULL
sucesosotrosdelcuando_sel$tipo_delito <- NULL
sucesosotrosdelcuando_sel

# Data transformation
porcent_grup_sucesosotrosdelcuando <- sucesosotrosdelcuando_sel %>%
  group_by(cuando_delito) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesosotrosdelcuando



#graficando

sucesosotrosdelcuando_barras <- ggplot(porcent_grup_sucesosotrosdelcuando,
                                 aes(x = reorder(cuando_delito, freq), y = freq))+
  geom_col(aes(fill = cuando_delito), position = 'identity') +
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
  geom_label(aes(x = cuando_delito, y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesosotrosdelcuando)} ({sum(is.na(sucesosotrosdelcuando$cuando_delito)| sucesosotrosdelcuando$cuando_delito == 'NA'|is.na(sucesosotrosdelcuando$infodelito2)|sucesosotrosdelcuando$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_sucesosotrosdelcuando_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesosotrosdelcuando_barras

