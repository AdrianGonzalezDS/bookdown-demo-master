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
sucesoshi_cercania <- sucesos %>%
  select(infodelito2, prensa, tipo_delito, cercania_delito) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional")

sucesoshi_cercania


sucesoshi_cercania_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,cercania_delito) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional"& !infodelito2 %in% c(NA, "NA") &
           !cercania_delito %in% c(NA, "NA"))

sucesoshi_cercania_sel

prensa_sucesoshi_cercania_sel <- length(unique(sucesoshi_cercania_sel[["prensa"]]))
prensa_sucesoshi_cercania_sel

sucesoshi_cercania_sel$prensa<- NULL


#Poniendo acentos en la leyenda
sucesoshi_cercania_sel$cercania_delito <- str_replace_all(sucesoshi_cercania_sel$cercania_delito,
                                                    "[^[:alnum:]]"," ")

sucesoshi_cercania_sel$cercania_delito <- str_replace_all(sucesoshi_cercania_sel$cercania_delito,
                                                    "Hogar de la victima","Hogar de la víctima")
sucesoshi_cercania_sel$cercania_delito <- str_replace_all(sucesoshi_cercania_sel$cercania_delito,
                                                    "Entornos de atenci n institucional","Entornos de atención institucional")

# sucesoshi_cercania_sel$cercania_delito <- str_replace_all(sucesoshi_cercania_sel$cercania_delito,
#                                                       "D a de la semana en la noche","D?a de la semana en la noche")
# sucesoshi_cercania_sel$cercania_delito <- str_replace_all(sucesoshi_cercania_sel$cercania_delito,
#                                                       "D a de la semana en la madrugada","D?a de la semana en la madrugada")
# sucesoshi_cercania_sel$cercania_delito <- str_replace_all(sucesoshi_cercania_sel$cercania_delito,
#                                                       "Fin de semana en el d a","Fin de semana en el d?a")
# sucesoshi_cercania_sel$cercania_delito <- str_replace_all(sucesoshi_cercania_sel$cercania_delito,
#                                                       "D?a de la semana en el d a","D?a de la semana en el d?a")


# Data transformation
porcent_grup_sucesoshicercania <- sucesoshi_cercania_sel %>%
  group_by(cercania_delito) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesoshicercania



#graficando

sucesoshicercania_barras <- ggplot(porcent_grup_sucesoshicercania,
                                aes(x = reorder(cercania_delito, freq), y = freq))+
  geom_col(aes(fill = cercania_delito), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 280),
                      breaks=seq(0,280,30))+
  geom_label(aes(x = cercania_delito, y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesoshi_cercania)} ({sum(is.na(sucesoshi_cercania$cercania_delito)| sucesoshi_cercania$cercania_delito == 'NA'|is.na(sucesoshi_cercania$infodelito2)|sucesoshi_cercania$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_sucesoshi_cercania_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesoshicercania_barras
