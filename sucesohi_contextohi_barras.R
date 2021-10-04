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
sucesoshi_contextohi <- sucesos %>%
  select(infodelito2, prensa,tipo_delito, contexto_homicidio) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional")

sucesoshi_contextohi


sucesoshi_contextohi_sel <- sucesos %>%
  select(infodelito2, prensa,tipo_delito,contexto_homicidio) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional"& !infodelito2 %in% c(NA, "NA") &
           !contexto_homicidio %in% c(NA, "NA"))

sucesoshi_contextohi_sel

prensa_sucesoshi_contextohi_sel <- length(unique(sucesoshi_contextohi_sel[["prensa"]]))
prensa_sucesoshi_contextohi_sel

sucesoshi_contextohi_sel$prensa<- NULL


#Poniendo acentos en la leyenda
sucesoshi_contextohi_sel$contexto_homicidio <- str_replace_all(sucesoshi_contextohi_sel$contexto_homicidio,
                                                                   "[^[:alnum:]]"," ")

sucesoshi_contextohi_sel$contexto_homicidio <- str_replace_all(sucesoshi_contextohi_sel$contexto_homicidio,
                                                                   "Homicidio cometido durante la comisi n de otro delito","Comisión de otro delito")
sucesoshi_contextohi_sel$contexto_homicidio <- str_replace_all(sucesoshi_contextohi_sel$contexto_homicidio,
                                                                   "Homicidio relacionado con grupos delictivos organizados","Grupos delictivos organizados")

sucesoshi_contextohi_sel$contexto_homicidio <- str_replace_all(sucesoshi_contextohi_sel$contexto_homicidio,
                                                                   "Homicidio relacionado con prejuicios sociales","Prejuicios sociales")
 sucesoshi_contextohi_sel$contexto_homicidio <- str_replace_all(sucesoshi_contextohi_sel$contexto_homicidio,
                                                       "Homicidio relacionado con disturbios civiles","Disturbios civiles")
# sucesoshi_contextohi_sel$contexto_homicidio <- str_replace_all(sucesoshi_contextohi_sel$contexto_homicidio,
#                                                       "Fin de semana en el d a","Fin de semana en el d?a")
# sucesoshi_contextohi_sel$contexto_homicidio <- str_replace_all(sucesoshi_contextohi_sel$contexto_homicidio,
#                                                       "D?a de la semana en el d a","D?a de la semana en el d?a")


# Data transformation
porcent_grup_sucesoshicontextohi <- sucesoshi_contextohi_sel %>%
  group_by(contexto_homicidio) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesoshicontextohi



#graficando

sucesoshicontextohi_barras <- ggplot(porcent_grup_sucesoshicontextohi,
                                            aes(x = reorder(contexto_homicidio, freq), y = freq))+
  geom_col(aes(fill = contexto_homicidio), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 120),
                      breaks=seq(0,120,10))+
  geom_label(aes(x = contexto_homicidio, y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesoshi_contextohi)} ({sum(is.na(sucesoshi_contextohi$contexto_homicidio)| sucesoshi_contextohi$contexto_homicidio == 'NA'|is.na(sucesoshi_contextohi$infodelito2)|sucesoshi_contextohi$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_sucesoshi_contextohi_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesoshicontextohi_barras

