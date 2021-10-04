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
sucesoshi_conquerelacionahi <- sucesos %>%
  select(infodelito2, prensa, tipo_delito, contexto_suceso) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional")

sucesoshi_conquerelacionahi


sucesoshi_conquerelacionahi_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,contexto_suceso) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional"& !infodelito2 %in% c(NA, "NA") &
           !contexto_suceso %in% c(NA, "NA"))

sucesoshi_conquerelacionahi_sel

prensa_sucesoshi_conquerelacionahi_sel <- length(unique(sucesoshi_conquerelacionahi_sel[["prensa"]]))
prensa_sucesoshi_conquerelacionahi_sel


#Poniendo acentos en la leyenda
sucesoshi_conquerelacionahi_sel$contexto_suceso <- str_replace_all(sucesoshi_conquerelacionahi_sel$contexto_suceso,
                                                                                 "[^[:alnum:]]"," ")

sucesoshi_conquerelacionahi_sel$contexto_suceso <- str_replace_all(sucesoshi_conquerelacionahi_sel$contexto_suceso,
                                                                                 "Relacionado con la delincuencia organizada","Delincuencia organizada")
sucesoshi_conquerelacionahi_sel$contexto_suceso <- str_replace_all(sucesoshi_conquerelacionahi_sel$contexto_suceso,
                                                                                 "Relacionado con la pareja o la familia","Pareja o la familia")

sucesoshi_conquerelacionahi_sel$contexto_suceso <- str_replace_all(sucesoshi_conquerelacionahi_sel$contexto_suceso,
                                                                                "Relacionado con pandillas","Pandillas")
sucesoshi_conquerelacionahi_sel$contexto_suceso <- str_replace_all(sucesoshi_conquerelacionahi_sel$contexto_suceso,
                                                       "Relacionado con el terrorismo","Terrorismo")
sucesoshi_conquerelacionahi_sel$contexto_suceso <- str_replace_all(sucesoshi_conquerelacionahi_sel$contexto_suceso,
                                                       "Relacionado con delitos empresariales","Delitos empresariales")
# sucesoshi_conquerelacionahi_sel$contexto_suceso <- str_replace_all(sucesoshi_conquerelacionahi_sel$contexto_suceso,
#                                                       "D?a de la semana en el d a","D?a de la semana en el d?a")

sucesoshi_conquerelacionahi_sel$prensa<- NULL
#sucesoshi_conquerelacionahi_sel$tipo_delito <- NULL
sucesoshi_conquerelacionahi_sel

# Data transformation
porcent_grup_sucesoshiconquerelacionahi <- sucesoshi_conquerelacionahi_sel %>%
  group_by(contexto_suceso) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesoshiconquerelacionahi



#graficando

sucesoshiconquerelacionahi_barras <- ggplot(porcent_grup_sucesoshiconquerelacionahi,
                                                    aes(x = reorder(contexto_suceso, freq), y = freq))+
  geom_col(aes(fill = contexto_suceso), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 170),
                      breaks=seq(0,170,20))+
  geom_label(aes(x = contexto_suceso, y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesoshi_conquerelacionahi)} ({sum(is.na(sucesoshi_conquerelacionahi$contexto_suceso )| sucesoshi_conquerelacionahi$contexto_suceso  == 'NA'|is.na(sucesoshi_conquerelacionahi$infodelito2)|sucesoshi_conquerelacionahi$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_sucesoshi_conquerelacionahi_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesoshiconquerelacionahi_barras

