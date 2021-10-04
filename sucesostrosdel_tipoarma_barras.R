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
suodeltipoarma <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,arma_delito1 ) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional"))

suodeltipoarma


suodeltipoarma_sel <- sucesos%>%
  select(infodelito2, prensa, tipo_delito,arma_delito1 ) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional")&
           !infodelito2 %in% c(NA, "NA")&
           !arma_delito1  %in% c(NA, "NA"))

suodeltipoarma_sel

prensa_suodeltipoarma_sel <- length(unique(suodeltipoarma_sel[["prensa"]]))
prensa_suodeltipoarma_sel


#Poniendo acentos en la leyenda
suodeltipoarma_sel$arma_delito1  <- str_replace_all(suodeltipoarma_sel$arma_delito1 ,
                                                               "[^[:alnum:]]"," ")

suodeltipoarma_sel$arma_delito1  <- str_replace_all(suodeltipoarma_sel$arma_delito1 ,
                                                               "Fuerza f sica","Fuerza física")
suodeltipoarma_sel$arma_delito1  <- str_replace_all(suodeltipoarma_sel$arma_delito1 ,
                                                               "Da de la semana en el d a","Día de la semana en el día")

suodeltipoarma_sel$arma_delito1  <- str_replace_all(suodeltipoarma_sel$arma_delito1 ,
                                                               "D a de la semana en la noche","Día de la semana en la noche")
suodeltipoarma_sel$arma_delito1  <- str_replace_all(suodeltipoarma_sel$arma_delito1 ,
                                                               "Fin de semana en el d a","Fin de semana en el día")
suodeltipoarma_sel$arma_delito1  <- str_replace_all(suodeltipoarma_sel$arma_delito1 ,
                                                               "Desaparici n forzada","Desaparición forzada")
suodeltipoarma_sel$arma_delito1  <- str_replace_all(suodeltipoarma_sel$arma_delito1 ,
                                                               "D a de la semana en la madrugada","Día de la semana en la madrugada")
suodeltipoarma_sel$arma_delito1  <- str_replace_all(suodeltipoarma_sel$arma_delito1 ,
                                                               "Día de la semana en el d a","Día de la semana en el día")
suodeltipoarma_sel$arma_delito1  <- str_replace_all(suodeltipoarma_sel$arma_delito1 ,
                                                               "Fin de semaan en la noche","Fin de semana en la noche")

suodeltipoarma_sel$prensa<- NULL
suodeltipoarma_sel$tipo_delito <- NULL
suodeltipoarma_sel

# Data transformation
porcent_grup_suodeltipoarma <- suodeltipoarma_sel %>%
  group_by(arma_delito1 ) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_suodeltipoarma



#graficando

suodeltipoarma_barras <- ggplot(porcent_grup_suodeltipoarma,
                                         aes(x = reorder(arma_delito1 , freq), y = freq))+
  geom_col(aes(fill = arma_delito1 ), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 520),
                      breaks=seq(0,520,40))+
  geom_label(aes(x = arma_delito1 , y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(suodeltipoarma)} ({sum(is.na(suodeltipoarma$arma_delito1 )| suodeltipoarma$arma_delito1  == 'NA'|is.na(suodeltipoarma$infodelito2)|suodeltipoarma$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_suodeltipoarma_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

suodeltipoarma_barras

