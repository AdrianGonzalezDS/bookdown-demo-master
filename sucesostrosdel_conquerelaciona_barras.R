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
suodelconquerelaciona <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,contexto_suceso, presuncion ) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional"))

suodelconquerelaciona


suodelconquerelaciona_sel <- sucesos%>%
  select(infodelito2, prensa, tipo_delito,contexto_suceso ) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional")&
           !infodelito2 %in% c(NA, "NA")&
           !contexto_suceso  %in% c(NA, "NA"))

suodelconquerelaciona_sel

prensa_suodelconquerelaciona_sel <- length(unique(suodelconquerelaciona_sel[["prensa"]]))
prensa_suodelconquerelaciona_sel


#Poniendo acentos en la leyenda
suodelconquerelaciona_sel$contexto_suceso  <- str_replace_all(suodelconquerelaciona_sel$contexto_suceso ,
                                                                         "[^[:alnum:]]"," ")

suodelconquerelaciona_sel$contexto_suceso  <- str_replace_all(suodelconquerelaciona_sel$contexto_suceso ,
                                                                         "Relacionado con la pareja o la familia","Pareja o la familia")
suodelconquerelaciona_sel$contexto_suceso  <- str_replace_all(suodelconquerelaciona_sel$contexto_suceso ,
                                                                         "Relacionado con pandillas","Pandillas")

suodelconquerelaciona_sel$contexto_suceso  <- str_replace_all(suodelconquerelaciona_sel$contexto_suceso ,
                                                                         "Relaci n laboral colegas","Relación laboral colegas")
suodelconquerelaciona_sel$contexto_suceso  <- str_replace_all(suodelconquerelaciona_sel$contexto_suceso ,
                                                                         "Relaci n de autoridad o cuidado  doctor enfermero policia  etc","Relación de autoridad o cuidado")
suodelconquerelaciona_sel$contexto_suceso  <- str_replace_all(suodelconquerelaciona_sel$contexto_suceso ,
                                                                         "Relacionado con la delincuencia organizada","Delincuencia organizada")
suodelconquerelaciona_sel$contexto_suceso  <- str_replace_all(suodelconquerelaciona_sel$contexto_suceso ,
                                                                         "Relacionado con delitos empresariales","Delitos empresariales")
suodelconquerelaciona_sel$contexto_suceso  <- str_replace_all(suodelconquerelaciona_sel$contexto_suceso ,
                                                                         "Día de la semana en el d a","Día de la semana en el día")
suodelconquerelaciona_sel$contexto_suceso  <- str_replace_all(suodelconquerelaciona_sel$contexto_suceso ,
                                                                         "Relacionado con el terrorismo","Terrorismo")

suodelconquerelaciona_sel$prensa<- NULL
suodelconquerelaciona_sel$tipo_delito <- NULL
suodelconquerelaciona_sel

# Data transformation
porcent_grup_suodelconquerelaciona <- suodelconquerelaciona_sel %>%
  group_by(contexto_suceso ) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_suodelconquerelaciona



#graficando

suodelconquerelaciona_barras <- ggplot(porcent_grup_suodelconquerelaciona,
                                            aes(x = reorder(contexto_suceso , freq), y = freq))+
  geom_col(aes(fill = contexto_suceso ), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 500),
                      breaks=seq(0,500,40))+
  geom_label(aes(x = contexto_suceso , y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(suodelconquerelaciona)} ({sum(is.na(suodelconquerelaciona$contexto_suceso )| suodelconquerelaciona$contexto_suceso  == 'NA'|is.na(suodelconquerelaciona$infodelito2)|suodelconquerelaciona$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_suodelconquerelaciona_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

suodelconquerelaciona_barras

