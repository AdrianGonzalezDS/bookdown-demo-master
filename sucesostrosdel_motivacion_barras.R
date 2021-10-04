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
suodelmotivacion <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,motivacion) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional"))

suodelmotivacion


suodelmotivacion_sel <- sucesos%>%
  select(infodelito2, prensa, tipo_delito,motivacion ) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional")&
           !infodelito2 %in% c(NA, "NA")&
           !motivacion  %in% c(NA, "NA"))

suodelmotivacion_sel

prensa_suodelmotivacion_sel <- length(unique(suodelmotivacion_sel[["prensa"]]))
prensa_suodelmotivacion_sel


#Poniendo acentos en la leyenda
suodelmotivacion_sel$motivacion  <- str_replace_all(suodelmotivacion_sel$motivacion ,
                                                              "[^[:alnum:]]"," ")

suodelmotivacion_sel$motivacion  <- str_replace_all(suodelmotivacion_sel$motivacion ,
                                                              "Provecho Il cito","Provecho Ilícito")
suodelmotivacion_sel$motivacion  <- str_replace_all(suodelmotivacion_sel$motivacion ,
                                                              "Basado en el g nero","Basado en el género")

suodelmotivacion_sel$motivacion  <- str_replace_all(suodelmotivacion_sel$motivacion ,
                                                              "Otra motivaci n","Otra motivación")
suodelmotivacion_sel$motivacion  <- str_replace_all(suodelmotivacion_sel$motivacion ,
                                                              "Ri a","Riña")
suodelmotivacion_sel$motivacion  <- str_replace_all(suodelmotivacion_sel$motivacion ,
                                                              "Conflicto interpersonal distinto de ri a y venganza","Conflicto distinto de riña y venganza")
suodelmotivacion_sel$motivacion  <- str_replace_all(suodelmotivacion_sel$motivacion ,
                                                              "Intereses pol ticos","Intereses políticos")
suodelmotivacion_sel$motivacion  <- str_replace_all(suodelmotivacion_sel$motivacion ,
                                                              "Día de la semana en el d a","Día de la semana en el día")
suodelmotivacion_sel$motivacion  <- str_replace_all(suodelmotivacion_sel$motivacion ,
                                                              "Relacionado con el terrorismo","Terrorismo")

suodelmotivacion_sel$prensa<- NULL
suodelmotivacion_sel$tipo_delito <- NULL
suodelmotivacion_sel

# Data transformation
porcent_grup_suodelmotivacion <- suodelmotivacion_sel %>%
  group_by(motivacion ) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_suodelmotivacion



#graficando

suodelmotivacion_barras <- ggplot(porcent_grup_suodelmotivacion,
                                       aes(x = reorder(motivacion , freq), y = freq))+
  geom_col(aes(fill = motivacion ), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 600),
                      breaks=seq(0,600,50))+
  geom_label(aes(x = motivacion , y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(suodelmotivacion)} ({sum(is.na(suodelmotivacion$motivacion )| suodelmotivacion$motivacion  == 'NA'|is.na(suodelmotivacion$infodelito2)|suodelmotivacion$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_suodelmotivacion_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

suodelmotivacion_barras

