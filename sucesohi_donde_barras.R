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
sucesoshi_donde <- sucesos %>%
  select(infodelito2, prensa, tipo_delito, donde_delito) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional")

sucesoshi_donde


sucesoshi_donde_sel <- sucesos %>%
  select(infodelito2, prensa, tipo_delito,donde_delito) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional"& !infodelito2 %in% c(NA, "NA") &
           !donde_delito %in% c(NA, "NA"))

sucesoshi_donde_sel

prensa_sucesoshi_donde_sel <- length(unique(sucesoshi_donde_sel[["prensa"]]))
prensa_sucesoshi_donde_sel

sucesoshi_donde_sel$prensa<- NULL


#Poniendo acentos en la leyenda
sucesoshi_donde_sel$donde_delito <- str_replace_all(sucesoshi_donde_sel$donde_delito,
                                                      "[^[:alnum:]]"," ")

sucesoshi_donde_sel$donde_delito <- str_replace_all(sucesoshi_donde_sel$donde_delito,
                                                      "Hogar de la victima","Hogar de la víctima")
sucesoshi_donde_sel$donde_delito <- str_replace_all(sucesoshi_donde_sel$donde_delito,
                                                      "Entornos de atenci n institucional","Entornos de atención institucional")

# sucesoshi_donde_sel$donde_delito <- str_replace_all(sucesoshi_donde_sel$donde_delito,
#                                                       "D a de la semana en la noche","D?a de la semana en la noche")
# sucesoshi_donde_sel$donde_delito <- str_replace_all(sucesoshi_donde_sel$donde_delito,
#                                                       "D a de la semana en la madrugada","D?a de la semana en la madrugada")
# sucesoshi_donde_sel$donde_delito <- str_replace_all(sucesoshi_donde_sel$donde_delito,
#                                                       "Fin de semana en el d a","Fin de semana en el d?a")
# sucesoshi_donde_sel$donde_delito <- str_replace_all(sucesoshi_donde_sel$donde_delito,
#                                                       "D?a de la semana en el d a","D?a de la semana en el d?a")


# Data transformation
porcent_grup_sucesoshidonde <- sucesoshi_donde_sel %>%
  group_by(donde_delito) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesoshidonde



#graficando

sucesoshidonde_barras <- ggplot(porcent_grup_sucesoshidonde,
                                 aes(x = reorder(donde_delito, freq), y = freq))+
  geom_col(aes(fill = donde_delito), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 350),
                      breaks=seq(0,350,30))+
  geom_label(aes(x = donde_delito, y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesoshi_donde)} ({sum(is.na(sucesoshi_donde$donde_delito)| sucesoshi_donde$donde_delito == 'NA')} casos perdidos por información faltante) en {prensa_sucesoshi_donde_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesoshidonde_barras
