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
sucesospol <- sucesos %>%
  select(mil,prensa,organanismo_seguridad_1) %>%
  filter(mil == "Si")

sucesospol

sucesospol_sel <- sucesos %>%
  select(mil,prensa,organanismo_seguridad_1) %>%
  filter(mil == "Si" & !mil %in% c(NA, "NA") &
           !organanismo_seguridad_1 %in% c(NA, "NA"))

sucesospol_sel

prensa_sucesospol_sel <- length(unique(sucesospol_sel[["prensa"]]))
prensa_sucesospol_sel

sucesospol_sel$prensa<- NULL

#Poniendo acentos en la leyenda
sucesospol_sel$organanismo_seguridad_1 <- str_replace_all(sucesospol_sel$organanismo_seguridad_1,
                                                           "[^[:alnum:]]"," ")

sucesospol_sel$organanismo_seguridad_1 <- str_replace_all(sucesospol_sel$organanismo_seguridad_1,
                                                           "Polic a Estadal","Policía Estadal")
sucesospol_sel$organanismo_seguridad_1 <- str_replace_all(sucesospol_sel$organanismo_seguridad_1,
                                                           "Polic a Municipal","Policía Municipal")

sucesospol_sel$organanismo_seguridad_1 <- str_replace_all(sucesospol_sel$organanismo_seguridad_1,
                                                           "D a de la semana en la noche","Día de la semana en la noche")
sucesospol_sel$organanismo_seguridad_1 <- str_replace_all(sucesospol_sel$organanismo_seguridad_1,
                                                           "D a de la semana en la madrugada","Día de la semana en la madrugada")
sucesospol_sel$organanismo_seguridad_1 <- str_replace_all(sucesospol_sel$organanismo_seguridad_1,
                                                           "Fin de semana en el d a","Fin de semana en el día")
sucesospol_sel$organanismo_seguridad_1 <- str_replace_all(sucesospol_sel$organanismo_seguridad_1,
                                                           "Día de la semana en el d a","Día de la semana en el día")




# Data transformation
porcent_grup_sucesospol <- sucesospol_sel %>%
  group_by(organanismo_seguridad_1) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  #arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesospol


#graficando

sucesospol_barras <- ggplot(porcent_grup_sucesospol,
                                aes(x = reorder(organanismo_seguridad_1, freq), y = freq))+
  geom_col(aes(fill = organanismo_seguridad_1), position = 'identity') +
  #geom_bar(aes(fill = organanismo_seguridad_1), position = "identity")+
  #geom_text(aes(label = porcentaje), position = position_fill(vjust = 1))+
  theme_classic()+ #scale_fill_brewer(palette = "GnBu")+coord_flip()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.justification = "left",
        axis.text = element_text(size= 10,color='black',
                                 angle=0, vjust = 0.5),
        legend.text = element_text(size = 9 ,
                                   color='black',
                                   angle=0,
                                   vjust = 0.5))+
  coord_flip()+
  scale_y_continuous( limits=c(0, 190),
                      breaks=seq(0,190,20))+
  geom_label(aes(x = organanismo_seguridad_1, y = freq, label = porcentaje),
             hjust = -0.0001,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 4)+
  labs(caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(sucesospol)} ({sum(is.na(sucesospol$organanismo_seguridad_1)| sucesospol$organanismo_seguridad_1 == 'NA')} casos perdidos por información faltante) en {prensa_sucesospol_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de sucesos")

sucesospol_barras




# sucesospol_barras <- ggplot(porcent_grup_sucesospol, aes(x=reorder(freq, organanismo_seguridad_1, function(x) length(x))))+
#   #ggplot(muertes_sel, aes(Estado, ..count..)) +
#   geom_bar(aes(fill = organanismo_seguridad_1), position = "identity")+
#   geom_hline(yintercept = 0, size = 1, colour="#333333") +
#   bbc_style() +
#   scale_y_continuous( limits=c(0, 180),
#                       breaks=seq(0,180,10))+
#   # scale_fill_manual(labels=c('Policial', 'Delincuencial'),
#   #                   values = c("#1380A1", "#FAAB18"))+
#   theme(legend.position="bottom",
#         axis.text = element_text(size= 10,color='black',
#                                  angle=0, vjust = 0.5),
#         legend.text = element_text(size = 9 ,color='black',
#                                    angle=0,vjust = 0.5))+
#   coord_flip()#+
# #facet_grid(cols = vars(Tipo_de_victima), scales="free_y")
#
#
# sucesospol_barras
#
#
# library(tidyverse)
#
# set.seed(1001) # for reproducible example data
# # generate df here as in your question
#
# sucesospol_sel %>%
#   gather(value, Count) %>%
#   ggplot(aes(value, Count)) +
#   geom_boxplot()
#
#
# ggplot(sucesospol_sel, aes(x = organanismo_seguridad_1, y = count)) + geom_bar(stat = "identity")
# plot(DS0012$age.category, DS0012$BMI.category, ylab = "BMI", xlab = "age")
#
# plot(sucesos$organanismo_seguridad_1)
# plot(sucesos$organanismo_seguridad_1, sucesos$numero_victimas_1)
# plot(sucesos$organanismo_seguridad_1, sucesos$numero_victimas_1)
# plot(sucesos$numero_victimas_1)
# plot(sucesos$organanismo_seguridad_1, sucesos$ocurrencia_muerte)
# plot(sucesos$donde_muerte,sucesos$organanismo_seguridad_1)
#
#
# boxplot(chickwts$weight ~ chickwts$feed)
#
# boxplot(numero_victimas_1 ~ organanismo_seguridad_1, data = sucesospol_sel)
#
# par("mar")
# #[1] 5.1 4.1 4.1 2.1
#
# #To change that write:
#
# par(mar=c(1,1,1,1))
#
# ggplot(sucesospol_sel, aes(organanismo_seguridad_1, numero_victimas_1 )) +
#   geom_boxplot()+ theme_classic() #+
#   #stat_summary(
#     # fun.data = stat_box_data,
#     # geom = "text",
#     # hjust = 0.5,
#     # vjust = 0.9) +
#   #theme_fivethirtyeight()
#
#
# ggplot(sucesos, aes(Species, Sepal.Length)) +
#   geom_boxplot() +
#   stat_summary(
#     fun.data = stat_box_data,
#     geom = "text",
#     hjust = 0.5,
#     vjust = 0.9
#   ) +
#   theme_fivethirtyeight()
#
# ggplot(muertes_sel, aes(x=reorder(Estado, Estado, function(x) length(x)))) +
#   geom_bar()
#
# sucesospol_barras <- ggplot(sucesospol_sel, aes(x=reorder(organanismo_seguridad_1, organanismo_seguridad_1, function(x) length(x))))+
#   #ggplot(muertes_sel, aes(Estado, ..count..)) +
#   geom_bar(aes(fill = organanismo_seguridad_1), position = "dodge")+
#   geom_hline(yintercept = 0, size = 1, colour="#333333") +
#   bbc_style() +
#   scale_y_continuous( limits=c(0, 180),
#                       breaks=seq(0,170,10))+
#   # scale_fill_manual(labels=c('Policial', 'Delincuencial'),
#   #                   values = c("#1380A1", "#FAAB18"))+
#   theme(legend.position="bottom",
#         axis.text = element_text(size= 10,color='black',
#                                  angle=0, vjust = 0.5),
#         legend.text = element_text(size = 9 ,color='black',
#                                    angle=0,vjust = 0.5))+
#   coord_flip()#+
# #facet_grid(cols = vars(Tipo_de_victima), scales="free_y")
#
#
# sucesospol_barras
