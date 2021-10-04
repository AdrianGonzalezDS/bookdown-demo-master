library(devtools)
library(ggplot2)
library(tidyverse) #requerido para la funcion gather
library(bbplot) #requerido para bbc style
library(plyr) #requerido para hacer gr?ficos de pir?mide
library(dplyr) #requerido para usar la funcion mutate
library(tidyr) #requerido para usar la funcion gather
library(stringr)#requerida para usar str_replace_all
library(janitor)
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

medios <- sucesos %>%
  select(prensa) %>%
  filter(!prensa %in% c("NA", NA))

medios

grup_medios <- medios %>%
  group_by(prensa) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

grup_medios

nmedios <- nrow(grup_medios)
nmedios
###############################################################
delitos <- sucesos %>%
  select(infodelito2) %>%
  filter(infodelito2=="Si" &
           !infodelito2 %in% c("NA", NA))

delitos

ndeli <- nrow(delitos)
ndeli

grup_delitos <- delitos %>%
  group_by(infodelito2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  mutate(tdel = `freq` * infodelito2) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%")) %>%
  adorn_totals(where = "row")

grup_delitos

ndelitos <- grup_delitos[6, 4]
ndelitos
