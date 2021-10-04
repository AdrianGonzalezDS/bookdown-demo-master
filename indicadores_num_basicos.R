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

sucesosmip <- sucesos %>%
  select(mil) %>%
  filter(mil == "Si"&
           !mil %in% c("NA", NA))

sucesosmip

sucesosdelito <- sucesos %>%
  select(mil) %>%
  filter(mil == "No"&
           !mil %in% c("NA", NA))

sucesosdelito

sucesoshi <- sucesos %>%
  select(tipo_delito) %>%
  filter(tipo_delito == "Homicidio intencional"&
           !tipo_delito %in% c(NA, "NA"))

sucesoshi

sucesosthi <- sucesos %>%
  select(tipo_delito) %>%
  filter(tipo_delito == "Tentativa de homicidio intencional"&
           !tipo_delito %in% c(NA, "NA"))

sucesosthi

sucesosodel <- sucesos %>%
  select(tipo_delito) %>%
  filter(!tipo_delito %in% c("Homicidio intencional")&
           !tipo_delito %in% c("Tentativa de homicidio intencional"))

sucesosodel

victimasmip <- sucesos %>%
  select(numero_victimas_1) %>%
  filter(!numero_victimas_1 %in% c(NA, "NA",99))

victimasmip

victimasmip$numero_victimas_1[victimasmip$numero_victimas_1 == 98] <- 10

porcent_grup_victimasmip <- victimasmip %>%
  group_by(numero_victimas_1) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  mutate(tvic = `freq` * numero_victimas_1) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%")) %>%
  adorn_totals(where = "row")

porcent_grup_victimasmip

nmip <- porcent_grup_victimasmip[9, 4]
nmip

##############################################################
victimasdel <- sucesos %>%
  select(numero_victimas_2) %>%
  filter(!numero_victimas_2 %in% c(NA, "NA",99))

victimasdel

victimasdel$numero_victimas_2[victimasdel$numero_victimas_2 == 98] <- 10

porcent_grup_victimasdel <- victimasdel %>%
  group_by(numero_victimas_2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  mutate(tvic = `freq` * numero_victimas_2) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%")) %>%
  adorn_totals(where = "row")

porcent_grup_victimasdel

ndel <- porcent_grup_victimasdel[12, 4]
ndel
#################################################################
vicdelhi <- sucesos %>%
  select(numero_victimas_2, tipo_delito) %>%
  filter(tipo_delito == "Homicidio intencional"&
           !numero_victimas_2 %in% c(NA, "NA",99))

vicdelhi

vicdelhi$numero_victimas_2[vicdelhi$numero_victimas_2 == 98] <- 10

grup_vicdelhi <- vicdelhi %>%
  group_by(numero_victimas_2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  mutate(tvic = `freq` * numero_victimas_2) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%")) %>%
  adorn_totals(where = "row")

grup_vicdelhi

ndelhi <- grup_vicdelhi[7, 5]
ndelhi
##################################################################
vicdelthi <- sucesos %>%
  select(numero_victimas_2, tipo_delito) %>%
  filter(tipo_delito == "Tentativa de homicidio intencional"&
           !numero_victimas_2 %in% c(NA, "NA",99))

vicdelthi

vicdelthi$numero_victimas_2[vicdelthi$numero_victimas_2 == 98] <- 10

grup_vicdelthi <- vicdelthi %>%
  group_by(numero_victimas_2) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  mutate(tvic = `freq` * numero_victimas_2) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%")) %>%
  adorn_totals(where = "row")

grup_vicdelthi

ndelthi <- grup_vicdelthi[8, 5]
ndelthi
