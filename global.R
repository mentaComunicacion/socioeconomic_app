library(tidyverse)
library(sf)


# Menta Pall
menta <- c("#179E8D","#A7DCDE","#57BEC3","#FFFD38", "#9FE73B", "#E13586","#C226FB","#FC8024")

#Results
resultados <- st_read("data/indice_socioeconomico3.shp") %>% 
  rename(provincia = provinc, depto = dprtmnt) %>% 
  mutate_at(vars(provincia, depto), str_to_title) %>% 
  mutate(scr_mnt = round(scr_mnt, 2)) 

names(st_geometry(resultados)) <- NULL

#Provinces
provincias <- as_data_frame(resultados) %>% 
  transmute(provincia = as.character(provincia)) %>%  
  distinct(provincia) %>%
  arrange(provincia) %>% 
  pull()

#Districts
departamentos <- as_data_frame(resultados) %>% 
  transmute(depto = as.character(depto)) %>%  
  distinct(depto) %>%
  arrange(depto) %>% 
  pull()

#Data explorer table
cleantable <- resultados %>% 
  as_data_frame() %>% 
  select(-geometry)

#Selections
selecciones <- cleantable %>% 
  select(provincia, depto) %>% 
  distinct() %>% 
  arrange(provincia, depto)
