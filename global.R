library(tidyverse)
library(sf)


#Paleta de colores de Menta
menta <- c("#179E8D","#A7DCDE","#57BEC3","#FFFD38", "#9FE73B", "#E13586","#C226FB","#FC8024")

#Resultados
resultados <- st_read("data/indice_socioeconomico3.shp") %>% 
  rename(provincia = provinc, depto = dprtmnt) %>% 
  mutate_at(vars(provincia, depto), str_to_title) %>% 
  mutate(scr_mnt = round(scr_mnt, 2)) 

names(st_geometry(resultados)) <- NULL

#Provincias
provincias <- as_data_frame(resultados) %>% 
  transmute(provincia = as.character(provincia)) %>%  
  distinct(provincia) %>%
  arrange(provincia) %>% 
  pull()

#Departamentos
departamentos <- as_data_frame(resultados) %>% 
  transmute(depto = as.character(depto)) %>%  
  distinct(depto) %>%
  arrange(depto) %>% 
  pull()

#Tabla para explorar datos
cleantable <- resultados %>% 
  as_data_frame() %>% 
  select(-geometry)

#Selecciones
selecciones <- cleantable %>% 
  select(provincia, depto) %>% 
  distinct() %>% 
  arrange(provincia, depto)
