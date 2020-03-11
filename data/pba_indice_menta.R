#Cargo las librerías
library(tidyverse)
library(FactoMineR)
library(gridExtra)
library(sf)
library(tidyimpute)

#Cargo las bases
sociodemo <- read_csv("data/nse_indice_menta.csv")

radios <- st_read("../../radios.shp") %>% 
  rename_all(str_to_lower) %>% 
  st_transform(4326)

radios_caba <- st_read("../../cabaxrdatos.shp") %>% 
  rename_all(str_to_lower) %>% 
  select(-c(perimeter:tipo)) %>% 
  st_transform(4326) %>% 
  st_cast(to = "MULTIPOLYGON")
   
names(radios_caba) <- names(radios)

radios <- rbind(radios, 
                    radios_caba)

#deptos <- st_read("../../pxdptodatos.shp") %>% 
#  mutate(iddpto = str_sub(link, start = 3))

cod_provincias <- read_csv2("data/COD_PROVINCIAS.csv") %>% 
  rename_all(function(x) str_to_lower(str_replace(x, " ", "_"))) %>%
  mutate_at(vars(codigo_provincia), as.integer)
  

cod_deptos <- read_csv2("data/COD_DEPARTAMENTOS.csv") %>%
  rename_all(function(x) str_to_lower(str_replace(x, " ", "_"))) %>%
  mutate_at(vars(codigo_provincia, codigo_departamento), as.integer) %>% 
  left_join(cod_provincias, by = "codigo_provincia") %>% 
  select("codprov" = "codigo_provincia", "coddepto" = "codigo_departamento", 
         "departamento" = "nombre.x", "provincia" =  "nombre.y")

cod_indec_dine <- read_csv("data/codigos_indec_dine.csv")

cod_deptos <- cod_deptos %>% 
  left_join(cod_indec_dine, by = c("codprov", "coddepto")) %>% 
  select(-c(nomdepto_censo, codprov, coddepto)) %>% 
  rename(idprov = codprov_censo, iddpto = coddepto_censo)

#Armo el conjunto de datos preliminar
sociodemo_para_pca <- sociodemo %>% 
  mutate_at(vars(nbi, jefeeduc, inmat, ocup), funs(. / hogares)) %>% 
  mutate(mujeres = mujeres / personas) %>%
  select(-c(idprov:link, hogares, personas)) %>% 
  mutate_if(is.numeric, scale) %>% 
  filter(!is.na(mujeres))

#Hago un análisis de componentes principales
pca <- PCA(sociodemo_para_pca)

#Veo un resumen del PCA
summary(pca)

#Estructuro las cargas (loadings para graficarlas)
pca_loadings <- pca$var$coord %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename_all(function(x) str_to_lower(str_replace(x, "[.]", "_")))

#Grafico las cargas de las dos primeras dimensiones
pca_loadings %>% 
  ggplot() +
  geom_col(aes(rowname, dim_1, fill = "")) +
  scale_fill_manual(values = menta) +
  scale_x_discrete(labels = c("Median age", "Mean age", "% hhs with poor housing conditions", 
                              "% hhs with householder who has at least a high school degree", "% women",  "% of hhs with at least one Unmet Basic Needs (NBI)", 
                              "% of hhs with employed householder")
                   ) +
  lims(y = c(- 1, 1)) +
  labs(title = "Socioeconomic Index",
    subtitle = "Primary Dimension",
       y = "",
       x = "",
    fill = "") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

dev.off() 

ggsave("pca_dim1.png",width = 30, units = "cm")

pca_loadings %>% 
  ggplot() +
  geom_col(aes(rowname, dim_2, fill = "")) +
  scale_fill_manual(values = menta) +
  scale_x_discrete(labels = c("Median age", "Mean age", "% hhs with poor housing conditions", 
                              "% hhs with householder who has at least a high school degree", "% women",  "% of hhs with at least one Unmet Basic Needs (NBI)", 
                              "% of hhs with employed householder")
  ) +
  lims(y = c(- 1, 1)) +
  labs(title = "Socioeconomic Index",
       subtitle = "Secondary Dimension",
       y = "",
       x = "",
       fill = "") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

dev.off()  
ggsave("pca_dim2.png",width = 30, units = "cm")

grid.arrange(plot_pca_dim1, plot_pca_dim2, ncol = 2, 
             top = "Características del Índice Socioeconómico Menta")

#Agrego los scores a la base
score_menta_vector <- pca$ind$coord %>% 
  as_tibble() %>% 
  select(score_menta = `Dim.1`) %>% 
  mutate(score_menta = ((score_menta - min(score_menta)) / 
                          (max(score_menta) - min(score_menta))) * 5)
  

score_menta_vector_2 <- pca$ind$coord %>% 
  as_tibble() %>% 
  select(score_menta = `Dim.2`) %>% 
  transmute(score_menta_2 = ((score_menta - min(score_menta)) / 
                          (max(score_menta) - min(score_menta))) * 5)


base_scores <- bind_cols(sociodemo %>%
            select(idprov, iddpto, idfrac, idradio, link), 
          score_menta_vector,
          score_menta_vector_2)

#Agrego geometrías a la base
base_con_geometrias <-  radios %>%
  left_join(base_scores, by = "link")

#Chequeo datos faltantes por departamento
base_con_geometrias %>%
  filter(is.na(score_menta)) %>%
  mutate(depto = str_sub(link, end = 2)) %>% 
  count(depto) %>% 
  arrange(desc(n))

#Existen 48 polígonos con datos faltantes. 
# Imputemos por la mediana del departamento
base_georref <- base_con_geometrias %>% 
  mutate(idprov = ifelse(is.na(score_menta), 
                         as.integer(str_sub(link, end = 2)), idprov),
         iddpto = ifelse(is.na(score_menta), 
                         as.integer(str_sub(link, start =  3, end = 5)), iddpto)) %>% 
  arrange(score_menta) %>% 
  group_by(iddpto) %>% 
  do(impute_median(., score_menta)) %>% 
  ungroup() %>% 
  inner_join(cod_deptos, by = c("idprov", "iddpto")) %>% 
  st_as_sf(crs = 4326)

st_write(base_georref, "indice_socioeconomico.shp")
