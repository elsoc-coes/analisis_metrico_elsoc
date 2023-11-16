library(readxl)
library(expss)
library(tidyverse)
library(vtable)
library(plyr)
library(kableExtra)

#setwd("/Users/gustavoahumada/Dropbox/ELSOC_2/analisis_metrico/Datos ELSOC")
load("./Datos/ELSOC_Long.RData")
lc <- read_xlsx("./Analisis de Correlaciones y Consistencia/Libro de Codigo/0A_Listado_Variables_Global_ELSOC_v2022_v5.xlsx",
                sheet = "2_Items_Total")


ell <- elsoc_long_2016_2022; elsoc_long_2016_2022 <- NULL



ell <- ell %>%
  mutate_all(list(~ifelse(.<0,NA,.))) ## Todos los -999 y -888 pasan a NA.

conceptos_grls <- unique(lc$concepto_general)
conceptos_eps <- unique(lc$Concepto_especifico)

vars <- list()
df <- NULL

for (i in 1:length(conceptos_grls)){
  df <- lc %>% filter(concepto_general == conceptos_grls[i])
  variables <- unique(df$codigo_longitudinal)
  variables.name <- unique(df$etiqueta)
  concepto.esp <- unique(df$Concepto_especifico)
  vars[[i]] <- c(`Nombre de la Variable`= variables.name,
                 `Codigo Longitudinal` = variables,
                 `Concepto Especifico` = concepto.esp)
  names(vars)[i] <- conceptos_grls[i] 
}

# etiqueta de variables
ell = apply_labels(ell,
                   m0_sexo ="Sexo",
                   m0_edad = "Edad",
                   m01 ="Actividad principal",
                   m17 ="Horas/semana cuidado familiar", 
                   m19 ="Sostenedor hogar",
                   m20 ="Educación sostenedor",
                   m27 ="Nivel educacional del padre",
                   m28 ="Nivel educacional de la madre",
                   m32 ="Vehiculos motorizados en hogar",
                   m35 ="Numero de libros en el hogar",
                   m36 ="Estado civil",
                   m41 ="Medio de transporte que utiliza",
                   m48 ="Tiene hijos",
                   m48_edad ="Edad que tuvo primer hijo",
                   m55 ="Apoyo al encontrar trabajo",
                   m56 ="Principal apoyo para encontrar trabajo",
                   m62 ="Acceso a alimentos y suministros en pandemia")

# Se crea una lista similar pero para los conceptos especificos

lista_conceptos_especificos <- list()

for (i in 1:length(conceptos_eps)){
  df <- lc %>% filter(Concepto_especifico == conceptos_eps[i])
  variables <- unique(df$codigo_longitudinal)
  lista_conceptos_especificos[[i]] <- c(`Codigo Longitudinal` = variables)
  names(lista_conceptos_especificos)[i] <- conceptos_eps[i]
}


# Se hace un segundo subset: se crean dos listas, una que contiene aquellos conceptos que tienen una variable,
# y otra lista que contiene a los conceptos que tienen más de una variable

concept.1 <- lista_conceptos_especificos[sapply(lista_conceptos_especificos,length)==1]
concept.2 <- lista_conceptos_especificos[sapply(lista_conceptos_especificos,length)>1] # A partir de aqui, hacer alfas.

# Tabla de resumen para conceptos un solo item

list_vartable <- list()

for(i in 1:length(concept.1)) {
  data <- ell %>%  select(starts_with(c(concept.1[[i]][1], "ola", "muestra"))) %>% 
  mutate_all(list(~ifelse(.<0,NA,.))) %>% 
  select_if(is.numeric) 
  data_subset <- select(data, -c("ola_w01", "ola_w02", "ola_w03", "ola_w04",
                                          "ola_w05", "ola_w06"))
  list_vartable[[i]] <- st(data_subset, group = "ola", out = "return", group.test = FALSE)
  names(list_vartable)[i] <- names(concept.1)[i]
}

df_vartable <- ldply(list_vartable, data.frame)%>% 
  select(-c(".id")) %>% distinct() %>% filter(Variable != "muestra") %>% 
  filter(Variable != "ola") 


df_vartable %>% 
  kbl(caption = "Tabla 1: Estadísticas descriptivas",
      format = "html",
      col.names = c("Variables", "N", "Mean", "SD", "N", "Mean", "SD", "N", "Mean", "SD",
                    "N", "Mean", "SD", "N", "Mean", "SD", "N", "Mean", "SD"),
      align = "lcccccccccccccccccc") %>%
  kable_classic(full_width = F, html_font = "helvetica") %>% 
  add_header_above(c(" " = 1, "Ola 1" = 3, "Ola 2" = 3, "Ola 3" = 3, "Ola 4" = 3,
                     "Ola 5" = 3, "Ola 6" = 3))
  

# Tabla resumen para conceptos con más de un item
list_vartable.2 <- list()

for(i in 1:length(concept.2)) {
  data.2 <- ell %>%  select(starts_with(c(concept.2[[i]][1], "ola", "muestra"))) %>% 
    mutate_all(list(~ifelse(.<0,NA,.))) %>% 
    select_if(is.numeric) 
  data_subset.2 <- select(data.2, -c("ola_w01", "ola_w02", "ola_w03", "ola_w04",
                                 "ola_w05", "ola_w06"))
  list_vartable.2[[i]] <- st(data_subset.2, group = "ola", out = "return", group.test = FALSE)
  names(list_vartable.2)[i] <- names(concept.2)[i]
}

df_vartable.2 <- ldply(list_vartable.2, data.frame)%>% 
  select(-c(".id")) %>% distinct() %>% filter(Variable != "muestra") %>% 
  filter(Variable != "ola") 


df_vartable.2 %>% 
  kbl(caption = "Tabla 1: Estadísticas descriptivas",
      format = "html",
      col.names = c("Variables", "N", "Mean", "SD", "N", "Mean", "SD", "N", "Mean", "SD",
                    "N", "Mean", "SD", "N", "Mean", "SD", "N", "Mean", "SD"),
      align = "lcccccccccccccccccc") %>%
  kable_classic(full_width = F, html_font = "helvetica") %>% 
  add_header_above(c(" " = 1, "Ola 1" = 3, "Ola 2" = 3, "Ola 3" = 3, "Ola 4" = 3,
                     "Ola 5" = 3, "Ola 6" = 3))



