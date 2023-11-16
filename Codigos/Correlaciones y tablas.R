# Correlaciones y tablas

library(readxl)
library(tidyverse)
library(psych)

#setwd("/Users/gustavoahumada/Dropbox/ELSOC_2/analisis_metrico/Datos ELSOC")
load("./Datos/ELSOC_Wide.RData")
lc <- read_xlsx("./Libro de Codigo/0A_Listado_Variables_Global_ELSOC_v2022_v4.xlsx",
                sheet = "2_Items_Total")

el <- elsoc_wide_2016_2022; elsoc_wide_2016_2022 <- NULL
el <- el %>% rename("m48_hijos_w02" = "m48_w02",
                    "m48_hijos_w04" = "m48_w04")

el <- el %>%
  select_if(~is.numeric(.)) %>%
  mutate_all(list(~ifelse(.<0,NA,.))) %>% ## Todos los -999 y -888 pasan a NA.
  mutate(
    # Izquierda y derecha
    across(starts_with("c15"),~ifelse(. %in% c(11, 12),11,.))) 


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

# Se crea una lista similar pero para los conceptos especificos

lista_conceptos_especificos <- list()

for (i in 1:length(conceptos_eps)){
  df <- lc %>% filter(Concepto_especifico == conceptos_eps[i])
  variables <- unique(df$codigo_longitudinal)
  
  lista_conceptos_especificos[[i]] <- c(`Codigo Longitudinal` = variables)
  names(lista_conceptos_especificos)[i] <- conceptos_eps[i]
}


# Se hace un subset de la lista anterior.

vars.subset <- vars[setdiff(names(vars), c("Características encuestador/a",
                                           "Tabla Kish/Registro del Hogar",
                                           "Características entrevista",
                                           "Identificación de Casos y Datos",
                                           "Diseño Muestral del Estudio",
                                           "Autorizacion profundizacion estudio",
                                           "Acuerdo metodo seguimiento",
                                           "RUT"))]

lista_conceptos_especificos <- lista_conceptos_especificos[setdiff(names(lista_conceptos_especificos),
                                                                   c("Características encuestador/a",
                                                                     "Tabla Kish/Registro del Hogar",
                                                                     "Características entrevista",
                                                                     "Identificación de Casos y Datos",
                                                                     "Diseño Muestral del Estudio",
                                                                     "Autorizacion profundizacion estudio",
                                                                     "Acuerdo metodo seguimiento",
                                                                     "RUT",
                                                                     "Fecha entrevista",
                                                                     "Duración entrevista",
                                                                     "Supervisión entrevista"))]


# Se hace un segundo subset: se crean dos listas, una que contiene aquellos conceptos que tienen una variable,
# y otra lista que contiene a los conceptos que tienen más de una variable

concept.1 <- lista_conceptos_especificos[sapply(lista_conceptos_especificos,length)==1]
concept.2 <- lista_conceptos_especificos[sapply(lista_conceptos_especificos,length)>1] # A partir de aqui, hacer alfas.
# Y calcular las medias. Luego estas medias,
# Hacer correlaciones inter-olas.
# Excluir aquellas variables que no refieran a un
# concepto (por ejemplo, identificación política)

length(concept.1)+length(concept.2)==length(lista_conceptos_especificos) #para comprobar que el subset esté bien hecho

cors.full <- list()
cors.full.1 <- list()
cors.full.2 <- list()

for (i in 1:length(lista_conceptos_especificos)){
  data <- el %>%  select(starts_with(c(lista_conceptos_especificos[[i]][1],"muestra"))) %>% 
    mutate_all(funs(ifelse(.<0,NA,.)))
  
  # Se crean df según muestras
  muestra.1 <- data %>% filter(muestra==1) %>% select(-muestra) %>% 
    select_if(colSums(is.na(.))<nrow(.))
  muestra.2 <- data %>% filter(muestra==2) %>% select(-muestra) %>% 
    select_if(colSums(is.na(.))<nrow(.))
  data <- data %>% select(-muestra)
  
  # se seleccionan solamente aquellos data que tengan más de una ola (se consideran ambas muestras juntas):
  if (length(data)>1){
    cors.full[[i]] <- cor(data,use="pairwise.complete.obs")
  } else{
    cors.full[[i]] <- "No aplica. Variable medida solamente en una ola."
  }
  names(cors.full)[i] <- names(lista_conceptos_especificos)[i]
  # se calculan correlaciones inter-ola por muestra
  ## Muestra 1
  if (length(muestra.1)>1){
    cors.full.1[[i]] <- cor(muestra.1,use="complete.obs")
  } else{
    cors.full.1[[i]] <- "No aplica. Variable medida solamente en una ola."
  }
  names(cors.full.1)[i] <- names(lista_conceptos_especificos)[i]
  ## Muestra 2
  if (length(muestra.2)>1){
    cors.full.2[[i]] <- cor(muestra.2,use="complete.obs")
  } else{
    cors.full.2[[i]] <- "No aplica. Variable medida solamente en una ola."
  }
  names(cors.full.2)[i] <- names(lista_conceptos_especificos)[i]
}


#### Tablas de sociodemograficos

# Muestra 1 y 2

knitr::kable(cors.full[["sexo"]],booktabs=T,
             caption="Correlación Inter-Olas de Sexo (Muestra 1 y 2)")

knitr::kable(cors.full[["edad"]],booktabs=T,
             caption="Correlación Inter-Olas de Edad (Muestra 1 y 2)")

knitr::kable(cors.full[["Educación"]],booktabs=T,
             caption="Correlación Inter-Olas de Educación (Muestra 1 y 2)")

knitr::kable(cors.full[["Educación"]],booktabs=T,
             caption="Correlación Inter-Olas de Educación (Muestra 1 y 2)")

knitr::kable(cors.full[["Ingresos"]],booktabs=T,
             caption="Correlación Inter-Olas de Ingreso (Muestra 1 y 2)")

knitr::kable(cors.full[["Horas destinadas al cuidado familiar"]],booktabs=T,
             caption="Correlación Inter-Olas de Cuidado Familiar (Muestra 1 y 2)")

knitr::kable(cors.full[["Previsión social"]],booktabs=T,
             caption="Correlación Inter-Olas de Previsión social (Muestra 1 y 2)")

knitr::kable(cors.full[["Sostenedor hogar"]],booktabs=T,
             caption="Correlación Inter-Olas de Sostenedor del hogar (Muestra 1 y 2)")

knitr::kable(cors.full[["Educación sostenedor"]],booktabs=T,
             caption="Correlación Inter-Olas de Educación del sostenedor del hogar (Muestra 1 y 2)")

knitr::kable(cors.full[["Clase y Estatus sostenedor"]],booktabs=T,
             caption="Correlación Inter-Olas de Clase y Estatus sostenedor (Muestra 1 y 2)")

knitr::kable(cors.full[["Ingreso familiar"]],booktabs=T,
             caption="Correlación Inter-Olas de Ingreso Familiar (Muestra 1 y 2)")

knitr::kable(cors.full[["Bienes 2"]],booktabs=T,
             caption="Correlación Inter-Olas de Bienes en el Hogar (Muestra 1 y 2)")

knitr::kable(cors.full[["Caracterización vivienda"]],booktabs=T,
             caption="Correlación Inter-Olas de Caracterización Vivienda (Muestra 1 y 2)")

knitr::kable(cors.full[["Tiempo residencia"]],booktabs=T,
             caption="Correlación Inter-Olas de Tiempo de residencia (Muestra 1 y 2)")

knitr::kable(cors.full[["Libros en hogar"]],booktabs=T,
             caption="Correlación Inter-Olas de Libros en el hogar (Muestra 1 y 2)")

knitr::kable(cors.full[["Estado civil"]],booktabs=T,
             caption="Correlación Inter-Olas de Estado civil (Muestra 1 y 2)")

knitr::kable(cors.full[["Religion"]],booktabs=T,
             caption="Correlación Inter-Olas de Religion (Muestra 1 y 2)")

knitr::kable(cors.full[["Transporte"]],booktabs=T,
             caption="Correlación Inter-Olas de Transporte (Muestra 1 y 2)")

knitr::kable(cors.full[["Deuda"]],booktabs=T,
             caption="Correlación Inter-Olas de Deuda (Muestra 1 y 2)")

knitr::kable(cors.full[["Nacionalidad"]],booktabs=T,
             caption="Correlación Inter-Olas de Nacionalidad (Muestra 1 y 2)")

knitr::kable(cors.full[["Etnia"]],booktabs=T,
             caption="Correlación Inter-Olas de Etnia (Muestra 1 y 2)")

#  Muestra 1

knitr::kable(cors.full.1[["sexo"]],booktabs=T,
             caption="Correlación Inter-Olas de Sexo (Muestra 1)")
knitr::kable(cors.full.1[["edad"]],booktabs=T,
             caption="Correlación Inter-Olas de Edad (Muestra 1)")
knitr::kable(cors.full.1[["Educación"]],booktabs=T,
             caption="Correlación Inter-Olas de Educación (Muestra 1)")

# Muestra 2

knitr::kable(cors.full.2[["sexo"]],booktabs=T,
             caption="Correlación Inter-Olas de Sexo (Muestra 2)")
knitr::kable(cors.full.2[["edad"]],booktabs=T,
             caption="Correlación Inter-Olas de Edad (Muestra 2)")
knitr::kable(cors.full.2[["Educación"]],booktabs=T,
             caption="Correlación Inter-Olas de Educación (Muestra 2)")



