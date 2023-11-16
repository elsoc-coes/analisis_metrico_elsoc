library(readxl)
library(expss)
library(tidyverse)
library(vtable)
library(plyr)
library(kableExtra)


setwd("/Users/gustavoahumada/Dropbox/ELSOC_2/analisis_metrico/Datos ELSOC")
load("./Datos/ELSOC_Long.RData")
lc <- read_xlsx("./Analisis de Correlaciones y Consistencia/Libro de Codigo/0A_Listado_Variables_Global_ELSOC_v2022_v5.xlsx",
                sheet = "2_Items_Total")


ell <- elsoc_long_2016_2022; elsoc_long_2016_2022 <- NULL



ell <- ell %>%
  mutate_all(list(~ifelse(.<0,NA,.))) ## Todos los -999 y -888 pasan a NA.

conceptos_grls <- unique(lc$concepto_general)
conceptos_eps <- unique(lc$Concepto_especifico_2)

vars <- list()
df <- NULL

for (i in 1:length(conceptos_grls)){
  df <- lc %>% filter(concepto_general == conceptos_grls[i])
  variables <- unique(df$codigo_longitudinal)
  variables.name <- unique(df$etiqueta)
  concepto.esp <- unique(df$Concepto_especifico_2)
  vars[[i]] <- c(`Nombre de la Variable`= variables.name,
                 `Codigo Longitudinal` = variables,
                 `Concepto Especifico` = concepto.esp)
  names(vars)[i] <- conceptos_grls[i] 
}



# Se crea una lista similar pero para los conceptos especificos

lista_conceptos_especificos <- list()

for (i in 1:length(conceptos_eps)){
  df <- lc %>% filter(Concepto_especifico_2 == conceptos_eps[i])
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

for(i in 1:length(lista_conceptos_especificos)) {
  data <- ell %>%  select(starts_with(c(lista_conceptos_especificos[[i]], "ola", "muestra"))) %>% 
  mutate_all(list(~ifelse(.<0,NA,.))) %>% 
  select_if(is.numeric) 
  data_subset <- select(data, -c("ola_w01", "ola_w02", "ola_w03", "ola_w04",
                                          "ola_w05", "ola_w06"))
  list_vartable[[i]] <- st(data_subset, group = "ola", out = "return", group.test = FALSE)
  names(list_vartable)[i] <- names(concept.1)[i]
}

df_vartable <- ldply(list_vartable, data.frame)%>% 
  select(-c(".id")) %>% distinct() %>% filter(Variable != "muestra") %>% 
  filter(Variable != c("ola", "ciuo88_m03", "ciuo08_m03", "ciiu3_m04", "ciiu4_m04")) %>% 
  select(-c("Variable"))

# etiqueta de variables
rownames(df_vartable) <- c("Sexo del entrevistado", "Edad", "Nivel educacional", "Actividad principal", 
"Relacion de empleo", "Tipo de trabajo", "Antiguedad en el trabajo","Contrato de trabajo", "Boleta de trabajo",
"Horas semanales de trabajo", "Cantidad de trabajadores en empresa","Cantidad de personas supervisadas",
"Ingreso mensual entrevistado (monto)","Ingreso mensual (16 tramos)","Ingreso mensual (en 5 tramos)",
"Remuneracion justa entrevistado","Satisfaccion con ingreso","Horas semanales a cuidados familiares",
"Sistema previsional", "Principal sostenedor del hogar","Nivel educacional del sostenedor del hogar",
"Actividad principal del sostenedor del hogar","Cantidad de trabajadores en el trabajo del sostenedor",
"Cantidad de personas supervisadas por el sostenedor","Relacion de empleo del sostenedor del hogar",
"Nivel educacional del padre","Nivel educacional de la madre","Ingreso del hogar (monto)",
"Ingreso del hogar (20 tramos)","Ingreso del hogar (5 tramos)","Conexion a TV cable","Secadora de ropa",
"Computador","Conexion a internet","Cantidad de vehiculos motorizados en hogar","Propiedad de vivienda donde reside",
"Tiempo residiendo en ciudad","Tiempo residiendo en comuna","Tiempo residiendo en barrio",
"Numero de libros en el hogar","Estado civil del entrevistado","Numero de hijos hombres","Numero de hijas mujeres",
"Religion del entrevistado","Frecuencia de asistencia a servicios religiosos",
"Medio de transporte que utiliza habitualmente","Deuda: Casas comerciales","Deuda: Bancos","Deuda: Parientes",
"Deuda: Otra","Percepcion de sobrecarga por endeudamiento","Nivel de ahorros del entrevistado",
"Nacionalidad del entrevistado","Hogar formado por una o varias personas","Numero de personas en el hogar",
"Miembro del hogar 1: Ocupacion","Miembro del hogar 1: Educacion",
"Miembro del hogar 2: Ocupacion","Miembro del hogar 2: Educacion",
"Miembro del hogar 3: Ocupacion","Miembro del hogar 3: Educacion",
"Miembro del hogar 4: Ocupacion","Miembro del hogar 4: Educacion",
"Miembro del hogar 5: Ocupacion","Miembro del hogar 5: Educacion",
"Miembro del hogar 6: Ocupacion","Miembro del hogar 6: Educacion",
"Miembro del hogar 7: Ocupacion","Miembro del hogar 7: Educacion",
"Miembro del hogar 8: Ocupacion","Miembro del hogar 8: Educacion",
"Miembro del hogar 9: Ocupacion","Miembro del hogar 9: Educacion",
"Miembro del hogar 10: Ocupacion","Miembro del hogar 10: Educacion",
"Tiene hijos","Edad en la que tuvo primer hijo","Primeros 15 anios vivio con alguno de sus padres",
"Primerios 15 anios la madre trabajo fuera del hogar","Pertenencia a pueblo indigena",
"Tamannio del Hogar","Redes: Apoyo al encontrar trabajo","Redes: Principal apoyo para encontrar trabajo",
"Relacion de empleo Padre/Sostenedor del Entrevistado","Supervision Padre/Sostenedor del Entrevistado",
"Trabajo desde el hogar","Motivo desempleo","Acceso a alimentos y suministros basicos en pandemia",
"Accedio a beneficios del Estado","Accedio a primer retiro 10% AFP","Accedio a segundo retiro 10% AFP")


df_vartable %>% 
  kbl(caption = "Tabla 1: Estadísticas descriptivas",
      format = "html",
      col.names = c("N", "Mean", "SD", "N", "Mean", "SD", "N", "Mean", "SD",
                    "N", "Mean", "SD", "N", "Mean", "SD", "N", "Mean", "SD"),
      align = "lcccccccccccccccccc") %>%
  kable_classic(full_width = F, html_font = "helvetica") %>% 
  add_header_above(c(" " = 1, "Ola 1" = 3, "Ola 2" = 3, "Ola 3" = 3, "Ola 4" = 3,
                     "Ola 5" = 3, "Ola 6" = 3))
  

