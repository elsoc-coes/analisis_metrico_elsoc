### Análisis de Consistencia y Correlaciones Interola

library(readxl)
library(tidyverse)
library(psych)

load("./Datos/ELSOC_Wide.RData")
lc <- read_xlsx("./Libro de Codigo/0A_Listado_Variables_Global_ELSOC_v2022.xlsx",
                sheet = "2_Items_Total")

el <- elsoc_wide_2016_2022; elsoc_wide_2016_2022 <- NULL


## Funciones para crear tablas de correlaciones y reportar los alfas de Cronbach
## revisar que los codigos funcionen

## Modificar un poco.
## Hacer las correlaciones para itemes (correlación inter-olas para las medias), 
## una vez calculados los alfas.



cor.table <- function(cor_matrix){
  # Obtener nombres de las variables
  var_names <- colnames(cor_matrix)
  
  # Crear la tabla LaTeX
  latex_table <- "\\begin{table}[h]\n\\centering\n\\begin{tabular}{l" 
  for (i in 1:length(var_names)) {
    latex_table <- paste0(latex_table, "c")
  }
  latex_table <- paste0(latex_table, "}\n\\hline\n")
  
  # Añadir encabezados de columnas
  col_headers <- paste("& \\textbf{", var_names, "}", collapse = " & ")
  latex_table <- paste0(latex_table, col_headers, "\\\\\n\\hline\n")
  
  # Añadir filas de datos
  for (i in 1:length(var_names)) {
    row_data <- c(var_names[i], cor_matrix[i, ])
    row_data <- format(row_data, width = 10, justify = "left")
    row_data <- paste(row_data, collapse = " & ")
    latex_table <- paste0(latex_table, row_data, "\\\\\n")
  }
  
  # Añadir línea final y cerrar la tabla
  latex_table <- paste0(latex_table, "\\hline\n\\end{tabular}\n\\caption{Correlation Table}\n\\end{table}")
  
  return(latex_table)
}


#0. Hacer recodificaciones donde sea necesario. (PENDIENTE)

el <- el %>%
  mutate_all(funs(ifelse(.<0,NA,.))) %>% ## Todos los -999 y -888 pasan a NA.
  mutate_at(
  # Izquierda y derecha
  vars(starts_with("c15"),function(x)ifelse(x %in% c(11,12),11,x))) #Por algúna razón arroja error.
  

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

concept.1 <- vars.subset[sapply(vars.subset,length)/2==1]
concept.2 <- vars.subset[sapply(vars.subset,length)/2>1] # A partir de aqui, hacer alfas.
                                                         # Y calcular las medias. Luego estas medias,
                                                         # Hacer correlaciones inter-olas.
                                                         # Excluir aquellas variables que no refieran a un
                                                         # concepto (por ejemplo, identificación política)

# length(concept.1) + length(concept.2) == length(vars.subset) para comprobar que el subset esté bien hecho
 
# En el caso de concept.1 solo se calcula la correlación entre olas.

cor.olas <- list() # lista para la correlación entre olas 
alfas <- list() # lista para los alfas de Cronbach
cor.in <- list() # lista para las correlaciones al interior de cada ola


### Mismas listas pero segmentadas por muestra

#### Muestra 1

cor.olas.1 <- list() # lista para la correlación entre olas 

alfas.1 <- list() # lista para los alfas de Cronbach
cor.in.1 <- list() # lista para las correlaciones al interior de cada ola

#### Muestra 2

cor.olas.2 <- list() # lista para la correlación entre olas 

alfas.2 <- list() # lista para los alfas de Cronbach
cor.in.2 <- list() # lista para las correlaciones al interior de cada ola


# Loop para las correlaciones inter-olas para los conceptos que tienen solo una variable

for (i in 1:length(concept.1)){
  data <- el %>%  select(starts_with(c(concept.1[[i]][2],"muestra"))) %>% 
    mutate_all(funs(ifelse(.<0,NA,.)))
  
  # Se crean df según muestras
  
  muestra.1 <- data %>% filter(muestra==1) %>% select(-muestra) %>% 
    select_if(colSums(is.na(.))<nrow(.))
  
  muestra.2 <- data %>% filter(muestra==2) %>% select(-muestra) %>% 
    select_if(colSums(is.na(.))<nrow(.))
  
  data <- data %>% select(-muestra)
  
  # se seleccionan solamente aquellos data que tengan más de una ola (se consideran ambas muestras juntas):
  
  if (length(data)>1){
    cor.olas[[i]] <- cor(data,use="pairwise.complete.obs")
  } else{
    cor.olas[[i]] <- "No aplica. Variable medida solamente en una ola."
  }
  names(cor.olas)[i] <- names(concept.1)[i]
  
  # se calculan correlaciones inter-ola por muestra
  ## Muestra 1
  
  if (length(muestra.1)>1){
    cor.olas.1[[i]] <- cor(muestra.1,use="complete.obs")
  } else{
    cor.olas.1[[i]] <- "No aplica. Variable medida solamente en una ola."
  }
  names(cor.olas.1)[i] <- names(concept.1)[i]
  
  ## Muestra 2
  
  if (length(muestra.2)>1){
    cor.olas.2[[i]] <- cor(muestra.2,use="complete.obs")
  } else{
    cor.olas.2[[i]] <- "No aplica. Variable medida solamente en una ola."
  }
  names(cor.olas.2)[i] <- names(concept.1)[i]
  
}

# Vector para identificar las variables estilo izquierda-derecha. Esto se hace para dejarlas en una lista aparte
# para que solamente se haga análisis de correlaciones

vars.aut <- c(names(el)[grep("c15|c16",names(el))]) # Por completar
vars.exclude <- c("c15") # Por completar

cors.aut <- list() # Correlaciones inter-olas de las variables de vars.aut

cors.n2 <- list() # Correlaciones para indicadores que han sido medidos con dos variables
cors.int <- list()  # Correlaciones de los indicadores combinados
alfas <- list() # 

v <- c()

list_df <- list()

# Se almacenan los distintos df en una lista

for (i in 1:length(lista_conceptos_especificos)){
  v <- unlist(lista_conceptos_especificos[i])
  df <- el %>% select(starts_with(v)) # En esta parte, decirle que excluya las variables de vars.exclude()
  
  # Se guardan los df para poder visualizarlos
  list_df[[i]] <- df
  names(list_df)[i] <- names(lista_conceptos_especificos)[i]
  for (j in 1:6){
    
    # Se separan las variables según sus respectivas olas
    l <- grep(paste0("_w0",j),names(df),value = TRUE)
    df_subset <- df[,l,drop=FALSE]
    
    # Para las bases de datos con más de dos variables por concepto
    if (length(df_subset) > 2){
      alfa_df_subset <- alpha(df_subset,check.keys = T,na.rm = T)
      alfas[[i]] <- alfa_df_subset
      names(alfas)[i] <- paste("Alfa de Cronbach para",l,sep=": ")
    } else if(length(df_subset)==2){
      alfa_df_subset <- cor(df_subset,use="complete.obs")
      alfas[[i]] <- alfa_df_subset
      names(alfas)[i] <- paste("Correlaciones para",l,sep=": ")
    } else if(length(df_subset)==1){
      alfas[[i]] <- "No aplica"
      names(alfas)[i] <- l
    }
  }
}  




