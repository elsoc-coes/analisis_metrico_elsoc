### Análisis de Consistencia y Correlaciones Interola

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

# Funciones para la presentación de los resultados.

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
  data <- el %>%  select(starts_with(c(concept.1[[i]][1],"muestra"))) %>% 
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

# Loop para las correlaciones inter-olas para todas las variables
# (se hace para extraer las correlaciones de algunos sociodemograficos)


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
    cor.olas[[i]] <- cor(data,use="pairwise.complete.obs")
  } else{
    cor.olas[[i]] <- "No aplica. Variable medida solamente en una ola."
  }
  names(cor.olas)[i] <- names(lista_conceptos_especificos)[i]
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


cor.olas[["sexo"]]
cor.olas[["edad"]]
cor.olas[["Educación"]]

knitr::kable(cor.olas[["sexo"]],booktabs=T,
             caption="Correlación Inter-Olas de Sexo (Muestra 1 y 2)")
knitr::kable(cor.olas[["edad"]],booktabs=T,
             caption="Correlación Inter-Olas de Edad (Muestra 1 y 2)")
knitr::kable(cor.olas[["Educación"]],booktabs=T,
             caption="Correlación Inter-Olas de Educación (Muestra 1 y 2)")

knitr::kable(cor.olas.1[["sexo"]],booktabs=T,
             caption="Correlación Inter-Olas de Sexo (Muestra 1)")
knitr::kable(cor.olas.1[["edad"]],booktabs=T,
             caption="Correlación Inter-Olas de Edad (Muestra 1)")
knitr::kable(cor.olas.1[["Educación"]],booktabs=T,
             caption="Correlación Inter-Olas de Educación (Muestra 1)")

knitr::kable(cor.olas.2[["sexo"]],booktabs=T,
             caption="Correlación Inter-Olas de Sexo (Muestra 2)")
knitr::kable(cor.olas.2[["edad"]],booktabs=T,
             caption="Correlación Inter-Olas de Edad (Muestra 2)")
knitr::kable(cor.olas.2[["Educación"]],booktabs=T,
             caption="Correlación Inter-Olas de Educación (Muestra 2)")


# Vector para identificar las variables estilo izquierda-derecha. 
# Esto se hace para dejarlas en una lista aparte, para que no se calcule el Alfa

vars.aut <- c(names(el)[grep("c15|c16",names(el))]) # Por completar
vars.exclude <- c("c15") # Por completar

# Después de completar el vector: filtrar. 

cors.aut <- list() # Correlaciones inter-olas de las variables de vars.aut

cors.n2 <- list() # Correlaciones para indicadores que han sido medidos con dos variables
cors.int <- list()  # Correlaciones de los indicadores combinados
alfas <- list() # 
alfas.plot <- list()

v <- c()

list_df <- list()

# Se almacenan los distintos df en una lista

for (i in 1: length(lista_conceptos_especificos)) {
  v <- unlist(lista_conceptos_especificos[i])
  df <- el %>% dplyr::select(starts_with(v))
  list_df[[i]] <- df
  names(list_df)[i] <- names(lista_conceptos_especificos)[i]
  for (j in 1:6) {
    # Se separan las variables según sus respectivas olas
    l <- grep(paste0("_w0",j),names(df),value = TRUE)
    df_subset <- df[,l,drop=FALSE]
    # Para las bases de datos con más de dos variables por concepto: se calculan alfas
    if (length(df_subset) > 2) {
      # indicadores seleccionados
      alfa_v <- alpha(df_subset,check.keys = T,na.rm = T)
      alfa_df <- alfa_v$alpha.drop
      alfa_df$Question <- rownames(alfa_df)
      #alfa_df <- alfa_df %>% rownames_to_column("Question")
      alfa_df$total_alfa <- alfa_v$total$raw_alpha
      name_alpha <- paste("Alpha", i,j, sep = "_")
      alfas[[name_alpha]] <- alfa_df
    } else if(length(df_subset)==2) {
      correlation <- cor(df_subset,use="pairwise.complete.obs")
      name_cor <- paste("Correlacion", i,j, sep = "_")
      alfas[[name_cor]] <- correlation
    } else if(length(df_subset)==1) {
      alfas[[i]] <- "No aplica"
      name_no.aplica <- l
      alfas[[name_no.aplica]] <- alfas[[i]]
    }
  }
}

# Se seleccionan todos los df del loop anterior
alfas_df_sub <- alfas[sapply(alfas,is.data.frame)]

# Se grafican los aflas:
# la linea roja indica el alfa de cronbach cuando todas las variables están consideradas
# cada punto indica el valor del alfa cuando esa variable es excluida

library(ggplot2)

for (k in 1:length(alfas_df_sub)){
  name <- names(alfas_df_sub[k]) # Hay que cambiarlo de tal manera que indique el nombre del concepto
  total_alfa <- alfas_df_sub[[k]]$total_alfa
  total_alfa <- round(total_alfa,2)
  y_min <- total_alfa - .1
  y_max <- total_alfa + .25
  y_max <- ifelse(y_max > 1,1,y_max)
  y_min <- ifelse(y_min > min(alfas_df_sub[[k]]$raw_alpha),
                  min(alfas_df_sub[[k]]$raw_alpha)-.1,y_min)
  limit <- unique(c(y_min,y_max))
  alfa.plot <- ggplot(alfas_df_sub[[k]],aes(y=raw_alpha,x=Question)) + geom_point() +
    geom_hline(yintercept = total_alfa, color = "red") +
    annotate("text",x = 1,y = total_alfa + .01, label = paste("Alfa =",total_alfa,sep=" "),
             color = "red") +
    labs(title = name, caption = "Fuente: elaboración propia a partir de ELSOC 2016-2022. \nLos puntos indican el valor del alfa de Cronbach cuando se omite el item.",
         y = "Alfa de Cronbach", x = "Item") +
    ylim(limit) + theme_bw() +
    theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16), legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  alfas.plot[[k]] <- alfa.plot
  names(alfas.plot[k]) <- name
}

# Gráfico con todos los alfas

alfas_df_sub <- do.call("rbind", alfas_df_sub)
alfas_df_sub <- alfas_df_sub %>% rownames_to_column("Item") %>%
  mutate(Item = gsub("\\..*","",Item))
ggplot(alfas_df_sub,aes(x=Item,y=total_alfa)) + geom_point() + coord_flip() + 
  labs(x="Item",y="Alfa de Cronbach") +
  theme_bw() + theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
                     axis.text.y = element_text(size = 12),
                     axis.title = element_text(size = 14, face = "bold"),
                     plot.title = element_text(size = 16), legend.title = element_text(size = 14),
                     legend.text = element_text(size = 12))
  
# Calcular correlacción interolas de conceptos especificos

q <- c() # Vector para almacenar las "Question"
df_mean <- NULL # Df para las medias
list_df_mean <- list() # Lista donde se guardan las correlaciones y 
                       # la base de datos con la cual se calculo

# función para extraer valores de un string

substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Extraer el número "36" usando sub para poder iniciar el loop

item <- alfas_df_sub$Item
n <- as.numeric(sub("Alpha_(\\d+)_(\\d+)", "\\1", item[length(item)]))
n

# Loop para el calculo de las correlaciones. 

for (k in 1:n){
  # Se selecciona el concepto
  p <- paste("Alpha_",k,sep="")
  p <- paste(p,"_",sep="")
  l_sub <- alfas_df_sub %>% filter(startsWith(Item,p))
  if(nrow(l_sub) > 1){
    q <- l_sub$Question
    for (t in 1:length(q)){
      # Se elimina el "-" que se crea por el check.keys = TRUE de la función alpha()
      q[t] <- ifelse(substrRight(q[t],1)=="-",gsub("-","",q[t]),q[t])
    }
    df <- el %>% select(all_of(q))
    for (i in 1:6){
      # Se separan las variables por ola
      l <- grep(paste0("_w0",i),names(df),value = TRUE)
      df_subset <- df %>% select(ends_with(l))
      # Se calculan las medias
      df_subset$mean <- apply(df_subset,1,mean,na.rm=T)
      df_mean <- cbind(df_mean,df_subset[,dim(df_subset)[2]])
      df_mean <- data.frame(df_mean)
      # Se reemplaza el nombre de las columnas
      for (j in 1:length(df_mean)){
        colnames(df_mean)[j] <- paste("Indicador_w0",j,sep="")
        if(all(is.na(df_mean[j]))){df_mean[j] <- NULL}
      }
      # Se calcula la correlación inter-ola de cada concepto
      if(length(df_mean) > 1){
        corr <- cor(df_mean,use="complete.obs")
      } else{corr <- "No aplica"}
      list_df_mean[[k]] <- list(data = df_mean, corr = corr)
    }
    df_mean <- NULL
  }
}
