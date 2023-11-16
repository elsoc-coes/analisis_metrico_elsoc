
# set working directory
#setwd("/Users/gustavoahumada/Downloads/analisis_metrico/Datos ELSOC")

#  libraries
library(readxl)
library(tidyverse)
library(psych)

## more complex dataframe
# Define numeric values
values11_w1 <- c(1, NA, 3, -888, 2, 12, 11)
values11_w2 <- c(4, 5, 5, -888, NA, 12, 11)
values11_w3 <- c(6, 3, 8, -888, NA, 12, 11)
values21_w1 <- c(1, 2, 3, -888, NA, 10, 9)
values22_w1 <- c(4, 3, 5, -888, NA, 12, 11)
values23_w1 <- c(6, 3, 8, -999, NA, 12, 11)
values21_w2 <- c(1, 2, -888, 3, NA, 12, 11)
values22_w2 <- c(4, 5, NA, -888, 5, 12, 11)
values23_w2 <- c(6, 3, 8, -888, NA, 10, 11)
values21_w3 <- c(1, 2, 3, -999, NA, 12, 8)
values22_w3 <- c(4, NA, 5, -888, 6, 12, 11)
values23_w3 <- c(6, 3, 8, -888, NA, 12, 11)
values31_w1 <- c(1, 2, 3, -888, NA, 12, 11)
values32_w1 <- c(4, NA, NA, -999, 5, 10, 7)
values31_w2 <- c(1, 2, -888, 3, 6, 12, 11)
values32_w2 <- c(4, 5, 5, -888, NA, 12, 11)

# Define column names
column.names <- c("Column11_w1", "Column11_w2", "Column11_w3", "Column21_w1", "Column22_w1",
                  "Column23_w1", "Column21_w2", "Column22_w2", "Column23_w2", "Column21_w3",
                  "Column22_w3", "Column23_w3", "Column31_w1", "Column32_w1", "Column31_w2",
                  "Column32_w2")


# Create a dataframe with numeric values and column names
df <- data.frame( 
  Column11_w1 = values11_w1, Column11_w2 =values11_w2, Column11_w3 = values11_w3,
  Column21_w1 = values21_w1, Column22_w1 = values22_w1, Column23_w1 = values23_w1,
  Column21_w2 = values21_w2, Column22_w2 = values22_w2, Column23_w2 = values23_w2,
  Column21_w3 = values21_w3, Column22_w3 = values22_w3, Column23_w3 = values23_w3,
  Column31_w1 = values31_w1, Column32_w1 = values32_w1, Column31_w2 = values31_w2,
  Column32_w2 = values32_w2
)

# code book
lc <- read_xlsx("/Users/gustavoahumada/Dropbox/ELSOC_2/analisis_metrico/Datos ELSOC/Libro de Codigo/prueba.xlsx",
                sheet = "Sheet1")

# filter
df <- df %>%
  mutate_all(list(~ifelse(.<0,NA,.))) %>% 
  mutate(across(starts_with("Column"), ~ ifelse(. %in% c(11, 12), 11, .)))

# define specific concepts
conceptos_eps <- unique(lc$concepto_especifico)
lista_conceptos_especificos <- list()
df2 <- NULL

for (i in 1:length(conceptos_eps)){
  df2 <- lc %>% filter(concepto_especifico == conceptos_eps[i])
  variables <- unique(df2$codigo_longitudinal)
  lista_conceptos_especificos[[i]] <- c(`Codigo Longitudinal` = variables)
  names(lista_conceptos_especificos)[i] <- conceptos_eps[i]
}


cors.n2 <- list() # Correlaciones para indicadores que han sido medidos con dos variables
#cors.int <- list()  # Correlaciones de los indicadores combinados
alfas <- list() # 

v <- c()

list_df3 <- list()


for (i in 1: length(lista_conceptos_especificos)) {
  v <- unlist(lista_conceptos_especificos[i])
  df3 <- df %>% dplyr::select(starts_with(v))
  list_df3[[i]] <- df3
  names(list_df3)[i] <- names(lista_conceptos_especificos)[i]
  for (j in 1:3) {
    # Se separan las variables según sus respectivas olas
    l <- grep(paste0("_w",j),names(df3),value = TRUE)
    df_subset <- df3[,l,drop=FALSE]
    # Para las bases de datos con más de dos variables por concepto
    if (length(df_subset) > 2) {
      # indicadores seleccionados
      alfa_df_subset <- alpha(df_subset,check.keys = T,na.rm = T)
      alfas_values <- c(alfa_df_subset[[10]][[1]], alfa_df_subset[[10]][[2]],
                        alfa_df_subset[[10]][[3]], alfa_df_subset[[1]][[2]],
                        alfa_df_subset[[1]][[3]], alfa_df_subset[[5]])
      names(alfas_values) <- c("lower", "alpha", "upper", "std.alpha", "average_r",
                              "variables")
      name_alpha <- paste("Alpha", i,j, sep = "_")
      alfas[[name_alpha]] <- alfas_values
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


alpha_2_1 <- alfas[[5]]
data_table <- data.frame(alpha_2_1)


############################################################################################
df_sub1 <- subset(df, select = c(Column21_w1, Column22_w1, Column23_w1))
df_sub2 <- subset(df, select = c(Column21_w2, Column22_w2, Column23_w2))
alpha_handle1 <- alpha(df_sub1)
alpha_handle2 <- alpha(df_sub2)

alpha_handle1$total$raw_alpha


library(ggplot2)

coeff_df <- data.frame(alpha_handle1$alpha.drop) %>%
  rownames_to_column("Question") %>% 
  select(Question,std.alpha) %>%
  mutate(Question = str_replace(Question, '\\-$', '')) %>%
  rename(CronbachAlphaItemDrop = std.alpha) %>%
  mutate(CronbachAlpha = alpha_handle1$alpha.drop$result$total$std.alpha)



alfas1 <- c(alpha_handle1[[10]][[1]], alpha_handle1[[10]][[2]], alpha_handle1[[10]][[3]],
           alpha_handle1[[1]][[2]], alpha_handle1[[1]][[3]])

alfas2 <- c(alpha_handle2[[10]][[1]], alpha_handle2[[10]][[2]], alpha_handle2[[10]][[3]],
            alpha_handle2[[1]][[2]], alpha_handle2[[1]][[3]])

names(alfas1) <- c("lower", "alpha", "upper", "std.alpha", "average_r")
names(alfas2) <- c("lower", "alpha", "upper", "std.alpha", "average_r")
list_alpha1 <- list()
list_alpha2 <- list()
list_alpha1[[1]] <- alfas1
list_alpha2[[1]] <- alfas2
names(list_alpha1)[[1]] <- conceptos_eps[1]
names(list_alpha2)[[1]] <- conceptos_eps[2]

df_sub3 <- subset(df, select = c(Column31_w2, Column32_w2))
correlacion1 <- cor(df_sub3, use="pairwise.complete.obs")

list_cor <- list()
list_cor[[1]] <- correlacion1
names(list_cor)[[1]] <- conceptos_eps[3]









