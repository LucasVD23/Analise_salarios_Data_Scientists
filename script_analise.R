library(VIM)
library(tidyverse)

ds_salary <- read_csv("data_cleaned_2021.csv")

qnt_valores_faltantes <- function(data) {
  column_names <- colnames(data)
  for (j in 1:ncol(data)) {
    print(
      paste(
        "A coluna ",
        dQuote(column_names[j]),
        " possui ",
        sum(is.na(data[, j])),
        " valores faltantes.",
        sep = ""
      )
    )
  }
}

# Transformando strings "na" em NAs
# num_faltantes <-c()
# prop_faltantes <- c()
for(i in 1:ncol(ds_salary)){
  ds_salary[which(ds_salary[,i]=="na"),i] <- NA
  ds_salary[which(ds_salary[,i]==-1),i] <- NA
  # num_nas =  sum(is.na(ds_salary[,i]))
  # num_faltantes <- append(num_faltantes,num_nas)
  # prop_faltantes <- append(prop_faltantes,num_nas*100/nrow(ds_salary))
}
# faltantes <- tibble(colunas = names(ds_salary),num_faltantes = num_faltantes, prop_faltantes = prop_faltantes)

qnt_valores_faltantes(ds_salary)

# Imputação de valores contínuos e inteiros
ds_salary$Rating[is.nan(ds_salary$Rating)] <- mean(ds_salary$Rating, na.rm = TRUE)
ds_salary$Founded[is.nan(ds_salary$Founded)] <- median(ds_salary$Founded, na.rm = TRUE)
ds_salary$Age[is.nan(ds_salary$Age)] <- as.integer(mean(ds_salary$Age, na.rm = TRUE))

# Remoção do atributo 'Competitors'
ds_salary$Competitors <- NULL

# Imputação dos valores com o uso do kNN
ds_salary <- kNN(ds_salary)
ds_salary <- subset(ds_salary, select = -c(42:82))

qnt_valores_faltantes(ds_salary)
