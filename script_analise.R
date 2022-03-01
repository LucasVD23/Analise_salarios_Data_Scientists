library(tidyverse)
ds_salary <- read_csv("{Local do arquivo}")

#transformando strings "na" em NAs
num_faltantes <-c()
prop_faltantes <- c()
for(i in 1:ncol(ds_salary)){
  ds_salary[(ds_salary[,i]=="na"),i] <- NA
  num_nas =  sum(is.na(ds_salary[,i]))
  num_faltantes <- append(num_faltantes,num_nas)
  prop_faltantes <- append(prop_faltantes,num_nas*100/nrow(ds_salary))
}
faltantes <- tibble(colunas = names(ds_salary),num_faltantes = num_faltantes, prop_faltantes = prop_faltantes)