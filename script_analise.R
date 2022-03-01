library(tidyverse)
ds_salary <- read_csv("D:/Windows/Documentos/PVD/Analise_salarios_Data_Scientists/data_cleaned_2021.csv")

#transformando strings "na" em NAs
num_faltantes <-c()
prop_faltantes <- c()
for(i in 1:ncol(ds_salary)){
  ds_salary[(ds_salary[,i]=="na" | ds_salary[,i]==-1),i] <- NA
  num_nas =  sum(is.na(ds_salary[,i]))
  num_faltantes <- append(num_faltantes,num_nas)
  prop_faltantes <- append(prop_faltantes,num_nas*100/nrow(ds_salary))
}
faltantes <- tibble(colunas = names(ds_salary),num_faltantes = num_faltantes, prop_faltantes = prop_faltantes)