library(tidyverse)
ds_salary <- read_csv("D:/Windows/Documentos/PVD/Trabalho/data_cleaned_2021.csv")

#transformando strings "na" em NAs

for(i in 1:ncol(ds_salary)){
  ds_salary[(ds_salary[,i]=="na"),i] <- NA
}