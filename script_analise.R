library(VIM)
library(tidyverse)

data <- read_csv("data_cleaned_2021.csv")




verifica_faltantes <- function(dataset){
  num_faltantes <-c()
  prop_faltantes <- c()
  for (i in 1:ncol(dataset)){
    num_nas =  sum(is.na(dataset[,i]))
    num_faltantes <- append(num_faltantes,num_nas)
    prop_faltantes <- append(prop_faltantes,num_nas*100/nrow(dataset))
    print(paste0("A coluna ",names(dataset[,i]), " possui ",num_nas," dados faltantes"))
  }
  faltantes <- tibble(colunas = names(dataset),num_faltantes = num_faltantes, prop_faltantes = prop_faltantes)
  
}

#transformando strings "na" em NAs
for(i in 1:ncol(data)){
  data[(data[,i]=="na" | data[,i]==-1),i] <- NA
  
}



faltantes <- verifica_faltantes(data)


# Imputacao de valores contínuos e inteiros
data$Rating[is.nan(data$Rating)] <- mean(data$Rating, na.rm = TRUE)
data$Founded[is.nan(data$Founded)] <- median(data$Founded, na.rm = TRUE)
data$Age[is.nan(data$Age)] <- as.integer(mean(data$Age, na.rm = TRUE))

# Remocao do atributo 'Competitors', 'seniority\_by\_title' e 'Degree' por serem superiores a 60%
data$Competitors <- NULL
data$seniority_by_title <- NULL
data$Degree <- NULL

# Imputacao dos valores do tipo 'string' a partir do uso do kNN
data <- kNN(data)

## Removendo colunas extras inseridas pelo kNN
select(data, -c(39:78))

faltantes <- verifica_faltantes(data)


##Retira coluna de estimativa de salários (ordinal) pois os intervalos da estimativa já estão em outro atributo
data <- select(data,-'Salary Estimate')
