library(VIM)
library(tidyverse)

data <- read_csv("data_cleaned_2021.csv")

calcular_covariancia <- function(vec1,vec2){
  covariancia <-0
  for(i in 1:length(vec1)){
    dif_a = vec1[i] - mean(vec1) 
    dif_b = vec2[i] - mean(vec2)
    covariancia <- covariancia + dif_a*dif_b
  }
  covariancia <-covariancia/(length(vec1))
}

calcular_corrs_numericos <- function(dataset_numeric,na.rm=TRUE){
  redundantes <- c()
  for(i in 1:ncol(dataset_numeric)){
    for(j in 1:ncol(dataset_numeric)){
      if(i==j||i<j){
        next
      }
      vec1 <- dataset_numeric[,i]
      vec2 <- dataset_numeric[,j]
      r <- calcular_covariancia(vec1,vec2)/(sd(vec1)*sd(vec2))
      soma_quadrados <- sum((vec1 - mean(vec2)) ^ 2)
      soma_quadrados_residuais <- sum((vec1 - vec2)^2)
      r_2 <- 1 - (soma_quadrados_residuais/soma_quadrados) 
      if((r > 0.7 || r < -0.7) || r_2 >0.7){
        print(paste(names(data_numeric[i])," e ", names(data_numeric[j]),"são correlacionados"))
        append(redundantes,names(data_numeric[j]))
        redundantes <- union(redundantes, names(data_numeric[j]))
        print(paste(r,", ",r_2))
      }
    }
    
  }
  return(redundantes)

}



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

data_numeric <- data %>% select_if(is.numeric)

redundantes <- calcular_corrs_numericos(data_numeric)
print(paste0("são redundantes e podem ser removidos: ",toString(redundantes)))

data_nomimal <- data %>% select_if(is.character)
