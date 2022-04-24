library(VIM)
library(tidyverse)
library(plotly)
data <- read_csv("data_cleaned_2021.csv")

norm_min_max <- function(vec, na.rm = TRUE) {
  return((vec- min(vec)) /(max(vec)-min(vec)))
}

norm_z_score <- function(vec, na.rm=TRUE){
  for(i in 1:ncol(vec)){
    vec[,i]<- ((vec[,i]-mean(vec[,i]))/sd(vec[,i]))
  }
  return(vec)
}

calcular_covariancia <- function(vec1,vec2,na.rm=TRUE){
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
      if(i==j||i>j){
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

      }
      print(paste("r: ",r,", r²: ",r_2))
      
    }
    
  }
  return(redundantes)

}

calcula_corrs_nominais <- function(dataset_nominal,na.rm=TRUE){
  redundantes <- c()
  for(i in 1:ncol(dataset_nominal)){
    for(j in 1:ncol(dataset_nominal)){
      if(i==j||i<j){
        next
      }
      corr = chisq.test(data_nominal[,i],data_nominal[,j],simulate.p.value = TRUE)
      if(corr$p.value <= 0.05){
        print(paste(names(data_nominal[i])," e ", names(data_nominal[j]),"são correlacionados"))
        append(redundantes,names(data_nominal[j]))
        redundantes <- union(redundantes, names(data_nominal[j]))
      }
    }
    
  }
  return(redundantes)
}

library(plotly)

fig <- plot_ly(
  type="treemap",
  labels=c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  parents=c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
)
fig

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

levels_size <- levels(factor(data$Size))
#ordena e retira unkown
data$Size <- factor(data$Size, levels = c(levels_size[1],levels_size[7],levels_size[4],
                                          levels_size[6],levels_size[3],levels_size[5],
                                          levels_size[2]))


data$Size <- as.numeric(data$Size,na.rm=FALSE)
#ordena e retira unkown
levels_Revenue <- levels(factor(data$Revenue))
data$Revenue <- factor(data$Revenue, levels = c(levels_Revenue[12],levels_Revenue[2],levels_Revenue[9],levels_Revenue[3],
                                                levels_Revenue[7], levels_Revenue[10],levels_Revenue[5],levels_Revenue[11],
                                                levels_Revenue[1],levels_Revenue[6],levels_Revenue[8],levels_Revenue[4]))
data$Revenue <- as.numeric(data$Revenue,na.rm=FALSE)
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
data <- kNN(data,imp_var = FALSE)

## Removendo colunas extras inseridas pelo kNN
select(data, -c(39:78))

faltantes <- verifica_faltantes(data)


##Retira coluna de estimativa de salários (ordinal) pois os intervalos da estimativa já estão em outro atributo
data <- select(data,-'Salary Estimate')

data_numeric <- data %>% select_if(is.numeric)


redundantes <- calcular_corrs_numericos(data_numeric)

print(paste0("são redundantes: ",toString(redundantes)))

data_nominal <- data %>% select_if(is.character)

#calcula_corrs_nominais(data_nominal)

#reitando redundantes 
data$`Lower Salary` <- NULL
data$`Upper Salary`<- NULL
data$Founded <- NULL
data$`Job Description`<-NULL
data$company_txt <-NULL
data$`Job Location`<- NULL
data$`Job Title`<-NULL
data$Industry <- NULL


data_numeric <- data %>% select_if(is.numeric)
data_numeric_norm <- norm_min_max(data_numeric)

calcular_corrs_numericos(data_numeric_norm)


#normalizações nos atribtuos Age e avg Salary
atributos_nao_norm <- data.frame('Avg Salary(K)' = data$`Avg Salary(K)`,'Rating'= data$Rating)
calcular_corrs_numericos(atributos_nao_norm)

atributos_norm_min_max <- norm_min_max(atributos_nao_norm)
atributos_norm_z_score <- norm_z_score(atributos_nao_norm)

print("Resultado das correlações com normalização min-max")
calcular_corrs_numericos(atributos_norm_min_max)
print("Resultado das correlações com normalização z-score")
calcular_corrs_numericos(atributos_norm_z_score)



totalAndMissingValuesPerCol <- c()
for (i in 1:nrow(faltantes)) {
  # Faltantes
  totalAndMissingValuesPerCol <- append(totalAndMissingValuesPerCol,
                                        faltantes[i, 2])
  # Não Faltantes
  totalAndMissingValuesPerCol <- append(totalAndMissingValuesPerCol,
                                        (nrow(data) - faltantes[i, 2]))
}

library(plotly)

labels=c("Type of Ownership", "Sector", "Company Name", "Rating","Size","Revenue","job_title_sim", "Avg Salary(K)","Location")
parents=c("", "Type of Ownership", "Sector", "Company Name", "Company Name","Company Name","Company Name","job_title_sim","job_title_sim")

fig <- plot_ly(
  type="treemap",
  labels=labels,
  parents=parents
)
fig