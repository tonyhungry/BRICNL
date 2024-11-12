# BRIC Sensitivity Analysis

library(sensitivity)
library(tidyverse)
set.seed(123)

finaldf = read.csv("/Volumes/SD Drive/Geo Data/BRIC ALL DATA.csv")

# Social Dimension ####
social = finaldf %>% 
  dplyr::select(WORKING,BORND,CARS,NBENEFITS,EDUC,EDUCSPEND,HEALTHSPEND,PSYCHRES,PSYCH,DOCTOR) 

# cor(social,method="spearman")

model_function <- function(X) {
  num_vars <- ncol(social)
  coefficient <- 1 / num_vars  
  output <- rowSums(X) * coefficient
  return(output)
}

n <- nrow(social)
X1 <- as.matrix(social[sample(1:n, n, replace = TRUE), ])
X2 <- as.matrix(social[sample(1:n, n, replace = TRUE), ])

sobol_result <- sobol2002(
  model = model_function,  
  X1 = X1,             
  X2 = X2,
  nboot = 10000
)

print(sobol_result)

## MDS ####
dissimilarity_matrix <- as.dist(1 - abs(cor(social)))
mds_result <- cmdscale(dissimilarity_matrix, k = 2)
plot(mds_result)

mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$Variable <- rownames(mds_df)

ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Variable)) +
  geom_point() +
  geom_text(vjust = 1.5) +
  ggtitle("MDS Plot of Variables") +
  xlab("Dimension 1") +
  ylab("Dimension 2")

# Economic Dimension ####
econ = finaldf %>% 
  select(BUSLOC,WOZ,FEMINC,GINI,ECONSPEND,LABSPEND,BANKS)

model_function <- function(X) {
  num_vars <- ncol(econ)
  coefficient <- 1 / num_vars  
  output <- rowSums(X) * coefficient
  return(output)
}

n <- nrow(econ)
X1 <- as.matrix(econ[sample(1:n, n, replace = TRUE), ])
X2 <- as.matrix(econ[sample(1:n, n, replace = TRUE), ])

sobol_result <- sobol2002(
  model = model_function,  
  X1 = X1,             
  X2 = X2,
  nboot = 10000
)

print(sobol_result)

# Institutional Dimension ####
inst = finaldf %>% 
  select(CRISISSPEND,SOLVENCY,SAFETYSPEND)

model_function <- function(X) {
  return (1/3*X[,1] + 1/3*X[,2] + 1/3*X[,3])
}

n <- nrow(inst)
X1 <- as.matrix(inst[sample(1:n, n, replace = TRUE), ])
X2 <- as.matrix(inst[sample(1:n, n, replace = TRUE), ])

sobol_result <- sobol2002(
  model = model_function,  
  X1 = X1,             
  X2 = X2,
  nboot = 10000
)

print(sobol_result)

# Infrastructure Dimension ####

infra = finaldf %>% 
  select(HOSPITAL,VACHOME,HOTELS,TRAIN,PHARMACY,FIRE,ROAD,POLICE)

model_function <- function(X) {
  num_vars <- ncol(infra)
  coefficient <- 1 / num_vars  
  output <- rowSums(X) * coefficient
  return(output)
}

n <- nrow(infra)
X1 <- as.matrix(infra[sample(1:n, n, replace = TRUE), ])
X2 <- as.matrix(infra[sample(1:n, n, replace = TRUE), ])

sobol_result <- sobol2002(
  model = model_function,  
  X1 = X1,             
  X2 = X2,
  nboot = 10000
)

print(sobol_result)

# Community Dimension ####
comm = finaldf %>% 
  select(COMMSPEND,RECRSPEND,VOLUNTEER,SPORTS,MIGRATION,CRIMES)

model_function <- function(X) {
  num_vars <- ncol(comm)
  coefficient <- 1 / num_vars  
  output <- rowSums(X) * coefficient
  return(output)
}

n <- nrow(comm)
X1 <- as.matrix(comm[sample(1:n, n, replace = TRUE), ])
X2 <- as.matrix(comm[sample(1:n, n, replace = TRUE), ])

sobol_result <- sobol2002(
  model = model_function,  
  X1 = X1,             
  X2 = X2,
  nboot = 1000000
)

print(sobol_result)

# Environment Dimension ####
envi = finaldf %>% 
  select(LAND,GREENSPEND,WASTESPEND,GAS,ELEC,BUFFER,OPENSPACE,CULTIVATE,FLOOD)

model_function <- function(X) {
  num_vars <- ncol(envi)
  coefficient <- 1 / num_vars  
  output <- rowSums(X) * coefficient
  return(output)
}

n <- nrow(envi)
X1 <- as.matrix(envi[sample(1:n, n, replace = TRUE), ])
X2 <- as.matrix(envi[sample(1:n, n, replace = TRUE), ])

sobol_result <- sobol2002(
  model = model_function,  
  X1 = X1,             
  X2 = X2,
  nboot = 10000
)

print(sobol_result)
