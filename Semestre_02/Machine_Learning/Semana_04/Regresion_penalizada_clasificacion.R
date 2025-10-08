rm(list=ls())

#########################################################################
### -- Maestria en Ciencia de Datos -- ## 
#########################################################################
### Autor: MSc. Jose Cardenas  ## 
### Tema: Regresion Penalizada  ## 

#########################################################################

########### 1) LIBRERIAS A UTILIZAR ################# 

library(ggplot2)
library(glmnet)
library(gee)
library(leaps)
library(dplyr)
library(caret)
library(pROC)
library(mlr)
library(MLmetrics)

########### 2) DATA A UTILIZAR ################# 

data <- read.csv("PimaIndiansDiabetes.csv")

########### 3) TRATAMIENTO DE LA DATA ################# 

## en primer lugar ver el analisis descriptivo de la data

resumen <- data.frame(summarizeColumns(data))
#write.csv(resumen,"tabla_resumen.csv")

########### 4) VARIABLES DEL MODELO ################# 

## variables del modelo ##

data$diabetes <- ifelse(data$diabetes=="pos",1,0)

x <- model.matrix(diabetes~., data)[,-1]
y <- as.numeric(as.character(data$diabetes))

########### 5) CORRELACION ################# 

source("/funciones.R")

corre <- cor(data,method = c("spearman"))

## colocamos la primera funcion de correlacion
corre <- correlacionS(corre)
corre$filtro <- ifelse(abs(corre$cor)>0.6,1,0)

#write.csv(corre,"correlacion_variables.csv",row.names = F)

########### 6) MODELADO ################# 

#### -- Primer Modelo Rigde ########

# Modelo Base
modelo_ridge <- glmnet(x, y, family = "binomial", alpha = 0) # alpha = 0 modelo ridge 

# busqueda de los mejores parametros mediante CV
set.seed(123)
cv.ridge <- cv.glmnet(x, y, alpha = 0, family = "binomial",nfolds=5)

## valores lambda mediante academico y de negocio
cv.ridge$lambda.1se 
cv.ridge$lambda.min 

## muestra de los coef por cada uno
coef(modelo_ridge,s=cv.ridge$lambda.1se) # Muestra el lambda optimo sugerencia
coef(modelo_ridge,s=cv.ridge$lambda.min) # Muestra el lambda mediante Negocio

## Predicciones e indicadores de validacion

prediridge1 <- predict(cv.ridge,x,s="lambda.1se",type = 'response')
prediridge2 <- predict(cv.ridge,x,s="lambda.min",type = 'response')

# Indicadores

GINI_ridge1    <- Gini(prediridge1,data$diabetes)
GINI_ridge2    <- Gini(prediridge2,data$diabetes)
ks_ridge1      <- KS_Stat(prediridge1,data$diabetes)
ks_ridge2      <- KS_Stat(prediridge2,data$diabetes)
LogLoss_ridge1 <- LogLoss(prediridge1,data$diabetes)
LogLoss_ridge2 <- LogLoss(prediridge2,data$diabetes)

#### -- Segundo Modelo Lasso ########

# Modelo Base
modelo_lasso <- glmnet(x, y, family = "binomial", alpha = 1)

# busqueda de los mejores parametros mediante CV
set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial",nfolds=5)

## valores lambda mediante academico y de negocio
cv.lasso$lambda.1se 
cv.lasso$lambda.min 

## muestra de los coef por cada uno
coef(modelo_lasso,s=cv.lasso$lambda.1se) # Muestra el lambda optimo sugerencia
coef(modelo_lasso,s=cv.lasso$lambda.min) # Muestra el lambda mediante Negocio

## Predicciones e indicadores de validacion

predilasso1 <- predict(cv.lasso,x,s="lambda.1se",type = 'response')
predilasso2 <- predict(cv.lasso,x,s="lambda.min",type = 'response')

# Indicadores

GINI_lasso1     <- Gini(predilasso1,data$diabetes)
GINI_lasso2     <- Gini(predilasso2,data$diabetes)
ks_lasso1       <- KS_Stat(predilasso1,data$diabetes)
ks_lasso2       <- KS_Stat(predilasso2,data$diabetes)
LogLoss_lasso1  <- LogLoss(predilasso1,data$diabetes)
LogLoss_lasso2  <- LogLoss(predilasso2,data$diabetes)

########### 7) COMPARACION DE INDICADORES #################

tabla <- cbind(
  GINI_ridge1   ,
  GINI_ridge2   ,
  GINI_lasso1   ,
  GINI_lasso2   ,
  ks_ridge1     ,
  ks_ridge2     ,
  ks_lasso1     ,
  ks_lasso2     ,
  LogLoss_ridge1,
  LogLoss_ridge2,
  LogLoss_lasso1,
  LogLoss_lasso2
)

data.frame(tabla)

#### -- Tercer Modelo Elasticnet ########

# Modelo Base
modelo_elastic <- glmnet(x, y, family = "binomial", alpha = 0.5)

### Tarea:

# 1) realizar la particion muestral (80%,20%) con la semilla 1234
# 2) PROBAR CON LOS DISTINTOS TECNCIAS DE ESTADNRIZACION (PROBAR CON 2) 
# 3) relizar el ejercicio con los 3 tipos de modelos y 
#    compararlos mediante sus indicadores en la bbdd test

