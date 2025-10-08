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

data <- read.csv("carros2011.csv")

data <- data[,2:18]

########### 3) TRATAMIENTO DE LA DATA ################# 

## en primer lugar ver el analisis descriptivo de la data

resumen <- data.frame(summarizeColumns(data))
#write.csv(resumen,"tabla_resumen.csv")

########### 4) VARIABLES DEL MODELO ################# 

## variables del modelo ##

x <- model.matrix(precio_promedio~., data)[,-1]
y <- data$precio_promedio

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
modelo_ridge <- glmnet(x, y, alpha = 0)

# busqueda de los mejores parametros mediante CV
set.seed(123)
cv.ridge <- cv.glmnet(x, y, alpha = 0, nfolds=5)

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

MSE_ridge1           <- MSE(prediridge1,data$precio_promedio)
MSE_ridge2           <- MSE(prediridge2,data$precio_promedio)
R2_Score_ridge1      <- R2_Score(prediridge1,data$precio_promedio)
R2_Score_ridge2      <- R2_Score(prediridge2,data$precio_promedio)

#### -- Segundo Modelo Lasso ########

# Modelo Base
modelo_lasso <- glmnet(x, y, alpha = 1)

# busqueda de los mejores parametros mediante CV
set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1,nfolds=5)

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

MSE_lasso1           <- MSE(predilasso1,data$precio_promedio)
MSE_lasso2           <- MSE(predilasso2,data$precio_promedio)
R2_Score_lasso1      <- R2_Score(predilasso1,data$precio_promedio)
R2_Score_lasso2      <- R2_Score(predilasso2,data$precio_promedio)

########### 7) COMPARACION DE INDICADORES #################

tabla <- cbind( 
  MSE_ridge1      ,
  MSE_ridge2      ,
  MSE_lasso1      ,
  MSE_lasso2      ,
  R2_Score_ridge1 ,
  R2_Score_ridge2 ,
  R2_Score_lasso1 ,
  R2_Score_lasso2 
)

tabla

# para elasticnet se coloca alpha=0.5
# 1) realizar la particion muestral (90%,10%) con la semilla 1234
# 2) relizar el ejercicio con los 3 tipos de modelos y 
#    compararlos mediante sus indicadores 
#    y escribir el mejor modelo, enviar resultados y codigo


