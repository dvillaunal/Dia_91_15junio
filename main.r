## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: ANÁLISIS DISCRIMINANTE LINEAL Y CUADRÁTICO EN R [Parte 3]
## 
##  4. Fuentes:
##     https://www.r-bloggers.com/2018/11/linear-quadratic-and-regularized-discriminant-analysis/"


## ------------------------------------------------------------------------------------
# Cargamos la base de datos del replit anterior:
Auto <- read.csv(file = "Auto.csv")
Auto2 <- read.csv(file = "Auto2.csv")



## ------------------------------------------------------------------------------------
# Guardamos los OUTPUTS:
sink("OUTPUTS.txt", split = T)

# Matrices de Covarianza:
#cargarmos la libreria que contiene la función "boxM()":
#install.packages("biotools")
library(biotools)

# Realizamos una matriz de covarainza
boxM(data = Auto2[, 2:4], grouping = Auto2[, 6])

# Resultados:
print("El test M de Box muestra evidencias significativas de que la matriz de covarianza no es constante para todos los grupos")

print("lo que haría apropiado aplicar QDA en lugar de LDA, pero en este caso ante la falta de normalidad multivariante en los datos, el test podría haberse visto afectado por ello.")


## ------------------------------------------------------------------------------------
# Creamos una semilla:
set.seed(1)

# Creamos una muestra pseudoAletorio del tamano de las filas * 0.8
entrenamiento <- sample(x = nrow(Auto), size = nrow(Auto)*0.8, replace = FALSE)

# Subgrupo de datos de entrenamiento
Auto.train <- Auto[entrenamiento,]

# Subgrupo de datos de test
Auto.test <- Auto[-entrenamiento,]

# Comprobamos que la suma de observaciones de cada subgrupo iguala al set de datos original
print("# Comprobamos que la suma de observaciones de cada subgrupo iguala al set de datos original")

print(nrow(Auto.train)) #1.


print(nrow(Auto.test)) #2.

# nrow(Auto.train) + nrow(Auto.test) == nrow(Auto)
print("nrow(Auto.train) + nrow(Auto.test) == nrow(Auto)")
print("OUTPUT:")
print(nrow(Auto.train) + nrow(Auto.test) == nrow(Auto))


## ------------------------------------------------------------------------------------
# Cargar el paquete:
#install.packages("MASS")
library(MASS)

# Modelo LDA con los datos de entrenamiento
print("# Modelo LDA con los datos de entrenamiento")
modelo.lda <- lda(formula = mpg01 ~ cylinders + displacement + horsepower + weight, data = Auto.train)

print(modelo.lda)

# Resultado:
"El modelo calcula automáticamente las probabilidades a priori"
"(PI_0 = 0,514, PI_1 = 0,485)"
" y el promedio de cada predictor dentro de cada clase, usados por el modelo como estimadores de µk."
"Los coeficientes proporcionan la combinación de los predictores"
"(- 0,4183 cylinders - 0,0017 displacement + 0,0028 – 0,0009), para generar los discriminantes lineales para cada una de las observaciones de entrenamiento."

# Modelo QDA con los datos de entrenamiento
print("# Modelo QDA con los datos de entrenamiento")
modelo.qda <- qda(formula = mpg01 ~ cylinders + displacement + horsepower + weight, data = Auto.train)

print(modelo.qda)

# Explicación:
"A diferencia del LDA, el QDA no contiene los coeficientes de los discriminantes lineales, puesto que el clasificador QDA se basa en una función cuadrática de los predictores, no lineal."


## ------------------------------------------------------------------------------------
# Cargamos el paquete "caret"
#install.packages("caret")
library(caret)

#Se ajusta sólo el modelo LDA con este paquete:
print("#Se ajusta sólo el modelo LDA con este paquete:")

modelo.lda.caret <- train(as.factor(mpg01) ~ cylinders + displacement + horsepower + weight, method ='lda',data=Auto.train)

print(modelo.lda.caret)

sink()