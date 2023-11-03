install.packages("rpart")
install.packages("tidyverse")
find.package('rpart')
find.package("rpart.plot")
install.packages("remotes")
remotes::install_github("bryanhanson/parttree")
remotes::install_github("tidyverse/tidyverse")
require(palmerpenguins)
require(tidyverse)
require(rpart)
require(class)
require(ggplot2)
# no cargan
require(patchwork)
require(rpart.plot)   
require(parttree) 
library(parttree)
require(tidyr)

penguins_Gentoo = penguins %>% drop_na() %>% filter(species =="Gentoo")
penguins = penguins %>% drop_na()
# El objetivo de los ejercicios es trabajar con árboles de decisión para clasificar a pinguinos de la especie Gentoo en macho y hembra
# utilizando como variables a el largo del pico y el largo de la aleta.

# Realizar visualizaciones adecuadas para explorar si es esperable que las variables largo del pico y largo de la aleta serán útiles para
# clasificar a los pinguinos en machos y hembras. Evaluar cada variable por separado y las dos juntas.

# Largo del pico 
ggplot(penguins_Gentoo, aes(sex, bill_length_mm)) + geom_boxplot() + labs(title = "Distribucion del largo del pico en pingüinos macho vs hembras", x="Sexo",y="Largo del pico")
# Largo de la aleta
ggplot(penguins_Gentoo, aes(sex, flipper_length_mm)) + geom_boxplot() + labs(title = "Distribucion del largo de las aletas en pingüinos macho vs hembras", x="Sexo",y="Largo de las aletas")
# Ambas juntas
ggplot(penguins_Gentoo, aes(bill_length_mm,flipper_length_mm)) + geom_point(aes(color=sex)) + labs(title = "Distribucion del largo del pico y de las aletas en pingüinos machos y hembras", x="Largo del pico",y="Largo de las aletas")

# Por separado se puede ver que son buenas predictoras ya que los pinguinos machos suelen ser mas grandes (picos y aletas mas largos).



# Ejercicio 1

# Dividir el dataset en un conjunto de entrenamiento y uno de test (80%-20%) en forma aleatoria utilizando una semilla fija. 
# Utilizar, para este ejercicio, la variable largo del pico del pinguino para predecir el sexo de los pinguinos.
set.seed(199)
sample = sample(c(TRUE, FALSE), nrow(penguins_Gentoo), replace = T,prob=c(0.8,0.2))
train  <- penguins_Gentoo[sample, ]
test   <- penguins_Gentoo[!sample, ]

# a. Escribir el modelo de clasificación. Es decir, cómo se decide si un nuevo pinguino, del que se conoce su largo del pico, 
# es macho o hembra. ¿Cuál es el “accuracy” mínimo esperable para un modelo de clasificación con estos datos?
fit = rpart(sex ~ bill_length_mm, data = train, method = "class") # yo supuse que era sobre train
summary(fit)
ggplot(train, aes(sex, bill_length_mm)) +
  geom_point(aes(col=sex)) + 
  theme_minimal()

rpart.plot(fit, type = 4, extra = 2)

# acc del train
train$pred = predict(fit, train, type = "class") # sobre penguins o sobre train??
acc = mean(train$sex == train$pred)

# Es sobre test o sobre el df original ? 
test$pred = predict(fit, test, type = "class") # sobre penguins o sobre train??
accTest = mean(test$sex == test$pred)

# b. Usando el conjunto de entrenamiento, encontrar el clasificador que minimiza el error. Para hacerlo, tomar una grilla de valores 
# posibles de la “masa crítica” y calcular el error de clasificación para cada uno de estos valores. Hacer un gráfico del error de 
# clasificación en función de la “masa crítica”.

# Definir una secuencia de valores de masa crítica
masa_critica_values <- seq(0.01, 0.5, by = 0.01)

# Inicializar vectores para almacenar los errores
errores <- numeric(length(masa_critica_values))

# Ajustar modelos con diferentes valores de masa crítica y calcular el error de clasificación
for (i in 1:length(masa_critica_values)) {
  masa_critica <- masa_critica_values[i]
  model <- rpart(sex ~ bill_length_mm, data = train, method = "class", control = rpart.control(cp = masa_critica))
  predictions <- predict(model, newdata = train, type = "class")
  errores[i] <- mean(predictions != train$sex)
}

# Crear un gráfico del error de clasificación en función de la masa crítica
plot(masa_critica_values, errores, type = "l", xlab = "Masa Crítica", ylab = "Error de Clasificación")

# Encontrar el valor de masa crítica que minimiza el error
min_error <- min(errores)
best_masa_critica <- masa_critica_values[errores == min_error]
cat("Mejor valor de Masa Crítica:", best_masa_critica, "\n")


# c. Probar la capacidad predictiva de este clasificador en el conjunto de test. ¿Cuál es la matriz de confusión y la “accuracy” del 
# modelo?
confusion_matrix <- table(Real = test$sex, Predicted = test$pred)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy del modelo en el conjunto de prueba:", accuracy, "\n")
  
### Ejercicio 2
# Repetir los pasos del ejercicio 1 utilizando esta vez sólo la variable largo de la aleta. 
# Comparar la capacidad predictiva de este modelo con el encontrado en el ejercicio 1. ¿Era previsible el resultado en base a lo visto exploratoriamente en el ejercicio 0?
  
# a. Escribir el modelo de clasificación. Es decir, cómo se decide si un nuevo pinguino, del que se conoce su largo del pico, 
# es macho o hembra. ¿Cuál es el “accuracy” mínimo esperable para un modelo de clasificación con estos datos?
fit2 = rpart(sex ~ flipper_length_mm, data = train, method = "class") # yo supuse que era sobre train
summary(fit2)
ggplot(train, aes(sex, flipper_length_mm)) +
  geom_point(aes(col=sex)) + 
  theme_minimal()

rpart.plot(fit2, type = 4, extra = 2)

# acc del train
predictions2 = predict(fit2, train, type = "class") # sobre penguins o sobre train??
acc = mean(train$sex == predictions2)

# Es sobre test o sobre el df original ? 
test_predictions2 = predict(fit2, test, type = "class") # sobre penguins o sobre train??
accTest = mean(test$sex == test_predictions2)

# b. Usando el conjunto de entrenamiento, encontrar el clasificador que minimiza el error. Para hacerlo, tomar una grilla de valores 
# posibles de la “masa crítica” y calcular el error de clasificación para cada uno de estos valores. Hacer un gráfico del error de 
# clasificación en función de la “masa crítica”.

# Definir una secuencia de valores de masa crítica
masa_critica_values <- seq(0.01, 0.5, by = 0.01)

# Inicializar vectores para almacenar los errores
errores <- numeric(length(masa_critica_values))

# Ajustar modelos con diferentes valores de masa crítica y calcular el error de clasificación
for (i in 1:length(masa_critica_values)) {
  masa_critica <- masa_critica_values[i]
  model <- rpart(sex ~ flipper_length_mm, data = train, method = "class", control = rpart.control(cp = masa_critica))
  predictions <- predict(model, newdata = train, type = "class")
  errores[i] <- mean(predictions != train$sex)
}

# Crear un gráfico del error de clasificación en función de la masa crítica
plot(masa_critica_values, errores, type = "l", xlab = "Masa Crítica", ylab = "Error de Clasificación")

# Encontrar el valor de masa crítica que minimiza el error
min_error <- min(errores)
best_masa_critica <- masa_critica_values[errores == min_error]
cat("Mejor valor de Masa Crítica:", best_masa_critica, "\n")


# c. Probar la capacidad predictiva de este clasificador en el conjunto de test. ¿Cuál es la matriz de confusión y la “accuracy” del 
# modelo?
confusion_matrix2 <- table(Real = test$sex, Predicted = test_predictions2)
print(confusion_matrix2)

accuracy2 <- sum(diag(confusion_matrix2)) / sum(confusion_matrix2)
cat("Accuracy del modelo en el conjunto de prueba:", accuracy2, "\n")


### Ejercicio 3
# Crear un árbol de decisión para predecir el sexo de los pinguinos en función del peso y el largo de la aleta. 
# Evaluar la perfomance del modelo usando la matriz de confusión y “accuracy”. Realizar visualizaciones pertinentes.

fit3 = rpart(sex ~ flipper_length_mm + body_mass_g, data = train, method = "class") # yo supuse que era sobre train
summary(fit3)
ggplot(train, aes(sex, flipper_length_mm)) +
  geom_point(aes(col=sex)) + 
  theme_minimal()

rpart.plot(fit3, type = 4, extra = 2)

# acc del train
predictions3 = predict(fit3, train, type = "class") # sobre penguins o sobre train??
acc = mean(train$sex == predictions3)

# Es sobre test o sobre el df original ? 
test_predictions3 = predict(fit3, test, type = "class") # sobre penguins o sobre train??
accTest = mean(test$sex == test_predictions3)

confusion_matrix3 <- table(Real = test$sex, Predicted = test_predictions3)
print(confusion_matrix3)

accuracy3 <- sum(diag(confusion_matrix3)) / sum(confusion_matrix3)
cat("Accuracy del modelo en el conjunto de prueba:", accuracy3, "\n")

# Este es el mejor de los 3 modelos, tenemos como evidencia la matriz de confusion y el accuracy de aprox 0.9.


##### Ejercicio 4
# Repetir el ejercicio 3 para otras 1000 particiones train-test. Para cada una guardar el “accuracy” en el grupo de test. Con esos datos hacer una visualización del “accuracy”.
# ¿Qué se puede decir del accuracy del modelo (por ejemplo, en qué rango se encuentra)?

accuracy_vector <- rep(0,1000)

for(i in 1:length(accuracy_vector)) {
  # armado de particiones train-test
  sample = sample(c(TRUE, FALSE), nrow(penguins_Gentoo), replace = T,prob=c(0.8,0.2))
  train  <- penguins_Gentoo[sample, ]
  test   <- penguins_Gentoo[!sample, ]
  
  # Entrenar el modelo
  model <- rpart(sex ~ flipper_length_mm + body_mass_g, data = train, method = "class") 
  
  # Medir accuracy
  test_pred = predict(model, test, type = "class") # sobre penguins o sobre train??
  accTest = mean(test$sex == test_pred)
  
  c_matrix <- table(Real = test$sex, Predicted = test_pred)
  print(c_matrix)
  
  accuracy_vector[i] <- sum(diag(c_matrix)) / sum(c_matrix)
}

plot(density(accuracy_vector), main="Distribución de Precisión del Modelo", xlab="Precisión")
boxplot(accuracy_vector, main="Distribución de Precisión del Modelo", ylab="Precisión")

## con la evidencia empirica obtenida vemos que la accuracy esta por 0.88 aprox

#### Ejercicio 5
# A partir de lo hecho en el ejercicio 4, explorar diferentes parámetros de los árboles de decisión para encontrar la mejor configuración en términos de mejorar la predicción en el grupo de test.
# Si se aumenta el número de particiones del árbol, ¿qué es esperable que ocurra con el error de predicción dentro del dataset de entrenamiento y en el de test?

accuracy_vector <- rep(0,1000)

for(i in 1:length(accuracy_vector)) {
  # armado de particiones train-test
  sample = sample(c(TRUE, FALSE), nrow(penguins_Gentoo), replace = T,prob=c(0.8,0.2))
  train  <- penguins_Gentoo[sample, ]
  test   <- penguins_Gentoo[!sample, ]
  
  # Entrenar el modelo
  model <- rpart(sex ~ body_mass_g + bill_length_mm + bill_depth_mm, data = train, method = "class") 
  
  # Medir accuracy
  test_pred = predict(model, test, type = "class") # sobre penguins o sobre train??
  accTest = mean(test$sex == test_pred)
  
  c_matrix <- table(Real = test$sex, Predicted = test_pred)
  print(c_matrix)
  
  accuracy_vector[i] <- sum(diag(c_matrix)) / sum(c_matrix)
}

plot(density(accuracy_vector), main="Distribución de Precisión del Modelo", xlab="Precisión")
boxplot(accuracy_vector, main="Distribución de Precisión del Modelo", ylab="Precisión")

# El error de predicción dentro del conjunto de entrenamiento tenderá a disminuir a medida que aumentas la profundidad del árbol. Un árbol más profundo puede ajustarse mejor a los datos de entrenamiento, lo que lleva a una mayor precisión en el conjunto de entrenamiento. Sin embargo, un árbol muy profundo puede llevar a un sobreajuste (overfitting) en el conjunto de entrenamiento, lo que significa que el modelo se adaptará en exceso a los datos de entrenamiento y no generalizará bien en datos no vistos.
# El error de predicción en el conjunto de prueba disminuirá hasta cierto punto a medida que aumentes la profundidad del árbol, ya que el modelo tendrá más capacidad para capturar patrones en los datos. Sin embargo, después de un cierto punto, el error de predicción en el conjunto de prueba comenzará a aumentar nuevamente debido al sobreajuste. El modelo se volverá demasiado complejo y no generalizará bien a nuevos datos.
# En resumen, al aumentar la profundidad del árbol, es esperable que el error de predicción disminuya en el conjunto de entrenamiento y, posteriormente, aumente en el conjunto de prueba debido al sobreajuste


###Ejercicio 6
# Implementar un clasificador de k-NN que prediga el sexo de los pinguinos utilizando como variables a el largo del pico y el largo de la aleta. 
# Hacerlo para diferentes valores de k (impares) y evaluar el error de predicción en cada caso. ¿Cómo elegiría el valor de k óptimo? 
# Comparar con el modelo de árboles de decisión de la parte 1.

test_predk = knn(
  train = penguins_Gentoo[,c("bill_length_mm","flipper_length_mm")],
  test = penguins_Gentoo[,c("bill_length_mm","flipper_length_mm")],
  cl = penguins_Gentoo$sex,
  k = 5
)

penguins_Gentoo$pred_knn = test_predk
prediction_error_1 = 1-mean(penguins_Gentoo$sex == penguins_Gentoo$pred_knn)
#-
test_predk = knn(
  train = penguins_Gentoo[,c("bill_length_mm","flipper_length_mm")],
  test = penguins_Gentoo[,c("bill_length_mm","flipper_length_mm")],
  cl = penguins_Gentoo$sex,
  k = 7
)

penguins_Gentoo$pred_knn = test_predk
prediction_error_2 = 1 - mean(penguins_Gentoo$sex == penguins_Gentoo$pred_knn)
#_
test_predk = knn(
  train = penguins_Gentoo[,c("bill_length_mm","flipper_length_mm")],
  test = penguins_Gentoo[,c("bill_length_mm","flipper_length_mm")],
  cl = penguins_Gentoo$sex,
  k = 9
)

penguins_Gentoo$pred_knn = test_predk
prediction_error_3 = 1 - mean(penguins_Gentoo$sex == penguins_Gentoo$pred_knn)
#-
test_predk = knn(
  train = penguins_Gentoo[,c("bill_length_mm","flipper_length_mm")],
  test = penguins_Gentoo[,c("bill_length_mm","flipper_length_mm")],
  cl = penguins_Gentoo$sex,
  k = 3
)

penguins_Gentoo$pred_knn = test_predk
prediction_error_4 = 1 - mean(penguins_Gentoo$sex == penguins_Gentoo$pred_knn)
## Este modelo funciona un poco mejor que el visto con arboles de decision, el antterior tenia acc 0.83 y este tiene 0.88
#### Parte 2
# Este ejercicio es más libre, tienen que montar un modelo de clasificiacón para la especie de los pinguinos. 
# Pueden elegir las variables y el método (k-NN o árboles de decisión). Reporten los resultados con visualizaciones adecuadas.


# Analisis exploratorio para saber cuales datos me van a ser utiles para predecir la especie de los pinguinos

ggplot(penguins, aes(
    x = species,
    y = body_mass_g
  ),
fill=species) + geom_boxplot() + labs(title = "Distribución de pesos por especie",y="Peso (g)",x="Especie")
## Lo mas pesados son los Gentoo, las otras dos especies son similares

ggplot(penguins, aes(
  x = species,
  y = flipper_length_mm
),
fill=species) + geom_boxplot() + labs(title = "Distribución de largo de las aletas por especie",y="Largo de las aletas (mm)",x="Especie")
## Los Gentoo tienen las aletas mas largas, les siguen los Chinstrap

ggplot(penguins, aes(
  x = species,
  y = bill_length_mm
),
fill=species) + geom_boxplot() + labs(title = "Distribución de largo del pico por especie",y="Largo del pico (mm)",x="Especie")
## Los Adelie tienen los picos mas cortos.

ggplot(penguins, aes(
  x = species,
  y = bill_depth_mm
),
fill=species) + geom_boxplot() + labs(title = "Distribución de ancho del pico por especie",y="Ancho del pico (mm)",x="Especie")
## Los Gentoo tienen los picos mas angostos. Las otras dos especies tienen un grosor similar.

ggplot(penguins,
       aes(
         x = bill_length_mm,
         y = bill_depth_mm
       ))  + geom_point(aes(color = species)) + labs(title = "Distribución de largo y ancho del pico por especie",y="Ancho del pico (mm)",x="Largo del pico (mm)")

## * Los Adelie tienen picos mas anchos y cortos. 
#  * Los Gentoo tienen picos mas largos y angostos.
#  *Los Chinstrap tienen picos largos y anchos.

ggplot(penguins,
       aes(
         y = bill_length_mm,
         x = flipper_length_mm
       ))  + geom_point(aes(color = species)) + labs(title = "Distribución de largo del pico y las aletas por especie",y="Largo del pico (mm)",x="Largo de las aletas (mm)")

### se seoaran basttante bien las especies con estas variables !!
## * Los Adelie tienen picos cortos y aletas cortas. 
#  * Los Gentoo tienen picos y aletas largos.
#  *Los Chinstrap tienen picos largos y aletas medianas.

ggplot(penguins,
       aes(
         y = bill_depth_mm,
         x = flipper_length_mm
       ))  + geom_point(aes(color = species)) + labs(title = "Distribución de ancho del pico y largo de las aletas por especie",y="Ancho del pico (mm)",x="Largo de las aletas (mm)")

### Los Adelie y los Chinstrap son muy similares (intuyo que no son buenas indicadoras juntas)
## * Los Adelie tienen picos anchos y aletas cortas. 
#  * Los Gentoo tienen picos angostos y aletas largas.
#  *Los Chinstrap tienen picos anchos y aletas cortas.


ggplot(penguins,
       aes(
         y = body_mass_g,
         x = bill_length_mm
       ))  + geom_point(aes(color = species)) + labs(title = "Distribución de largo del pico y peso por especie",y="Peso (g)",x="Largo del pico (mm)")

### se seoaran basttante bien las especies con estas variables !!
## * Los Adelie tienen picos cortos y pesan poco. 
#  * Los Gentoo tienen picos largos y son pesados.
#  *Los Chinstrap tienen picos largos y pesan poco.


# Vamos a usar un modelo de arboles de division
set.seed(3399)
sample = sample(c(TRUE, FALSE), nrow(penguins), replace = T,prob=c(0.8,0.2))
train  <- penguins[sample, ]
test   <- penguins[!sample, ]


fit = rpart(species ~ bill_length_mm + body_mass_g, data = train, method = "class")
summary(fit)

rpart.plot(fit, type = 4, extra = 2)

# acc del train
pred_train = predict(fit, train, type = "class") # sobre penguins o sobre train??
acc_train = mean(train$species == pred_train)

# Es sobre test o sobre el df original ? 
pred_test = predict(fit, test, type = "class") # sobre penguins o sobre train??
acc_test = mean(test$species == pred_test)

##### Muy buena accuracy!! 0.93 en el test

confusion_matrix_1 <- table(Real = test$species, Predicted = pred_test)
print(confusion_matrix_1)

accuracy_1 <- sum(diag(confusion_matrix_1)) / sum(confusion_matrix_1)
cat("Accuracy del modelo en el conjunto de prueba:", accuracy_1, "\n")

### -----

# Vamos con otro modelo:  Largo del pico y largo de las aletas


fit2 = rpart(species ~ bill_length_mm + flipper_length_mm, data = train, method = "class")
summary(fit2)

rpart.plot(fit, type = 4, extra = 2)

# acc del train
pred_train_2 = predict(fit2, train, type = "class") 
acc_train_2 = mean(train$species == pred_train_2)

# Es sobre test o sobre el df original ? 
pred_test_2 = predict(fit2, test, type = "class")
acc_test_2 = mean(test$species == pred_test_2)

##### Muy buena accuracy!! 0.93 en el test tambien

confusion_matrix_2 <- table(Real = test$species, Predicted = pred_test_2)
print(confusion_matrix_2)

accuracy_2 <- sum(diag(confusion_matrix_2)) / sum(confusion_matrix_2)
cat("Accuracy del modelo en el conjunto de prueba:", accuracy_2, "\n")

## Ambos modelos son muy buenos
