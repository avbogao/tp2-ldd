library(tidyverse)
library(ggplot2)
library(class)
library(rpart)
library(rpart.plot)





#BASE MODIFICADA

fake_news= fake_news %>%
  mutate(categoria_negativa = cut(negative, breaks=c(-Inf, 2,4,6,8,Inf), labels = c('Menor a 2%','2% a 4%', '4% a 6%', '6% a 8%','Más de 8%')))%>%
  mutate(cantidad_palabras_titulo = cut(title_words, breaks=c(-Inf, 5,10,15,20,Inf), labels = c('Menor a 5','5 a 10', '10 a 15', '15 a 20','Más de 20')))



#Cantidad de noticias que tienen signos de exclamación en el titulo

fake_news %>%
  group_by(type)%>%
  count(title_has_excl)%>%
  ggplot(aes(x=title_has_excl, y = n, fill = type))+
  geom_col()+
  labs(title = 'Cantidad de noticias que contienen signos de exclamación', x= 'Contiene o no contiene', y='Cantidad')+
  theme(plot.title = element_text(face='bold'))

#Cantidad de noticias por % de expresiones negativas

fake_news %>%
  group_by(type)%>%
  count(categoria_negativa)%>%
  ggplot(aes(x=categoria_negativa, y = n, fill = type))+
  geom_col()+
  labs(title = 'Cantidad de noticias que contienen % expresiones negativas en el titulo', x= '% contenido', y='Cantidad')+
  theme(plot.title = element_text(face='bold'))

#Cantidad de palabras en el titulo
fake_news %>%
  group_by(type)%>%
  count(cantidad_palabras_titulo)%>%
  ggplot(aes(x=cantidad_palabras_titulo, y = n, fill = type))+
  geom_col()+
  labs(title = 'Cantidad de palabras en el titulo', x= 'Cantidad de palabras en el titulo', y='Cantidad de noticias')+
  theme(plot.title = element_text(face='bold'))



#Podemos observar que las noticias falsas tienen mayor cantidad de signos de exclamación en el titulo que las noticias reales, tienen mayor porcentaje de palabras 
#de connotación negativa en sus titulos que las noticias reales y que los titulos son más cortos.



#reduzco base para trabajar mejor
fake_news2 = fake_news %>%
  select(type, title_has_excl, negative, title_words)


#BASES TRAIN Y TEST
set.seed(123)
fakenews_train = fake_news2 %>%
  sample_frac(0.8,replace = F) 

fakenews_test = anti_join(fake_news2,fakenews_train) #??


#para poder armar el modelo de KNN es necesario elegir primero un K que sirva de referencia para que el modelo pueda predecir la respuesta. Dependiendo el K elegido,
# va a variar la precisión del modelo. Como la base de entrenamiento tiene 120 observaciones, el K máximo que se puede elegir es 120, y el mínimo es 1. 
# Para poder ver cuales serian los K que mayor precisión arrojan, elaboramos el siguiente grafico de evolución donde se evalua la precisión en función del K.

k = 1:120

resultado = data.frame(k, precision = 0)

for (n in k){
  fakenews_model = knn(train=fakenews_train[, -1],
                                 cl=fakenews_train[, 1],
                                 test=fakenews_test[, -1],
                                 k=n)
  resultado$precision[n] = mean(fakenews_model == fakenews_test[, 1])
}





#K con precisión máxima (K = 1)

k_maximo = resultado$k[order(resultado$precision, decreasing = TRUE)[1]]


fakenews_model_kmax = knn(train=fakenews_train[, -1],
                                    cl=fakenews_train[, 1],
                                    test=fakenews_test[, -1],
                                    k=k_maximo)
#ACCURANCY

precision_kmax = mean(fakenews_model_kmax == fakenews_test[, 1])

#MATRIZ DE CONFUSION
table(fakenews_model_kmax,fakenews_test[, 1])

#El modelo predijo 10 noticias fake y en el test el valor coincidia. Respecto de las noticias reales, predijo 20 noticias pero solo 18 coinciden.



#K con segunda precisión (K = 9)

k_segundo = resultado$k[order(resultado$precision, decreasing = TRUE)[2]]

fakenews_model_2k = knn(train=fakenews_train[, -1],
                        cl=fakenews_train[, 1],
                        test=fakenews_test[, -1],
                        k=k_segundo)

#ACCURANCY

precision_2k = mean(fakenews_model_2k == fakenews_test[, 1])

#MATRIZ DE CONFUSION
table(fakenews_model_2k,fakenews_test[, 1])

#El modelo predijo 9 noticias fake y en el test el valor real es 8. Respecto de las noticias reales, predijo 21 noticias pero solo 17 coinciden.


#GRAFICO DE EVOLUCION DE LAS K EN FUNCION DE LA PRECISION
resultado %>%
  ggplot(aes(k,precision))+
  geom_line()+
  geom_vline(xintercept = k_maximo,
             linetype = 2, 
             color = 2)+
  geom_vline(xintercept = k_segundo,
             linetype = 2,
             color= 4)+
  geom_hline(yintercept = precision_kmax,
             linetype = 2, 
             color = 2)+
  geom_hline(yintercept = precision_2k,
             linetype = 2,
             color= 4)+
  geom_point(aes(k_maximo,precision_kmax), color = 'red')+
  geom_point(aes(k_segundo,precision_2k), color = 'blue')+
  labs(title = 'Evolución de la precisión según K elegido para el modelo')


#ARBOL DE DECISION

#modelo
arbol_fake = rpart(type~., data = fakenews_train)

#grafico del modelo
rpart.plot(arbol_fake)


#predicción
fakenews_test$pred_arbol=predict(arbol_fake,fakenews_test,type = 'class')


#matriz de confusión
table(fakenews_test$pred_arbol,fakenews_test$type)

#El modelo predice 12 noticias fake pero en realidad son 8, y noticias reales predice 18 pero en realidad son 14.


#precisión del modelo
precision_arbol = 100*mean(fakenews_test$pred_arbol==fakenews_test$type)

#el modelo tiene un 73,33% de precisión


#CONCLUSIÓN: Entre ambos modelos, para este caso conviene utilizar el modelo KNN ya que la precisión de 
#predicción es mayor (93,33% y 83,33% en KNN con K=1 y K=9 respectivamente vs 73,33% de precisión del arbol de decisión)

#creamos un nuevo dataframe con los datos nuevos que hay que predecir

new_data = data.frame(type = NA,
                      title_has_excl= FALSE,
                      negative = 6.00,
                      title_words = 15)


#prediccion segun modelo KNN

#con K=maximo

prob_kmax = predict(fakenews_model_kmax, newdata = new_data, type='prob')


#con k=segundo
prob_2k = predict(fakenews_model_2k, new_data, type = 'prob')

#prediccion segun arbol de decisión
new_data$pred_arbol=predict(arbol_fake,new_data,type = 'prob')

#resultado
new_data$pred_arbol

#CONCLUSION: El modelo de predicción más preciso es el KNN con K=1, por lo que la noticia probablemente sea real. Sim embargo, el modelo KNN con K=9 y 
#el arbol de decisión predicen que la noticia es fake aunque la precisión de ambos mode
