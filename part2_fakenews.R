#BASE MODIFICADA

fake_news= fake_news %>%
  mutate(categoria_negativa = cut(negative, breaks=c(-Inf, 2,4,6,8,Inf), labels = c('Menor a 2%','2% a 4%', '4% a 6%', '6% a 8%','Más de 8%')))%>%
  mutate(cantidad_palabras_titulo = cut(title_words, breaks=c(-Inf, 5,10,15,20,Inf), labels = c('Menor a 5','5 a 10', '10 a 15', '15 a 20','Más de 20')))



#Cantidad de noticias que tienen signos de exclamación en el titulo

fake_news %>%
  count(title_has_excl)%>%
  ggplot(aes(x=title_has_excl, y = n))+
  geom_bar(stat = 'identity')+
  labs(title = 'Cantidad de noticias que contienen signos de exclamación', x= 'Contiene o no contiene', y='Cantidad')+
  theme(plot.title = element_text(face='bold'))

#Cantidad de noticias por % de expresiones negativas

fake_news %>%
  count(categoria_negativa)%>%
  ggplot(aes(x=categoria_negativa, y = n))+
  geom_bar(stat = 'identity')+
  labs(title = 'Cantidad de noticias que contienen % expresiones negativas en el titulo', x= 'Contiene o no contiene', y='Cantidad')+
  theme(plot.title = element_text(face='bold'))


#Cantidad de palabras en el titulo
fake_news %>%
  count(cantidad_palabras_titulo)%>%
  ggplot(aes(x=cantidad_palabras_titulo, y = n))+
  geom_bar(stat = 'identity')+
  labs(title = 'Cantidad de palabras en el titulo', x= 'Cantidad de palabras en el titulo', y='Cantidad de noticias')+
  theme(plot.title = element_text(face='bold'))
