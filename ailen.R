library(tidyr)
library(tidyverse)
library(ggplot2)
load("tp2.RData")

# Agregamos columnas que faltan (ignoramos feriados)
clima_ecobici = clima_ecobici %>% mutate(llueve = prcp > 0) %>% mutate(dia_semana = weekdays(date)) %>% 
  mutate(dia_laborable = dia_semana != 'Saturday' & dia_semana != 'Sunday') %>% mutate(mes = months(date)) 

#Armamos vectores de meses y dias para ordenar los df que creemos
meses_ordenados = c('enero','febrero','marzo','abril','mayo','junio','julio','agosto','septiembre','octubre','noviembre','diciembre')
dias_ordenados = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo')



# DF temperatura por mes (promedio, minima promedio, maxima promedio)
temperatura_mensual = clima_ecobici %>%
  group_by(mes)%>%
  summarise(tavg_prom = mean(tavg),
            tmin_prom = mean(tmin),
            tmax_prom = mean(tmax))

temperatura_mensual$mes = factor(temperatura_mensual$mes, levels = meses_ordenados)

temperatura_mensual = temperatura_mensual[order(temperatura_mensual$mes),]

#
temperatura_dia_semana = clima_ecobici %>%
  group_by(dia_semana)%>%
  summarise(tavg_prom = mean(tavg),
            tmin_prom = mean(tmin),
            tmax_prom = mean(tmax))

temperatura_dia_semana$dia_semana = factor(temperatura_dia_semana$dia_semana, levels = dias_ordenados)

temperatura_dia_semana = temperatura_dia_semana[order(temperatura_dia_semana$dia_semana),]




# analisis exploratorio
