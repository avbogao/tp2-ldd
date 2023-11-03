library(tidyr)
library(tidyverse)
load("tp2.RData")

# Agregamos columnas que faltan (ignoramos feriados)
clima_ecobici = clima_ecobici %>% mutate(llueve = prcp > 0) %>% mutate(dia_semana = weekdays(date)) %>% 
  mutate(dia_laborable = dia_semana != 'Saturday' & dia_semana != 'Sunday') %>% mutate(mes = months(date)) 

temperatura_mensual = clima_ecobici %>%
  group_by(mes)%>%
  summarise(tavg_prom = mean(tavg),
            tmin_prom = mean(tmin),
            tmax_prom = mean(tmax))


meses_ordenados = c('enero','febrero','marzo','abril','mayo','junio','julio','agosto','septiembre','octubre','noviembre','diciembre')

temperatura_mensual$mes = factor(temperatura_mensual$mes, levels = meses_ordenados)

temperatura_mensual = temperatura_mensual[order(temperatura_mensual$mes),]

# analisis exploratorio