library(tidyr)
library(tidyverse)
library(ggplot2)
load("tp2.RData")

# Agregamos columnas que faltan (ignoramos feriados) TODO: tener en cuenta feriados
clima_ecobici = clima_ecobici %>% mutate(llueve = prcp > 0) %>% mutate(dia_semana = weekdays(date)) %>% 
  mutate(dia_laborable = dia_semana != 'sábado' & dia_semana != 'domingo' & date != '2022-02-28' &
           date != '2022-03-01' &
           date != '2022-03-24' &
           date != '2022-04-14' &
           date != '2022-04-15' &
           date != '2022-05-18' &
           date != '2022-05-25' &
           date != '2022-06-17' &
           date != '2022-06-20' &
           date != '2022-08-15' &
           date != '2022-10-10' &
           date != '2022-11-21' &
           date != '2022-12-08' &
           date != '2022-12-09' ) %>% mutate(mes = months(date)) 

temperatura_mensual = clima_ecobici %>%
  group_by(mes)%>%
  summarise(tavg_prom = mean(tavg),
            tmin_prom = mean(tmin),
            tmax_prom = mean(tmax))


meses_ordenados = c('enero','febrero','marzo','abril','mayo','junio','julio','agosto','septiembre','octubre','noviembre','diciembre')

temperatura_mensual$mes = factor(temperatura_mensual$mes, levels = meses_ordenados)

temperatura_mensual = temperatura_mensual[order(temperatura_mensual$mes),]

# analisis exploratorio

# temperatura del día (promedio, mínima o máxima).
# si llueve o no llueve.
# presión atmosférica.
# velocidad del viento.
# si es día laborable o no laborable.

## Usos vs cada variable individualmente

# si llueve o no llueve.
ggplot(clima_ecobici, aes(x = llueve, y = n)) + geom_col() + labs(title = "Usos dìas de lluvia", y = "Cantidad de usos")
# buena

# presión atmosférica
ggplot(clima_ecobici, aes(x = pres, y = n)) + geom_point() + labs(title = "Usos segun presion atmosferica", y = "Cantidad de usos")

# velocidad del viento
ggplot(clima_ecobici, aes(x = wspd, y = n)) + geom_point() + labs(title = "Usos segun velocidad del viento", y = "Cantidad de usos", x = "Velocidad del viento")
# puede servir, parece que si hay mucho viento no se usan mucho

# si es día laborable o no laborable.
ggplot(clima_ecobici, aes(x = dia_laborable, y = n)) + geom_col() + labs(title = "Usos segun dia laborable o no laborable", y = "Cantidad de usos")
# sirve

# esta en una escala horrible: TODO: evaluar si lo cambiamos de escala

# temperatura del día (promedio).
ggplot(clima_ecobici, aes(x = tavg, y = n)) + geom_point() + labs(title = "Usos segun temperatura promedio", y = "Cantidad de usos")

# temperatura del día (tmin).
ggplot(clima_ecobici, aes(x = tmin, y = n)) + geom_point() + labs(title = "Usos segun temperatura minima", y = "Cantidad de usos")

# temperatura del día (tmax).
ggplot(clima_ecobici, aes(x = tmax, y = n)) + geom_point() + labs(title = "Usos segun temperatura maxima", y = "Cantidad de usos")


## Usos vs dos variables

# lluvia vs dia laborable
ggplot(clima_ecobici, aes(x = dia_laborable, y = n, fill = llueve)) + geom_col() + labs(title = "Usos segun dia laborable o no laborable y lluvia", y = "Cantidad de usos")


# temperatura del día (promedio) y lluvia.
ggplot(clima_ecobici, aes(x = tavg, y = n)) + geom_point(aes(color=llueve)) + labs(title = "Usos segun temperatura promedio y lluvia", y = "Cantidad de usos")

# temperatura del día (tmin) y lluvia.
ggplot(clima_ecobici, aes(x = tmin, y = n)) + geom_point(aes(color=llueve)) + labs(title = "Usos segun temperatura minima y lluvia", y = "Cantidad de usos")

# temperatura del día (tmax) y lluvia.
ggplot(clima_ecobici, aes(x = tmax, y = n)) + geom_point(aes(color=llueve)) + labs(title = "Usos segun temperatura maxima y lluvia", y = "Cantidad de usos")

# velocidad del viento
ggplot(clima_ecobici, aes(x = wspd, y = n)) + geom_point(aes(color=llueve)) + labs(title = "Usos segun velocidad del viento", y = "Cantidad de usos", x = "Velocidad del viento")

### usando dia laborable 

# temperatura del día (promedio) y lluvia.
ggplot(clima_ecobici, aes(x = tavg, y = n)) + geom_point(aes(color=dia_laborable)) + labs(title = "Usos segun temperatura promedio y dia laborable", y = "Cantidad de usos")

# temperatura del día (tmin) y lluvia.
ggplot(clima_ecobici, aes(x = tmin, y = n)) + geom_point(aes(color=dia_laborable)) + labs(title = "Usos segun temperatura minima y dia laborable", y = "Cantidad de usos")

# temperatura del día (tmax) y lluvia.
ggplot(clima_ecobici, aes(x = tmax, y = n)) + geom_point(aes(color=dia_laborable)) + labs(title = "Usos segun temperatura maxima y dia laborable", y = "Cantidad de usos")

# velocidad del viento
ggplot(clima_ecobici, aes(x = wspd, y = n)) + geom_point(aes(color=dia_laborable)) + labs(title = "Usos segun velocidad del viento y dia laborable", y = "Cantidad de usos", x = "Velocidad del viento")


## usamos lluvia y dia laborable
