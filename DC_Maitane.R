# DATA CLEANING
library(digest)
library(discretization)
library(dplyr)
library(EnvStats)
library(fastDummies)
library(naniar)
library(nortest)
library(readxl)
library(smoothmest)
library(VIM)
library(plumber)
# analizar missing tanto por variable como por registro
c <- miss_case_summary(datos2) # la mayoria de registros tienen 4 columnas con NA
v <- miss_var_summary(datos2) # hay cuatro variables con missings, el resto no

# ¿En qué variable se concentran la mayoría de NA-s? ¿Qué porcentaje de NA-s presenta? 
max(v$pct_miss) # variable RDL y presenta el 51,9% de missings

#•	¿Cuántos NA tienen los primeros 5 registros con más NA? ¿Qué porcentaje de NA presentan? 
# tienen 4 NA's y presentan el 36,4 % de valores ausentes

# visualizar NA con sample eligiendo solo el 1% de los regisros y determinar si es MAR, MNAR, o MCAR

# 1. Calcular el tamaño de la muestra (1% del total de filas)
tamano_muestra <- round(0.01 * nrow(datos2))

# 2. Tomar una muestra aleatoria de filas
muestra <- datos2[sample(nrow(datos2), tamano_muestra), ]

# visualizamos:
vis_miss(muestra) # missing at random

# -------- OUTLIERS EN LAS VARIABLES VALUE, MDL Y RDL
# Si pudieramos asegurar que las variables siguen distribucion normal y por el gran 
# volumen de datos, deberiamos de utilizar el test de rosner para identificar
# outliers y el test de lillie para saber si siguen una distribucion normal

# mirar si siguen distribucion normal y si siguen aplicarlo para detectar outliers

lillie.test(datos2$RDL) # p < 0.05 --> NO
lillie.test(datos2$MDL) # p < 0.05 --> NO
lillie.test(datos2$Value) # p < 0.05 --> NO

# como no podemos utilizar ningun test, utilizamos el filtro de HAMPEL pero utilizando
# 12 veces la desviacion media para el limite superior
# valores que se salen fuera de este rango: (media-3*sd, media+12*sd)

# RDL
media_rdl <- mean(datos2$RDL,na.rm=T)
sd_rdl <- sd(datos2$RDL,na.rm=T)
datos2 %>% filter(RDL < (media_rdl-3*sd_rdl)) # no hay ningun valor que se sale de este rango
datos2 %>% filter(RDL > media_rdl+12*sd_rdl) # viendo que la media es muy baja, consideramos como outlier
# sustituimos por NA
datos2$RDL[datos2$RDL > media_rdl+12*sd_rdl] <- NA

# MDL
media_mdl <- mean(datos2$MDL,na.rm=T)
sd_mdl <- sd(datos2$MDL,na.rm=T)
datos2 %>% filter(MDL < (media_mdl-3*sd_mdl))# no hay ningun valor
outliers_mdl <- datos2 %>% filter(MDL > media_mdl+12*sd_mdl) # parecen ser outliers
# sustituimos los de 100 por NA
datos2$MDL[datos2$MDL > media_mdl+12*sd_mdl] <- NA


# VALUE
media_value <- mean(datos2$Value,na.rm=T)
sd_value <- sd(datos2$Value,na.rm=T)
datos2 %>% filter(Value < (media_value-3*sd_value)) # ningun valor
value_outliers <- datos2 %>% filter(Value > media_value+12*sd_value)
# sustituimos por NA
datos2$Value[datos2$Value > media_value+12*sd_mdl] <- NA

# el patron es que siempre hay outliers a partir del tercer quartil





