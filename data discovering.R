# examen 2024: 
library(dplyr)
# --- DATA DISCOVERING: 
datos <- read.csv("datos\\water-quality-context.csv")
datos2 <- read.csv("datos\\water-quality-data.csv")


# rango de las variables numericas
summary(datos2)

# desviacion estandar de mdl y rdl: 
sd(datos2$MDL, na.rm=T) #0.4951991
sd(datos2$RDL, na.rm=T) #3.280455
# la desviacion estandar de la variable rdl es mayor por lo que los datos varian
# mas, es decir, estan más dispersos de la media que la variable mdl que al tener
# una desviacion estandar mayor, nos indica que los datos no son tan uniformes

# cambiar los valores de la media y la desviacion estandar de estas dos variables, 
# # cambiarai la dispersion de los datos. 
# MDL, que antes tenía una media muy baja y muy poca variabilidad, ahora parecería mucho más dispersa y centrada en valores más altos.
# 
# RDL, que era más dispersa y centrada en valores mayores, ahora parecería una variable más estable y de menor magnitud.
# 
# Comparabilidad entre variables cambiaría:
#   
#   Antes: RDL parecía una variable de mayor magnitud e inestabilidad, posiblemente más difícil de modelar.
# 
# Después del intercambio: parecería que MDL es más caótica y RDL más controlada.


# -------- VALOR MEDIO DE LA CALIDAD DE AGUA POR AREA (AREAS 5,20,33)
datos2$Area

valor_medio<- datos2 %>% 
  group_by(Area) %>% 
  summarise(MDL=mean(MDL, na.rm = T), RDL=mean(RDL, na.rm = T), Value=mean(Value, na.rm = T))

valor_medio<- valor_medio %>% 
  arrange(Area)






