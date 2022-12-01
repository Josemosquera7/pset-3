# Jose Esteban Mosquera - 202110281
# David Chaparro - 202023984
# Santiago Huertas -

# Problem Set 3 - Taller de R estadistica y programacion 
# Profesor Eduard Martinez

##packages
require(pacman)
p_load(idyverse, rio,estimatr,lmtest,fixest, modelsummary, stargazer)

##Punto 1

## load data
base_regresiones <- import(file="input/data_regresiones.rds")

# Punto 1.1
modelo_1 <- lm(price ~ surface_total + rooms + bathrooms, data = base_regresiones)
modelo_2 <- lm(price ~ dist_cbd + dist_cole + dist_park, data = base_regresiones)
modelo_3 <- lm(price ~ surface_total +  rooms + bathrooms + as.factor(property_type), data = base_regresiones )

# Punto 1.2

tabla_resultados <- msummary(list(modelo_1, modelo_2, modelo_3))
tabla_resultados
