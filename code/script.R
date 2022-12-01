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




##Punto 2
##Incisio 1
## llamar y/o instalar librerias que nos permitirán trabajar con los datos espaciales.
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata,
       ggplot2, 
       ggmap)

##Primero, obtendremos la caja de coordenada que contiene el poligono (que es un tipo de arreglo espacial) para Bucaramanga
opq(bbox = getbb("Bucaramanga Colombia")) #queda pintado en la consola

##Ahora vamos a crear un objeto osm que contenga un feature sobre los restaurantes en Bucaramanga.
## Para los restaurantes, usaremos como key=amenity y value=restaurant. La clase del objeto osm sera una lista que contiene los datos espaciales de los restaurates en bucaramanga
osm_rest <- opq(bbox = getbb("Bucaramanga Colombia")) %>%
  add_osm_feature(key="amenity" , value="restaurant") 
class(osm_rest) 

##Haremos lo mismo para los parques en Bucaramanga. La idea es que la geometria espacial del objeto sea un polígono.
##  Para esto, usaremos como key=leisure y value=park
osm_parques <- opq(bbox = getbb("Bucaramanga Colombia")) %>%
  add_osm_feature(key="leisure" , value="park") 
class(osm_parques) 

##Ahora vamos a acceder a los Simple Features de los objetos osm usando la función osmdata_sf(). El objeto contiene una lista de objetos con los puntos, lineas y poligonos disponibles
#para los restaurantes.
osm_rest_1 = osm_rest %>% osmdata_sf()
osm_rest_1 ##note que tiene tanto informacion en puntos como en poligonos

##para los parques.
osm_parques_1 <- osm_parques %>% osmdata_sf()
osm_parques_1 ##tiene informacion en poligonos, puntos y multipoligonos

##Podemos acceder a los elementos de la lista para crear un objeto sf

## Obtener un objeto sf
restaurantes = osm_rest_1$osm_points %>% select(osm_id,amenity) ##Al utilizar osm_points y seleccionar la columna id, obtenemos los puntos sobre los cuales se encuentran los restaurantes en bucaramanga 
restaurantes
parques <- osm_parques_1$osm_polygons %>% select(osm_id, leisure) ##Al utilizar osm_polygons y seleccionar la columna id , obtenemos los poligonos sobre los cuales se encuentran los parques en bucaramanga
parques
##Con estos objetos, tenemos las ubicaciones de los lugares que queremos (restaurantes y parques en bucaramanga)
##Inciso 2
##Al tener un objeto del tipo sf, podemos pintarlos restaurantes y parques en bucaramanga con la función Leaflet() que nos permite hacer visualizaciones

##pintar los restaurantes
leaflet() %>% addTiles() %>% addCircles(data=restaurantes, color="red") ##utilizamos add circles para verlos como puntos en el mapa

##Pintar los parques como polygonos
leaflet() %>% addTiles() %>% addPolygons(data=parques) ##utilizamos addPolygons para que los objetos se visualizen como poligonos en el mapa

##Todo el procedimiento anterior se puede hacer en pocas linea usando el conector %>%. Sin embargo, decidimos hacer el paso a paso para que sea claro.

##Inciso 3
##usaremos la función geocode_osm para codificar la dirección de la alcadia de bucaramanga
##la direccion de la alcadia es cra 11 #34-52 . 
alcaldia_bucaramanga <- geocode_OSM("Carrera 11 %23% 34-52, Bucaramanga", as.sf=T) ## usamos el conector %23% para que r reconozca el caracter #. asimismo, se usa la as.sf=true para que reconozca el objeto como sf
leaflet() %>% addTiles() %>% addCircles(data=alcaldia_bucaramanga, color = "green")

##inciso 4

##primero, obtendremos el polygono para bucaramanga con fines esteticos
buc <- opq(bbox = getbb("Bucaramanga Colombia")) %>% 
  add_osm_feature(key="boundary", value="administrative") %>%
  osmdata_sf()
buc <- buc$osm_multipolygons %>% subset(admin_level==6)
buc
## add osm layer
osm_layer <- get_stamenmap(bbox= as.vector(st_bbox(buc)), maptype="toner", source="osm", zoom=13) 

map <- ggmap(osm_layer) + 
  geom_sf(data=buc , aes(fill=normal) , alpha=0.3 , inherit.aes=F) +
  geom_sf(data=restaurantes, aes(color="red"), inherit.aes = F) + 
  geom_sf(data=parques, aes(color="blue"), inherit.aes = F)+
  geom_sf(data=alcaldia_bucaramanga, aes(color="green"),inherit.aes = F)+
  scale_color_manual(labels=c("red"="restaurantes bucaramanga","green"="alcaldia bucaramanga"))
