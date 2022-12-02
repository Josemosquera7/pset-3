# Jose Esteban Mosquera - 202110281
# David Chaparro - 202023984
# Santiago Huertas - 202021311

# Problem Set 3 - Taller de R estadistica y programacion 
# Profesor Eduard Martinez

##packages
require(pacman)
p_load(tidyverse, rio, arrow, broom, mfx, margins,estimatr,lmtest,fixest, modelsummary, stargazer, writexl)

##Punto 1

## load data
base_regresiones <- import(file="input/data_regresiones.rds")

# Punto 1.1
modelo_1 <- lm(price ~ surface_total + rooms + bathrooms, data = base_regresiones)
modelo_2 <- lm(price ~ dist_cbd + dist_cole + dist_park, data = base_regresiones)
modelo_3 <- lm(price ~ surface_total +  rooms + bathrooms + as.factor(property_type), data = base_regresiones )

# Punto 1.2

tabla_resultados <- msummary(list(modelo_1, modelo_2, modelo_3),output="data.frame")
tabla_resultados

# Punto 1.3 exportar

export(tabla_resultados, "output/resultados_regresiones.xlsx")

## Graficar y exportar
mods <- list("Modelo 1"= modelo_1, "Modelo 2"= modelo_2, "Modelo 3"= modelo_3)
png(filename = "output/plot_regresiones.png", width = 800, height = 600)
modelplot(mods) + coord_flip() + labs(title = "Precio de la vivienda", subtitle = "comparación entre los tres modelos")
dev.off()


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
buc <- buc$osm_multipolygons %>% subset(admin_level==6) %>% subset(name=="Bucaramanga")
buc
## add osm layer
osm_layer <- get_stamenmap(bbox= as.vector(st_bbox(buc)), maptype="toner-lines", source="osm", zoom=12) 

map <- ggmap(osm_layer) + 
  geom_sf(data=buc, alpha=0.3 , inherit.aes=F) +
  geom_sf(data=restaurantes, aes(color="red"), inherit.aes = F) + 
  geom_sf(data=parques, aes(color="blue"), inherit.aes = F)+
  geom_sf(data=alcaldia_bucaramanga, aes(color="green"),inherit.aes = F)+
  scale_color_manual(labels=c("red"="restaurantes bucaramanga","green"="alcaldia bucaramanga"))



##Punto 3 Web-scraping y procesamiento de texto

rm(list=ls())
require(pacman)
p_load(tidyverse,rio,data.table,
       rvest,xml2,
       textcat,stringi,cluster,wordcloud, tm, writexl, rvest) 

##Punto 3.1

##Se crea el objeto xml_document que tiene el html de la pagina de wikipedia
wiki<- "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
xml_document <-read_html(wiki) ## leer el html de la página

## Punto 3.2 extraer titulo de la pagina

xml_document %>% html_node(xpath='//*[@id="firstHeading"]') %>% html_text()


##Punto 3.3 guardar tabla de departamentos

my_table = xml_document %>% html_table()

length(my_table)

tabla_departamento<-my_table[[4]]

##<Exportar la tabla a excel 
write_xlsx(tabla_departamento,"Output/tabla_departamento.xlsx")


##Punto 3.4

parrafos<-xml_document %>% html_elements("p") %>% html_text()

df_parrafos<-as.data.frame(parrafos)


## detectar codificacion
stri_enc_mark(parrafos)
stri_enc_isascii(parrafos)
stri_enc_isutf8(parrafos)

## recodificar
stri_enc_mark("Ábcdêãçoàúü") 
iconv("Ábcdêãçoàúü", from = 'UTF-8', to = 'ASCII//TRANSLIT')

stri_encode(parrafos,"UTF-8", "ASCII") %>% head()

stri_trans_general(parrafos, "Latin-ASCII") %>% head() 

stri_trans_general("Ahí está la Economía", "Latin-ASCII")
parrafos[5]
Encoding(parrafos) = "ASCII"
stri_enc_mark(parrafos)
parrafos[5]


## Vamos a limpiar nuestros caracteres
"todos los caracteres a minusculas"
parrafos[5]
parrafos = tolower(parrafos)
parrafos[5]

## Eliminar carcateres especiales
str_remove_all(string="Washington (D.C.)" , pattern="[^[:alnum:] ]")
str_replace_all(string="Washington (D.C.)" , pattern="[^[:alnum:] ]" , replacement="")
parrafos = str_replace_all(string=parrafos , pattern="[^[:alnum:] ]" , replacement=" ")

## Eliminar  acentos
stri_trans_general("Ahí está la Economía", "Latin-ASCII")
stri_trans_general("Colombia in the late 1990âs", "Latin-ASCII")
parrafos = stri_trans_general(parrafos, "Latin-ASCII")

## Remover puntuacion
parrafos = removePunctuation(parrafos)

## Remover numeros
parrafos = removeNumbers(parrafos)

## Remover preposiciones y/o conectores
stopwords("spanish") 
parrafos = removeWords(parrafos,stopwords("spanish"))

## Remover otras cadena de caracteres
parrafos = removeWords(parrafos,c("et","abstrac","documento", "paper"))

## Remover exceso de espacios
parrafos = stripWhitespace(parrafos) 

## Remover espacios iniaciales y finales
parrafos = trimws(parrafos)


## vector de caracteres a corpues
parrafos_corpus = Corpus(VectorSource(parrafos)) # formato de texto
class(parrafos_corpus)

## matriz con terminos
tdm_parrafos = TermDocumentMatrix(parrafos_corpus)
class(tdm_parrafos)  
dim(tdm_parrafos)

## frecuencia de palabras
findFreqTerms(tdm_parrafos, lowfreq = 20)
frecuentes = findFreqTerms(tdm_parrafos, lowfreq = 20)

## palabras con las que mas se asocian las primeras 5 palabras del vector frecuentes
findAssocs(tdm_parrafos, frecuentes[1:5], rep(x = 0.45, rep = 50))

## Se convierte el objeto tdm_abstrac en una matriz con frecuencias
matrix_parrafos = as.matrix(tdm_parrafos) #lo vuelve una matriz
dim(matrix_parrafos)
view(matrix_parrafos)

## Frecuencia de cada palabra
frec_words = sort(rowSums(matrix_parrafos),decreasing=T) 
class(frec_words)
df_words = data.frame(word = names(frec_words) , n = frec_words)


## Grafico nube de palabras
wordcloud(words = df_words$word, freq = df_words$n, min.freq = 6,
          max.words = 250 , random.order = T , rot.per = 0.35 , scale = c(10,1))


png(filename = "output/nube_palabras.png", width = 800, height = 600)
nube_palabras<-wordcloud(words = df_words$word, freq = df_words$n, min.freq = 1,
                max.words = 2000 , random.order = F ,colors = brewer.pal(10, "Dark2"))
dev.off()