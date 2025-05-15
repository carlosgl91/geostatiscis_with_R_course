if (!require('pacman'))
  install.packages("pacman")
library(pacman)
#Install Java for your system type
pacman::p_load(
  rgdal, #Geospatial Data Abstraction Library o GDAL
  terra, # Lectura, escritura, procesamiento de datos raster 
  sf, # Lectura, escritura, procesamiento de datos vectoriales 
  raster,# Predecesor de Terra, lectura, escritura, procesamiento de datos raster 
  tidyverse, # Conjunto de paquetes para manipulación abreviada y flujo de procesos
  readxl # Lectura de archivos excel
  
)

# Clase 3
# Geoprocesamiento con vectores

# 1- Importar datos en formato vectorial----

# Estaciones de servicio de Paraguay - Datos descargados de Open Street Map 2022
estaciones_serv_py <- read_sf("clase_03_m7/datos/estaciones_serv_paraguay.shp")
# Distritos de Paraguay - Distritos cartografía digital censal 2012, excluyendo distritos del Chaco
distritos_py_RO <-  read_sf("clase_03_m7/datos/py_distritos248.shp") %>% filter(!DPTO %in% c("15","16","17"))

distritos_py_df <- distritos_py_RO %>% st_drop_geometry()


# Obtener 
# 2- Subset ----
# 1- Subset de estaciones de servicio ubicadas solo en la región Oriental
estaciones_RO <- estaciones_serv_py[distritos_py_RO, op=st_intersects]
# Los sistemas de referencias de coordenadas de los datos no son iguales, por lo que R arroja un error
# utilizaremos el crs WGS 84 UTM 21S, para ello debemos tener ambas variables en el mismos sistema 
# crs, utilizado el código EPSG 32721

# Verificamos el sistema de coordenadas de la variable distritos_py_RO
crs(distritos_py_RO)
# Aparentemente la variable no cuenta con un sistema de coordenadas definido,
# por lo que probaremos realizar la transformación
distritos_py_RO <- distritos_py_RO %>% st_transform(32721)
# No es posible realizar la transformación debido a que no se cuenta con un crs de origen
# Entonces, como sabemos cual es el crs podemos directamente establecerlo a través de
# el crs vía st_set_crs, podría darse el caso en donde el archivo tenga un crs distinto
# por ejemplo WGS 84 sistema de coordenadas geográficas EPSG 4326 en cuyo caso utilizariamos st_transform(32721)
# puesto que estaríamos realizando una proyección de los datos a un sistema distinto.

distritos_py_RO_WGS84_UTM21S <- distritos_py_RO %>%  st_set_crs(32721)

# Verificamos el crs de la variable estaciones_serv_py
crs(estaciones_serv_py)


# Transformamos de EPSG 4326 (WGS84) a 32721 (WGS84 UTM21S)
estaciones_serv_py_WGS84_UTM21S <- estaciones_serv_py %>% st_transform(32721)
# Verificamos el sistema de coordenadas actual 
crs(estaciones_serv_py_WGS84_UTM21S)

# el subset es posible una vez que ambos objetos sf se encuentran en el mismo crs 
estaciones_RO <- estaciones_serv_py_WGS84_UTM21S[distritos_py_RO_WGS84_UTM21S, op=st_intersects] %>% select(name, brand)

# Cuantas estaciones hay por departamentos y distritos en la Región Oriental
# Para esto realizamos una unión espacial

# 3- Unión espacial ####

estaciones_RO <- estaciones_RO %>% st_join(distritos_py_RO_WGS84_UTM21S[,c("DIST_DESC", "DPTO_DESC")])

# Resumen DPTO----

cant_est_dpto <- estaciones_RO %>% count(DPTO_DESC, name = "cantidad") %>% st_drop_geometry() %>%  arrange(cantidad)

# Ploteamos los resultados con ggplot2
bar_dpto <-
  ggplot(data = cant_est_dpto, aes(reorder(DPTO_DESC, -cantidad), y = cantidad, fill= cantidad)) +
  geom_bar(stat = "identity") + scale_fill_gradient(low = "green",
                                                    high = "red",
                                                    na.value = NA,
                                                    name = "Nro. de estaciones") +  geom_text(
                                                      aes(label = cantidad),
                                                      vjust = 1.6,
                                                      color = "white",
                                                      size = 3.5
                                                    ) + theme(axis.text = element_text(angle = 90),
                                                              axis.title = element_blank()) +
  labs(title = "Cantidad de estaciones de servicio por Dpto", caption = "OSM 2022")

# Resumen Dist----

cant_est_dist <- estaciones_RO %>% count(DIST_DESC, name = "cantidad") %>% st_drop_geometry() %>%  arrange(desc( cantidad) )
# Solo el top 10
cant_est_dist <- cant_est_dist[1:10,]

# Ploteamos los resultados con ggplot2 DIST
bar_dist <-
  ggplot(data = cant_est_dist, aes(reorder(DIST_DESC, -cantidad), y = cantidad, fill= cantidad)) +
  geom_bar(stat = "identity") + scale_fill_gradient(low = "green",
                                                    high = "red",
                                                    na.value = NA,
                                                    name = "Nro. de estaciones") +  geom_text(
                                                      aes(label = cantidad),
                                                      vjust = 1.6,
                                                      color = "white",
                                                      size = 3.5
                                                    ) + theme(axis.text = element_text(angle = 90),
                                                              axis.title = element_blank()) +
  labs(title = "Cantidad de estaciones de servicio por Distrito", caption = "OSM 2022")

# 4 Análisis espacial ---- 

### 4.1 Distancia entre puntos, distancia entre estaciones de servicio, solo asunción

est_distancia <- estaciones_RO %>%  filter(DIST_DESC == "ASUNCION") %>% 
mutate(
  lead = geometry[row_number() + 1],
  dist = st_distance(geometry, lead, by_element = T),
)
# Importamos escuelas en Asunción (OSM)
escuelas_asuncion <-
  read_sf("clase_03_m7/datos/escuelas_asuncion.shp") %>% st_transform(32721) %>% filter(!is.na(name) &
                                                                                          !is.na(amenity)) %>% select(amenity, name)


# Estaciones de servicio cuya distancia al set de escuelas es menor a 100
# Podemos hacerlo de dos maneras
# 1- Directamente mediante la función st_is_within_distance que evalua dicha relación topológica 
estaciones_escuelas_dist = st_is_within_distance(estaciones_ASU, escuelas_asuncion, dist = 100)
summary(lengths(estaciones_escuelas_dist) > 0)

# 2- Filtrando espacialmente
# Estaciones cercanas a escuelas 
estaciones_escuelas_dist <- estaciones_ASU[escuelas_asuncion,op=st_is_within_distance,dist = 100]
# Escuelas cercanas a estaciones de servicio 
escuelas_estaciones_Dist <- escuelas_asuncion[estaciones_ASU,op=st_is_within_distance,dist = 100]

plot(estaciones_escuelas_dist$geometry, col="red",axes = TRUE)

plot(escuelas_estaciones_Dist$geometry,col= "blue", add=T)

# 5 Datos RASTER ----

# Creación de un raster desde 0

r <- rast(ncol=5, nrow=5)# se indican las columnas y filas
values(r) <- 1# se asigna un valor 

r
# 5.1 Algunas operaciones aritmeticas ----
s <- c(r, r+1)
s
q <- c(r, r+2, r+4, r+6)
q
x <- r + s + q
x

plot(x)

# 5.2 Funciones de resumen----

a <- mean(r, s)# media
a
plot(a)
b <- sum(r, s)# suma
b
st <- c(r, s, a, b)# stack
sst <- sum(st)# Suma del stack
sst
plot(sst)
# Resumen por capa
global(st, 'sum')


#5.3 Importar archivos raster----
mi_raster = rast("clase_03_m7/datos/dem_ejercicio.tif")# Se admiten varios tipos de formato, ver documentación
# verificamos el tipo de objeto
class(mi_raster)
# Ploteamos nuestro raster
plot(mi_raster)
# 5.4 Algunas funciones descriptivas----
dim(mi_raster) # número de columnas, filas y capas o layers
ncell(mi_raster)# número de celdas, píxeles
res(mi_raster)# Resolución espacial
ext(mi_raster)# coordenadas de la extensión del raster
crs(mi_raster) # Indica si el raster esta alojado en memoria o disco
# 5.5 Geoprocesamiento
# Importamos un archivo vectorial 
mi_polygono <- read_sf("clase_03_m7/datos/parque_nacional_ybycui.shp")

plot(mi_raster)

raster_crop <- terra::crop(mi_raster,vect(mi_polygono))
# Debemos transformar/projectar el  crs del raster
mi_raster <- mi_raster %>% project("EPSG:32721")

raster_crop <- terra::crop(mi_raster,vect(mi_polygono))
plot(raster_crop)
raster_mask <- raster_crop %>% terra::mask(vect(mi_polygono)) 
plot(raster_mask)
# Visualizamos los datos como df
raster_mask_df <- raster_mask %>% as.data.frame(xy = TRUE)
# Filtramos valores utilizando funciones
D<- app(raster_mask, fun=function(x){ x[x < 200] <- NA; return(x)} )
s
# Convertimos a vector
vectorizado <-
  s %>% terra::as.polygons() %>%  st_as_sf()
# Exportamos raster

terra::writeRaster("s.tif")
plot(s)
