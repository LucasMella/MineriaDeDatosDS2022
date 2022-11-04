library(dplyr)
library(tidyr)
library(readr)

#Mascaras
mask_agv <- readRDS("data/data_raw/station_id_agv_Lucas.rds")
mask_agromet <- readRDS("data/data_raw/station_id_agromet_Lucas.rds")

#Sets de datos
agv_meta <- readRDS("data/data_raw/metadata_estaciones_agvAPI.rds")
api_agv <- readRDS("data/data_raw/data_agvAPI.rds")
api_agromet <- readRDS("data/data_raw/data_estaciones_agrometAPI.rds")
agromet_meta <- readRDS("data/data_raw/metadata_estaciones_agrometAPI.rds")

#Aplicación de máscaras
agromet_meta <- agromet_meta[agromet_meta$ema %in% mask_agromet,]
agv_meta <- agv_meta[agv_meta$serial %in% mask_agv,]
api_agromet <- api_agromet[api_agromet$station_id %in% mask_agromet,]
api_agv_rows <- as.numeric(rownames(agv_meta[agv_meta$serial %in% mask_agv,]))
api_agv <- api_agv[c(api_agv_rows)]

#------------------I-------------------
#ReadR

#1. Leer los datos agromet del taller 1 con read_rds() ({readr})
rds_data <- read_rds("data/data_raw/data_estaciones_agrometAPI.rds")

#2. Guardarlos en formato .csv con write_csv()
write_csv(rds_data, 'data/data_proceseed/data_estaciones_agrometAPI.csv')

#3. Leer el archivo en formato .csv con read_csv()
read_csv('data/data_proceseed/data_estaciones_agrometAPI.csv')

#4. Comparar el formato de los objetos en (1) y en (2)
system.time(read_rds("data/data_raw/data_estaciones_agrometAPI.rds"))
system.time(read_csv("data/data_proceseed/data_estaciones_agrometAPI.csv"))


#-----------------II-------------------
  
# 1 Con los datos de agromet cree datos anidados por estación (station_id) considerando todas las variables climáticas.
  
data_nest <- 
  api_agromet |>  
  group_by(station_id) |> 
  nest()

# 2 De la data anidada extraiga el primer valor de la variable humedad relativa de la estación que se encuetra en la posición 5.

data_nest$data[5][[1]][1,4] |> 
  print()

purrr::pluck(data_nest$data, 5,4,1)

# 3 Agregue la variable precipitación extraida de la columna anidada como una variable adicional

data_nest |> 
  hoist(data, precipitacion_horaria = 'precipitacion_horaria')

# 4 Aplane (desanide) la variable de precipitación anterior.
data_nest |> 
  unnest(precipitacion_horaria)
#hoist select unnest glimpse

unnest
# 5 Haga explicitos los valores NA implicitos de precipitación. Comparé la cantidad de observaciones con la data original.

# 6 Realice el rellenado de los valores NA de precipitación horaria tomando el valor anterior.

#------------------III------------------

#### 1. Filtrar con los datos de agromet para las estaciones asignadas (ej., Pablo, Jessica y Lucas)
#PREGUNTAR SI SE HACE
#### 2. Filtrar los datos para los meses de Mayo a Julio en las estaciones asignadas.
mayojulio <- api_agromet |> 
  select()
#### 3. Tome una muestra de 1000 filas de forma aleatoria sin reemplazo.

#### 4. Para cada estación seleccione los valores máximos de precipitación horaria.

#### 5. Identifique las estaciones que tienen los mayores valores de precipitación horaria.

#### 6. Idem, pero para cada estación.

#### 7. Seleccione las columnas que tienen temperatura.

#### 8. Seleccione las columnas que tienen valores no numéricos.

#------------------IV--------------------

#### 1. Agrupe los datos de las climáticos de agromet por estación (group_by)

####2. Haga un sumarizado de promedio mensual de las variables de temperatura para cada estación (group_by, summarize y across).

####3. Renombre y reordene las variables como temp_prom, temp_max y Temp_min (rename_with y `relocate``)

####4. Cree las columnas var_temp y temp, en formato largo que contenga las variables de temp_prom, temp_max y Temp_min y sus valores. (pivot_longer)

####5. Ordene los datos anteriores de mayor a menor.

####6. Vuelva a formato ancho los datos del punto anterior.

####7. Caclule cuantos datos no faltantes (!is.na) tiene cada estacion para cada una de las variables.

#------------------V--------------------

##### 1. Con los datos de agromet, calcule el promedio de temperatura (media, máxima, mínima) por día y estación. Utilice summarize y across.

##### 2. Para cada estacion calcule el valor de la mediana.

##### 3. Filtre las estaciones cuyo valor de temperatura promedio es mayor que la temperatura mediana.

##### 4. Verifique que los nombres de la estaciones son únicos (distinct).

##### 5. Extraiga la columna del nombre de la estaciones.

##### 6. Una el data.frame de los valores promedios de temperatura diario por estación con el data.frame de la metadata de las estaciones de agromet.

##### 7. Cree un data.frame con datos anidados con los valores de promedios de temperatura diario, por estación.

