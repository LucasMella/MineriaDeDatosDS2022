library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

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

# 1 Con los datos de agromet cree datos anidados por estación (station_id) considerando todas las variables climáticas.
  
data_nest <- 
  api_agromet |>  
  group_by(station_id) |> 
  nest()

# 2 De la data anidada extraiga el primer valor de la variable humedad relativa de la estación que se encuetra en la posición 5.

data_nest$data[5][[1]][1,4] |> 
  print()

purrr::pluck(data_nest$data, 5,4,1)

# 3 Agregue la variable precipitación extraida de la columna anidada como una variable adicional y aplane (desanide) la variable de precipitacion, luego, haga explicitos los valores NA implicitos de precipitación. Compare la cantidad de observaciones con la data original. 

data_nest |> 
  ungroup() |> 
  hoist(data, 'precipitacion_horaria') |> 
  unnest(precipitacion_horaria) |> 
  select(1:2) |> 
  complete(station_id, precipitacion_horaria)


# 4 Realice el rellenado de los valores NA de precipitación horaria tomando el valor anterior.

api_agromet |> 
  select(station_id, fecha_hora, precipitacion_horaria) |> 
  complete(fecha_hora, station_id) |> 
  glimpse()


#------------------II------------------

#### 1. Filtrar los datos para los meses de Mayo a Julio en las estaciones asignadas.
mayojulio <- api_agromet |> 
  mutate(fecha_hora = as.Date(api_agromet$fecha_hora), month = month(fecha_hora)) |> 
  filter(month >= 05 & month <= 08)

#### 2. Tome una muestra de 1000 filas de forma aleatoria sin reemplazo.
api_agromet |> 
  slice_sample(n = 1000)

#### 3. Para cada estación seleccione los valores máximos de precipitación horaria.
api_agromet |> 
  group_by(station_id) |> 
  slice_max(precipitacion_horaria, n= 5)

#### 4. Identifique las estaciones que tienen los mayores valores de precipitación horaria.
api_agromet |> 
  slice_max(precipitacion_horaria, n = 5)
  select(station_id)

#### 5. Seleccione las columnas que tienen temperatura.

api_agromet |>
  select(station_id, fecha_hora, contains('temp'))
  
  
#### 6. Seleccione las columnas que tienen valores no numéricos.

api_agromet |> 
  select(where(\(x) !is.numeric(x))) |> 
  glimpse()

#------------------III--------------------

#### 1. Agrupe los datos de las climáticos de agromet por estación (group_by)

api_agromet |> 
  group_by(station_id)

#### 2. Haga un sumarizado de promedio mensual de las variables de temperatura para cada estación (group_by, summarize y across).

api_agromet |> 
  group_by(station_id) |> 
  summarise(across(contains('Temp'), mean, na.rm = TRUE))

#### 3. Renombre y reordene las variables como temp_prom, temp_max y Temp_min (rename_with y `relocate``)

data_agromet <- api_agromet |> 
  rename_with(contains('temp'),.fn = \(x) substr(x, start = 1, stop = 8))

data_agromet |> 
  relocate(temp_max, .after = temp_pro) |> 
  relocate(temp_min, .after = last_col())
  
#### 4. Cree las columnas var_temp y temp, en formato largo que contenga las variables de temp_prom, temp_max y Temp_min y sus valores. (pivot_longer)

data_agromet |> 
  pivot_longer(c(3, 9, 10), names_to = 'var_temp', values_to = 'temp',)

#### 5. Ordene los datos anteriores de mayor a menor.
data_agromet |> 
  pivot_longer(c(3, 9, 10), names_to = 'var_temp', values_to = 'temp',) |> 
  arrange(desc(temp))

#### 6. Vuelva a formato ancho los datos del punto anterior.
data_agromet |> 
  pivot_longer(c(3, 9, 10), names_to = 'var_temp', values_to = 'temp',) |> 
  arrange(desc(temp)) |> 
  pivot_wider(names_from = var_temp, values_from = temp)

#### 7. Calcule cuantos datos no faltantes (!is.na) tiene cada estacion para cada una de las variables.

data_agromet |> 
  select(station_id, contains('temp')) |> 
  pivot_longer(2:4, names_to = 'var_temp', values_to = 'temp',) |> 
  group_by(station_id, var_temp) |> 
  summarise(NAs = sum(!is.na(temp)))

#------------------IV--------------------

##### 1. Con los datos de agromet, calcule el promedio de temperatura (media, máxima, mínima) por día y estación. Utilice summarize y across.

prom_agromet <- api_agromet |>
  group_by(station_id) |> 
  summarise(across(contains("temp"),\(x) mean(x, na.rm = TRUE), .names = "{.col}_prom"))

##### 2. Para cada estacion calcule el valor de la mediana.

med_agromet <- api_agromet |> 
  group_by(station_id) |> 
  summarise(across(contains('temp'),\(x) median(x, na.rm = TRUE), .names = "{.col}_mediana"))
  
##### 3. Filtre las estaciones cuyo valor de temperatura promedio es mayor que la temperatura mediana.

prom_agromet |> 
  filter(temp_promedio_aire_prom > med_agromet$temp_promedio_aire_mediana)

##### 4. Verifique que los nombres de la estaciones son únicos (distinct).

prom_agromet |>
  distinct(station_id)
  
##### 5. Extraiga la columna del nombre de la estaciones.

prom_agromet |> 
  pull(station_id)

##### 6. Una el data.frame de los valores promedios de temperatura diario por estación con el data.frame de la metadata de las estaciones de agromet.

agromet_meta |>
  rename(station_id = ema) |> 
  right_join(med_agromet, by = 'station_id') |>
  glimpse()

##### 7. Cree un data.frame con datos anidados con los valores de promedios de temperatura diario, por estación.
prom_agromet |> 
  group_by(station_id) |> 
  nest()
  
  
  
  
  