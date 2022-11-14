library('dplyr')
library('tidyr')
library('readr')

#Mascaras
mask_agv <- readRDS("data/data_raw/station_id_agv_Lucas.rds")

#Sets de datos
agv_meta <- readRDS("data/data_raw/metadata_estaciones_agvAPI.rds")
api_agv <- readRDS("data/data_raw/data_agvAPI.rds")

#Aplicación de máscaras
agv_meta <- agv_meta[agv_meta$serial %in% mask_agv,]
api_agv_rows <- as.numeric(rownames(agv_meta[agv_meta$serial %in% mask_agv,]))

api_agv_rows <- as.numeric(rownames(agv_meta[agv_meta$tipo == "Humedad_Suelo",]))
api_agv_rows <- api_agv_rows[!is.na(api_agv_rows)]
api_agv <- api_agv[c(api_agv_rows)]

#--------------------------------------------------------------------------------------

#cantidad de variables
cols <- sapply(api_agv,ncol)
vars <- sapply(api_agv,nrow)

# varibles unicas 
vars_un<- unique(vars)

#Indicadores de estaciones con 7 Variables
ind7 <- vars %in% 7
serial_est<- agv_meta$serial[ind7]

# separar para cada caso
data_sep <- lapply(vars_un,function(x){
  api_agv[x == vars]
})

#nombres de variables 
names <-  sapply(data_sep[[1]], purrr::pluck,1)

#seleccionar las filas de profundidad
a <- lapply(data_sep[[1]],function(l){
  out <- l |>
    slice(1:3) #seleciona filas por indice o nombre
  out$z <- c(90,60,30)
  return(out)
})

#Desanida los datos
r <- lapply(seq_along(a), function(i){
  out <- tryCatch(
    a[[i]] |>
      tidyr::unnest('data') |>
      tidyr::hoist(3,'value')|>
      dplyr::select(2,3,5) ,
    error = function(e) NULL)
  
  if (!is.null(out)) out$serial <- serial_est[i]
  if (is.list(out)) out <- out |> unnest('value')
  return(out)
})


data_final<- do.call(rbind,r)
data_final

names <- c('Fecha_Hora', 'Valor', 'Profundidad', 'Serial')
names -> colnames(data_final)

data_final <- tibble(data_final)

saveRDS(data_final, 'data/data_proceseed/Data_Final.rds')

#-------------------------------------------------------------------------------

data_final_ordenada <- arrange(data_final, desc(Valor))
data_final_ordenada

saveRDS(data_final_ordenada, 'data/data_proceseed/Data_Final_Ordenada.rds')

#-------------------------------------------------------------------------------

vec <- c('Fecha', 'Hora')
data_final_separada <- separate(data_final_ordenada, 'Fecha_Hora', vec, sep=' ')

#-------------------------------------------------------------------------------

nas <- data_final |> 
  complete(Fecha_Hora, Serial, fill = list(valores = NA))