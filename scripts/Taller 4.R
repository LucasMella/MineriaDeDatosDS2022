library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)

#Mascaras
mask_agv <- read_rds("data/data_raw/station_id_agv_Lucas.rds")
mask_agromet <- read_rds("data/data_raw/station_id_agromet_Lucas.rds")

#Sets de datos
agv_meta <- read_rds("data/data_raw/metadata_estaciones_agvAPI.rds")
api_agv <- read_rds("data/data_raw/data_agvAPI.rds")
api_agromet <- read_rds("data/data_raw/data_estaciones_agrometAPI.rds")
agromet_meta <- read_rds("data/data_raw/metadata_estaciones_agrometAPI.rds")

#Aplicación de máscaras
agromet_meta <- agromet_meta[agromet_meta$ema %in% mask_agromet,]
agv_meta <- agv_meta[agv_meta$serial %in% mask_agv,]
api_agromet <- api_agromet[api_agromet$station_id %in% mask_agromet,]
api_agv_rows <- as.numeric(rownames(agv_meta[agv_meta$serial %in% mask_agv,]))
api_agv <- api_agv[c(api_agv_rows)]

###############################################################################
#1.1 Eliminar valores anómalos
#1.1.1
anomalos <- function(x,...){
  lim_inf <- quantile(x,.25,...) - 1.5*IQR(x,...)
  lim_sup <- quantile(x,.75,...) + 1.5*IQR(x,...)
  x[x < lim_inf | x > lim_sup] <- NA
  return(x)
}

#1.1.2
vector1 <- c(rnorm(100, mean = 0, sd = 1))
anomalos(vector1)

#1.1.3
iris |>   
  mutate(across(is.numeric, anomalos,na.rm = TRUE))

#1.1.4

#1.1.5
anoapi_agromet <- api_agromet |> 
  mutate(across(is.numeric, anomalos,na.rm = TRUE))

###############################################################################
#1.2 Funcion con multiples columnas como argumentos
#1.2.1
rqmean <- mean(sqrt(iris$Sepal.Length))
iris |> 
  mutate(across(is.numeric))/rqmean

#1.2.2
iris |> 
  group_by(Species) |>
  mutate(across(where(is.numeric),.fns = \(x) x/rqmean))

#1.2.3
rqmeanfun <- function(x,y){group_by(y) |>
    mutate(across(where(is.numeric))) |> 
    mutate(rqmean_ = x/rqmean)}

rqmeanfun(iris$Sepal.Width, iris[iris$Species,])

rqmeanfun_<- function(x,y,z){
  group_by(z) |>
  mutate(across(where(is.numeric))) |> 
  mutate(rqmean_ = x/sqrt(mean(y)))}

rqmeanfun_(iris$Sepal.Length, iris$Sepal.Width, iris[iris$Species,])

###############################################################################
#1.3 Resumir por rangos tipo categórica
#1.3.1
x <- 1:100
clasifica <- function(x,...){
  cut(x,3, labels=c('Low', 'Med', 'High'))}

#1.3.2
clasifica(vector1)

#1.3.3
iris |>
  mutate(across(where(is.numeric),.fns=clasifica))  

#1.3.4
iris |> 
  group_by(Species) |>
  mutate(across(where(is.numeric),.fns = \(x) cut(x, 3, labels = c('Bajo', 'Medio', 'Alto'))))

#1.3.5
#api_agromet[3:11] |> 
#  mutate(across(where(is.numeric)), na.rm = TRUE) |> 
#  clasifica(na.rm=TRUE)

#api_agromet$station_id <- as.factor(api_agromet$station_id)
#api_agromet[,-c(12,13)] |> 
#  group_by(station_id) |> 
#  mutate(across(3:10),.fn = \(x) cut(x, 3, labels = c('Bajo', 'Medio', 'Alto')),drop_na)


###############################################################################
#1.4 Coeficiente de Variación
#1.4.1
cvar <- function(x){
  if(is.numeric(x)){
    x <- na.omit(x)
    cov <- sd(x)/mean(x)
    out <- cov < 0.3
  } else out <- FALSE
}

#1.4.2
vector4 <- runif(10)
cvar(vector4)

#1.4.3
apply(iris[,1:4], 2, FUN = cvar)
apply(select(iris,where(is.numeric)),2,fun =cvar)

#1.4.4
iris |> 
  select(where(cvar))

#1.4.5
api_agromet |> 
  mutate(across(everything(), anomalos,na.rm = TRUE)) |> 
  select(where(cvar))

###############################################################################
#### 2 
#### EJERCICIOS DE TRANSFORMACIÓN Y VISUALIZACIÓN
###############################################################################
###############################################################################
#2.1. Comparar Distribuciones
#2.1.1

data_graf <- api_agromet |> 
  mutate(dia = as_date(fecha_hora)) |> 
  group_by(station_id, dia = as_date(fecha_hora)) |> 
  summarise(across(temp_promedio_aire:temp_maxima,
                   .fns = \(q) sum(is.na(q)),
                   .names = '{.col}_nas'))

data_graf_longer <- data_graf |> 
  pivot_longer(temp_promedio_aire_nas:temp_maxima_nas)

ggplot(data_graf_longer, aes(dia, as.factor(station_id), fill = value))+
  geom_tile() +
  facet_grid(.~name)

###############################################################################
#2.2
#2.2.1


#2.2.2
cols <- sapply(api_agv,ncol)
vars <- sapply(api_agv,nrow)
vars_un<- unique(vars)
ind7 <- vars %in% 7
serial_est<- agv_meta$serial[ind7]
data_sep <- lapply(vars_un,function(x){
  api_agv[x == vars]
})
a <- lapply(data_sep[[1]],function(l){
  out <- l |>
    slice(1:3) #seleciona filas por indice o nombre
  out$z <- c(90,60,30)
  return(out)
})

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

agv_final<- do.call(rbind,r)
names <- c('timestamp', 'Valor', 'Profundidad', 'Serial')
names -> colnames(agv_final)

agv_final <- tibble(agv_final)

agv_graf <- agv_final |> 
  mutate(fecha_hora = ymd_hm(timestamp)) |> 
  select(everything()) |> 
  group_by(dia = as_date(fecha_hora), Serial, Profundidad) |> 
  summarise(n = n(),
            NAs = sum(is.na(Valor)),
            NAs_prop = NAs/n)

ggplot(agv_graf, aes(dia, Serial, fill = NAs)) +
  geom_tile() +
  facet_grid(.~Profundidad) +
  scale_x_date(limits = c(ymd(20220831), ymd(20220905)), expand = c(0,0))


###############################################################################
#2.3.1
regionA <- api_agromet |> 
  group_by(station_id, dia = as_date(fecha_hora)) |> 
  summarise(temp_prom = mean(temp_promedio_aire, na.rm = TRUE)) |> 
  left_join(agromet_meta, by = c('station_id' = 'ema')) |> 
  arrange(desc(latitud))

regionA |> 
  ggplot(aes(region, temp_prom)) +
  geom_boxplot() +
  coord_flip()

#2.3.2
region <- api_agromet |> 
  group_by(station_id, dia = as_date(fecha_hora)) |> 
  summarise(temp_prom = mean(temp_promedio_aire, na.rm = TRUE)) |> 
  mutate(mes = floor_date(dia, '1 month')) |> 
  left_join(agromet_meta, by = c('station_id' = 'ema')) |> 
  arrange(desc(latitud))

region <- region[,-2] |> 
  group_by(mes)

region |> 
  ggplot(aes(region, temp_prom, dia)) +
  geom_point()

#2.3.3

radsolar <- api_agromet[c('station_id', 'fecha_hora', "radiacion_solar_max")]

radsolar <- api_agromet |> 
  group_by(station_id, dia = as_date(fecha_hora)) |> 
  summarise(rad_solar = mean(radiacion_solar_max, na.rm = TRUE)) |> 
  mutate(mes = floor_date(dia, '1 month')) |> 
  left_join(agromet_meta, by = c('station_id' = 'ema')) |> 
  arrange(desc(latitud))
  
  
  
radsolar <- radsolar |> 
  group_by(station_id, dia = as_date(fecha_hora)) |> 
  summarise(rad_solar = mean(radiacion_solar_max), na.rm = TRUE) |> 
  mutate(mes = floor_date(dia, '1 month')) |> 
  left_join(agromet_meta, by = c('station_id' = 'ema'))

radsolar |> 
  ggplot(aes(radsolar, rad_solar))+
  geom_point()
