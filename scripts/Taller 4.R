library(ggplot2)
library(tidyr)
library(dplyr)
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

#1. api_agromet grafico

data_graf <- api_agromet |> 
  mutate(dia = as_date(fecha_hora)) |> 
  group_by(station_id, dia = as_date(fecha_hora)) |> 
  summarise(across(temp_promedio_aire:temp_maxima,
                   .fns = \(x) sum(is.na(x)),
                   .names = '{.col}_nas'))

data_graf_longer <- data_graf |> 
  pivot_longer(temp_promedio_aire_nas:temp_maxima_nas)

ggplot(data_graf_longer, aes(dia, as.factor(station_id), fill = value))+
  geom_tile() +
  facet_grid(.~name)

#2. 
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
  
#3
#Agrupar por region.
region <- api_agromet |> 
  group_by(station_id, dia = as_date(fecha_hora)) |> 
  summarise(temp_prom = mean(temp_promedio_aire, na.rm = TRUE)) |> 
  mutate(mes = floor_date(dia, '1 month')) |> 
  left_join(agromet_meta, by = c('station_id' = 'ema')) |> 
  arrange(desc(latitud))


region |> 
  ggplot(aes(region, temp_prom)) +
  geom_boxplot() +
  coord_flip()
  
#regs <- agromet_meta |> 
  
anomalos <- function(x,...){
  lim_inf <- quantile(x,.25,...) - 1.5*IQR(x,...)
  lim_sup <- quantile(x,.75,...) - 1.5*IQR(x,...)
  x[x < lim_inf | x > lim_sup] <- NA
  return(x)
}  

api_agromet |> 
  mutate(across(is.numeric, anomalos,na.rm = TRUE))


ggplot()+
  geom_boxplot(data_agro, aes())


###

iris |> 
  group_by(Species) |> 
  mutate(rcsl = sqrt(mean(Sepal.Length))) |> 
  mutate(across(where(is.numeric),.fns = \(x) x/sqrt(mean(y)), y=Sepal.Length))


f <- function(x,y){
  x/sqrt(mean(x))
}