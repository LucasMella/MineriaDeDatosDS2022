library(tibble)
library(arrow)

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


#1
class(agromet_meta)
class(agv_meta)
class(api_agromet)
class(api_agv)


#2
df_agv <- lapply(api_agv, unlist)

df_agv <- lapply(df_agv, length)
df_agv <- as.data.frame(t(t(df_agv)), row.names = c(mask_agv))


#3 ---

agromet_agr <- api_agromet[c(1, 4, 9, 10)]
agromet_agr$dia <- format(api_agromet$fecha_hora, "%d-%m-%Y")
pdiaria <- tapply(agromet_agr$precipitacion_horaria, agromet_agr$dia, sum, na.rm = TRUE)
#agromet_agr$precipitacion_diaria <- pdiaria

#4
tibble_api_agromet <- as_tibble(agromet_agr)

#5 ++++++++++
tibble_api_agv <- as_tibble(api_agv)

#6
summary(tibble_api_agromet)
sum(is.na(api_agromet))

summary(api_agv)
sum(is.na(api_agv))

#8
write.csv(api_agromet,"data/data_proceseed/api_agromet.csv", row.names = FALSE)
write_parquet(api_agromet, "data/data_proceseed/api_agromet.parquet")
readr::write_rds(api_agromet, "data/data_proceseed/api_agromet.rds")

library(microbenchmark)
microbenchmark(
  opcion1 = read.csv("data/data_proceseed/api_agromet.csv"),
  opcion2 = read_parquet("data/data_proceseed/api_agromet.parquet") ,
  opcion3 = readRDS("data/data_proceseed/api_agromet.rds"))
