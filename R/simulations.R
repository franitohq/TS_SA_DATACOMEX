
# Librerias Generales-----
library("shiny")
library("tidyverse")
library("bslib")
library("future")
library("promises")
library("plotly")
library("tsibble")
library("rJava")
library("openxlsx")
library("lubridate")
library("zoo")
library("scales")
library("patchwork")
library("tools")

# DATACOMEXR
library("datacomexr")

# GENERAL TSA
library("TSA")
library("astsa")
library("tseries")
library("forecast")
library("fma")
library("timeSeries")
# library("Rmetrics")
library("fGarch")
library("fUnitRoots")
library("MTS") #MULTIVARIATE TIME SERIES ANALYSIS PACKAGE
library("seasonal")
library("seasonalview")
library("feasts")
# library(c("XLConnect", "XML"))
library("fpp3")
library("fable")
library("car")
library("dygraphs")
library("LSTS")
library("signal")

# RJDemetra v.2
library("RJDemetra")
library("rjdworkspace")
# library("rjdmarkup")
library("JDCruncheR")
library("ggdemetra")
library("rjdqa")
library("rjwsacruncher")


# RJDemetra v.3
library("rjd3toolkit")
library("rjd3x13")
library("rjd3tramoseats")
library("rjd3providers")
library("rjd3workspace")
library("rjd3highfreq")
library("rjd3filters")
library("rjd3x11plus")
library("rjd3bench")
library("rjd3revisions")
library("rjd3nowcasting")
library("rjd3sts")
library("rjd3stl")
library("ggdemetra3")



# Gráficos
library("GGally")
library("urca")
library("fracdiff")
library("glue")
library("seasonalview")

# Análisis Multivariante
library("broom") #PCA

rm(list = ls())

# CARGAR SERIE DATACOMEX PARA EXTRAER LAS SUBSERIES MENSUALES-----
datacomex_E_raw <- datacomexr::sec(flujo = "E", nivel=1, desde=2010, nocache = TRUE) |>
  dplyr::group_by(year, mes, flujo) |>
  dplyr::summarise(euros=sum(euros, na.rm=T)) |>
  dplyr::select(year, mes, euros)

ts_datacomex_E_0 <- stats::ts(datacomex_E_raw$euros,
                              start = c(2010, 1),
                              frequency =12)

y_raw <- ts_datacomex_E_0

# CREAMOS LAS SERIES MENSUALES-----

y_12_23 <- window(y_raw, end = end(y_raw) - c(0, 13))
y_1_24  <- window(y_raw, end = end(y_raw) - c(0, 12))
y_2_24  <- window(y_raw, end = end(y_raw) - c(0, 11))
y_3_24  <- window(y_raw, end = end(y_raw) - c(0, 10))
y_4_24  <- window(y_raw, end = end(y_raw) - c(0, 9))
y_5_24  <- window(y_raw, end = end(y_raw) - c(0, 8))
y_6_24  <- window(y_raw, end = end(y_raw) - c(0, 7))
y_7_24  <- window(y_raw, end = end(y_raw) - c(0, 6))
y_8_24  <- window(y_raw, end = end(y_raw) - c(0, 5))
y_9_24  <- window(y_raw, end = end(y_raw) - c(0, 4))
y_10_24 <- window(y_raw, end = end(y_raw) - c(0, 3))
y_11_24 <- window(y_raw, end = end(y_raw) - c(0, 2))
y_12_24 <- window(y_raw, end = end(y_raw) - c(0, 1))
y_1_25  <- y_raw


# CREAR ESTRUCTURA DE CARPETAS PARA GUARDAR LOS INPUTS Y OUTPUTS DE LOS ANALISIS-----
data_file_name <- paste0("SERIES_SIMULATION_2024", ".RData")
data_full_path <- file.path("simulations", data_file_name)


save(y_12_23,
     y_1_24,
     y_2_24,
     y_3_24,
     y_4_24,
     y_5_24,
     y_6_24,
     y_7_24,
     y_8_24,
     y_9_24,
     y_10_24,
     y_11_24,
     y_12_24,
     y_1_25,
     file = data_full_path)


# SIMULACION
load(file = data_full_path)


source("C:/Users/francisco.hernando/Documents/src/TS_SA_DATACOMEX/R/SA_ANALISIS.R")
source("C:/Users/francisco.hernando/Documents/src/TS_SA_DATACOMEX/R/SA_REVISION.R")

SA_analisis(ts_tibble = y_12_23)
SA_revision(ts_tibble = y_1_24)
SA_revision(ts_tibble = y_2_24)
SA_revision(ts_tibble = y_3_24)
SA_revision(ts_tibble = y_4_24)
SA_revision(ts_tibble = y_5_24)
SA_revision(ts_tibble = y_6_24)
SA_revision(ts_tibble = y_7_24)
SA_revision(ts_tibble = y_8_24)
SA_revision(ts_tibble = y_9_24)
SA_revision(ts_tibble = y_10_24)
SA_revision(ts_tibble = y_11_24)
SA_revision(ts_tibble = y_12_24)
SA_analisis(ts_tibble = y_1_25)
