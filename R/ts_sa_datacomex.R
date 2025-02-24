# PAQUETES-----
# Generales
# install.packages("plotly")
# install.packages("tsibble")
# install.packages("remotes")
# install.packages("devtools")
# install.packages("rJava")
# install.packages("openxlsx")
# install.packages("writexl")  
# install.packages("zoo")
# install.packages("lubridate")

# DATACOMEXR
# remotes::install_github("fabiansalazares/datacomexr")

# GENERAL TSA
# install.packages("TSA")
# install.packages("astsa")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("fma")
# install.packages("Rmetrics")
# install.packages("fGarch")
# install.packages("fUnitRoots")
# install.packages("MTS") #MULTIVARIATE TIME SERIES ANALYSIS PACKAGE
# install.packages("seasonal")
# install.packages("seasonalview")
# install.packages("feasts")
# install.packages(c("XLConnect", "XML"))
# install.packages("fpp3")
# install.packages("fable")
# install.packages("car")
# install.packages("dygraphs")
# install.packages("LSTS")
# install.packages("signal")


# RJDemetra v.2
# install.packages("RJDemetra")
# install.packages("rjdworkspace")
# install.packages("rjdmarkup")
# install.packages("JDCruncheR")
# install.packages("ggdemetra")
# install.packages("rjdqa")
# install.packages("rjwsacruncher")


# RJDemetra v.3
# remotes::install_github("palatej/rjd3modelling", INSTALL_opts = "--no-multiarch", quiet = T) #No longer available
# remotes::install_github("rjdverse/rjd3toolkit")
# remotes::install_github("rjdverse/rjd3x13")
# remotes::install_github("rjdverse/rjd3tramoseats")
# remotes::install_github("rjdverse/rjd3providers")
# remotes::install_github("rjdverse/rjd3workspace")
# remotes::install_github("rjdverse/rjd3highfreq")
# remotes::install_github("rjdverse/rjd3filters")
# remotes::install_github("rjdverse/rjd3x11plus")
# remotes::install_github("rjdverse/rjd3bench")
# remotes::install_github("rjdverse/rjd3revisions")
# remotes::install_github("rjdverse/rjd3nowcasting")
# remotes::install_github("rjdverse/rjd3sts")
# remotes::install_github("rjdverse/rjd3stl")
# remotes::install_github("AQLT/ggdemetra3")


# Gráficos
# install.packages("GGally")
# install.packages("urca")
# install.packages("fracdiff")
# install.packages("glue")
# install.packages("seasonalview")


# Análisis Multivariante
# install.packages("broom")

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




# CREAR ESTRUCTURA DE CARPETAS PARA GUARDAR LOS INPUTS Y OUTPUTS DE LOS ANALISIS-----
current_date <- Sys.Date()
formatted_date <- format(current_date, "%d.%m.%Y")
folder_name <- paste0("ANALISIS_", formatted_date)
full_path <- file.path("output", folder_name)
dir.create(full_path)

# CARGA DE DATOS CON DATACOMEX-----

datacomex_E_raw <- datacomexr::sec(flujo = "E", nivel=1, desde=2010, nocache = TRUE) |>
  dplyr::group_by(year, mes, flujo) |>
  dplyr::summarise(euros=sum(euros, na.rm=T)) |>
  dplyr::select(year, mes, euros)



datacomex_I_raw <- datacomexr::sec(flujo = "I", nivel=1, desde=2010, nocache = TRUE) |>
  dplyr::group_by(year, mes, flujo) |>
  dplyr::summarise(euros=sum(euros, na.rm=T)) |>
  dplyr::select(year, mes, euros)



ts_air <- AirPassengers

# PREPARACIÓN DE LAS SERIES

ts_datacomex_E_0 <- stats::ts(datacomex_E_raw$euros,
                              start = c(2010, 1),
                              frequency =12)

ts_datacomex_I_0 <- stats::ts(datacomex_I_raw$euros,
                              start = c(2010, 1),
                              frequency =12)

ts_df <- data.frame(Time = as.Date(as.yearmon(time(ts_air))), 
                    Value = as.numeric(ts_air) 
                    )
ts_df$Time <- format(ts_df$Time, "1.%m.%Y")

openxlsx::write.xlsx(ts_df, file = "output/ts_data.xlsx", rowNames = FALSE)

y_raw <- ts_air

