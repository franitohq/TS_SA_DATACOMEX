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
# rm(list = ls())

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

# PREPARACIÓN DE LAS SERIES-----

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

xls_output_path <- file.path("output", folder_name, "ts_data.xlsx")
openxlsx::write.xlsx(ts_df, file = xls_path, rowNames = FALSE)

y_raw <- ts_air


# ESPECIFICACIONES PARA EL ANALISIS-----

# CALENDARIO FIESTAS FIJAS NACIONALES
spanish_calendar <- rjd3toolkit::national_calendar(days = list(
  special_day('NEWYEAR'),
  fixed_day(1,6), # Reyes
  special_day('MAYDAY'),
  fixed_day(8,15), # Asunción de la "Virgen"
  fixed_day(10,12), # Fiesta Nacional
  special_day('ALLSAINTSDAY'),
  fixed_day(12,6), # Día de la Constitución Española
  fixed_day(12,8), # Inmaculada Concepción
  special_day('CHRISTMAS')
)
)

# CREAMOS REGRESORES
regs_td <- rjd3toolkit::calendar_td(
  calendar = spanish_calendar,
  s = y_raw,
  # frequency = 12,
  # start = c(2000,1),
  # length = 300,
  holiday = 7,
  groups = c(1, 2, 3, 4, 5, 6, 0),
  # groups = c(1, 2, 1, 1, 1, 1, 0),
  contrasts = TRUE
)

my_regressors <- list( Monday = regs_td[,1],
                       Tuesday = regs_td[,2],
                       Wednesday = regs_td[,3],
                       Thursday = regs_td[,4],
                       Friday = regs_td[,5],
                       Saturday = regs_td[,6])

# my_regressors <- list( NoTuesday = regs_td[,1],
#                        Tuesday = regs_td[,2])

my_context <- rjd3toolkit::modelling_context(variables = my_regressors)
rjd3toolkit::.r2jd_modellingcontext(my_context)$getTsVariableDictionary()

core_tramoseats_spec <- rjd3tramoseats::tramoseats_spec("rsafull")
str(core_tramoseats_spec)

tramoseats_spec1_estimate_default <- rjd3toolkit::set_estimate(core_tramoseats_spec,
                                                               tol = 0.0000001,
                                                               exact.ml = TRUE,
                                                               unit.root.limit = 0.96)
str(tramoseats_spec1_estimate_default)

tramoseats_spec2_transform <- rjd3toolkit::set_transform(tramoseats_spec1_estimate_default,
                                                         fun = "Auto")
str(tramoseats_spec2_transform)


tramoseats_spec3_TD_LY <- rjd3toolkit::set_tradingdays(tramoseats_spec2_transform,
                                                       option = "UserDefined",
                                                       uservariable = c("r.Monday", "r.Tuesday","r.Wednesday",
                                                                       "r.Thursday","r.Friday","r.Saturday"),
                                                       test = "Separate_T",
                                                       automatic = "Unused",
                                                       leapyear = "LeapYear")
str(tramoseats_spec3_TD_LY)


tramoseats_spec4_EASTER <- rjd3toolkit::set_easter(tramoseats_spec3_TD_LY,
                                                   enabled = "TRUE",
                                                   test = "Remove",
                                                   type =  "IncludeEasterMonday")
str(tramoseats_spec4_EASTER)

tramoseats_spec5_AUTO_OUTLIER <- rjd3toolkit::set_outlier(tramoseats_spec4_EASTER,
                                                         span.type = "All",
                                                         outliers.type = c("AO", "LS", "TC", "SO"),
                                                         critical.value = 3.5)
str(tramoseats_spec5_AUTO_OUTLIER)

tramoseats_spec_final <- tramoseats_spec5_AUTO_OUTLIER


tramoseats_spec_final$tramo$regression$td$users
str(tramoseats_spec_final)

# EJECUCION DEL ANALISIS CON ESPECIFICACIONES SELECCIONADAS-----
sa_tramoseats_ud <- rjd3tramoseats::tramoseats(y_raw, tramoseats_spec_final, context = my_context)

# TRAMO-SEATS SUMMARY
summary(sa_tramoseats_ud)
# str(sa_tramoseats_ud)

# OBTENER SERIES FINALES------
str(sa_tramoseats_ud$result$final)

original_ts <- sa_tramoseats_ud$result$final$series$data
seasonally_adjusted_ts <- sa_tramoseats_ud$result$final$sa$data
trend_ts <- sa_tramoseats_ud$result$final$t$data
seasonal_component_ts <- sa_tramoseats_ud$result$final$s$data
irregular_ts <- sa_tramoseats_ud$result$final$i$data


# GRÁFICOS 

plot(original_ts)
plot(seasonally_adjusted_ts)
plot(trend_ts)
plot(seasonal_component_ts)
plot(irregular_ts)

# CUADRO S-I 