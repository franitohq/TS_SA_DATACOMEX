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
# install.packages("scales")
# remove.packages("patchwork")
# install.packages("patchwork")
# install.packages("tools")


DATACOMEXR
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




# CREAMOS EL DIRECTORIO PARA GUARDAR LOS RESULTADOS-----
current_date <- Sys.Date()
current_formatted_date <- format(current_date, "%m.%Y")
current_folder_name <- paste0("REVISION_", current_formatted_date)
current_full_path <- file.path("output", current_folder_name)
dir.create(current_full_path)

# CARGAR DATOS DEL ANALISIS/REVISION ANTERIOR-----
previous_date <- Sys.Date() - months(1)
formatted_previous_date <- format(previous_date, "%m.%Y")
previous_folder_name <- paste0("ANALISIS_", formatted_previous_date)
previous_file_name <- paste0("DATOS_ANALISIS_", formatted_previous_date, ".RData")
previous_full_path <- file.path("output", previous_folder_name,previous_file_name)

load(file = previous_full_path)

# CARGAR SERIES DATACOMEX ACTUALIZADAS-----

datacomex_E_raw <- datacomexr::sec(flujo = "E", nivel=1, desde=2010, nocache = TRUE) |>
  dplyr::group_by(year, mes, flujo) |>
  dplyr::summarise(euros=sum(euros, na.rm=T)) |>
  dplyr::select(year, mes, euros)

ts_datacomex_E_0 <- stats::ts(datacomex_E_raw$euros,
                              start = c(2010, 1),
                              frequency =12)

serie_actual_name <- paste0("y_", current_formatted_date)
assign(serie_actual_name, ts_datacomex_E_0)


# COMPROBAR SERIE ORIGINAL VS. SERIE ORIGINAL ACTUALIZADA-----
serie_previa_name <- paste0("y_", formatted_previous_date)
assign(serie_previa_name, original_ts)

# QUITAMOS LA ULTIMA OBSERVACION DE LA SERIA ACTUALIZADA


ts_ref <- eval(parse(text = serie_previa_name))
current_ts_trimmed <- window(eval(parse(text = serie_actual_name)), 
                             end = end(eval(parse(text = serie_actual_name))) - c(0, 1))

ts_diferencia <- current_ts_trimmed - ts_ref

test_nonzero <- any(ts_diferencia != 0) # TIENE QUE SALIR FALSE PARA QUE LAS SERIES SIN LA ULTIMA OBSERVACION COINCIDAN.

test_nonzero

if (any(ts_diferencia != 0)) {
  stop("The difference series contains non-zero values. Program terminated.")
}

print("The difference series has no non-zero values. Continuing execution...")

# ESPECIFICACIONES ANTERIORES
current_result_spec <- sa_x13_ud$result_spec
current_domain_spec <- sa_x13_ud$estimation_spec

# CREAR FECHA AUTOMATICA PARA LA DETECCION DE OUTLIERS----- 

# GENERAR LA ESPECIFICACION PARA LA REVISION-----
tramo_refreshed_spec <- rjd3tramoseats::tramoseats_refresh(
  spec = current_result_spec, # point spec to be refreshed
  refspec = current_domain_spec, #domain spec (set of constraints)
  policy = "Outliers",
  period = 12, # Monthly series
  start = c(2011, 1) #Date from which outliers will be re-detected
)

# EJECUTAR LA REVISION DE LA SERIE ACTUALIZADA CON LA ESPECIFICACION ACTUALZIADA-----
sa_tramoseats_ud_17_1 <- rjd3tramoseats::tramoseats(y_17_1, tramo_refreshed_spec, context = my_context)


# COMPROBAR LA VENTANA DE DETECCION DE OUTLIERS QUE DEBERÍA SER LA ESPECIFICADA EN end = above-----
sa_x13_ud$result_spec
sa_x13_ud_17_1$result_spec
sa_x13_ud_17_1$estimation_spec

# HISTORIAL DE REVISIONES
# CARGA DATOS DE ANALISIS ANTERIORES PARA EL HISTORIAL-----
# (5 ULTIMOS AÑOS + AÑO CORRIENTE)