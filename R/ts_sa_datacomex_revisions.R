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




# CREAMOS EL DIRECTORIO PARA GUARDAR LOS RESULTADOS DE LA REVISION-----
current_date <- Sys.Date()
current_formatted_date <- format(current_date, "%m.%Y")
current_folder_name <- paste0("REVISION_", current_formatted_date)
current_full_path <- file.path("output", current_folder_name)
dir.create(current_full_path)
current_year <- year(current_date)
current_month <- month(current_date)
current_year
current_month

# CARGAR DATOS DEL ANALISIS/REVISION ANTERIOR (HACER PARA QUE COJA CARPETA DE ANALISIS O REVISION SEGUN CORRESPONDA)-----
# HACER LA CARGA USANDO SIMPLEMNTE EL MES ANTERIOR, USAR UNA REGULAR EXPRESSION, QUE TERMINE EN "%m.%Y", ASI COGERA ANALISIS O REVISION INDISTINTAMENTE
previous_date <- Sys.Date() - months(1)
formatted_previous_date <- format(previous_date, "%m.%Y")
formatted_previous_date <- current_formatted_date
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

# QUITAMOS LA ULTIMA OBSERVACION DE LA SERIA ACTUALIZADA-----

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

# ESPECIFICACIONES ANTERIORES-----

previous_est_spec_name <- paste0("ANALISIS_", formatted_previous_date, "_estimation_spec")
previous_res_spec_name <- paste0("ANALISIS_", formatted_previous_date, "_result_spec")

previous_estimation_spec <- eval(parse(text = previous_est_spec_name))
previous_result_spec <- eval(parse(text = previous_est_spec_name))


# CREAR FECHA AUTOMATICA PARA LA RE-DETECCION DE OUTLIERS----- 

outlier_detect_date <- Sys.Date() - months(12)
outlier_detect_date <- as.Date(outlier_detect_date)
year_outlier_detect <- year(outlier_detect_date)
month_outlier_detect <- month(outlier_detect_date)
year_outlier_detect 
month_outlier_detect

# GENERAR LA ESPECIFICACION PARA LA REVISION (VER QUE PASA CON LA VENTANA DE ESTIMACION DE OOUTLIERS)-----
tramo_refreshed_current_spec <- rjd3tramoseats::tramoseats_refresh(
  spec = previous_result_spec, # point spec to be refreshed
  refspec = previous_estimation_spec, #domain spec (set of constraints)
  policy = "Outliers",
  period = 12, # Monthly series
  start = c(2023,1),
  end = c(2024,12)
  # start = c(year_outlier_detect, month_outlier_detect), #Date from which outliers will be re-detected
  # end = c(current_year, current_month) 
)

# EJECUTAR LA REVISION DE LA SERIE ACTUALIZADA CON LA ESPECIFICACION ACTUALZIADA-----
current_ts <- eval(parse(text = serie_actual_name))
sa_tramoseats_ud_revised <- rjd3tramoseats::tramoseats(current_ts, tramo_refreshed_current_spec, context = my_context)

sa_tramoseats_ud
sa_tramoseats_ud_revised

sa_tramoseats_ud_revised_name <- paste0("sa_tramoseats_ud_REVISION_", current_formatted_date) 
assign(sa_tramoseats_ud_revised_name, sa_tramoseats_ud_revised) 

# COMPROBAR LA VENTANA DE DETECCION DE OUTLIERS QUE DEBERÍA SER LA ESPECIFICADA EN star = .....-----
str(sa_tramoseats_ud_revised$estimation_spec)
sa_tramoseats_ud_revised$estimation_spec
str(sa_tramoseats_ud_revised$result_spec)
sa_tramoseats_ud_revised$result_spec

rev_est_spec_name <- paste0("REVISION_", current_formatted_date, "_estimation_spec") 
rev_result_spec_name <- paste0("REVISION_", current_formatted_date, "_result_spec") 

assign(rev_est_spec_name, sa_tramoseats_ud_revised$estimation_spec) 
assign(rev_result_spec_name, sa_tramoseats_ud_revised$result_spec) 

# OBTENER SERIES FINALES------
str(sa_tramoseats_ud_revised$result$final)

rev_original_ts_name <- paste0("original_ts_REVISION_", current_formatted_date) 
rev_seasonally_adjusted_ts_name <- paste0("seasonally_adjusted_ts_REVISION_", current_formatted_date) 
rev_trend_ts_name <- paste0("trend_ts_REVISION_", current_formatted_date) 
rev_seasonal_component_ts_name <- paste0("seasonal_component_ts_REVISION_", current_formatted_date) 
rev_irregular_ts_name <- paste0("irregular_ts_REVISION_", current_formatted_date) 

assign(rev_original_ts_name, sa_tramoseats_ud_revised$result$final$series$data) 
assign(rev_seasonally_adjusted_ts_name, sa_tramoseats_ud_revised$result$final$sa$data) 
assign(rev_trend_ts_name, sa_tramoseats_ud_revised$result$final$t$data) 
assign(rev_seasonal_component_ts_name, sa_tramoseats_ud_revised$result$final$s$data) 
assign(rev_irregular_ts_name, sa_tramoseats_ud_revised$result$final$i$data) 


# HISTORIAL DE REVISIONES
# CARGA DATOS DE ANALISIS ANTERIORES PARA EL HISTORIAL-----
# (7 ULTIMOS AÑOS + AÑO CORRIENTE)



# GRAFICOS DE LAS SERIES Y LAS TASAS DE VARIACION INTERANUALES-----
# USAR UNA FUNCION QUE HAGA LOS GRAFICOS

# GUARDADO DE DATOS DE LA REVISION Y EL HISTORIAL-----

ts_df <- data.frame(Time = as.Date(as.yearmon(time(eval(parse(text = serie_previa_name))))), 
                    Value = as.numeric(eval(parse(text = serie_previa_name))) 
)
ts_df$Time <- format(ts_df$Time, "1.%m.%Y")

ts_data_name <- paste0("ts_data_rev_", formatted_date, ".xlsx")
xls_output_path <- file.path("output", current_folder_name, ts_data_name)
openxlsx::write.xlsx(ts_df, file = xls_output_path, rowNames = FALSE)


data_file_name <- paste0("DATOS_REVISION_", formatted_date, ".RData")
data_full_path <- file.path("output", current_folder_name, data_file_name)




save(spanish_calendar,
     list = c(
              sa_tramoseats_ud_revised_name,
              rev_result_spec_name, 
              rev_est_spec_name,
              
     ),
     file = data_full_path)


save(spanish_calendar,
     list = c(regs_td_name,
              my_regressors_name,
              my_context_name,
              core_tramoseats_spec_name,
              tramoseats_spec_final_name,
              sa_tramoseats_ud_revised_name,
              rev_result_spec_name, 
              rev_est_spec_name,
              original_ts_name,
              seasonally_adjusted_ts_name,
              trend_ts_name,
              seasonal_component_ts_name,
              irregular_ts_name,
              TV_original_ts_name,
              TV_seasonally_adjusted_ts_name
     ),
     file = data_full_path)





