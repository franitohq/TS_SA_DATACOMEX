#' @export
# TENEMOS LA POSIBILIDAD DE AJUSTAR LA FECHA DE INICIO DE LA SERIE SELECCIONADA:
# start = c(YYYY, M) 
# frequency = n (NUMERO DE PERIODOS EN UN AÑO, 12 para series mensuales, 4 para trimestrales, etc)
# La función ejecuta la revisión mensual de la serie cuando se recibe el dato mensual de AEAT. La política de revisión es 
# "Partial Concurrent Adjustment" según la cual el modelo NO se reidentifica en la revisión, y solo se RE-ESTIMAN los parametros.
# Dentro de esta política elegimos la opción de detección de outliers a partir de una fecha dererminada en adelante, 
# dejando fijos los outliers anteriores. La ventana temporal pra re-detectar los outliers será de los últimos 12 meses.
# *********HAY QUE REVISAR ESTA PARTE DEL CODIGO YA QUE PARECE QUE NO ESTA HACIENDO BIEN LA DETECCIÓN.******
# El output de la funcion consta de lo siguiente:
#   -Excel con la serie original revisada que puede importarse en la GUI de JDemetra+ para posteriores analisis.
#   -Archivo de datos R que contine:  especificacion inical y final de la revisión,
#                                     resultados de la revision,
#                                     serie original revisada,
#                                     serie desestacionalizada revisada,
#                                     serie tendencia revisada,  
#                                     serie componente estacional revisada,
#                                     serie componente irregular revisada.
#   -Lista con las 5 series revisadas anteriores par su carga en servidor.
# 
# Los dos primeros elementos del output se guardan en una carpeta con el nombre: REVISION_%m.%Y


SA_revision <- function(ts_tibble, 
                        inicio, 
                        freq)
{
  # CREAMOS EL DIRECTORIO PARA GUARDAR LOS RESULTADOS DE LA REVISION-----
  current_date <- Sys.Date()
  current_formatted_date <- format(current_date, "%m.%Y")
  current_folder_name <- paste0("REVISION_E_", current_formatted_date)
  current_full_path <- file.path("output/EXPORTACIONES", current_folder_name)
  # current_folder_name <- paste0("REVISION_I_", current_formatted_date)
  # current_full_path <- file.path("output/IMPORTACIONES", current_folder_name)
  dir.create(current_full_path)
  current_year <- year(current_date)
  current_month <- month(current_date)
  current_year
  current_month
  
  # CARGAR DATOS DEL ANALISIS/REVISION ANTERIOR-----
  previous_date <- Sys.Date() - months(1)
  formatted_previous_date <- format(previous_date, "%m.%Y")
  # formatted_previous_date <- current_formatted_date
  
  # HABRÁ MESES EN LOS QUE EL MES ANTERIOR SE HAYA HECHO UNA REVISION Y OTROS UN ANALISIS. 
  # AL TENER DIFERENTES NOMBRES EN CADA CASO LAS CARPETAS Y FICHEROS  NECESITAMOS 
  # EXTRAER ESTOS NOMBRES DE LA CARPETA "OUTPUT". DE ESTA MANERA EL PROCESO SE HACE DE FORMA 
  # AUTOMATICA PARA CUALQUIER MES INDEPENDIENTEMENTE DE LA OPERACIÓN LLEVADA A CABO EL MES ANTERIOR.
  folders <- list.dirs("output/EXPORTACIONES")
  # folders <- list.dirs("output/IMPORTACIONES")
  matching_folder <- folders[grep(paste0(formatted_previous_date, "$"), folders)]  
  
  # AHORA QUE TENEMOS EL NOMBRE DE LA CARPETA QUE CONTIENE LOS DATOS DE LA OPERACIÓN 
  # ANTERIOR PODEMOS USARLO PARA ACCEDER A DICHOS DATOS Y CARGARLOS.
  
  file_ending <- paste0(formatted_previous_date,".RData")
  matching_files <- list.files(matching_folder, pattern = paste0(file_ending, "$"), full.names = TRUE)
  data_file_name <- basename(matching_files)
  previous_full_path <- file.path(matching_folder, data_file_name)
  
  load(file = previous_full_path)
  
  # PREPARACIÓN DE LAS SERIES-----

  y_raw <- stats::ts(datacomex_E_raw$euros,
                     start = c(2010, 1),
                     frequency = 12)
  
  # y_raw <- stats::ts(ts_tibble$euros,
  #                    start = inicio,
  #                    frequency = freq)
  
  
  
  serie_actual_name <- paste0("original_ts_", current_formatted_date)
  assign(serie_actual_name, y_raw)
  
  # COMPROBAR SERIE ORIGINAL VS. SERIE ORIGINAL ACTUALIZADA-----
  serie_previa_name <- paste0("original_ts_", formatted_previous_date)
  
  
  # QUITAMOS LA ULTIMA OBSERVACION DE LA SERIA ACTUALIZADA (METER UN TARGET)-----
  
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
  
  all_objects <- ls()
  
  previous_est_spec_name <- paste0(formatted_previous_date, "_estimation_spec", "$")
  previous_est_spec_name_files <- all_objects[grep(paste0(previous_est_spec_name, "$"), all_objects)]
  
  previous_res_spec_name <- paste0(formatted_previous_date, "_result_spec", "$")
  previous_res_spec_name_files <- all_objects[grep(paste0(previous_res_spec_name, "$"), all_objects)]
  
  
  previous_estimation_spec <- eval(parse(text = previous_est_spec_name_files))
  previous_result_spec <- eval(parse(text = previous_res_spec_name_files))
  
  
  # CREAR FECHA AUTOMATICA PARA LA RE-DETECCION DE OUTLIERS (REVISAR EL PROGRAMA VS WEBINAR Y ESCRIBIR A ANNA SMYK)----- 
  
  # outlier_detect_date <- Sys.Date() - months(12)
  # outlier_detect_date <- as.Date(outlier_detect_date)
  # year_outlier_detect <- year(outlier_detect_date)
  # month_outlier_detect <- month(outlier_detect_date)
  # year_outlier_detect 
  # month_outlier_detect
  
  
  # GENERAR LA ESPECIFICACION PARA LA REVISION (VER QUE PASA CON LA VENTANA DE ESTIMACION DE OOUTLIERS)-----
  tramo_refreshed_current_spec <- rjd3tramoseats::tramoseats_refresh(
    spec = previous_result_spec, # point spec to be refreshed
    refspec = previous_estimation_spec, #domain spec (set of constraints)
    # policy = "Outliers",
    policy = "FreeParameters",
    # period = 12, # Monthly series
    # start = c(2023,1)
    # start = c(year_outlier_detect, month_outlier_detect), #Date from which outliers will be re-detected
    # end = c(current_year, current_month) 
  )
  
  
  
  # EJECUTAR LA REVISION DE LA SERIE ACTUALIZADA CON LA ESPECIFICACION ACTUALZIADA-----
  current_ts <- eval(parse(text = serie_actual_name))
  
  my_context_name <- paste0("my_context_", formatted_previous_date)
  my_context <- eval(parse(text = my_context_name))
  

  sa_tramoseats_ud_revised <- rjd3tramoseats::tramoseats(current_ts, tramo_refreshed_current_spec, context = my_context)
  
  
  sa_tramoseats_ud_revised_name <- paste0("REVISION_sa_tramoseats_ud_", current_formatted_date) 
  assign(sa_tramoseats_ud_revised_name, sa_tramoseats_ud_revised) 
  
  
  # GUARDAR LAS ESPECIFICACIONES DE LA REVISION-----
  # COMPROBAR LA VENTANA DE DETECCION DE OUTLIERS QUE DEBERÍA SER LA ESPECIFICADA EN star = .....
  
  rev_est_spec_name <- paste0("REVISION_", current_formatted_date, "_estimation_spec") 
  rev_result_spec_name <- paste0("REVISION_", current_formatted_date, "_result_spec") 
  
  assign(rev_est_spec_name, sa_tramoseats_ud_revised$estimation_spec) 
  assign(rev_result_spec_name, sa_tramoseats_ud_revised$result_spec) 
  
  
  
  # OBTENER SERIES FINALES------
  str(sa_tramoseats_ud_revised$result$final)
  
  rev_original_ts_name            <- paste0("REVISION_original_ts_", current_formatted_date) 
  rev_seasonally_adjusted_ts_name <- paste0("REVISION_seasonally_adjusted_ts_", current_formatted_date) 
  rev_trend_ts_name               <- paste0("REVISION_trend_ts_", current_formatted_date) 
  rev_seasonal_component_ts_name  <- paste0("REVISION_seasonal_component_ts_", current_formatted_date) 
  rev_irregular_ts_name           <- paste0("REVISION_irregular_ts_", current_formatted_date) 
  
  assign(rev_original_ts_name, sa_tramoseats_ud_revised$result$final$series$data) 
  assign(rev_seasonally_adjusted_ts_name, sa_tramoseats_ud_revised$result$final$sa$data) 
  assign(rev_trend_ts_name, sa_tramoseats_ud_revised$result$final$t$data) 
  assign(rev_seasonal_component_ts_name, sa_tramoseats_ud_revised$result$final$s$data) 
  assign(rev_irregular_ts_name, sa_tramoseats_ud_revised$result$final$i$data) 
  
  original_ts             <- sa_tramoseats_ud_revised$result$final$series$data
  seasonally_adjusted_ts  <- sa_tramoseats_ud_revised$result$final$sa$data
  trend_ts                <- sa_tramoseats_ud_revised$result$final$t$data
  seasonal_component_ts   <- sa_tramoseats_ud_revised$result$final$s$data
  irregular_ts            <- sa_tramoseats_ud_revised$result$final$i$data

  
  
  # GUARDADO DE RESULTADOS-----
  # GUARDAMOS LA SERIE ANALIZADA COMO EXCEL POR SI ES NECESARIO USARLA EN LA GUI JDEMETRA+
  
  ts_df <- data.frame(Time = as.Date(zoo::as.yearmon(time(y_raw))), 
                      Value = as.numeric(y_raw) 
  )
  ts_df$Time <- format(ts_df$Time, "1.%m.%Y")
  
  ts_data_name <- paste0("ts_raw_REVISION_", current_formatted_date, ".xlsx")
  xls_output_path <- file.path("output/EXPORTACIONES", current_folder_name, ts_data_name)
  # xls_output_path <- file.path("output/IMPORTACIONES", folder_name, ts_data_name)
  openxlsx::write.xlsx(ts_df, file = xls_output_path, rowNames = FALSE)
  
  
  data_file_name <- paste0("DATOS_REVISION_", current_formatted_date, ".RData")
  data_full_path <- file.path("output/EXPORTACIONES", current_folder_name, data_file_name)
  # data_full_path <- file.path("output/IMPORTACIONES", folder_name, data_file_name)
  
    
  save(list = c(sa_tramoseats_ud_revised_name,
                rev_result_spec_name, 
                rev_est_spec_name,
                rev_original_ts_name,
                rev_seasonally_adjusted_ts_name,
                rev_trend_ts_name,
                rev_seasonal_component_ts_name,
                rev_irregular_ts_name
       ),
       file = data_full_path)
  
  ts_list_name <- paste0("REVISION_ts_list_", current_formatted_date) 
  ts_list <-   list(original = original_ts,
                    seasonally_adjusted = seasonally_adjusted_ts,
                    trend = trend_ts,
                    seasonal_component = seasonal_component_ts,
                    irregular = irregular_ts)
  
  assign(ts_list_name, ts_list)
  
  return(eval(parse(text = ts_list_name)))
} 
