#' @export
# TENEMOS LA POSIBILIDAD DE AJUSTAR LA FECHA DE INICIO DE LA SERIE SELECCIONADA:
# start = c(YYYY, M) 
# frequency = n (NUMERO DE PERIODOS EN UN AÑO, 12 para series mensuales, 4 para trimestrales, etc)
# La función ejecuta el analisis estacional con el algoritmo TRAMO-SEATS en la implementación de RJDemetra+ v.3.
# Las especificaciones de partida pueden consultarse en el cuerpo de la función que puede modificarse si se deasea.
# No se ha incluido la opción de modificar estas especificaciones en los argumentos de la función por simplicidad y 
# seguridad evitando que se modifique algún parametro por error y se realicen analisis con especificaciones diferentes sin darse cuenta.
# El output de la funcion consta de lo siguiente:
#   -Excel con la serie original analizada que puede importarse en la GUI de JDemetra+ para posteriores analisis.
#   -Archivo de datos R que contine:  calendario, 
#                                     regresores, 
#                                     contexto, 
#                                     especificacion inical y final para el analisis, 
#                                     resultados del analisis,
#                                     especificaciones para la revision,
#                                     serie original,
#                                     serie desestacionalizada,
#                                     serie tendencia,  
#                                     serie componente estacional,
#                                     serie componente irregular.
#   -Lista con las 5 series anteriores par su carga en servidor.
# 
# Los dos primeros elementos del output se guardar en una carpeta con el nombre: ANALISIS_%m.%Y
  

SA_analisis <- function(ts_tibble, 
                        inicio, 
                        freq)
{
  # CREAR ESTRUCTURA DE CARPETAS PARA GUARDAR LOS INPUTS Y OUTPUTS DE LOS ANALISIS-----
  current_date <- Sys.Date()
  formatted_date <- format(current_date, "%m.%Y")
  folder_name <- paste0("ANALISIS_", formatted_date)
  full_path <- file.path("output", folder_name)
  dir.create(full_path)
  
  # PREPARACIÓN DE LAS SERIES-----
  
  y_raw <- stats::ts(ts_tibble$euros,
                     start = inicio,
                     frequency = freq)
  
  
  
  # ESPECIFICACIONES PARA EL ANALISIS-----
  
  # CALENDARIO FIESTAS FIJAS NACIONALES
  spanish_calendar <- rjd3toolkit::national_calendar(days = list(
    rjd3toolkit::special_day('NEWYEAR'),
    rjd3toolkit::fixed_day(1,6), # Reyes Republicanos
    rjd3toolkit::special_day('MAYDAY'),
    rjd3toolkit::fixed_day(8,15), # Presunción de la "Virgen"
    rjd3toolkit::fixed_day(10,12), # Fiesta Nacional
    rjd3toolkit::special_day('ALLSAINTSDAY'),
    rjd3toolkit::fixed_day(12,6), # Día de la Prostitución Española
    rjd3toolkit::fixed_day(12,8), # Inmaculada Concepción
    rjd3toolkit::special_day('CHRISTMAS')
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
    contrasts = TRUE
  )
  
  regs_td_name <- paste0("regs_td_", formatted_date)
  assign(regs_td_name, regs_td)
  
  my_regressors <- list( Monday = regs_td[,1],
                         Tuesday = regs_td[,2],
                         Wednesday = regs_td[,3],
                         Thursday = regs_td[,4],
                         Friday = regs_td[,5],
                         Saturday = regs_td[,6])
  
  my_regressors_name <- paste0("my_regressors_", formatted_date)
  assign(my_regressors_name, my_regressors)
  
  
  my_context <- rjd3toolkit::modelling_context(variables = my_regressors)
  rjd3toolkit::.r2jd_modellingcontext(my_context)$getTsVariableDictionary()
  
  my_context_name <- paste0("my_context_", formatted_date)
  assign(my_context_name, my_context)
  
  core_tramoseats_spec <- rjd3tramoseats::tramoseats_spec("rsafull")

  core_tramoseats_spec_name <- paste0("core_tramoseats_spec_", formatted_date)
  assign(core_tramoseats_spec_name, core_tramoseats_spec)
  
  tramoseats_spec1_estimate_default <- rjd3toolkit::set_estimate(core_tramoseats_spec,
                                                                 tol = 0.0000001,
                                                                 exact.ml = TRUE,
                                                                 unit.root.limit = 0.96)

  tramoseats_spec2_transform <- rjd3toolkit::set_transform(tramoseats_spec1_estimate_default,
                                                           fun = "Auto")

  
  tramoseats_spec3_TD_LY <- rjd3toolkit::set_tradingdays(tramoseats_spec2_transform,
                                                         option = "UserDefined",
                                                         uservariable = c("r.Monday", "r.Tuesday","r.Wednesday",
                                                                          "r.Thursday","r.Friday","r.Saturday"),
                                                         test = "Joint_F",
                                                         automatic = "Unused",
                                                         leapyear = "LeapYear")

  
  tramoseats_spec4_EASTER <- rjd3toolkit::set_easter(tramoseats_spec3_TD_LY,
                                                     enabled = "TRUE",
                                                     test = "Remove",
                                                     type =  "IncludeEasterMonday")

  tramoseats_spec5_AUTO_OUTLIER <- rjd3toolkit::set_outlier(tramoseats_spec4_EASTER,
                                                            span.type = "All",
                                                            outliers.type = c("AO", "LS", "TC", "SO"),
                                                            critical.value = 3.5)

  tramoseats_spec_final <- tramoseats_spec5_AUTO_OUTLIER
  
  
  tramoseats_spec_final$tramo$regression$td$users

  tramoseats_spec_final_name <- paste0("tramoseats_spec_final_", formatted_date)
  assign(tramoseats_spec_final_name, tramoseats_spec_final)
  
  

  # EJECUCION DEL ANALISIS CON ESPECIFICACIONES SELECCIONADAS-----
  sa_tramoseats_ud <- rjd3tramoseats::tramoseats(y_raw, tramoseats_spec_final, context = my_context)
  
  # TRAMO-SEATS SUMMARY
  sa_tramoseats_ud_name <- paste0("sa_tramoseats_ud_", formatted_date)
  assign(sa_tramoseats_ud_name, sa_tramoseats_ud)
  
  
  # OBTENER Y GUARDAR LAS ESPECIFICACIONES PARA LAS REVISIONES-----
  
  result_spec_name <- paste0("ANALISIS_", formatted_date, "_result_spec") 
  est_spec_name <- paste0("ANALISIS_", formatted_date, "_estimation_spec") 
  
  assign(result_spec_name, sa_tramoseats_ud$result_spec) 
  assign(est_spec_name, sa_tramoseats_ud$estimation_spec) 
  
  # OBTENER SERIES FINALES------

  original_ts_name <-             paste0("original_ts_", formatted_date) 
  seasonally_adjusted_ts_name <-  paste0("seasonally_adjusted_ts_", formatted_date) 
  trend_ts_name <-                paste0("trend_ts_", formatted_date) 
  seasonal_component_ts_name <-   paste0("seasonal_component_ts_", formatted_date) 
  irregular_ts_name <-            paste0("irregular_ts_", formatted_date)
  
  assign(original_ts_name, sa_tramoseats_ud$result$final$series$data) 
  assign(seasonally_adjusted_ts_name, sa_tramoseats_ud$result$final$sa$data) 
  assign(trend_ts_name, sa_tramoseats_ud$result$final$t$data) 
  assign(seasonal_component_ts_name, sa_tramoseats_ud$result$final$s$data) 
  assign(irregular_ts_name, sa_tramoseats_ud$result$final$i$data) 
  
  
  original_ts <- sa_tramoseats_ud$result$final$series$data
  seasonally_adjusted_ts <- sa_tramoseats_ud$result$final$sa$data
  trend_ts <- sa_tramoseats_ud$result$final$t$data
  seasonal_component_ts <- sa_tramoseats_ud$result$final$s$data
  irregular_ts <- sa_tramoseats_ud$result$final$i$data
  
  
  # GUARDADO DE RESULTADOS-----
  # GUARDAMOS LA SERIE ANALIZADA COMO EXCEL POR SI ES NECESARIO USARLA EN LA GUI JDEMETRA+
  ts_df <- data.frame(Time = as.Date(zoo::as.yearmon(time(y_raw))), 
                      Value = as.numeric(y_raw) 
                     )
  ts_df$Time <- format(ts_df$Time, "1.%m.%Y")
  
  ts_data_name <- paste0("ts_raw_ANALISIS_", formatted_date, ".xlsx")
  xls_output_path <- file.path("output", folder_name, ts_data_name)
  openxlsx::write.xlsx(ts_df, file = xls_output_path, rowNames = FALSE)
  
  
  data_file_name <- paste0("DATOS_ANALISIS_", formatted_date, ".RData")
  data_full_path <- file.path("output", folder_name, data_file_name)
  
  
  save(spanish_calendar,
       list = c(regs_td_name,
                my_regressors_name,
                my_context_name,
                core_tramoseats_spec_name,
                tramoseats_spec_final_name,
                sa_tramoseats_ud_name,
                result_spec_name, 
                est_spec_name,
                original_ts_name,
                seasonally_adjusted_ts_name,
                trend_ts_name,
                seasonal_component_ts_name,
                irregular_ts_name
       ),
       file = data_full_path)
  
  ts_list_name <- paste0("ts_list_ANALISIS_", formatted_date) 
  ts_list <-   list(original = original_ts,
                    seasonally_adjusted =seasonally_adjusted_ts,
                    trend = trend_ts,
                    seasonal_component = seasonal_component_ts,
                    irregular = irregular_ts)

  assign(ts_list_name, ts_list)
  
  return(eval(parse(text = ts_list_name)))
  
  } 
