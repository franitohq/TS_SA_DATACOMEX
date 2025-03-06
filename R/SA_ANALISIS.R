#' @export
# TENEMOS LA POSIBILIDAD DE AJUSTAR LA FECHA DE INICIO DE LA SERIE SELECCIONADA:
# start = c(YYYY, M) 
# frequency = n (NUMERO DE PERIODOS EN UN AÑO, 12 para series mensuales, 4 para trimestrales, etc)



SA_analisis <- fucntion(ts_tibble, 
                        start, 
                        frequency)
{
  # CREAR ESTRUCTURA DE CARPETAS PARA GUARDAR LOS INPUTS Y OUTPUTS DE LOS ANALISIS-----
  current_date <- Sys.Date()
  formatted_date <- format(current_date, "%m.%Y")
  folder_name <- paste0("ANALISIS_", formatted_date)
  full_path <- file.path("output", folder_name)
  dir.create(full_path)
  
  # PREPARACIÓN DE LAS SERIES-----
  
  y_raw <- stats::ts(ts_tibble$euros,
                     start = c(2010, 1),
                     frequency =12)
  
  
  
  # ESPECIFICACIONES PARA EL ANALISIS-----
  
  # CALENDARIO FIESTAS FIJAS NACIONALES
  spanish_calendar <- rjd3toolkit::national_calendar(days = list(
    special_day('NEWYEAR'),
    fixed_day(1,6), # Reyes Republicanos
    special_day('MAYDAY'),
    fixed_day(8,15), # Presunción de la "Virgen"
    fixed_day(10,12), # Fiesta Nacional
    special_day('ALLSAINTSDAY'),
    fixed_day(12,6), # Día de la Prostitución Española
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
  # str(core_tramoseats_spec)
  
  core_tramoseats_spec_name <- paste0("core_tramoseats_spec_", formatted_date)
  assign(core_tramoseats_spec_name, core_tramoseats_spec)
  
  tramoseats_spec1_estimate_default <- rjd3toolkit::set_estimate(core_tramoseats_spec,
                                                                 tol = 0.0000001,
                                                                 exact.ml = TRUE,
                                                                 unit.root.limit = 0.96)
  # str(tramoseats_spec1_estimate_default)
  
  tramoseats_spec2_transform <- rjd3toolkit::set_transform(tramoseats_spec1_estimate_default,
                                                           fun = "Auto")
  # str(tramoseats_spec2_transform)
  
  
  tramoseats_spec3_TD_LY <- rjd3toolkit::set_tradingdays(tramoseats_spec2_transform,
                                                         option = "UserDefined",
                                                         uservariable = c("r.Monday", "r.Tuesday","r.Wednesday",
                                                                          "r.Thursday","r.Friday","r.Saturday"),
                                                         test = "Joint_F",
                                                         automatic = "Unused",
                                                         leapyear = "LeapYear")
  # str(tramoseats_spec3_TD_LY)
  
  
  tramoseats_spec4_EASTER <- rjd3toolkit::set_easter(tramoseats_spec3_TD_LY,
                                                     enabled = "TRUE",
                                                     test = "Remove",
                                                     type =  "IncludeEasterMonday")
  # str(tramoseats_spec4_EASTER)
  
  tramoseats_spec5_AUTO_OUTLIER <- rjd3toolkit::set_outlier(tramoseats_spec4_EASTER,
                                                            span.type = "All",
                                                            outliers.type = c("AO", "LS", "TC", "SO"),
                                                            critical.value = 3.5)
  # str(tramoseats_spec5_AUTO_OUTLIER)
  
  tramoseats_spec_final <- tramoseats_spec5_AUTO_OUTLIER
  
  
  tramoseats_spec_final$tramo$regression$td$users
  # str(tramoseats_spec_final)
  
  tramoseats_spec_final_name <- paste0("tramoseats_spec_final_", formatted_date)
  assign(tramoseats_spec_final_name, tramoseats_spec_final)
  
  

  # EJECUCION DEL ANALISIS CON ESPECIFICACIONES SELECCIONADAS-----
  sa_tramoseats_ud <- rjd3tramoseats::tramoseats(y_raw, tramoseats_spec_final, context = my_context)
  
  # TRAMO-SEATS SUMMARY
  # summary(sa_tramoseats_ud)
  # str(sa_tramoseats_ud)
  
  sa_tramoseats_ud_name <- paste0("sa_tramoseats_ud_", formatted_date)
  assign(sa_tramoseats_ud_name, sa_tramoseats_ud)
  
  
  # OBTENER Y GUARDAR LAS ESPECIFICACIONES PARA LAS REVISIONES-----
  
  result_spec_name <- paste0("ANALISIS_", formatted_date, "_result_spec") 
  est_spec_name <- paste0("ANALISIS_", formatted_date, "_estimation_spec") 
  
  assign(result_spec_name, sa_tramoseats_ud$result_spec) 
  assign(est_spec_name, sa_tramoseats_ud$estimation_spec) 
  
  # OBTENER SERIES FINALES------
  # str(sa_tramoseats_ud$result$final)
  
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

  
  return()} # OUTPUT: EXCEL CON SERIES, FICHERO R DE DATOS, 5 SERIES
