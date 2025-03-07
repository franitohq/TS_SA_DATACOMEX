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
#   -Archivo de datos R que contine:  calendario, 
#                                     regresores, 
#                                     contexto, 
#                                     especificacion inical y final del analisis anual, 
#                                     especificacion inical y final de la revisión,
#                                     resultados de la revision,
#                                     serie original,
#                                     serie desestacionalizada,
#                                     serie tendencia,  
#                                     serie componente estacional,
#                                     serie componente irregular.
#   -Lista con las 5 series anteriores par su carga en servidor.
# 
# Los dos primeros elementos del output se guardar en una carpeta con el nombre: ANALISIS_%m.%Y


SA_revision <- function(ts_tibble, 
                        inicio, 
                        freq)
{
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
  
  # CARGAR DATOS DEL ANALISIS/REVISION ANTERIOR-----
  previous_date <- Sys.Date() - months(1)
  formatted_previous_date <- format(previous_date, "%m.%Y")
  formatted_previous_date <- current_formatted_date
  
  # HABRÁ MESES EN LOS QUE EL MES ANTERIOR SE HAYA HECHO UNA REVISION Y OTROS UN ANALISIS. 
  # AL TENER DIFERENTES NOMBRES EN CADA CASO LAS CARPETAS Y FICHEROS  NECESITAMOS 
  # EXTRAER ESTOS NOMBRES DE LA CARPETA "OUTPUT". DE ESTA MANERA EL PROCESO SE HACE DE FORMA 
  # AUTOMATICA PARA CUALQUIER MES INDEPENDIENTEMENTE DE LA OPERACIÓN LLEVADA A CABO EL MES ANTERIOR.
  folders <- list.dirs("output")
  matching_folder <- folders[grep(paste0(formatted_previous_date, "$"), folders)]  
  
  # AHORA QUE TENEMOS EL NOMBRE DE LA CARPETA QUE CONTIENE LOS DATOS DE LA OPERACIÓN 
  # ANTERIOR PODEMOS USARLO PARA ACCEDER A DICHOS DATOS Y CARGARLOS.
  
  file_ending <- paste0(formatted_previous_date,".RData")
  matching_files <- list.files(matching_folder, pattern = paste0(file_ending, "$"), full.names = TRUE)
  data_file_name <- basename(matching_files)
  previous_full_path <- file.path(matching_folder, data_file_name)
  load(file = previous_full_path)
  
  
  
} 
