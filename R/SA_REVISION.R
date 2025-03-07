#' @export
# TENEMOS LA POSIBILIDAD DE AJUSTAR LA FECHA DE INICIO DE LA SERIE SELECCIONADA:
# start = c(YYYY, M) 
# frequency = n (NUMERO DE PERIODOS EN UN AÑO, 12 para series mensuales, 4 para trimestrales, etc)
# La función ejecuta el analisis estacional con el algoritmo TRAMO-SEATS en la implementación de RJDemetra+ v.3.
# Las especificaciones de partida pueden consultarse en el cuerpo de la función que puede modificarse si se deasea.
# No se ha incluido la opción de modificar estas especificaciones en los argumentos de la función por simplicidad y 
# seguridad evitando que se modifique algún parametro por error y se realicen analisis con especificaciones diferentes sin darse cuenta.
# 


# El output de la funcion consta de lo siguiente:
#   -Excel con la serie original analizada que puede importarse en la GUI de JDemetra+ para posteriores analisis.
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
 
  
} 
