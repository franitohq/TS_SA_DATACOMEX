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

ts_air <- ts_datacomex_E_0

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
                                                       test = "Joint_F",
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
SI_ts <- seasonal_component_ts*irregular_ts

plot(original_ts)

tsibble_o <- tsibble::as_tsibble(original_ts)
tsibble_sa <- tsibble::as_tsibble(seasonally_adjusted_ts)
tsibble_t <- tsibble::as_tsibble(trend_ts)
tsibble_sc <- tsibble::as_tsibble(seasonal_component_ts)
tsibble_i <- tsibble::as_tsibble(irregular_ts)
tsibble_SI <- tsibble::as_tsibble(SI_ts)


# Convert the tsibble index to Date format
tsibble_o <- tsibble_o |> 
  mutate(Date = as.Date(index))  # Convert yearmonth to Date
tsibble_sa <- tsibble_sa |> 
  mutate(Date = as.Date(index))
tsibble_t <- tsibble_t |> 
  mutate(Date = as.Date(index))
tsibble_sc <- tsibble_sc |> 
  mutate(Date = as.Date(index))
tsibble_i <- tsibble_i |> 
  mutate(Date = as.Date(index))
tsibble_SI <- tsibble_SI |> 
  mutate(Date = as.Date(index))

# GRÁFICOS----- 


# Define colors
left_axis_color <- "black"  # Color for the left axis labels and ticks
right_axis_color <- "black"  # Color for the right axis labels and ticks



plot1 <- ggplot(tsibble_o, aes(x = index, y = value)) +
  geom_line(color = "darkblue", size = 0.5) +  
  labs(
    title = "Original Time Series",
    x = "Time",
    y = "Measured Quantity [units]"
  ) +
  scale_x_yearmonth(
    labels = scales::date_format(format = "%Y"),  # Format date labels as years
    breaks = seq(min(tsibble_o$Date), max(tsibble_o$Date), by = "2 year")  # Add ticks every year
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 1, name = "Measured Quantity [units]", breaks = seq(0, 700, by = 100)),  # Add a secondary y-axis
    breaks = seq(0, 700, by = 100)  # Add ticks every 100 units on the y-axis
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,           # Center the title
      size = 13,             # Increase font size
      face = "bold",         # Make the title bold
      color = "darkblue"     # Change title color
    ),
    axis.title.y.left = element_text(color = left_axis_color),  # Left axis color
    axis.text.y.left = element_text(color = left_axis_color),
    axis.title.y.right = element_text(color = right_axis_color),  # Right axis color
    axis.text.y.right = element_text(color = right_axis_color)
  )

plot2 <-ggplot(tsibble_sa, aes(x = index, y = value)) +
  geom_line(color = "darkgreen", size = 0.5) +  
  labs(
    title = "Seasonally Adjusted Time Series",
    x = "Time",
    y = "Measured Quantity [units]"
  ) +
  scale_x_yearmonth(
    labels = scales::date_format(format = "%Y"),  # Format date labels as years
    breaks = seq(min(tsibble_o$Date), max(tsibble_o$Date), by = "2 year")  # Add ticks every year
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 1, name = "Measured Quantity [units]", breaks = seq(0, 500, by = 100)),  # Add a secondary y-axis
    breaks = seq(0, 500, by = 100)  # Add ticks every 100 units on the y-axis
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,           # Center the title
      size = 13,             # Increase font size
      face = "bold",         # Make the title bold
      color = "darkgreen"     # Change title color
    ),
    axis.title.y.left = element_text(color = left_axis_color),  # Left axis color
    axis.text.y.left = element_text(color = left_axis_color),
    axis.title.y.right = element_text(color = right_axis_color),  # Right axis color
    axis.text.y.right = element_text(color = right_axis_color)
  )



combined_plot <- (plot1 | plot2) 
  # (plot3 | plot4) /
  # (plot5 | plot6)

print(combined_plot)

# CUADRO S-I(REVISAR Y MODIFICAR FUNCION)-----


feasts::gg_subseries(tsibble_sa_ts) +
  ggplot2::labs(y = "Units",
                title = "Seasonally Adjusted Series")


feasts::gg_subseries(tsibble_sc_ts) +
  ggplot2::labs(y = "Units",
                title = "Seasonal Component")

feasts::gg_subseries(tsibble_SI_ts) +
  ggplot2::labs(y = "Units",
                title = "SI Chart")



# TASAS DE VARIACION-----


# GUARDADO DE ARCHIVOS Y ORGANIZACIÓN

