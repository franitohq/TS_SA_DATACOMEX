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




# CREAR ESTRUCTURA DE CARPETAS PARA GUARDAR LOS INPUTS Y OUTPUTS DE LOS ANALISIS-----
current_date <- Sys.Date()
formatted_date <- format(current_date, "%m.%Y")
folder_name <- paste0("ANALISIS_", formatted_date)
full_path <- file.path("output", folder_name)
dir.create(full_path)
rm(list = ls())


# CARGA DE DATOS CON DATACOMEX-----

datacomex_E_raw <- datacomexr::sec(flujo = "E", nivel=1, desde=2010, nocache = TRUE) |>
  dplyr::group_by(year, mes, flujo) |>
  dplyr::summarise(euros=sum(euros, na.rm=T)) |>
  dplyr::select(year, mes, euros)



datacomex_I_raw <- datacomexr::sec(flujo = "I", nivel=1, desde=2010, nocache = TRUE) |>
  dplyr::group_by(year, mes, flujo) |>
  dplyr::summarise(euros=sum(euros, na.rm=T)) |>
  dplyr::select(year, mes, euros)


load(file = "output/EXPORTACIONES/ANALISIS_E_03.2025/DATOS_ANALISIS_03.2025.RData")
# load(file = "output/IMPORTACIONES/ANALISIS_I_03.2025/DATOS_ANALISIS_03.2025.RData")

# ts_air <- AirPassengers

# PREPARACIÓN DE LAS SERIES-----

ts_datacomex_E_0 <- stats::ts(datacomex_E_raw$euros,
                              start = c(1995, 1),
                              frequency =12)

ts_datacomex_I_0 <- stats::ts(datacomex_I_raw$euros,
                              start = c(2010, 1),
                              frequency =12)
ts_air <- ts_datacomex_E_0

ts_df <- data.frame(Time = as.Date(zoo::as.yearmon(time(ts_air))), 
                    Value = as.numeric(ts_air) 
                    )
ts_df$Time <- format(ts_df$Time, "1.%m.%Y")

ts_data_name <- paste0("ts_data_", formatted_date, ".xlsx")
xls_output_path <- file.path("output", folder_name, ts_data_name)
openxlsx::write.xlsx(ts_df, file = xls_output_path, rowNames = FALSE)

y_raw <- ts_air
# plot(y_raw)

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

# my_regressors <- list( NoTuesday = regs_td[,1],
#                        Tuesday = regs_td[,2])

my_context <- rjd3toolkit::modelling_context(variables = my_regressors)
rjd3toolkit::.r2jd_modellingcontext(my_context)$getTsVariableDictionary()

my_context_name <- paste0("my_context_", formatted_date)
assign(my_context_name, my_context)

core_tramoseats_spec <- rjd3tramoseats::tramoseats_spec("rsafull")
str(core_tramoseats_spec)

core_tramoseats_spec_name <- paste0("core_tramoseats_spec_", formatted_date)
assign(core_tramoseats_spec_name, core_tramoseats_spec)

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

tramoseats_spec_final_name <- paste0("tramoseats_spec_final_", formatted_date)
assign(tramoseats_spec_final_name, tramoseats_spec_final)

# EJECUCION DEL ANALISIS CON ESPECIFICACIONES SELECCIONADAS-----
sa_tramoseats_ud <- rjd3tramoseats::tramoseats(y_raw, tramoseats_spec_final, context = my_context)

# TRAMO-SEATS SUMMARY
summary(sa_tramoseats_ud)
str(sa_tramoseats_ud)

sa_tramoseats_ud_name <- paste0("sa_tramoseats_ud_", formatted_date)
assign(sa_tramoseats_ud_name, sa_tramoseats_ud)


# OBTENER Y GUARDAR LAS ESPECIFICACIONES PARA LAS REVISIONES-----

result_spec_name <- paste0("ANALISIS_", formatted_date, "_result_spec") 
est_spec_name <- paste0("ANALISIS_", formatted_date, "_estimation_spec") 

assign(result_spec_name, sa_tramoseats_ud$result_spec) 
assign(est_spec_name, sa_tramoseats_ud$estimation_spec) 


# OBTENER SERIES FINALES------
str(sa_tramoseats_ud$result$final)

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
SI_ts <- seasonal_component_ts*irregular_ts


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

# SERIE ORIGINAL-----
y_max1 <- ceiling(max(abs(tsibble_o$value)))


plot1 <- ggplot(tsibble_o, aes(x = index, y = value)) +
  geom_line(color = "darkblue", linewidth = 0.5) +  
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
    sec.axis = sec_axis(~ . * 1, name = "Measured Quantity [units]", breaks = seq(0, y_max1, by = 2000000000)),  # Add a secondary y-axis
    breaks = seq(0, y_max1, by = 2000000000)  # Add ticks every 100 units on the y-axis
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

plot1

# SERIE AJUSTADA ESTACIONALMENTE-----
y_max2 <- ceiling(max(abs(tsibble_sa$value)))

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
    sec.axis = sec_axis(~ . * 1, name = "Measured Quantity [units]", breaks = seq(0, y_max2, by = 2000000000)),  # Add a secondary y-axis
    breaks = seq(0, y_max2, by = 2000000000)  # Add ticks every 100 units on the y-axis
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

plot2

# TENDENCIA Y SERIE AJUSTADA ESTACIONALMENTE-----
# COMBINAMOS LA SERIE DE TENDENCIA Y LA AJUSTADA ESTACIONALMENTE
tsibble_combined <- bind_rows(
  tsibble_sa  |>  as_tibble()  |>  mutate(Series = "Seasonally Adjusted"),
  tsibble_t  |>  as_tibble()  |>  mutate(Series = "Trend")
)  |>  as_tsibble(index = index, key = Series) 

y_max3 <- ceiling(max(abs(tsibble_combined$value)))

custom_colors <- c("Seasonally Adjusted" = "darkblue", "Trend" = "darkred")

plot3 <-ggplot(tsibble_combined, aes(x = index, y = value, color = Series)) +
  geom_line(size = 0.5) +  
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Trend and Seasonally Adjusted Series",
    x = "Time",
    y = "Measured Quantity [units]"
  ) +
  scale_x_yearmonth(
    labels = scales::date_format(format = "%Y"),  # Format date labels as years
    breaks = seq(min(tsibble_combined$Date), max(tsibble_combined$Date), by = "2 year")  # Add ticks every year
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 1, name = "Measured Quantity [units]", breaks = seq(0, y_max3, by = 2000000000)),  # Add a secondary y-axis
    breaks = seq(0, y_max3, by = 2000000000)  # Add ticks every 100 units on the y-axis
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,           # Center the title
      size = 13,             # Increase font size
      face = "bold",         # Make the title bold
      color = "black"     # Change title color
    ),
    axis.title.y.left = element_text(color = left_axis_color),  # Left axis color
    axis.text.y.left = element_text(color = left_axis_color),
    axis.title.y.right = element_text(color = right_axis_color),  # Right axis color
    axis.text.y.right = element_text(color = right_axis_color)
  )

plot3

# TENDENCIA-----
y_max4 <- ceiling(max(abs(tsibble_t$value)))

plot4 <-ggplot(tsibble_t, aes(x = index, y = value)) +
  geom_line(color = "orange", size = 0.5) +  
  labs(
    title = "Trend Time Series",
    x = "Time",
    y = "Measured Quantity [units]"
  ) +
  scale_x_yearmonth(
    labels = scales::date_format(format = "%Y"),  # Format date labels as years
    breaks = seq(min(tsibble_o$Date), max(tsibble_o$Date), by = "2 year")  # Add ticks every year
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 1, name = "Measured Quantity [units]", breaks = seq(0, y_max4, by = 2000000000)),  # Add a secondary y-axis
    breaks = seq(0, y_max4, by = 2000000000)  # Add ticks every 100 units on the y-axis
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,           # Center the title
      size = 13,             # Increase font size
      face = "bold",         # Make the title bold
      color = "black"     # Change title color
    ),
    axis.title.y.left = element_text(color = left_axis_color),  # Left axis color
    axis.text.y.left = element_text(color = left_axis_color),
    axis.title.y.right = element_text(color = right_axis_color),  # Right axis color
    axis.text.y.right = element_text(color = right_axis_color)
  )

plot4

# COMPONENTE ESTACIONAL-----
y_max5 <- ceiling(max(abs(tsibble_sc$value)))

plot5 <-ggplot(tsibble_sc, aes(x = index, y = value)) +
  geom_line(color = "black", size = 0.5) +  
  labs(
    title = "Seasonal Component Time Series",
    x = "Time",
    y = "Measured Quantity [units]"
  ) +
  scale_x_yearmonth(
    labels = scales::date_format(format = "%Y"),  # Format date labels as years
    breaks = seq(min(tsibble_o$Date), max(tsibble_o$Date), by = "2 year")  # Add ticks every year
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 1, name = "Measured Quantity [units]", breaks = seq(0, y_max5, by = 0.05)),  # Add a secondary y-axis
    breaks = seq(0, y_max5, by = 0.05)  # Add ticks every 100 units on the y-axis
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,           # Center the title
      size = 13,             # Increase font size
      face = "bold",         # Make the title bold
      color = "black"     # Change title color
    ),
    axis.title.y.left = element_text(color = left_axis_color),  # Left axis color
    axis.text.y.left = element_text(color = left_axis_color),
    axis.title.y.right = element_text(color = right_axis_color),  # Right axis color
    axis.text.y.right = element_text(color = right_axis_color)
  )

plot5

# COMPONENTE IRREGULAR-----
y_max6 <- ceiling(max(abs(tsibble_i$value)))

plot6 <-ggplot(tsibble_i, aes(x = index, y = value)) +
  geom_line(color = "red", size = 0.5) +  
  labs(
    title = "Irregular Component Time Series",
    x = "Time",
    y = "Measured Quantity [units]"
  ) +
  scale_x_yearmonth(
    labels = scales::date_format(format = "%Y"),  # Format date labels as years
    breaks = seq(min(tsibble_o$Date), max(tsibble_o$Date), by = "2 year")  # Add ticks every year
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 1, name = "Measured Quantity [units]", breaks = seq(0, y_max6, by = 0.1)),  # Add a secondary y-axis
    breaks = seq(0, y_max6, by = 0.1)  # Add ticks every 100 units on the y-axis
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,           # Center the title
      size = 13,             # Increase font size
      face = "bold",         # Make the title bold
      color = "black"     # Change title color
    ),
    axis.title.y.left = element_text(color = left_axis_color),  # Left axis color
    axis.text.y.left = element_text(color = left_axis_color),
    axis.title.y.right = element_text(color = right_axis_color),  # Right axis color
    axis.text.y.right = element_text(color = right_axis_color)
  )

plot6

# GRAFICO COMBINADO-----
combined_plot <- 
  (plot1 | plot2) /
  (plot3 | plot4) /
  (plot5 | plot6)

print(combined_plot)

# CUADRO S-I
# CREAMOS EL DF DE LA COMPONENTE ESTACIONAL-----
tsibble_sc

tsibble_sc <- tsibble_sc |> 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
         year = year(index))
 
df_SC <- as.data.frame(tsibble_sc)

df_SC <- df_SC |> 
  select(year, month, value)

df_SC$month <- df_SC$month  |> 
  as.character()  |> 
  toTitleCase()

# ESTA PARTE DE CODIGO NO ES NECESARIA-----
# monthly_series <- df |> 
#   group_by(month) |> 
#   group_split()
# 
# names(monthly_series) <- unique(tsibble_sc$month)
# 
# # Ensure each dataframe keeps only "year" and "value"
# monthly_series <- lapply(monthly_series, function(df) {
#   df |> select(year, value) 
# })
# 
# monthly_series <- imap(monthly_series, function(df, month_name) {
#   rename(df, !!month_name := value)
# })
# 
# wide_df <- reduce(monthly_series, full_join, by = "year")
# 
# wide_df <- dplyr::arrange(wide_df, year)
# 
# print(wide_df)
# 
# long_df <- wide_df |> 
#   pivot_longer(cols = starts_with("enero"):starts_with("diciembre"), 
#                names_to = "month", 
#                values_to = "value")

# -----

df_SC$month <- factor(df_SC$month, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                                        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))
monthly_avg_SC <- df_SC  |> 
  group_by(month)  |> 
  summarize(monthly_avg = mean(value, na.rm = TRUE))


# CREAMOS EL DF DE LA COMPONENTE ESTACIONAL-IRREGULAR -----
tsibble_SI

tsibble_SI <- tsibble_SI |> 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
         year = year(index))

df_SI <- as.data.frame(tsibble_SI)

df_SI <- df_SI |> 
  select(year, month, value)

df_SI$month <- df_SI$month  |> 
  as.character()  |> 
  toTitleCase()


df_SI$month <- factor(df_SI$month, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                                        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))

# CREAMOS EL GRAFICO S-I-----
df_SC <- df_SC |> 
  mutate(Series = "Componente Estacional")

df_SI <- df_SI |> 
  mutate(Series = "Estacional-Irregular")

# Combine the two dataframes
combined_df <- bind_rows(df_SC, df_SI)

# Ensure that 'dataset' column is a factor with consistent levels
combined_df$Series <- factor(combined_df$Series, levels = c("Componente Estacional", "Estacional-Irregular"))

ggplot(combined_df, aes(x = year, y = value)) +
  geom_line(aes(color = Series)) +  # Add line plot
  facet_wrap(~month, scales = "fixed", ncol = 12) +  # Facet by month, fixed y-axis, 1x12 format
  scale_x_continuous(breaks = seq(min(combined_df$year), max(combined_df$year), by = 5)) +  # Ticks every 5 years
  labs(title = "Gráfica S-I", 
       x = "Year", 
       y = "Value") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, color = "black", face = "bold"),  # Bold and black title text
    strip.background = element_rect(fill = "gray90", color = "gray50", linewidth = 1),  # Gray background for facet titles
    panel.border = element_rect(color = "gray", fill = NA, linewidth = 1),  # Gray border around each subgraph
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels for readability
    panel.spacing = unit(1, "lines"),  # Space between facets
    plot.title = element_text(hjust = 0.5)  # Center the main title
  ) +
  geom_hline(aes(yintercept = monthly_avg), data = monthly_avg_SC, color = "blue", linetype = "dashed") +
  # geom_point( data = combined_df$df_SI, aes(x = year, y = value), color = "red", shape = 8, size = 3) +  # Add red asterisks for new data
  # scale_color_manual(values = c("Componente Estacional" = "black"))
  scale_color_manual(values = c("Componente Estacional" = "black", "Estacional-Irregular" = "red"))


# ESTAS FUNCIONES YA ESTAN DISPONIBLES PERO NO SE PUEDEN USAR PARA SUPERPONER DOS SERIES COMO ES NECESARIO PARA EL GRAFICO S-I
feasts::gg_subseries(tsibble_sc) +
  ggplot2::labs(y = "Units",
                title = "Seasonal Component")

feasts::gg_subseries(tsibble_SI) +
  ggplot2::labs(y = "Units",
                title = "SI Chart")


# TASAS DE VARIACION INTERANUALES DE LOS ÚLTIMOS 12 MESES-----

last24_original_ts <- tail(original_ts, 24) 
last24_seasonally_adjusted_ts <- tail(seasonally_adjusted_ts, 24)

start_month <- "Enero" #El último mes de la serie
months_spanish <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
start_index <- which(months_spanish == start_month)

if (start_index == 12) {
  months_reverse <- months_spanish[12:1]
} else {
  months_reverse <- c(months_spanish[start_index:1], months_spanish[12:(start_index+1)])
}

# SERIE ORIGINAL
df_original_ts <- data.frame(
  Time = as.numeric(time(last24_original_ts)),  
  as.data.frame(last24_original_ts)             
)

df_original_ts$Time <- 1:length(last24_original_ts)

df_original_ts$x <- rev(df_original_ts$x) #ordenamos los valores de más reciente a más antiguo

TV_original_ts <- data.frame(
  Time = 1:12,
  Value = (df_original_ts$x[1:12] / df_original_ts$x[13:24] - 1) * 100
)

TV_original_ts$Mes <- months_reverse
TV_original_ts$Color <- ifelse(TV_original_ts$Value >= 0, "Positive", "Negative")
TV_original_ts
TV_original_ts <- TV_original_ts[nrow(TV_original_ts):1, ]
TV_original_ts$Mes <- factor(TV_original_ts$Mes, levels = TV_original_ts$Mes)
TV_original_ts

y_max <- ceiling(max(abs(TV_original_ts$Value)))
y_min <- -1*y_max

plot7 <- ggplot(TV_original_ts, aes(x = Mes, y = Value, fill = Color)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +  # Use stat = "identity" for bar graphs
  scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) +  # Define colors
  labs(
    title = "Tasas de Variación Interanuales para Serie Original (últimos 12 meses)",
    x = "Mes",
    y = "Tasa de Variación Inrteranual (%)"
  ) +
  scale_y_continuous(
    limits = c(y_min, y_max),  # Set primary y-axis limits
    sec.axis = sec_axis(~ ., name = "Tasa de Variación Inrteranual (%)", breaks = seq(y_min, y_max, by = 5))  # Add secondary y-axis
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,           # Center the title
      size = 8,             # Increase font size
      face = "bold",         # Make the title bold
      color = "black"     # Change title color
    ),
    axis.text.x = element_text(angle = 45, 
                               hjust = 1)
  )  # Rotate x-axis labels for better readability

plot7


# SERIE DESESTACIONALIZADA
df_seasonally_adjusted_ts <- data.frame(
  Time = as.numeric(time(last24_seasonally_adjusted_ts)),  
  as.data.frame(last24_seasonally_adjusted_ts)             
)

df_seasonally_adjusted_ts$Time <- 1:length(last24_seasonally_adjusted_ts)
df_seasonally_adjusted_ts$x <- rev(df_seasonally_adjusted_ts$x)

TV_seasonally_adjusted_ts <- data.frame(
  Time = 1:12,
  Value = (df_seasonally_adjusted_ts$x[1:12] / df_seasonally_adjusted_ts$x[13:24] - 1) * 100
)

TV_seasonally_adjusted_ts$Mes <- months_reverse
TV_seasonally_adjusted_ts$Color <- ifelse(TV_seasonally_adjusted_ts$Value >= 0, "Positive", "Negative")
TV_seasonally_adjusted_ts
TV_seasonally_adjusted_ts <- TV_seasonally_adjusted_ts[nrow(TV_seasonally_adjusted_ts):1, ]
TV_seasonally_adjusted_ts$Mes <- factor(TV_seasonally_adjusted_ts$Mes, levels = TV_seasonally_adjusted_ts$Mes)
TV_seasonally_adjusted_ts


y_max <- ceiling(max(abs(TV_seasonally_adjusted_ts$Value)))
y_min <- -1*y_max

plot8 <- ggplot(TV_seasonally_adjusted_ts, aes(x = Mes, y = Value, fill = Color)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +  # Use stat = "identity" for bar graphs
  scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) +  # Define colors
  labs(
    title = "Tasas de Variación Interanuales para Serie Desestacionalizada (últimos 12 meses)",
    x = "Mes",
    y = "Tasa de Variación Inrteranual (%)"
  ) +
  scale_y_continuous(
    limits = c(y_min, y_max),  # Set primary y-axis limits
    sec.axis = sec_axis(~ ., name = "Tasa de Variación Inrteranual (%)", breaks = seq(y_min, y_max, by = 5))  # Add secondary y-axis
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,           # Center the title
      size = 8,             # Increase font size
      face = "bold",         # Make the title bold
      color = "black"     # Change title color
    ),
    axis.text.x = element_text(angle = 45, 
                               hjust = 1)
    )  # Rotate x-axis labels for better readability

plot8

combined_plot <- (plot7/plot8) 

print(combined_plot)

TV_original_ts_name <- paste0("TV_original_ts_", formatted_date) 
TV_seasonally_adjusted_ts_name <- paste0("TV_seasonally_adjusted_ts_", formatted_date)

assign(TV_original_ts_name, TV_original_ts) 
assign(TV_seasonally_adjusted_ts_name, TV_seasonally_adjusted_ts) 

# GUARDADO DE RESULTADOS-----
data_file_name <- paste0("DATOS_ANALISIS_95-25_", formatted_date, ".RData")
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
              irregular_ts_name,
              TV_original_ts_name,
              TV_seasonally_adjusted_ts_name
              ),
     file = data_full_path)


# save(spanish_calendar,
#      regs_td,
#      my_regressors,
#      my_context,
#      core_tramoseats_spec,
#      tramoseats_spec_final,
#      sa_tramoseats_ud_revised,
#      list = c(rev_est_spec_name, rev_result_spec_name),
#      original_ts,
#      seasonally_adjusted_ts,
#      trend_ts,
#      seasonal_component_ts,
#      irregular_ts,
#      TV_original_ts,
#      TV_seasonally_adjusted_ts,
#      file = data_full_path)



load(file = data_full_path)

