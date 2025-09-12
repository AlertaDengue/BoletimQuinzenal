r_pkgs <- c("tidyverse", "AlertTools", "sf", "arrow", "geobr",
            "ggplot2", "ggpubr", "geofacet", "cowplot", "lubridate",
            "janitor", "kableExtra", "xtable", "data.table",
            "gt", "gtExtras", "patchwork", "googledrive", "tictoc",
            "scales", "cowplot", "GHRexplore")


if(!"pacman" %in% rownames(installed.packages())) install.packages("pacman")
pacman::p_load(char = r_pkgs)

# removing NA from Kable
options(knitr.kable.NA = '')

# removing annoying scientific notation
options(scipen = 999)


load(file = "data/muns.R") 
muns <- muns %>% 
  mutate(regional_id = as.character(regional_id))

file_list <- list.files(path = "../load_infodengue_data/data/cases/", pattern = "dengue.parquet", full.names = TRUE)
df_dengue <- lapply(file_list, read_parquet)
df_dengue <- bind_rows(df_dengue) %>% 
  left_join(muns %>% 
              dplyr::select(-nome), by = "municipio_geocodigo") %>% 
  mutate(
    incest = casos_est/pop * 100000,
    arbovirose = "Dengue",
    date = ymd(substr(data_iniSE, 1, 10)),
    state =  substr(macroregional_id, 1, 2)
  ) #%>% 
#filter(substr(SE, 1, 4) < 2024)
se_max_dengue  <- max(df_dengue$SE) # Semana epidemiologia mais recente
ultima_semana_inicio <- ymd(substr(max(df_dengue$data_iniSE, na.rm = T), 1, 10))
ultima_semana_fim <- ultima_semana_inicio + 6

# Filtros
macroregionais <- unique(df_dengue$macroregional)
estados <- unique(df_dengue$state)

# Grafico
selected_state <- estados[20]
plot_heatmap(
  data = df_dengue %>% 
               filter(state == selected_state),
             var = "p_inc100k",
             type = "inc",
             pop = "pop",
             time = "date",          
             area = "municipio_nome",   
             aggregate_space = "regional",
             transform = "log10p1",
             title = paste0("Dengue incidence in Brazil (",selected_state ,")")
)

plot_timeseries(
  data = df_dengue %>% 
    filter(state == selected_state),
  var = "p_inc100k",
  type = "inc",
  pop = "pop",
  time = "date",          
  area = "municipio_nome",   
  aggregate_space = "regional",
  transform = "log10p1",
  title = paste0("Dengue incidence in Brazil (",selected_state ,")"),
  facet = "regional"
)


# plot_timeseries: Plots time series of covariates, case counts or incidence rates.
# plot_timeseries2: Plots time series of two covariates, case counts or incidence rates using a dual-axis plot.
# plot_heatmap: Plots a time series of covariates, case counts or incidence rates as heatmaps.
# plot_seasonality: Plots yearly time series to detect seasonal patterns of covariates, case counts or incidence rates.
# plot_correlation: Plots a correlation matrix of a series of variables.
# plot_map: Plots a choropleth map of covariates, case counts or incidence rates.
# plot_bivariate: Plots a bivariate plot of two numerical and/or categorical variables.
# plot_multiple, plot_combine and plot_compare: Used to generate graphs of several variables at the same time.
