r_pkgs <- c("tidyverse", "AlertTools", "sf", "arrow",
            "ggplot2", "ggpubr", "geofacet", "cowplot",
            "janitor", "kableExtra", "xtable", "data.table",
            "gt", "gtExtras")

if(!"pacman" %in% rownames(installed.packages())) install.packages("pacman")
pacman::p_load(char = r_pkgs)

source("scripts/funcoes_extras.R", encoding = "utf-8")

# removing NA from Kable
options(knitr.kable.NA = '')

# removing annoying scientific notation
options(scipen = 999)

start_time <- Sys.time()
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
    arbovirose = "Dengue"
  ) #%>% 
  #filter(substr(SE, 1, 4) < 2024)
se_max_dengue  <- max(df_dengue$SE) # Semana epidemiologia mais recente

# file_list <- list.files(path = "../data/cases2024/", pattern = "dengue.parquet", full.names = TRUE)
# df_dengue2 <- lapply(file_list, read_parquet)
# df_dengue2 <- bind_rows(df_dengue2) %>% 
#   left_join(muns %>% 
#               dplyr::select(-nome), by = "municipio_geocodigo") %>% 
#   mutate(
#     incest = casos_est/pop * 100000,
#     arbovirose = "Dengue"
#   ) %>% 
#   filter(substr(SE, 1, 4) >= 2024)

file_list <- list.files(path = "../load_infodengue_data/data/cases/", pattern = "chik.parquet", full.names = TRUE)
df_chik <- lapply(file_list, read_parquet)
df_chik <- bind_rows(df_chik) %>% 
  left_join(muns %>% 
              dplyr::select(-nome), by = "municipio_geocodigo")%>% 
  mutate(
    incest = casos_est/pop * 100000,
    arbovirose = "Chikungunya"
  )
se_max_chik  <- max(df_chik$SE) # Semana epidemiologia mais recente

# file_list <- list.files(path = "../data/cases2024/", pattern = "chik.parquet", full.names = TRUE)
# df_chik2 <- lapply(file_list, read_parquet)
# df_chik2 <- bind_rows(df_chik2) %>% 
#   left_join(muns %>% 
#               dplyr::select(-nome), by = "municipio_geocodigo") %>% 
#   mutate(
#     incest = casos_est/pop * 100000,
#     arbovirose = "Chikungunya"
#   ) %>% 
#   filter(substr(SE, 1, 4) >= 2024)

# df_dengue <- df_dengue %>% 
#   bind_rows(df_dengue2)
# 
# df_chik <- df_chik %>% 
#   bind_rows(df_chik2)

df_dengue_chik <- df_dengue %>% 
  bind_rows(df_chik) %>% 
  filter(SE != 202401)
rm(df_dengue, df_chik)

# df_macro_dengue_chik <- df_dengue_chik %>% 
#   group_by(arbovirose, macroregional, macroregional_id, SE) %>% 
#   reframe(
#     casos_est = sum(casos_est, na.rm = T),
#     casos = sum(casos_est, na.rm = T)
#   )

# save(df_macro_dengue_chik, file = "df_macro_dengue_chik.rds")

# df_dengue_chik_agregado <- df_dengue_chik%>% 
#   mutate(ano = substr(SE, 1, 4)) %>% 
#   group_by(ano, municipio_geocodigo) %>% 
#   reframe(
#     casos = sum(casos, na.rm = T),
#     casos_est = sum(casos_est, na.rm = T)
#   )
# save(df_dengue_chik_agregado, file = "df_dengue_chik_agregado.rds")

shape <- st_read("shape/rs_450_RepCor1.shp") %>% 
  rename(regional_id = primary.id) %>% 
  mutate(regional_id = as.character((regional_id))) %>% 
  dplyr::select(-c(id, secondary)) %>% 
  left_join(
    muns %>% 
      dplyr::select(regional_id, regional) %>% 
      distinct(regional_id, .keep_all = T),
    by = "regional_id"
  )

# Parametros
ano_selecionado <- year(Sys.Date())
ultima_semana_dengue <- substr(se_max_dengue, 5, 6)
ultima_semana_chik <- substr(se_max_chik, 5, 6)

ultima_semana_inicio <- SE2date(se_max_dengue)$ini
ultima_semana_fim <- ultima_semana_inicio + 6
ultima_semana_inicio <- paste0(substr(ultima_semana_inicio, 9, 10),"/", substr(ultima_semana_inicio, 6, 7), "/", substr(ultima_semana_inicio, 1, 4))

ultima_semana_fim <- paste0(substr(ultima_semana_fim, 9, 10),"/", substr(ultima_semana_fim, 6, 7), "/", substr(ultima_semana_fim, 1, 4))

# Ensemble
# df_essemble <- read.table(gzfile("data/ensemble_bayes_2025.csv.gz"), sep = ",", header = T) 

tab_rt_uf <- fread("data/tabela-resumo202442.csv") %>%
  rename(
    codigo = geocodigo,
    arbovirose = agravo
    ) %>% 
  mutate(
    codigo = as.character(codigo),
    arbovirose = ifelse(arbovirose == "chik", "Chikungunya", "Dengue") 
  ) %>% 
  obter_siglas_codigos() %>% 
  arrange(codigo) %>% 
  group_by(codigo) %>% 
  distinct(arbovirose, .keep_all = T) %>% 
  ungroup() %>% 
  dplyr::select(sigla, arbovirose, Rtmean) 

load("data/mem2024.RData")
load("data/memBR2024.RData")
load("data/mem2024temp.RData")
load("data/mem2024chitemp.RData")

# MEM
memBR_dengue_chik <- curveMEM %>% 
  set_names(c("preseason", "epidemic", "posseason", "arbovirose")) %>% 
  mutate(
    arbovirose = ifelse(arbovirose == "A92.0", "Chikungunya", "Dengue"),
    arbovirose = factor(arbovirose, levels = c("Dengue", "Chikungunya"))
  ) %>% 
  group_by(arbovirose) %>% 
  mutate(
    SEe = 1:52
  ) %>% 
  ungroup() %>% 
  mutate(
    SEe = SEe+ 40,
    SEe = ifelse(SEe > 52, SEe - 52, SEe)
  )

# Dengue
memUFanual <- memUFanual %>% 
  rename(codigo = nome) %>% 
  mutate(codigo = as.character(codigo)) %>% 
  obter_siglas_codigos(merge_by = "codigo")

memReganual <- memReganual %>%
  rename(
    regional_id = nome,
    codigo = uf) %>%
  mutate(
    regional_id = as.character(regional_id),
    codigo = as.character(codigo)
  ) %>%
  obter_siglas_codigos(merge_by = "codigo")

memUFsazonal <- memUFsazonal %>% 
  mutate(
    SEe = as.numeric(SE) + 40,
    SEe = ifelse(SEe > 52, SEe - 52, SEe)
  ) %>% 
  rename(codigo = uf) %>%
  mutate(codigo = as.character(codigo)) %>%
  obter_siglas_codigos(merge_by = "codigo")

memMacroanual <- memMacroanual %>% 
  rename(macroregional_id = nome) %>%
  mutate(macroregional_id = as.character(macroregional_id)) 

## Chikv
memUFanual.chi <- memUFanual.chi %>% 
  rename(codigo = nome) %>% 
  mutate(codigo = as.character(codigo)) %>% 
  obter_siglas_codigos(merge_by = "codigo")

memReganual.chi <- memReganual.chi %>% 
  rename(
    regional_id = nome,
    codigo = uf) %>% 
  mutate(
    regional_id = as.character(regional_id),
    codigo = as.character(codigo)
  ) %>% 
  obter_siglas_codigos(merge_by = "codigo")

memUFsazonal.chi <- memUFsazonal.chi %>%
  mutate(
    SEe = as.numeric(SE) + 40,
    SEe = ifelse(SEe > 52, SEe - 52, SEe)
  ) %>% 
  rename(codigo = uf) %>%
  mutate(codigo = as.character(codigo)) %>%
  obter_siglas_codigos(merge_by = "codigo")

memMacroanual.chi <- memMacroanual.chi %>% 
  rename(macroregional_id = nome) %>%
  mutate(macroregional_id = as.character(macroregional_id)) 

shape_state <- geobr::read_state(year = 2020) %>% 
  dplyr::select(code_state)

# RUN
output_directory <- paste0("report/outputs/")
output_filename <- paste0("Informe_Infodengue_SE", se_max_dengue,".pdf")

gc(reset = T)
dir.create(output_directory, recursive = T)
rmarkdown::render(input = "report/InfoDengue Informe.Rmd",
                  output_file = paste0(output_directory, output_filename),
                  clean = T)

end_time <- Sys.time()
end_time - start_time

