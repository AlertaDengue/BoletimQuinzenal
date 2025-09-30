tic("Boletim quinzenal")

# download_from_google <- FALSE
if(download_from_google == T){
  # Baixar arquivos do google drive
  drive_auth(email = "estatistico.bianchi@gmail.com")
  
  # ## for_ensemble.csv
  for_ensemble_id <- as_id("1hdUPpGn7qiKLDoaW4DnzYzJnZqdbSjLF")
  drive_download(for_ensemble_id, path = "data/for_ensemble.csv", overwrite = T)
  
  # ## graficos do modelo ensemble
  figures_ensemble_id <- as_id("1aQW95MhLvFJffd0OAaJwa_8PE345uUFN")
  figures_ensemble_id <- googledrive::drive_ls(figures_ensemble_id)$id
  walk(figures_ensemble_id, baixar_arquivo, path = "report/figures")
  print("for_ensemble.csv e graficos baixados do Google Drive.")
}

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
ultima_semana_inicio <- ymd(substr(max(df_dengue$data_iniSE, na.rm = T), 1, 10))
ultima_semana_fim <- ultima_semana_inicio + 6

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
ultima_semana_dengue <- as.numeric(substr(se_max_dengue, 5, 6))
ultima_semana_chik <- substr(se_max_chik, 5, 6)


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
    SEe = SEe + 40,
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

df_rt_dengue_br <- obter_rt_nacional(df_dengue_chik, 
                                     arbo = "Dengue")
df_rt_chik_br <- obter_rt_nacional(df_dengue_chik, 
                                   arbo = "Chikungunya")

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

#### Script da prof Claudia
states <- read_state(
  year = 2020, 
  showProgress = FALSE
)


## Mapa de incidencia total na temporada, de acordo com os ensembles ----
pred <- read.csv("data/predicoes_acumuladas.csv") # incidencia total no ano

shape_anual <- states %>%
  left_join(pred, join_by(abbrev_state == UF)) %>%
  group_by(abbrev_state) %>%
  summarise(geom = st_union(geom),
            code_state = unique(code_state),
            abbrev_state = unique(abbrev_state),
            ens_inc23 = as.numeric(unique(cenario23)),
            ens_inc24 = as.numeric(unique(cenario24))) %>%
  arrange(code_state)

common_yrange <- range(c(shape_anual$ens_inc24, shape_anual$ens_inc23))

m1 <- ggplot() +
  geom_sf(data = shape_anual, aes(fill = ens_inc23)) +
  scale_fill_viridis_c(option = "viridis", oob = scales::squish,
                       trans = "log",
                       limits = common_yrange,
                       breaks = c(100, 300, 1000, 10000),  # Define specific breakpoints
                       labels = c("100", "300", "1K", "10k"),
                       name = "Incidência acumulada") + 
  labs(title = "(A) Modelo ensemble 23") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, hjust = 0.5),
    #legend.position = "top",
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.text.x = element_text(size = 18, colour = "black"), 
    strip.background = element_rect(fill = "white")
  )   +
  theme(legend.position = "none")

m2 <- ggplot() +
  geom_sf(data = shape_anual, aes(fill = ens_inc24)) +
  scale_fill_viridis_c(option = "viridis", oob = scales::squish,
                       trans = "log",
                       limits = common_yrange,
                       breaks = c(100, 300, 1000, 10000),  # Define specific breakpoints
                       labels = c("100", "300", "1K", "10k"),
                       name = "Incidência\nacumulada") + 
  labs(title = "(B) Modelo ensemble 24") +
  guides(colour = guide_colorbar(barwidth = 50)) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.text.x = element_text(size = 18, colour = "black"), 
    strip.background = element_rect(fill = "white")
  )  

m3 <- ggplot(data = shape_anual, aes(x = factor(code_state))) +
  geom_point(aes(y = ens_inc23), color = "black", shape = "_", size = 3) +
  geom_point(aes(y = ens_inc24), color = "black", shape = "_", size = 3) +
  geom_segment(aes(x = factor(code_state), xend = factor(code_state), 
                   y = ens_inc23, yend = ens_inc24), color = "black") +
  scale_y_log10() +  
  scale_x_discrete(labels = shape_anual$abbrev_state) +
  labs(x = "", y = "Incidência acumulada (x10^5)", 
       title = "(C) barra: faixa de incidência acumulada prevista pelos ensembles ") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y  = element_line(linetype = "dotted",color = "grey", linewidth = 0.5),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, hjust = 0.5),
    legend.position = "top",
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.text.x = element_text(size = 18, colour = "black"), 
    strip.background = element_rect(fill = "white")
  )  +
  geom_hline(yintercept = 300, color = "red", linetype = "dashed")

combined_plots123 <- (m1 | m2) /
  m3 + plot_layout(heights = c(1.5, 1.5)) +
  plot_annotation(title = "Previsões para a temporada 2024-25 (médias dos modelos)")

# dados e modelos para a temporada ate agora ----
pred_se <- read.csv("data/ensemble_bayes_2025.csv") 

data_min_ensemble <- min(pred_se$date, na.rm = T)
data_atual <- ymd(substr(max(df_dengue_chik$data_iniSE, na.rm = T), 1, 10))

pred.casos.ac <- pred_se %>%
  filter(date <= data_atual) %>%
  group_by(state) %>%
  summarize(
    casos_e23 = sum(pred_ensemble_23),
    casos_e24 = sum(pred_ensemble_24)
  )

# agregando casos municipais, por UF, na temporada, ate agora
dd.uf <- df_dengue_chik %>% 
  filter(arbovirose == "Dengue") %>%
  filter(data_iniSE >= data_min_ensemble) %>%
  mutate(code_state = floor(municipio_geocodigo/10e4)) %>%
  group_by(code_state) %>%
  summarise(
    casos = sum(casos),
    casos_est = sum(casos_est),
    casprov = sum(casprov),   # utilizar essa
    pop = sum(pop),
    .groups = "drop" 
  ) %>%
  mutate(
    prop_susp = casprov/casos,
    inc_prov = casprov/pop * 1e5
  ) %>%
  arrange(code_state)

shapeNow <- states %>%
  left_join(pred.casos.ac, join_by(abbrev_state == state)) %>%
  left_join(dd.uf, join_by(code_state)) %>%
  mutate(casos_est_prov = round(casos_est * prop_susp)) %>%
  arrange(code_state)

shapeNow$color <- ifelse(shapeNow$casprov < shapeNow$casos_e23 &
                           shapeNow$casprov < shapeNow$casos_e24,
                         "blue", "red")

# mapa de casos
m4 <- ggplot() +
  geom_sf(data = shapeNow, aes(fill = casprov)) +
  scale_fill_viridis_c(option = "viridis",
                       oob = scales::squish, 
                       trans = "log",
                       breaks = c(1, 10, 100, 1000, 10000),  # Define specific breakpoints
                       labels = c("1", "10", "100", "1k", "10k"),
                       name = "Casos prováveis") + 
  labs(title = "(A) Número estimado de casos prováveis") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.position = "top",
    legend.key.width = unit(1, "cm"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.text.x = element_text(size = 18, colour = "black"), 
    strip.background = element_rect(fill = "white")
  )   

# mapa de incidencia
m5 <- ggplot() +
  geom_sf(data = shapeNow, aes(fill = inc_prov)) +
  scale_fill_viridis_c(option = "viridis",
                       oob = scales::squish, 
                       trans = "log",
                       breaks = c(1, 5, 10, 15, 20),  # Define specific breakpoints
                       labels = c("1", "5", "10", "15", "20"),
                       name = "Incidência (x10^5)") + 
  labs(title = "(B) Incidência acumulada") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.position = "top",
    legend.key.width = unit(1, "cm"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.text.x = element_text(size = 18, colour = "black"), 
    strip.background = element_rect(fill = "white")
  )  

# grafico de pontos
m6 <- ggplot(data = shapeNow, aes(x = factor(code_state))) +
  geom_point(aes(y = casos_e23), color = "black", shape = "_", size = 3) +
  geom_point(aes(y = casos_e24), color = "black", shape = "_", size = 3) +
  geom_segment(aes(x = factor(code_state), xend = factor(code_state), 
                   y = casos_e23, yend = casos_e24), color = "black") +
  geom_point(aes(y = casprov), color = shapeNow$color, shape = 16, size = 2) +
  #geom_point(aes(y = casos_est), color = "blue", shape = 16, size = 2) +
  scale_y_log10() +  
  scale_x_discrete(labels = shapeNow$abbrev_state) +
  labs(x = "", y = "Casos Acumulados", 
       title = "(C) Casos prováveis estimados de dengue") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y  = element_line(linetype = "dotted",color = "grey", linewidth = 0.5),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, hjust = 0.5),
    legend.position = "top",
    legend.key.width = unit(2, "cm"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.text.x = element_text(size = 18, colour = "black"), 
    strip.background = element_rect(fill = "white")
  ) 

combined_plots456 <- (m4 | m5) /
  m6 + plot_layout(heights = c(1.5, 1.5)) +
  plot_annotation(title = "Situação da dengue usando nowcast.")

## situacao prevista para as prox 3 semanas  

# previsoes geradas pelo modelo de short term forecast (Eduardo gera)
short.pred <- read.csv("data/for_ensemble.csv") %>%
  mutate(se = epiweek(date)) 

w3 <- unique(short.pred$se)

short.pred <- short.pred %>%  # agregando por UF
  group_by(state) %>%
  summarize(pred3w = sum(pred))


# previsao segundo modelos ensemble para esse periodo
ens3w <- pred_se %>%
  mutate(se = epiweek(date)) %>%
  filter(se %in% w3) %>%  
  group_by(state) %>%
  summarize(ens23w = sum(pred_ensemble_23),
            ens24w = sum(pred_ensemble_24)) %>%
  filter(state != "BR")

# pegando dado de pop para calcular incidencia
df_pop <- dd.uf[, c("code_state","pop")]

shape_short <- states %>%
  left_join(short.pred, join_by(abbrev_state == state)) %>%
  left_join(ens3w, join_by(abbrev_state == state)) %>%
  left_join(df_pop) %>%
  mutate(inc3w = pred3w/pop*1e5) %>%
  arrange(code_state)

shape_short$color <- ifelse(shape_short$pred3w < shape_short$ens23w &
                              shape_short$pred3w < shape_short$ens24w,
                            "blue", "red")

m7 <- ggplot() +
  geom_sf(data = shape_short, aes(fill = inc3w)) +
  scale_fill_viridis_c(option = "viridis",
                       oob = scales::squish, 
                       trans = "log",
                       breaks = c(1, 2, 3, 4, 5),  # Define specific breakpoints
                       labels = c("1", "2", "3", "4", "5"),
                       name = "Incidência (x10^5)") + 
  labs(title = "(A) Incidência prevista para o período") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.position = "top",
    legend.key.width = unit(1, "cm"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.text.x = element_text(size = 18, colour = "black"), 
    strip.background = element_rect(fill = "white")
  ) +
  theme(plot.margin = margin(5, 5, 5, 2)) # Top, right, bottom, left margins

m8 <- ggplot(data = shape_short, aes(x = factor(code_state))) +
  geom_point(aes(y = ens24w), color = "black", shape = "_", size = 3) +
  geom_point(aes(y = ens23w), color = "black", shape = "_", size = 3) +
  geom_segment(aes(x = factor(code_state), xend = factor(code_state), 
                   y = ens23w, yend = ens24w), color = "black") +
  geom_point(aes(y = pred3w), color = shape_short$color, shape = 16, size = 2) +
  scale_y_log10() +  
  labs(x = "", y = "Casos prováveis", 
       title = "(B) ponto = casos previstos; barra: casos esperados pelos ensembles" ) +
  scale_x_discrete(labels = shape_short$abbrev_state) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y  = element_line(linetype = "dotted",color = "grey", linewidth = 0.5),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.position = "top",
    legend.key.width = unit(1, "cm"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    strip.text.x = element_text(size = 18, colour = "black"), 
    strip.background = element_rect(fill = "white")
  ) +
  theme(plot.margin = margin(10, 10, 10, 10)) # Top, right, bottom, left margins

combined_plot78 <- m7 + m8 + 
  plot_layout(nrow = 2, heights = c(1.1,0.9)) + # Arrange plots in a single row, adjust ncol if needed
  plot_annotation(title = "Situação prevista para as proximas 3 semanas")

#############
#############

df_dengue_nacional <- df_dengue_chik %>%
  filter(arbovirose == "Dengue") %>% 
  obter_metricas_nacionais() #Nacional

df_dengue_uf <- df_dengue_chik %>% # Por UF e SE
  filter(arbovirose == "Dengue") %>% 
  obter_metricas_estaduais() %>%
  mutate(
    ano = as.numeric(substr(SE, 1, 4)),
    semana = as.numeric(substr(SE, 5, 6))
  ) 

df_dengue_total <- df_dengue_chik %>% 
  filter(arbovirose == "Dengue") %>% 
  obter_metricas_estaduais_por_ano() # Estadual (por ano)

# Macroregional (por semana)
df_dengue_macro_se <- df_dengue_chik %>% 
  filter(arbovirose == "Dengue") %>% 
  obter_metricas_macrorregionais_por_semana() %>% 
  data.frame()

df_dengue_reg <- df_dengue_chik %>% 
  filter(arbovirose == "Dengue") %>% 
  obter_metricas_regionais_por_semana() %>% 
  obter_siglas_codigos(merge_by = "codigo") # Regional

tabela_dengue <- obter_tabela(
  df = df_dengue_chik %>% filter(arbovirose == "Dengue"),
  df_mem = memUFanual, 
  se_max_dengue
) # Tabela Estadual

df_dengue_regional_se <- df_dengue_reg %>% 
  filter(SE == se_max_dengue) %>%
  select(regional_id, codigo, pop, incest) %>% 
  mutate(regional_id = as.character(regional_id))

tabela_reg_dengue <- obter_metricas_por_regional(
  df_mem_regional_anual = memReganual,
  df_regional_se = df_dengue_regional_se
)

tabela_reg_dengue <- obter_classificacao_das_regionais(df_dengue_reg, tabela_reg_dengue)
tab_pop_risco_dengue <- tabela_reg_dengue %>% obter_pop_sob_risco()

shape_dengue <- shape %>%
  left_join(tabela_reg_dengue, by = "regional_id")

gg_pop_risco_dengue <- tab_pop_risco_dengue %>% 
  gg_bar_pop_risco(por_regiao = F)

gg_map_risco_classif_dengue <- obter_mapa_risco_classif(
  shape_mun = shape_dengue %>% 
    mutate(
      status = case_when(
        # is.na(status) ~ "Não se aplica",
        TRUE ~ status
      ),
      status = factor(status, levels = c("Alto em subida", "Alto estável", "Alto em queda", "Baixo ou moderado")) #, "Não se aplica"
    ),
  shape_state = shape_state
)

# Informacoes textuais - dengue

# Ano selecionado
df_ano_selecionado_dengue <- df_dengue_chik %>% filter(arbovirose == "Dengue") %>% 
  filter(as.numeric(substr(SE, 1, 4)) == ano_selecionado) 

pop_brasil_ano_selecionado_dengue <- sum(df_ano_selecionado_dengue %>% 
                                           arrange(desc(SE)) %>% 
                                           dplyr::select(municipio_geocodigo, pop) %>% 
                                           distinct(municipio_geocodigo, .keep_all = T) %>% 
                                           pull(pop), na.rm = T)

total_casos_ano_selecionado_dengue <- sum(df_ano_selecionado_dengue %>% 
                                            pull(casos), na.rm = T)

total_casos_provaveis_ano_selecionado_dengue <- sum(df_ano_selecionado_dengue %>% 
                                                      pull(casprov), na.rm = T)

prop_casos_provaveis_dengue <- paste0(round(total_casos_provaveis_ano_selecionado_dengue/total_casos_ano_selecionado_dengue *100, 2),"%")

incidencia_acumulada_ano_selecionado_dengue <- total_casos_ano_selecionado_dengue/ pop_brasil_ano_selecionado_dengue * 10^5

# Ano anterior
df_ano_anterior_dengue <- df_dengue_chik %>% filter(arbovirose == "Dengue") %>% 
  filter(as.numeric(substr(SE, 1, 4)) == (ano_selecionado - 1))

pop_brasil_ano_anterior_dengue <- sum(df_ano_anterior_dengue %>% 
                                        arrange(desc(SE)) %>% 
                                        dplyr::select(municipio_geocodigo, pop) %>% 
                                        distinct(municipio_geocodigo, .keep_all = T) %>% 
                                        pull(pop), na.rm = T)

df_ano_anterior_dengue <- df_ano_anterior_dengue %>% 
  filter(SE <= (se_max_dengue - 100))

total_casos_ano_anterior_dengue <- sum(df_ano_anterior_dengue %>% 
                                         pull(casos), na.rm = T)
incidencia_acumulada_ano_anterior_dengue <- total_casos_ano_anterior_dengue/ pop_brasil_ano_anterior_dengue * 10^5

dif_prop_ano_dengue = total_casos_ano_selecionado_dengue/total_casos_ano_anterior_dengue

df_dengue_chik_estados <- df_dengue_chik %>% 
  dplyr::select(arbovirose, SE, municipio_geocodigo, casos, pop) %>%
  mutate(
    arbovirose = factor(arbovirose, levels = c("Dengue", "Chikungunya")),
    ano = substr(SE, 1, 4),
    municipio_geocodigo = substr(municipio_geocodigo, 1, 2),
    municipio_geocodigo = as.character(municipio_geocodigo)
  ) %>% 
  rename(codigo = municipio_geocodigo) 

df_dengue_chik_estados_casos <- df_dengue_chik_estados %>% 
  group_by(arbovirose, codigo, ano) %>% 
  reframe(
    casos = sum(casos, na.rm = T)
  ) %>% 
  mutate(
    id = paste(codigo, ano, sep = "_")
  ) %>% 
  mutate(
    ano = as.numeric(ano),
    grupo = ifelse(ano ==  max(ano), max(ano), paste0(min(ano),"-",max(ano)-1))
  ) 

df_dengue_chik_estados_casos_atual <- df_dengue_chik_estados %>% 
  filter(SE == max(se_max_dengue, se_max_chik)) %>% 
  group_by(arbovirose, codigo, ano) %>% 
  reframe(
    casos = sum(casos, na.rm = T)
  )  %>% 
  mutate(
    id = paste(codigo, ano, sep = "_"),
    ano = as.numeric(ano),
    grupo = "Semana\natual"
  ) 

df_dengue_chik_estados <- df_dengue_chik_estados_casos %>% 
  bind_rows(df_dengue_chik_estados_casos_atual) %>% 
  left_join(df_pop %>% 
              rename("codigo" = "code_state") %>% 
              mutate(codigo = as.character(codigo)),
            by = "codigo") %>% 
  mutate(
    inc = casos/pop * 100000
  ) %>% 
  mutate(
    grupo = factor(grupo, 
                   levels = c(
                     paste0(min(ano),"-",max(ano)-1),
                     max(ano), 
                     "Semana\natual")
    )
  ) %>% 
  obter_siglas_codigos() 

df_dengue_chik_estados$estado <-  stringi::stri_unescape_unicode(df_dengue_chik_estados$estado)

semana_epidemica_dengue_chik_estados <- df_dengue_chik_estados %>% 
  group_by(arbovirose, sigla) %>% 
  filter(ano == max(ano, na.rm = T)) %>% 
  filter(inc == max(inc, na.rm = T)) %>% 
  dplyr::select(sigla, grupo) %>% 
  ungroup()

dengue_semana_epidemica_estados <- semana_epidemica_dengue_chik_estados %>% 
  filter(arbovirose == "Dengue") %>% 
  filter(grupo == "Semana\natual") %>% 
  pull(sigla)

frase_dengue_1 <- texto_1(dengue_semana_epidemica_estados, arbovirose = "dengue")

############
############

df_chik_nacional <- df_dengue_chik %>%
  filter(arbovirose == "Chikungunya") %>% 
  obter_metricas_nacionais() #Nacional

df_chik_uf <- df_dengue_chik %>% 
  filter(arbovirose == "Chikungunya") %>% 
  obter_metricas_estaduais() %>% 
  mutate(
    ano = as.numeric(substr(SE, 1, 4)),
    semana = as.numeric(substr(SE, 5, 6))
  )  # Estadual

df_chik_total <- df_dengue_chik %>% 
  filter(arbovirose == "Chikungunya") %>% 
  obter_metricas_estaduais_por_ano() # Estadual (por ano)

# Macroregional (por semana)
df_chik_macro_se <- df_dengue_chik %>% 
  filter(arbovirose == "Chikungunya") %>% 
  obter_metricas_macrorregionais_por_semana() %>% 
  data.frame()

df_chik_reg <- df_dengue_chik %>% 
  filter(arbovirose == "Chikungunya") %>% 
  obter_metricas_regionais_por_semana() %>% 
  obter_siglas_codigos(merge_by = "codigo") # Regional

tabela_chik <- obter_tabela(
  df = df_dengue_chik %>% filter(arbovirose == "Chikungunya"), 
  df_mem = memUFanual, se_max_chik
) # Tabela Estadual

df_chik_regional_se <- df_chik_reg %>% 
  filter(SE == se_max_chik) %>%
  select(regional_id, codigo, pop, incest) %>% 
  mutate(regional_id = as.character(regional_id))

tabela_reg_chik <- obter_metricas_por_regional(
  df_mem_regional_anual = memReganual.chi,
  df_regional_se = df_chik_regional_se
)

tabela_reg_chik <- obter_classificacao_das_regionais(df_chik_reg, tabela_reg_chik)
tab_pop_risco_chik <- tabela_reg_chik %>% obter_pop_sob_risco()

shape_chik <- shape %>%
  left_join(tabela_reg_chik, by = "regional_id")

gg_pop_risco_chik <- tab_pop_risco_chik %>% 
  gg_bar_pop_risco(por_regiao = T)

gg_map_risco_classif_chik <- obter_mapa_risco_classif(
  shape_mun = shape_chik %>% 
    mutate(
      status = case_when(
        # is.na(status) ~ "Não se aplica",
        TRUE ~ status
      ),
      status = factor(status, levels = c("Alto em subida", "Alto estável", "Alto em queda", "Baixo ou moderado")) #, "Não se aplica"
    ),
  shape_state = shape_state
)

# Informacoes textuais - chikungunya

# Ano selecionado
df_ano_selecionado_chik <- df_dengue_chik %>% filter(arbovirose == "Chikungunya") %>% 
  filter(as.numeric(substr(SE, 1, 4))  == ano_selecionado) 

pop_brasil_ano_selecionado_chik <- sum(df_ano_selecionado_chik %>% 
                                         arrange(desc(SE)) %>% 
                                         dplyr::select(municipio_geocodigo, pop) %>% 
                                         distinct(municipio_geocodigo, .keep_all = T) %>% 
                                         pull(pop), na.rm = T)

total_casos_ano_selecionado_chik <- sum(df_ano_selecionado_chik %>% 
                                          pull(casos), na.rm = T)

total_casos_provaveis_ano_selecionado_chik <- sum(df_ano_selecionado_chik %>% pull(casprov), na.rm = T)

prop_casos_provaveis_chik <- paste0(round(total_casos_provaveis_ano_selecionado_chik/total_casos_ano_selecionado_chik *100, 2),"%")

incidencia_acumulada_ano_selecionado_chik <- total_casos_ano_selecionado_chik/ pop_brasil_ano_selecionado_chik * 10^5

# Ano anterior
df_ano_anterior_chik <- df_dengue_chik %>% filter(arbovirose == "Chikungunya") %>% 
  filter(as.numeric(substr(SE, 1, 4)) == (ano_selecionado - 1))

pop_brasil_ano_anterior_chik <- sum(df_ano_anterior_chik %>% 
                                      arrange(desc(SE)) %>% 
                                      dplyr::select(municipio_geocodigo, pop) %>% 
                                      distinct(municipio_geocodigo, .keep_all = T) %>% 
                                      pull(pop), na.rm = T)

df_ano_anterior_chik <- df_ano_anterior_chik %>% 
  filter(SE <= (se_max_chik - 100))

total_casos_ano_anterior_chik <- sum(df_ano_anterior_chik %>% 
                                       pull(casos), na.rm = T)
incidencia_acumulada_ano_anterior_chik <- total_casos_ano_anterior_chik/ pop_brasil_ano_anterior_chik * 10^5

dif_prop_ano_chik = total_casos_ano_selecionado_chik/total_casos_ano_anterior_chik

chik_semana_epidemica_estados <- semana_epidemica_dengue_chik_estados %>% 
  filter(arbovirose == "Chikungunya") %>% 
  filter(grupo == "Semana\natual") %>% 
  pull(sigla)

frase_chik_1 <- texto_1(chik_semana_epidemica_estados, arbovirose = "chikungunya")

##############
##############

# Total
total_casos_ano_selecionado <- total_casos_ano_selecionado_dengue + total_casos_ano_selecionado_chik

total_casos_provaveis_ano_selecionado <- total_casos_provaveis_ano_selecionado_dengue + total_casos_provaveis_ano_selecionado_chik

prop_casos_provaveis_total <- paste0(round(((total_casos_provaveis_ano_selecionado)/(total_casos_ano_selecionado_dengue + total_casos_ano_selecionado_chik)) *100, 2),"%")

dif_prop_ano_total <- (total_casos_ano_selecionado_dengue + total_casos_ano_selecionado_chik)/(total_casos_ano_anterior_dengue + total_casos_ano_anterior_chik)

tab1 <- data.frame(
  agravo = c("Chikungunya", "Dengue", "Total"),
  casos_suspeitos = c(total_casos_ano_selecionado_chik, total_casos_ano_selecionado_dengue, total_casos_ano_selecionado),
  percentual_casos_suspeitos = c(prop_casos_provaveis_chik, prop_casos_provaveis_dengue, prop_casos_provaveis_total),
  var_relacao_ano_anterior = paste0(round(c(dif_prop_ano_chik, dif_prop_ano_dengue, dif_prop_ano_total)*100,2),"%")
) 

tabela_1 <- tab1 %>%
  set_names(
    c(
      "Doença", "Casos Registrados",
      "Porcentagem de Casos Prováveis",
      "Variação em relação ao Ano Passado")
  ) %>% 
  flextable() %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  bg(bg = "#d1e2f2", part = "header") %>%
  color(color = "black", part = "header") %>%
  width(j = 1, width = 1.5) %>% 
  width(j = 2, width = 1.5) %>% 
  width(j = 3, width = 1.5) %>%   
  width(j = 4, width = 1.8)

ultima_semana_dengue_formatado <- format(ultima_semana_fim, format = "%d/%m/%Y")

titulo_relatorio <- paste0("(Semana epidemiológica ", ultima_semana_dengue, ")")

gerar_texto_boletim_1 <- function(df_rt_dengue_br, df_rt_chik_br, shape_dengue, shape_chik) {
  
  # ---- TEXTO INTRODUTÓRIO ----
  dengue_rt_atual <- df_rt_dengue_br$Rt
  incidencia_atual <- round(df_rt_dengue_br$inc_est, 1)
  
  # Determinar o status baseado no Rt e incidência
  status_transmissao <- if (dengue_rt_atual < 1 && incidencia_atual < 20) {
    "encontra-se em patamar estável"
  } else if (dengue_rt_atual >= 1 && incidencia_atual < 50) {
    "apresenta sinais de crescimento"
  } else if (incidencia_atual >= 50) {
    "encontra-se em período epidêmico"
  } else {
    "apresenta comportamento moderado"
  }
  
  periodo_desc <- if (dengue_rt_atual < 1 && incidencia_atual < 20) {
    "Trata-se do período inter-epidêmico nacional da dengue."
  } else if (dengue_rt_atual >= 1) {
    "Observa-se tendência de crescimento da transmissão."
  } else {
    "O cenário requer monitoramento contínuo."
  }
  
  texto_intro <- glue(
    "- A notificação de dengue no país {status_transmissao} com um número reprodutivo de ",
    "{round(dengue_rt_atual, 2)} e uma incidência de notificação de dengue de aproximadamente ",
    "{incidencia_atual} casos por 100.000 habitantes por semana. {periodo_desc}"
  )
  
  # ---- DENGUE ----
  dengue_rt <- df_rt_dengue_br$Rt
  dengue_trend <- ifelse(dengue_rt > 1, "cresce", "diminui")
  dengue_range <- glue("{round(dengue_rt, 2)} [{round(df_rt_dengue_br$lwr, 2)}, {round(df_rt_dengue_br$upr, 2)}]")
  
  regioes_dengue_alta <- shape_dengue %>%
    filter(!is.na(status), status == "Alto" | status == "Muito alto") %>%
    distinct(regiao, sigla)
  
  texto_regioes_dengue <- if (nrow(regioes_dengue_alta) > 0) {
    regioes_str <- paste(unique(regioes_dengue_alta$sigla), collapse = ", ")
    glue("há um leve aumento de casos nas regiões {unique(regioes_dengue_alta$regiao)} ({regioes_str}).")
  } else {
    "não há sinais relevantes de aumento regional no momento."
  }
  
  texto_dengue <- glue(
    "- A notificação de dengue no país {dengue_trend} com um número reprodutivo de {dengue_range}. ",
    # "Se esse {ifelse(dengue_rt > 1, 'crescimento', 'declínio')} se mantiver até as próximas semanas, ",
    # "podemos esperar entre 3,5 e 4,8 milhões de casos de dengue nessa temporada (semana 41 de 2024 a semana 40 de 2025). ",
    # "A incidência por UF de dengue está baixa (< 20 por 100.000 hab), mas {texto_regioes_dengue} ",
    "Das {nrow(shape_dengue)} regionais analisadas, ",
    "a maioria tem casos abaixo do limiar (", sum(shape_dengue$nivel == 0, na.rm = TRUE), 
    "), mas ", sum(shape_dengue$nivel > 0, na.rm = TRUE), 
    " delas estão com alta incidência e em ascensão."
  )
  
  # ---- CHIKUNGUNYA ----
  chik_rt <- df_rt_chik_br$Rt
  chik_trend <- ifelse(chik_rt > 1, "cresce", "diminui")
  chik_range <- glue("{round(chik_rt, 2)} [{round(df_rt_chik_br$lwr, 2)}, {round(df_rt_chik_br$upr, 2)}]")
  
  regioes_chik_alta <- shape_chik %>%
    filter(!is.na(status), status == "Alto" | status == "Muito alto") %>%
    distinct(regiao, sigla)
  
  texto_regioes_chik <- if (nrow(regioes_chik_alta) > 0) {
    regioes_str <- paste(unique(regioes_chik_alta$sigla), collapse = ", ")
    glue("mostra alguns sinais de aumento nas regiões {unique(regioes_chik_alta$regiao)} ({regioes_str}).")
  } else {
    "não há sinais relevantes de aumento regional no momento."
  }
  
  texto_chik <- glue(
    # "- Em relação à chikungunya, com número reprodutivo de {chik_range}, ",
    # "estima-se entre 120 e 350 mil casos na temporada. ",
    # "A incidência de chikungunya está baixa no geral (< 10 casos por 100.000 hab), mas {texto_regioes_chik} ",
    # "Das {nrow(shape_chik)} regionais analisadas, ",
    "a maioria tem casos baixos, mas ", sum(shape_chik$nivel > 0, na.rm = TRUE),
    " estão com alta incidência e em crescimento."
  )
  
  glue("{texto_intro}\n\n{texto_dengue}\n\n{texto_chik}")
}

tabela_dengue_uf <- memUFanual %>%
  left_join(df_dengue_uf %>% 
              filter(SE == max(SE, na.rm = T)) %>% 
              select(codigo, incest, inc) %>% 
              mutate(codigo = as.character(codigo)),
            by = "codigo") %>% 
  mutate(
    nivelNowcast = as.numeric(incest > (2 * veryhigh)),
    Rtmean = NA,
    secomp1 = NA,
    weekmax = NA
  )

tabela_dengue_uf <- map_dfr(
  unique(df_dengue_uf$codigo),
  obter_rt_por_uf,
  df_uf = df_dengue_uf,
  tabela_uf = tabela_dengue_uf
)

tabela_chik_uf <- memUFanual.chi %>%
  left_join(df_chik_uf %>% 
              filter(SE == max(SE, na.rm = T)) %>% 
              select(codigo, incest, inc) %>% 
              mutate(codigo = as.character(codigo)),
            by = "codigo") %>% 
  mutate(
    nivelNowcast = as.numeric(incest > (2 * veryhigh)),
    Rtmean = NA,
    secomp1 = NA,
    weekmax = NA
  )

tabela_chik_uf <- map_dfr(
  unique(df_chik_uf$codigo),
  obter_rt_por_uf,
  df_uf = df_chik_uf,
  tabela_uf = tabela_chik_uf
)

tabela_rt_dengue_chik_uf <- tabela_dengue_uf %>% 
  mutate(arbovirose = "Dengue") %>% 
  bind_rows(tabela_chik_uf %>% 
              mutate(arbovirose = "Chikungunya"))

tabela_tendencia_por_uf_inc_observada_dengue <- df_dengue_uf %>% 
  construir_tabela_incidencia_por_semana(var = "inc")

tabela_tendencia_por_uf_inc_estimada_dengue <- df_dengue_uf %>% 
  construir_tabela_incidencia_por_semana(var = "incest")

tabela_tendencia_por_uf_inc_observada_dengue <- tabela_tendencia_por_uf_inc_observada_dengue %>% 
  obter_siglas_codigos() %>% 
  relocate(regiao, .before = codigo) %>% 
  relocate(sigla, .after = regiao) %>% 
  as_tibble() %>% 
  dplyr::select(-c(codigo, estado))

tabela_tendencia_por_uf_inc_estimada_dengue <- tabela_tendencia_por_uf_inc_estimada_dengue %>% 
  obter_siglas_codigos() %>% 
  relocate(regiao, .before = codigo) %>% 
  relocate(sigla, .after = regiao) %>% 
  as_tibble() %>% 
  dplyr::select(-c(codigo, estado))

inc_obs_max_dengue <- ceiling(max(tabela_tendencia_por_uf_inc_observada_dengue[3:6], na.rm = T))
inc_est_max_dengue <- ceiling(max(tabela_tendencia_por_uf_inc_estimada_dengue[3:6], na.rm = T))

tab_tendencia_por_uf_inc_observada_dengue <- tabela_tendencia_por_uf_inc_observada_dengue %>% 
  left_join(tabela_rt_dengue_chik_uf %>% 
              filter(arbovirose == "Dengue") %>% 
              dplyr::select(sigla, Rtmean),
            by = "sigla") %>% 
  mutate(
    Rtmean = round(Rtmean, 2),
    tendencia = add_arrows(Rtmean, ref = 1)
  ) %>% 
  relocate(Rtmean, .after = sigla) %>% 
  relocate(tendencia, .after = Rtmean) %>% 
  bind_cols(tabela_tendencia_por_uf_inc_estimada_dengue %>% 
              dplyr::select(-c(regiao, sigla))) %>% 
  gerar_tabela_tendencia_flextable(inc_obs_max = inc_obs_max_dengue, inc_est_max = inc_est_max_dengue)

n_regionais_incidencia_dengue_alto_em_subida <- shape_dengue %>% filter(status == "Alto em subida") %>% pull() %>% length()

n_regionais_incidencia_dengue_alto_estavel <- shape_dengue %>% filter(status == "Alto estavel") %>% pull() %>% length()

n_regionais_incidencia_dengue_alto_em_queda <- shape_dengue %>% filter(status == "Alto em queda") %>% pull() %>% length()

n_regionais_incidencia_dengue_baixo <- shape_dengue %>% filter(status == "Baixo ou moderado") %>% pull() %>% length()

tabela_tendencia_por_uf_inc_observada_chik <- df_chik_uf %>% 
  construir_tabela_incidencia_por_semana(var = "inc")

tabela_tendencia_por_uf_inc_estimada_chik <- df_chik_uf %>% 
  construir_tabela_incidencia_por_semana(var = "incest")

tabela_tendencia_por_uf_inc_observada_chik <- tabela_tendencia_por_uf_inc_observada_chik %>% 
  obter_siglas_codigos() %>% 
  relocate(regiao, .before = codigo) %>% 
  relocate(sigla, .after = regiao) %>% 
  as_tibble() %>% 
  dplyr::select(-c(codigo, estado))

tabela_tendencia_por_uf_inc_estimada_chik <- tabela_tendencia_por_uf_inc_estimada_chik %>% 
  obter_siglas_codigos() %>% 
  relocate(regiao, .before = codigo) %>% 
  relocate(sigla, .after = regiao) %>% 
  as_tibble() %>% 
  dplyr::select(-c(codigo, estado))

inc_obs_max_chik <- ceiling(max(tabela_tendencia_por_uf_inc_observada_chik[3:6], na.rm = T))
inc_est_max_chik <- ceiling(max(tabela_tendencia_por_uf_inc_estimada_chik[3:6], na.rm = T))

tab_tendencia_por_uf_inc_observada_chik <-  tabela_tendencia_por_uf_inc_observada_chik %>% 
  left_join(tabela_rt_dengue_chik_uf %>% 
              filter(arbovirose == "Chikungunya") %>% 
              dplyr::select(sigla, Rtmean),
            by = "sigla") %>% 
  mutate(
    Rtmean = round(Rtmean, 2),
    tendencia = add_arrows(Rtmean, ref = 1)
  ) %>% 
  relocate(Rtmean, .after = sigla) %>% 
  relocate(tendencia, .after = Rtmean) %>% 
  bind_cols(tabela_tendencia_por_uf_inc_estimada_chik %>% 
              dplyr::select(-c(regiao, sigla))) %>% 
  gerar_tabela_tendencia_flextable(inc_obs_max = inc_obs_max_chik, inc_est_max = inc_est_max_chik)

n_regionais_incidencia_chik_alto_em_subida <- shape_chik %>% filter(status == "Alto em subida") %>% pull() %>% length()

n_regionais_incidencia_chik_alto_estavel <- shape_chik %>% filter(status == "Alto estavel") %>% pull() %>% length()

n_regionais_incidencia_chik_alto_em_queda <- shape_chik %>% filter(status == "Alto em queda") %>% pull() %>% length()

n_regionais_incidencia_chik_baixo <- shape_chik %>% filter(status == "Baixo ou moderado") %>% pull() %>% length()

tabela_dengue_macro <- memMacroanual %>%
  left_join(df_dengue_macro_se %>% 
              filter(SE == max(SE, na.rm = T)) %>% 
              select(macroregional_id, incest, inc) %>% 
              mutate(macroregional_id = as.character(macroregional_id)),
            by = "macroregional_id") %>% 
  mutate(
    nivelNowcast = as.numeric(incest > (2 * veryhigh)),
    Rtmean = NA,
    secomp1 = NA,
    weekmax = NA
  )

tabela_dengue_macro <- map_dfr(
  unique(df_dengue_macro_se$macroregional_id),
  obter_rt_por_macroregiao,
  df_macro = df_dengue_macro_se,
  tabela_macro = tabela_dengue_macro
)

tabela_chik_macro <- memMacroanual.chi %>%
  left_join(df_chik_macro_se %>% 
              filter(SE == max(SE, na.rm = T)) %>% 
              select(macroregional_id, incest, inc) %>% 
              mutate(macroregional_id = as.character(macroregional_id)),
            by = "macroregional_id") %>% 
  mutate(
    nivelNowcast = as.numeric(incest > (2 * veryhigh)),
    Rtmean = NA,
    secomp1 = NA,
    weekmax = NA
  )

tabela_chik_macro <- map_dfr(
  unique(df_chik_macro_se$macroregional_id),
  obter_rt_por_macroregiao,
  df_macro = df_chik_macro_se,
  tabela_macro = tabela_chik_macro
)

tabela_dengue_chik_macro <- tabela_dengue_macro %>% 
  mutate(arbovirose = "Dengue") %>% 
  bind_rows(
    tabela_chik_macro %>% 
      mutate(arbovirose = "Chikungunya")
  ) %>% 
  filter(tendencia %in% c("Crescente", "Crescente leve")) %>% 
  mutate(
    codigo = substr(macroregional_id, 1, 2),
    arbovirose = factor(arbovirose, levels = c("Dengue", "Chikungunya"))
  ) %>% 
  obter_siglas_codigos(merge_by = "codigo") %>% 
  dplyr::select(
    regiao,
    arbovirose,
    estado,
    macroregional,
    Rtmean,
    # status,
    tendencia
  ) %>% 
  arrange(regiao, arbovirose)

toc()
