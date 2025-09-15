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


# juntando tudo no objeto shape

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
pop <- dd.uf[, c("code_state","pop")]

shape_short <- states %>%
  left_join(short.pred, join_by(abbrev_state == state)) %>%
  left_join(ens3w, join_by(abbrev_state == state)) %>%
  left_join(pop) %>%
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
toc()
