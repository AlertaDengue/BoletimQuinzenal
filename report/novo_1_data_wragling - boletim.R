# ==============================================================================
# SCRIPT DE PREPARAÇÃO DE DADOS - BOLETIM QUINZENAL INFODENGUE
# ==============================================================================

# Configuração inicial --------------------------------------------------------
suppressPackageStartupMessages({
  library(tictoc)
  library(googledrive)
  library(dplyr)
  library(arrow)
  library(sf)
  library(geobr)
  library(ggplot2)
  library(patchwork)
  library(lubridate)
  library(data.table)
  library(scales)
  library(glue)
  library(viridis)
  library(purrr)  # Para a função walk
})

# Configurações globais
options(
  stringsAsFactors = FALSE,
  scipen = 999,
  timeout = 300  # Timeout para downloads
)

# Parâmetros de configuração --------------------------------------------------
CONFIG <- list(
  # Controle de execução
  download_from_google = TRUE,
  
  # Autenticação Google Drive 
  google_email = "estatistico.bianchi@gmail.com",
  
  # IDs do Google Drive
  drive_ids = list(
    for_ensemble = "1hdUPpGn7qiKLDoaW4DnzYzJnZqdbSjLF",
    figures_ensemble = "1aQW95MhLvFJffd0OAaJwa_8PE345uUFN"
  ),
  
  # Diretórios
  paths = list(
    data = "data/",
    figures = "report/figures/",
    shapes = "shape/",
    cases_data = "../load_infodengue_data/data/cases/",
    temp_data = "../data/cases2024/"
  ),
  
  # Arquivos específicos
  files = list(
    muns = "data/muns.R",
    shape = "shape/rs_450_RepCor1.shp",
    rt_table = "data/tabela-resumo202442.csv",
    predictions = "data/predicoes_acumuladas.csv",
    ensemble_bayes = "data/ensemble_bayes_2025.csv",
    for_ensemble = "data/for_ensemble.csv",
    mem_files = c(
      "data/mem2024.RData",
      "data/memBR2024.RData", 
      "data/mem2024temp.RData",
      "data/mem2024chitemp.RData"
    )
  )
)

# Funções auxiliares ----------------------------------------------------------
log_message <- function(msg, type = "INFO") {
  cat(sprintf("[%s] %s: %s\n", Sys.time(), type, msg))
}

safe_file_operation <- function(operation, description, error_action = "warn") {
  tryCatch({
    result <- operation()
    log_message(paste("Sucesso:", description), "SUCCESS")
    return(result)
  }, error = function(e) {
    log_message(paste("Erro em", description, ":", e$message), "ERROR")
    if (error_action == "stop") stop(e)
    return(NULL)
  })
}

# Função para download seguro do Google Drive
safe_drive_download <- function(file_id, path, overwrite = TRUE, description = "") {
  safe_file_operation(
    operation = function() {
      drive_download(as_id(file_id), path = path, overwrite = overwrite)
    },
    description = paste("download", description, "para", path)
  )
}

safe_baixar_arquivo <- function(file_id, path, description = "") {
  safe_file_operation(
    operation = function() {
      baixar_arquivo(file_id, path = path)
    },
    description = paste("download", description, "para", path)
  )
}

# Função para carregar dados parquet
load_parquet_data <- function(path_pattern, disease_name) {
  log_message(paste("Carregando dados de", disease_name))
  
  file_list <- list.files(
    path = CONFIG$paths$cases_data, 
    pattern = path_pattern, 
    full.names = TRUE
  )
  
  if (length(file_list) == 0) {
    log_message(paste("Nenhum arquivo encontrado para", disease_name), "WARNING")
    return(NULL)
  }
  
  safe_file_operation(
    operation = function() {
      data_list <- lapply(file_list, read_parquet)
      bind_rows(data_list)
    },
    description = paste("carregamento de dados", disease_name)
  )
}

# Função para processar dados de doenças
process_disease_data <- function(df, disease_name, muns_data) {
  if (is.null(df)) return(NULL)
  
  df %>%
    dplyr::select(-c("tweet", "casprov_est", "casprov_est_min", "casprov_est_max", "casconf")) %>% 
    filter(data_iniSE >= (Sys.Date() %m-% months(12))) %>% 
    left_join(
      muns_data %>% dplyr::select(-nome), 
      by = "municipio_geocodigo"
    ) %>%
    mutate(
      incest = casos_est / pop * 100000,
      arbovirose = disease_name
    )
}

# Preparação do ambiente ------------------------------------------------------
setup_environment <- function() {
  log_message("=== INICIANDO PREPARAÇÃO DO AMBIENTE ===")
  
  # Criar diretórios necessários
  sapply(CONFIG$paths, function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  })
  
  # Verificar arquivos essenciais
  essential_files <- c(CONFIG$files$muns, CONFIG$files$shape)
  missing_files <- essential_files[!file.exists(essential_files)]
  
  if (length(missing_files) > 0) {
    log_message(paste("Arquivos essenciais não encontrados:", 
                      paste(missing_files, collapse = ", ")), "ERROR")
    stop("Arquivos essenciais ausentes")
  }
  
  log_message("Ambiente preparado com sucesso", "SUCCESS")
}

# Download de dados do Google Drive -------------------------------------------
download_google_data <- function() {
  if (!CONFIG$download_from_google) {
    log_message("Download do Google Drive desabilitado")
    return(TRUE)
  }
  
  log_message("=== INICIANDO DOWNLOADS DO GOOGLE DRIVE ===")
  
  # Autenticar no Google Drive
  tryCatch({
    drive_auth(email = CONFIG$google_email)
    log_message("Autenticação Google Drive realizada", "SUCCESS")
  }, error = function(e) {
    log_message(paste("Erro na autenticação:", e$message), "ERROR")
    return(FALSE)
  })
  
  # Download for_ensemble.csv
  safe_drive_download(
    CONFIG$drive_ids$for_ensemble,
    file.path(CONFIG$paths$data, "for_ensemble.csv"),
    description = "for_ensemble.csv"
  )
  
  # Download figuras do ensemble
  tryCatch({
    figures_ensemble_id <- drive_ls(as_id(CONFIG$drive_ids$figures_ensemble))$id
    
    if (length(figures_ensemble_id) > 0) {
      walk(figures_ensemble_id, function(id) {
        safe_baixar_arquivo(id, CONFIG$paths$figures, description = "figura ensemble")
      })
      log_message("Figuras do ensemble baixadas", "SUCCESS")
    }
  }, error = function(e) {
    log_message(paste("Erro no download de figuras:", e$message), "WARNING")
  })
  
  log_message("Downloads concluídos")
  return(TRUE)
}

# Carregamento e processamento de dados principais ----------------------------
load_main_data <- function() {
  log_message("=== CARREGANDO DADOS PRINCIPAIS ===")
  
  # Carregar dados de municípios
  muns <- safe_file_operation(
    operation = function() {
      load(file = CONFIG$files$muns)
      muns %>% mutate(regional_id = as.character(regional_id))
    },
    description = "dados de municípios",
    error_action = "stop"
  )
  
  # Carregar dados de dengue
  df_dengue <- load_parquet_data("dengue.parquet", "dengue")
  df_dengue <- process_disease_data(df_dengue, "Dengue", muns)
  se_max_dengue <- if (!is.null(df_dengue)) max(df_dengue$SE, na.rm = TRUE) else NULL
  ultima_semana_inicio <- if (!is.null(se_max_dengue)) {
    ymd(substr(max(df_dengue$data_iniSE, na.rm = TRUE), 1, 10))
  } else NULL
  
  log_message(paste("SE máxima dengue:", se_max_dengue))
  
  # Carregar dados de chikungunya
  df_chik <- load_parquet_data("chik.parquet", "chikungunya")
  df_chik <- process_disease_data(df_chik, "Chikungunya", muns)
  se_max_chik <- if (!is.null(df_chik)) max(df_chik$SE, na.rm = TRUE) else NULL
  ultima_semana_fim <- if (!is.null(ultima_semana_inicio)) {
    ultima_semana_inicio + 6
  } else NULL
  log_message(paste("SE máxima chikungunya:", se_max_chik))
  
  # Combinar dados e calcular métricas
  df_combined <- NULL
  if (!is.null(df_dengue) && !is.null(df_chik)) {
    df_combined <- bind_rows(df_dengue, df_chik) #%>%
      # filter(SE != 202401)  # Filtrar semana específica se necessário
  }
  
  rm(df_dengue, df_chik)
  gc()

  # Retornar dados processados
  list(
    muns = muns,
    df_combined = df_combined,
    # df_dengue = df_dengue,
    # df_chik = df_chik,
    se_max_dengue = se_max_dengue,
    se_max_chik = se_max_chik,
    ultima_semana_inicio = ultima_semana_inicio,
    ultima_semana_fim = ultima_semana_fim
  )
}

# Carregamento de dados geoespaciais ------------------------------------------
load_spatial_data <- function(muns_data) {
  log_message("=== CARREGANDO DADOS GEOESPACIAIS ===")
  
  # Carregar shapefile
  shape <- safe_file_operation(
    operation = function() {
      st_read(CONFIG$files$shape, quiet = TRUE) %>%
        rename(regional_id = primary.id) %>%
        mutate(regional_id = as.character(regional_id)) %>%
        dplyr::select(-c(id, secondary)) %>%
        left_join(
          muns_data %>%
            dplyr::select(regional_id, regional) %>%
            distinct(regional_id, .keep_all = TRUE),
          by = "regional_id"
        )
    },
    description = "shapefile regional"
  )
  
  # Carregar dados estaduais
  shape_state <- safe_file_operation(
    operation = function() {
      read_state(year = 2020, showProgress = FALSE)
    },
    description = "shapefile estadual"
  )
  
  return(list(shape = shape, shape_state = shape_state))
}

# Carregamento de dados auxiliares --------------------------------------------
load_auxiliary_data <- function() {
  log_message("=== CARREGANDO DADOS AUXILIARES ===")
  
  # Tabela Rt por UF
  tab_rt_uf <- safe_file_operation(
    operation = function() {
      fread(CONFIG$files$rt_table) %>%
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
        distinct(arbovirose, .keep_all = TRUE) %>%
        ungroup() %>%
        dplyr::select(sigla, arbovirose, Rtmean)
    },
    description = "tabela Rt por UF"
  )
  
  # Carregar arquivos MEM
  mem_data <- list()
  for (mem_file in CONFIG$files$mem_files) {
    if (file.exists(mem_file)) {
      safe_file_operation(
        operation = function() load(mem_file, envir = .GlobalEnv),
        description = paste("arquivo MEM:", basename(mem_file))
      )
    }
  }
  
  return(list(tab_rt_uf = tab_rt_uf))
}

# Processamento de dados MEM --------------------------------------------------
process_mem_data <- function() {
  log_message("=== PROCESSANDO DADOS MEM ===")
  
  # Verificar se os dados MEM estão carregados
  if (!exists("curveMEM")) {
    log_message("Dados MEM não encontrados", "WARNING")
    return(NULL)
  }
  
  # Processar MEM Brasil
  memBR_dengue_chik <- curveMEM %>%
    set_names(c("preseason", "epidemic", "posseason", "arbovirose")) %>%
    mutate(
      arbovirose = ifelse(arbovirose == "A92.0", "Chikungunya", "Dengue"),
      arbovirose = factor(arbovirose, levels = c("Dengue", "Chikungunya"))
    ) %>%
    group_by(arbovirose) %>%
    mutate(SEe = 1:52) %>%
    ungroup() %>%
    mutate(
      SEe = SEe + 40,
      SEe = ifelse(SEe > 52, SEe - 52, SEe)
    )
  
  # Processar dados MEM estaduais para dengue
  processed_mem <- list()
  
  if (exists("memUFanual")) {
    processed_mem$memUFanual <- memUFanual %>%
      rename(codigo = nome) %>%
      mutate(codigo = as.character(codigo)) %>%
      obter_siglas_codigos(merge_by = "codigo")
  }
  
  if (exists("memUFanual.chi")) {
    processed_mem$memUFanual_chi <- memUFanual.chi %>%
      rename(codigo = nome) %>%
      mutate(codigo = as.character(codigo)) %>%
      obter_siglas_codigos(merge_by = "codigo")
  }
  
  return(list(
    memBR_dengue_chik = memBR_dengue_chik,
    processed_mem = processed_mem
  ))
}

# Criação de mapas e visualizações --------------------------------------------
create_ensemble_maps <- function() {
  log_message("=== CRIANDO MAPAS ENSEMBLE ===")
  
  if (!file.exists(CONFIG$files$predictions)) {
    log_message("Arquivo de predições não encontrado", "WARNING")
    return(NULL)
  }
  
  # Carregar dados de predições
  pred <- read.csv(CONFIG$files$predictions)
  states <- read_state(year = 2020, showProgress = FALSE)
  
  # Preparar dados para mapas
  shape_anual <- states %>%
    left_join(pred, join_by(abbrev_state == UF)) %>%
    group_by(abbrev_state) %>%
    summarise(
      geom = st_union(geom),
      code_state = unique(code_state),
      abbrev_state = unique(abbrev_state),
      ens_inc23 = as.numeric(unique(cenario23)),
      ens_inc24 = as.numeric(unique(cenario24)),
      .groups = "drop"
    ) %>%
    arrange(code_state)
  
  # Definir escala comum
  common_yrange <- range(c(shape_anual$ens_inc24, shape_anual$ens_inc23), na.rm = TRUE)
  
  # Criar mapas
  maps <- create_ensemble_visualization(shape_anual, common_yrange)
  
  log_message("Mapas ensemble criados", "SUCCESS")
  return(maps)
}

create_ensemble_visualization <- function(shape_anual, common_yrange) {
  # Tema comum para mapas
  map_theme <- theme_minimal() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12),
      strip.text.x = element_text(size = 18, colour = "black"),
      strip.background = element_rect(fill = "white")
    )
  
  # Mapa 1 - Ensemble 23
  m1 <- ggplot() +
    geom_sf(data = shape_anual, aes(fill = ens_inc23)) +
    scale_fill_viridis_c(
      option = "viridis", 
      oob = scales::squish,
      trans = "log",
      limits = common_yrange,
      breaks = c(100, 300, 1000, 10000),
      labels = c("100", "300", "1K", "10k"),
      name = "Incidência acumulada"
    ) +
    labs(title = "(A) Modelo ensemble 23") +
    map_theme +
    theme(legend.position = "none")
  
  # Mapa 2 - Ensemble 24
  m2 <- ggplot() +
    geom_sf(data = shape_anual, aes(fill = ens_inc24)) +
    scale_fill_viridis_c(
      option = "viridis", 
      oob = scales::squish,
      trans = "log",
      limits = common_yrange,
      breaks = c(100, 300, 1000, 10000),
      labels = c("100", "300", "1K", "10k"),
      name = "Incidência\nacumulada"
    ) +
    labs(title = "(B) Modelo ensemble 24") +
    guides(colour = guide_colorbar(barwidth = 50)) +
    map_theme
  
  # Gráfico de comparação
  m3 <- ggplot(data = shape_anual, aes(x = factor(code_state))) +
    geom_point(aes(y = ens_inc23), color = "black", shape = "_", size = 3) +
    geom_point(aes(y = ens_inc24), color = "black", shape = "_", size = 3) +
    geom_segment(aes(
      x = factor(code_state), xend = factor(code_state),
      y = ens_inc23, yend = ens_inc24
    ), color = "black") +
    geom_hline(yintercept = 300, color = "red", linetype = "dashed") +
    scale_y_log10() +
    scale_x_discrete(labels = shape_anual$abbrev_state) +
    labs(
      x = "", 
      y = "Incidência acumulada (x10^5)",
      title = "(C) Faixa de incidência acumulada prevista pelos ensembles"
    ) +
    map_theme +
    theme(
      panel.grid.major.y = element_line(
        linetype = "dotted", color = "grey", linewidth = 0.5
      )
    )
  
  # Combinar gráficos
  combined_plots <- (m1 | m2) / m3 + 
    plot_layout(heights = c(1.5, 1.5)) +
    plot_annotation(title = "Previsões para a temporada 2024-25 (médias dos modelos)")
  
  return(list(m1 = m1, m2 = m2, m3 = m3, combined = combined_plots))
}

# Função principal ------------------------------------------------------------
main_data_processing <- function() {
  log_message("=== INICIANDO PROCESSAMENTO DE DADOS BOLETIM QUINZENAL ===")
  tic("Boletim quinzenal")
  
  tryCatch({
    # 1. Preparar ambiente
    setup_environment()
    
    # 2. Download de dados
    download_google_data()
    
    # 3. Carregar dados principais
    main_data <- load_main_data()
    if (is.null(main_data$df_combined)) {
      stop("Falha no carregamento de dados principais")
    }
    
    # 4. Carregar dados geoespaciais
    spatial_data <- load_spatial_data(main_data$muns)
    
    # 5. Carregar dados auxiliares
    auxiliary_data <- load_auxiliary_data()
    
    # 6. Processar dados MEM
    mem_data <- process_mem_data()
    
    # 7. Criar visualizações
    ensemble_maps <- create_ensemble_maps()
    
    # 9. Definir variáveis globais para uso posterior (apenas se os dados existem)
    if (!is.null(main_data$df_combined)) {
      assign("df_dengue_chik", main_data$df_combined, envir = .GlobalEnv)
    }
    if (!is.null(main_data$se_max_dengue)) {
      assign("se_max_dengue", main_data$se_max_dengue, envir = .GlobalEnv)
    }
    if (!is.null(main_data$se_max_chik)) {
      assign("se_max_chik", main_data$se_max_chik, envir = .GlobalEnv)
    }
    if (!is.null(main_data$ultima_semana_inicio)) {
      assign("ultima_semana_inicio", main_data$ultima_semana_inicio, envir = .GlobalEnv)
      assign("ultima_semana_fim", main_data$ultima_semana_fim, envir = .GlobalEnv)
    }
    if (!is.null(spatial_data$shape)) {
      assign("shape", spatial_data$shape, envir = .GlobalEnv)
    }
    if (!is.null(spatial_data$shape_state)) {
      assign("shape_state", spatial_data$shape_state, envir = .GlobalEnv)
    }
    if (!is.null(auxiliary_data$tab_rt_uf)) {
      assign("tab_rt_uf", auxiliary_data$tab_rt_uf, envir = .GlobalEnv)
    }
    
    # Variáveis calculadas
    assign("ano_selecionado", year(Sys.Date()), envir = .GlobalEnv)
    if (!is.null(main_data$se_max_dengue)) {
      assign("ultima_semana_dengue", as.numeric(substr(main_data$se_max_dengue, 5, 6)), envir = .GlobalEnv)
    }
    if (!is.null(main_data$se_max_chik)) {
      assign("ultima_semana_chik", substr(main_data$se_max_chik, 5, 6), envir = .GlobalEnv)
    }
    
    toc()
    log_message("=== PROCESSAMENTO CONCLUÍDO COM SUCESSO ===", "SUCCESS")
    return(TRUE)
    
  }, error = function(e) {
    toc()
    log_message(paste("Erro fatal no processamento:", e$message), "ERROR")
    stop(e)
  })
}

# Executar processamento principal --------------------------------------------
if (interactive() || !exists("skip_data_processing")) {
  # Definir variável de controle se necessário
  if (!exists("download_from_google")) {
    download_from_google <- CONFIG$download_from_google
  } else {
    CONFIG$download_from_google <- download_from_google
  }
  
  main_data_processing()
}
