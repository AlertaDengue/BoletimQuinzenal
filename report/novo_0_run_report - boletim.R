# ==============================================================================
# SCRIPT DE GERAÇÃO DE BOLETINS INFODENGUE
# ==============================================================================

# Configuração inicial --------------------------------------------------------
suppressPackageStartupMessages({
  # Pacotes essenciais
  r_pkgs <- c(
    "tidyverse", "AlertTools", "sf", "arrow", "geobr",
    "ggplot2", "ggpubr", "geofacet", "cowplot", "lubridate",
    "janitor", "kableExtra", "xtable", "data.table",
    "gt", "gtExtras", "patchwork", "googledrive", "tictoc",
    "scales", "cowplot"
  )
  
  # Instalar e carregar pacotes
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
  }
  pacman::p_load(char = r_pkgs)
})

## Carregar funcoes extras
source("scripts/funcoes_extras.R", encoding = "utf-8")

## Carregar e gerar analises
# source("report/1_data_wragling - boletim.R", encoding = "utf-8")
source("report/novo_1_data_wragling - boletim.R", encoding = "utf-8")


# Configurações globais
options(
  knitr.kable.NA = '',     # Remove NA de tabelas kable
  scipen = 999,            # Remove notação científica
  stringsAsFactors = FALSE # Evita fatores automáticos
)

# Parâmetros de configuração --------------------------------------------------
CONFIG <- list(
  download_from_google = TRUE,
  get_boletim_estadual = TRUE,
  google_drive_ids = list(
    nacional = "1gFVYW5oCkKc5WQ8FVikKaGRrv_1lT-tR",
    estadual = "1bBmfKsEPcwqlXSMKCITa30ChtlmFHuEJ"
  ),
  directories = list(
    scripts = "scripts/",
    report = "report/",
    template = "template/",
    output_nacional = "report/outputs/boletins_nacionais/",
    output_estadual = "report/outputs/boletins_estaduais/"
  )
)

# Funções auxiliares ----------------------------------------------------------
log_message <- function(msg, type = "INFO") {
  cat(sprintf("[%s] %s: %s\n", Sys.time(), type, msg))
}

setup_directories <- function() {
  sapply(CONFIG$directories, function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      log_message(paste("Diretório criado:", dir))
    }
  })
}

safe_render <- function(input, output_file, output_format, clean = TRUE) {
  tryCatch({
    log_message(paste("Renderizando:", basename(output_file)))
    rmarkdown::render(
      input = input,
      output_file = output_file,
      output_format = output_format,
      clean = clean,
      quiet = TRUE
    )
    log_message(paste("Sucesso:", basename(output_file)), "SUCCESS")
    return(TRUE)
  }, error = function(e) {
    log_message(paste("Erro ao renderizar", basename(output_file), ":", e$message), "ERROR")
    return(FALSE)
  })
}

safe_upload <- function(file_path, drive_id, overwrite = TRUE) {
  if (!file.exists(file_path)) {
    log_message(paste("Arquivo não encontrado:", file_path), "WARNING")
    return(FALSE)
  }
  
  tryCatch({
    drive_upload(
      media = file_path,
      path = googledrive::as_id(drive_id),
      overwrite = overwrite
    )
    log_message(paste("Upload realizado:", basename(file_path)), "SUCCESS")
    return(TRUE)
  }, error = function(e) {
    log_message(paste("Erro no upload", basename(file_path), ":", e$message), "ERROR")
    return(FALSE)
  })
}

prepare_template <- function() {
  template_original <- file.path(CONFIG$directories$template, "custom-template.docx")
  template_copia <- file.path(CONFIG$directories$template, "custom-template_copia.docx")
  
  if (!file.exists(template_copia)) {
    if (file.exists(template_original)) {
      file.copy(template_original, template_copia, overwrite = TRUE)
      log_message("Template copiado com sucesso")
    } else {
      log_message("Template original não encontrado", "WARNING")
    }
  }
}

# Carregamento de dados -------------------------------------------------------
load_dependencies <- function() {
  tryCatch({
    source(file.path(CONFIG$directories$scripts, "funcoes_extras.R"), encoding = "utf-8")
    source(file.path(CONFIG$directories$report, "1_data_wragling - boletim.R"), encoding = "utf-8")
    log_message("Dependências carregadas com sucesso")
    return(TRUE)
  }, error = function(e) {
    log_message(paste("Erro ao carregar dependências:", e$message), "ERROR")
    return(FALSE)
  })
}

# Geração de boletim nacional -------------------------------------------------
generate_national_bulletin <- function(se_max_dengue) {
  log_message("=== INICIANDO GERAÇÃO DE BOLETIM NACIONAL ===")
  
  output_filename <- paste0("Informe_Infodengue_SE", se_max_dengue)
  output_file <- file.path(CONFIG$directories$output_nacional, output_filename)
  
  # Preparar template
  prepare_template()
  
  # Renderizar PDF
  # pdf_success <- safe_render(
  #   input = file.path(CONFIG$directories$report, "InfoDengue Informe_pdf.Rmd"),
  #   output_file = output_file,
  #   output_format = "pdf_document"
  # )
  
  # Renderizar DOCX
  docx_success <- safe_render(
    input = file.path(CONFIG$directories$report, "InfoDengue Informe_docx.Rmd"),
    output_file = output_file,
    output_format = "word_document"
  )
  
  # Upload dos arquivos
  upload_results <- c()
  if (pdf_success) {
    upload_results <- c(upload_results, 
                        safe_upload(paste0(output_file, ".pdf"), CONFIG$google_drive_ids$nacional))
  }
  if (docx_success) {
    upload_results <- c(upload_results, 
                        safe_upload(paste0(output_file, ".docx"), CONFIG$google_drive_ids$nacional))
  }
  
  log_message("=== BOLETIM NACIONAL CONCLUÍDO ===")
  return(all(upload_results))
}

# Geração de boletins estaduais -----------------------------------------------
generate_state_bulletins <- function(se_max_dengue) {
  if (!CONFIG$get_boletim_estadual) {
    log_message("Boletim estadual não foi solicitado", "INFO")
    return(TRUE)
  }
  
  log_message("=== INICIANDO GERAÇÃO DE BOLETINS ESTADUAIS ===")
  
  # Obter dados dos estados
  df_estados <- tryCatch({
    obter_siglas_codigos()
  }, error = function(e) {
    log_message(paste("Erro ao obter dados dos estados:", e$message), "ERROR")
    return(NULL)
  })
  
  if (is.null(df_estados)) {
    log_message("Não foi possível obter dados dos estados", "ERROR")
    return(FALSE)
  }
  
  log_message(paste("Processando", nrow(df_estados), "estados"))
  
  # Processar cada estado
  results <- vector("logical", nrow(df_estados))
  
  for (i in seq_len(nrow(df_estados))) {
    estado_info <- df_estados[i, ]
    log_message(paste("Processando estado:", estado_info$estado))
    
    # Preparar nomes de arquivo
    base_filename <- paste0("Informe_Infodengue_SE", se_max_dengue)
    output_estadual_filename <- paste0(
      substr(base_filename, 1, 19), 
      estado_info$sigla, "_", 
      substr(base_filename, 20, nchar(base_filename))
    )
    
    output_estadual_file <- file.path(
      CONFIG$directories$output_estadual, 
      output_estadual_filename
    )
    
    # Definir variáveis temporárias para o ambiente de renderização
    temp_uf <- estado_info$estado
    temp_sigla <- estado_info$sigla
    temp_codigo <- estado_info$codigo
    
    # Renderizar PDF estadual
    pdf_success <- safe_render(
      input = file.path(CONFIG$directories$report, "InfoDengue Informe_Estadual_pdf.Rmd"),
      output_file = output_estadual_file,
      output_format = "pdf_document"
    )
    
    # Upload do arquivo
    if (pdf_success) {
      upload_success <- safe_upload(
        paste0(output_estadual_file, ".pdf"), 
        CONFIG$google_drive_ids$estadual
      )
      results[i] <- upload_success
    } else {
      results[i] <- FALSE
    }
  }
  
  success_count <- sum(results)
  log_message(paste("Boletins estaduais concluídos:", success_count, "de", length(results)))
  log_message("=== BOLETINS ESTADUAIS CONCLUÍDOS ===")
  
  return(success_count == length(results))
}

check_report_status <- function(success_var, report_name = "RELATÓRIO") {
  # success_var: variável booleana ou NULL
  # report_name: string para exibir no log (ex: "RELATÓRIO NACIONAL" ou "RELATÓRIO ESTADUAL")
  
  if (!exists(success_var, envir = .GlobalEnv)) {
    log_message(
      paste("===", report_name, "NÃO FOI EXECUTADO (VARIÁVEL", success_var, "NÃO EXISTE) ==="),
      "WARNING"
    )
  }
  
  success <- get(success_var, envir = .GlobalEnv)
  
  if (isTRUE(success)) {
    log_message(
      paste("=== PROCESSO CONCLUÍDO COM SUCESSO EM", duration, "MINUTOS ==="),
      "SUCCESS"
    )
    
  } else if (identical(success, FALSE)) {
    log_message(
      paste("===", report_name, "NÃO FOI GERADO POR DECISÃO DO USUÁRIO ==="),
      "MESSAGE"
    )
    log_message(
      paste0("=== ALTERE main(", tolower(report_name), " = TRUE) PARA GERAR O", report_name, " ==="),
      "MESSAGE"
    )
    
  } else {
    log_message(
      paste("=== PROCESSO CONCLUÍDO COM ERROS EM", duration, "MINUTOS ==="),
      "WARNING"
    )
  }
}

# Função principal ------------------------------------------------------------
main <- function(boletim_nacional = TRUE, boletim_estadual = TRUE) {
  log_message("=== INICIANDO PROCESSO DE GERAÇÃO DE BOLETINS ===")
  start_time <- Sys.time()
  
  # Setup inicial
  setup_directories()
  
  # Carregar dependências
  if (!load_dependencies()) {
    log_message("Falha ao carregar dependências. Processo interrompido.", "ERROR")
    return(FALSE)
  }
  
  # Verificar se se_max_dengue está definida
  if (!exists("se_max_dengue")) {
    log_message("Variável se_max_dengue não encontrada. Definindo valor padrão.", "WARNING")
    se_max_dengue <- format(Sys.Date(), "%U")  # Semana atual como fallback
  }
  
  log_message(paste("Processando dados para SE:", se_max_dengue))
  
  # Executar processos
  if(boletim_nacional == T){
    nacional_success <- generate_national_bulletin(se_max_dengue)
  }else{
    nacional_success <- FALSE
  }
  
  if(boletim_estadual == T){
    estadual_success <- generate_state_bulletins(se_max_dengue)
  }else{
    estadual_success <- FALSE
  }
  
  # Relatório final
  end_time <- Sys.time()
  duration <- round(difftime(end_time, start_time, units = "mins"), 2)
  check_report_status("nacional_success", "RELATÓRIO NACIONAL")
  check_report_status("estadual_success", "RELATÓRIO ESTADUAL")
  

}

# Executar o processo principal -----------------------------------------------
if (interactive() || !exists("skip_execution")) {
  tryCatch({
    main(boletim_nacional = T, boletim_estadual = F)
  }, error = function(e) {
    log_message(paste("Erro fatal:", e$message), "ERROR")
    stop(e)
  })
}
