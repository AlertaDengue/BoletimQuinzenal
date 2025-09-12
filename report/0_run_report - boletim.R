r_pkgs <- c("tidyverse", "AlertTools", "sf", "arrow", "geobr",
            "ggplot2", "ggpubr", "geofacet", "cowplot", "lubridate",
            "janitor", "kableExtra", "xtable", "data.table",
            "gt", "gtExtras", "patchwork", "googledrive", "tictoc",
            "scales", "cowplot")


if(!"pacman" %in% rownames(installed.packages())) install.packages("pacman")
pacman::p_load(char = r_pkgs)

# removing NA from Kable
options(knitr.kable.NA = '')

# removing annoying scientific notation
options(scipen = 999)

# RUN
download_from_google <- T # get data from google drive folder
get_boletim_estadual <- T # generate boletins estaduais

## Loading Infodengue functions
source("scripts/funcoes_extras.R", encoding = "utf-8")
source("report/1_data_wragling - boletim.R", encoding = "utf-8")

## Setting directories
output_directory <- paste0("report/outputs/boletins_nacionais/")
output_filename <- paste0("Informe_Infodengue_SE", se_max_dengue)

dir.create(output_directory, recursive = T)
output_file = paste0(output_directory, output_filename)

##
## Boletim Nacional
##

### .pdf
rmarkdown::render(input = "report/InfoDengue Informe_pdf.Rmd",
                  output_file = output_file,
                  output_format = "pdf_document",
                  clean = T)

if(!"custom-template_copia.docx" %in% dir("template")){
  file.copy("template/custom-template.docx", "template/custom-template_copia.docx", overwrite = TRUE)
}
rmarkdown::render(input = "report/InfoDengue Informe_docx.Rmd",
                  output_file = output_file,
                  output_format = "word_document",
                  clean = T)

#### Fazer upload para a pasta especificada drive_get('Informe Infodengue/boletins estaduais')$id[1]
drive_upload(
  media = paste0(output_file, ".pdf"),
  path = as_id("1gFVYW5oCkKc5WQ8FVikKaGRrv_1lT-tR"),
  overwrite = T)

drive_upload(
  media = paste0(output_file, ".docx"),
  path = as_id("1gFVYW5oCkKc5WQ8FVikKaGRrv_1lT-tR"),
  overwrite = T)

##
## Boletim estadual
##

if(get_boletim_estadual == TRUE){
  
  output_directory <- paste0("report/outputs/boletins_estaduais/")
  output_filename <- paste0("Informe_Infodengue_SE", se_max_dengue)
  
  df_estados <- obter_siglas_codigos()
  tab_id_final <- data.frame()
  for(selected_estado in df_estados$estado){
    
    temp_uf <- df_estados %>% filter(estado == selected_estado) %>% pull(estado)
    temp_sigla <- df_estados %>% filter(estado == selected_estado) %>% pull(sigla)
    temp_codigo <- df_estados %>% filter(estado == selected_estado) %>% pull(codigo)
    
    output_estadual_file <- paste0(
      substr(output_filename, 1, 19), temp_sigla, "_", substr(output_filename, 20, 28)
    )
    
    output_estadual_file = paste0(output_directory, output_estadual_file)
    
    rmarkdown::render(input = "report/InfoDengue Informe_Estadual_pdf.Rmd",
                      output_file = output_estadual_file,
                      output_format = "pdf_document",
                      clean = T)
    
    drive_upload(
      media = paste0(output_estadual_file, ".pdf"),
      path = as_id("1bBmfKsEPcwqlXSMKCITa30ChtlmFHuEJ"),
      overwrite = T)
    
    # tab_id_final <- rbind(
    #   tab_id_final, tab_id
    # )
    
  }

}else{
  message("Boletim estadual não foi solicitado")
}
