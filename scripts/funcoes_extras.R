obter_siglas_codigos <- function(df, merge_by = "codigo"){
  
  df_uf = data.frame(
    estado = c("Rondônia", "Roraima", "Amazonas", "Pará", "Amapá", "Tocantins", "Alagoas",
               "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí",
               "Rio Grande do Norte", "Sergipe", "Mato Grosso", "Mato Grosso do Sul",
               "Distrito Federal", "Goiás", "São Paulo", "Rio Grande do Sul",
               "Rio de Janeiro", "Minas Gerais", "Espírito Santo", "Paraná",
               "Santa Catarina", "Acre"),
    sigla = c("RO", "RR", "AM", "PA", "AP", "TO", "AL",
              "BA", "CE", "MA", "PB", "PE", "PI",
              "RN", "SE", "MT", "MS", "DF", "GO", "SP", "RS",
              "RJ", "MG", "ES", "PR", "SC", "AC"),
    codigo = c("11", "14", "13", "15", "16", "17", "27",
               "29", "23", "21", "25", "26", "22",
               "24", "28", "51", "50", "53", "52", "35", "43",
               "33", "31", "32", "41", "42", "12"),
    stringsAsFactors = FALSE 
  )
  
  df <- df %>% 
    left_join(df_uf, by = merge_by)
  
  df$estado <- stringi::stri_unescape_unicode(df$estado)
  
  return(df)
}

obter_metricas_nacionais <- function(df){
  result <- df %>%
    group_by(SE) %>%
    summarise(
      casos = sum(casos),
      casos_est = sum(casos_est),
      pop = sum(pop)
    ) %>%
    mutate(
      inc = casos / pop * 100000,
      incest = casos_est/pop * 100000
    ) %>%
    arrange(SE)
  
  return(result)
}

obter_metricas_estaduais <- function(df){
  result <- df %>%
    mutate(codigo = floor(municipio_geocodigo/100000)) %>%
    group_by(codigo, SE) %>%
    summarise(
      casos = sum(casos),
      casos_est = sum(casos_est),
      pop = sum(pop)
    ) %>%
    mutate(
      inc = casos / pop * 100000,
      incest = casos_est/pop * 100000
    ) %>%
    arrange(codigo, SE)
  
  return(result)
}

obter_metricas_estaduais_por_ano <- function(df){
  result <- df %>%
    mutate(
      codigo = floor(municipio_geocodigo/100000),
      codigo = as.character(codigo)
    ) %>%
    group_by(codigo) %>%
    summarise(
      casos = sum(casos),
      casos_est = sum(casos_est),
      pop = sum(pop)
    ) %>%
    mutate(
      inc = casos / pop * 100000,
      incest = casos_est/pop * 100000
    ) 
  
  return(result)
}

obter_metricas_macrorregionais_por_semana <- function(df){
  result <- df %>% 
    mutate(
      codigo = floor(municipio_geocodigo/100000),
      codigo = as.character(codigo)
    ) %>%
    group_by(macroregional_id, SE) %>%
    summarise(
      casos = sum(casos),
      #casprov = sum(casprov),
      casos_est = sum(casos_est),
      pop = sum(pop),
      codigo = unique(codigo)
    ) %>%
    mutate(
      inc = casos / pop * 100000,
      #incprov = casprov / pop * 100000,
      incest = casos_est/pop * 100000
    ) %>%
    arrange(codigo, SE)
  
  return(result)
}

obter_metricas_regionais_por_semana <- function(df){
  result <- df %>% 
    mutate(
      codigo = floor(municipio_geocodigo/100000),
      codigo = as.character(codigo)
    ) %>%
    group_by(regional_id, SE) %>%
    summarise(casos = sum(casos),
              #casprov = sum(casprov),
              casos_est = sum(casos_est),
              pop = sum(pop),
              macroregional_id = unique( macroregional_id),
              codigo = unique(codigo)) %>%
    mutate(inc = casos / pop * 100000,
           #incprov = casprov / pop * 100000,
           incest = casos_est/pop * 100000) %>%
    arrange(codigo, macroregional_id, SE)
  
  return(result)
}


obter_tabela <- function(df, df_mem, se_max){
  
  # df = df_dengue
  # df_mem = memUFanual 
  # se_max = se_max_dengue
  
  df_total = df %>% obter_metricas_estaduais_por_ano()
  # colnames(UFs) <- c("estado", "codigo", "sigla" )
  
  tabela_UF <- df_mem %>% 
    select(sigla, codigo, estado, veryhigh) %>% 
    left_join(df_total, by = "codigo") %>% 
    mutate(nivelNowcast = as.numeric(incest > 1.2 * veryhigh))
  # sum(tabela_UF$nivel)  # numero de UFs acima do limiar epidemico
  
  df_estaduais <- df %>% 
    obter_metricas_estaduais()
  
  df_se <- df_estaduais %>% 
    filter(SE == se_max) %>%
    select(codigo, incest, inc) %>% 
    mutate(codigo = as.character(codigo))
  
  ## Calcula Rt por UF
  obj_df <- df_estaduais %>%
    group_by(codigo) %>%
    arrange(SE) %>% 
    data.frame() %>% 
    mutate(codigo = as.character(codigo))
  
  tabela_UF <- tabela_UF %>% 
    mutate(
      Rtmean = NA,
      secomp1 = NA,
      weekmax = NA
    )
  # sum(tabela_UF$nivel)  # numero de UFs acima do limiar epidemico
  
  
  for(i in 1:length(tabela_UF$estado)){
    df2 <- obj_df[obj_df$codigo == tabela_UF$codigo[i], ] %>%
      arrange(SE)
    df2 <- df2[df2$SE > 202400, ]
    semanas <- df2$SE[df2$incest > tabela_UF$veryhigh[i]]
    tabela_UF$selimiaralto[i] <- ifelse(length(semanas) == 0, NA, min(semanas))
    r <- Rt(obj_df[obj_df$codigo == tabela_UF$codigo[i],], count = "casos_est", 
            gtdist = "normal", meangt = 3, sdgt = 1)
    tabela_UF$Rtmean[i] <- mean(tail(r$Rt, n = 3))
    tabela_UF$secomp1[i] <- sum(tail(r$lwr, n = 3) > 1)
    df2 <- df_estaduais %>% filter(codigo == tabela_UF$codigo[i])
    tabela_UF$weekmax[i] <-  df_estaduais$SE[which.max(df2$casos_est)]
  }
  
  return(tabela_UF)
  
}

obter_pop_sob_risco <- function(df){
  df_result <- df %>% 
    group_by(sigla) %>% 
    summarise(
      pop_total = sum(pop, na.rm = T),
      pop_risco = sum(poprisco, na.rm = T),
      prop_pop_risco = pop_risco/pop_total * 100
    ) %>% 
    arrange(prop_pop_risco) 
  
  return(df_result)
}

obter_metricas_por_regional <- function(df_mem_regional_anual, df_regional_se){
  
  df_result <- df_mem_regional_anual %>%
    left_join(df_regional_se %>% 
                dplyr::select(-codigo), by = "regional_id") %>% 
    mutate(
      nivel = as.numeric(incest > (1.2 * veryhigh)),
      poprisco = nivel * populacao,
      Rtmean = NA,
      secomp1 = NA 
    )
  
  return(df_result)
  
}

obter_classificacao_das_regionais <- function(df_regional, tab_regional, se_min = 202400){
  
  obj_df_regional <- df_regional %>%
    group_by(regional_id) %>%
    arrange(SE) %>% 
    data.frame()
  
  for(i in 1:27){
    df <- obj_df_regional %>% 
      filter(codigo == tab_regional$codigo[i]) %>%
      filter(SE > se_min) %>%
      arrange(SE)
    
    semanas <- df %>% filter(incest > tab_regional$veryhigh[i]) %>% pull(SE)
    tab_regional$selimiaralto[i] <- ifelse(length(semanas) == 0, NA, min(semanas))
    
    r <- Rt(
      obj_df_regional %>% filter(codigo == tab_regional$codigo[i]),
      count = "casos_est", 
      gtdist = "normal", meangt = 3, sdgt = 1
    )
    
    tab_regional$Rtmean[i] <- mean(tail(r$Rt, n = 3))
    tab_regional$secomp1[i] <- sum(tail(r$lwr, n = 3) > 1)
    tab_regional$weekmax[i] <-  df$SE[which.max(df$casos_est)]
  }
  
  tab_regional <- tab_regional %>%
    mutate(status = case_when(
      Rtmean > 1.2 & nivel == 1 ~ "Alto em subida",
      Rtmean >= 0.9 & Rtmean < 1.2 & nivel == 1 ~ "Alto estável",
      Rtmean < 0.9 & nivel == 1 ~ "Alto em queda",
      nivel == 0 ~ "Baixo ou moderado",
      TRUE ~ "Não se aplica"
    ),
    status = ifelse(is.na(status), "Não se aplica", status),
    status = factor(status, levels = c("Alto em subida", "Alto estável", "Alto em queda", "Baixo ou moderado", "Não se aplica"))
    )
  
  return(tab_regional)
}

## Graficos

gg_bar_pop_risco <- function(df, por_regiao = T){
  
  g_chart <- df %>%
    mutate(regiao = case_when(
      sigla %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO") ~ "Norte",
      sigla %in% c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA") ~ "Nordeste",
      sigla %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
      sigla %in% c("SP", "RJ", "ES", "MG") ~ "Sudeste",
      sigla %in% c("PR", "SC", "RS") ~ "Sul",
      TRUE ~ "Desconhecida" 
    )) %>% 
    ggplot(aes(x = sigla, y = prop_pop_risco)) +
    geom_bar(stat = "identity", fill = "orange") +
    geom_hline(yintercept = 50, color = "red", linetype = "dashed") +
    labs(title = "População em alto risco por estado", y = "%", x = "") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if(por_regiao == T){
    g_chart <- g_chart + 
      facet_wrap(~regiao, ncol = 2, scales = "free_x")
  }
  
  return(g_chart)
  
}

gg_map_risco_classif <- function(shape){
  
  g_map <- shape %>% 
    ggplot() +
    geom_sf(aes(fill = status), show.legend=TRUE) +
    scale_fill_manual(
      values = c("#c93232", "#f2bd35", "yellow2","#acdb69", "#fefefe"),
      drop = FALSE) +
    labs(fill = "Rt <= 1") +
    theme_minimal() 
  
  return(g_map)
}

gg_timeline_dots <- function(df){
  df <- df_dengue_chik_estados 
  g_chart <- df %>% 
    mutate(
      ano = as.numeric(ano),
      data = ifelse(ano ==  max(ano), max(ano), paste0(min(ano),"-",max(ano)-1)),
      ano = as.factor(ano)
    ) %>% 
    ggplot(aes(x = reorder(sigla, inc_acumulada, max), y = inc_acumulada)) +
    geom_point(aes(color = data), size =  4) + 
    scale_color_manual(values = c('grey70', 'red'))+
    theme(
      panel.background = element_blank(),
      panel.grid.major.y  = element_line(linetype = "dotted",color = "grey", linewidth = 0.5),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16,hjust = 0.5),
      legend.position = "bottom",
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      strip.text.x = element_text(size = 18, colour = "black"), 
      strip.background = element_rect(fill = "white")
    ) +
    labs(title = "",
         y = "Incidência por 100 mil habitantes",
         x = "",
         color = "") +
    facet_wrap(~arbovirose, ncol = 1, scales = "free")
  
  return(g_chart)
}

gg_inc_dengue_chikv <- function(df){
  df <- df_inc_dengue_chik
  
  df %>% 
    ggplot() + 
    geom_bar(stat = "identity", aes(x = data, y = inc,fill = "Casos"))+
    geom_line(aes(x = data, y = inc_est, colour =  "Estimativa Corrigida")) +
    scale_x_continuous(
      breaks = c(min(df$data), min(df$data) + 21 ,min(df$data) + 42, min(df$data) + 63,max(df$data)), 
      labels = c(paste0(str_sub(unique(df$SE)[1], start  = -2L),"/",str_sub(unique(df$SE)[1], start  = 3L, end  = 4L)),
                 paste0(str_sub(unique(df$SE)[4], start  = -2L),"/",str_sub(unique(df$SE)[4], start  = 3L, end  = 4L)),
                 paste0(str_sub(unique(df$SE)[7], start  = -2L),"/",str_sub(unique(df$SE)[7], start  = 3L, end  = 4L)),
                 paste0(str_sub(unique(df$SE)[10], start  = -2L),"/",str_sub(unique(df$SE)[10], start  = 3L, end  = 4L)),
                 paste0(str_sub(unique(df$SE)[13], start  = -2L),"/",str_sub(unique(df$SE)[13], start  = 3L, end  = 4L)))
    ) +
    scale_fill_manual(values = "lightblue") +
    scale_colour_manual(values = "darkblue") +
    theme_light() +
    theme(
      legend.title = element_blank(), legend.position = "bottom",
      legend.text = element_text(size = 14),
      axis.title = element_text(size = 18),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      strip.text.x = element_text(size = 14, colour = "black"), strip.background = element_rect(fill = "white")
    ) +
    labs(
      x = "SE/Ano",
      y = "Incidência (dengue + chikungunya) por 100 mil hab.") +
    facet_geo(~sigla, grid = "br_states_grid1", scales = "free_y")
}

verificar_tendencia_regressao <- function(df) {
  
  tempos <- 1:(ncol(df)-1)
  
  calcular_tendencia <- function(valores) {
    dados <- data.frame(tempo = tempos, valor = valores)
    
    modelo <- lm(valor ~ tempo, data = dados)
    
    coef_angular <- coef(modelo)[2]
    
    if (coef_angular > 0) {
      return("Crescente")
    } else if (coef_angular < 0) {
      return("Decrescente")
    } else {
      return("Estável")
    }
  }
  
  df$tendencia <- apply(df[ , 2:ncol(df)], 1, calcular_tendencia)
  return(df)
}

adicionar_regiao <- function(df){
  estados_regioes <- data.frame(
    sigla = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", 
              "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", 
              "SE", "SP", "TO"),
    regiao = c("Norte", "Nordeste", "Norte", "Norte", "Nordeste", "Nordeste", "Centro-Oeste", 
               "Sudeste", "Centro-Oeste", "Nordeste", "Sudeste", "Centro-Oeste", "Centro-Oeste", 
               "Norte", "Nordeste", "Nordeste", "Nordeste", "Sul", "Sudeste", "Nordeste", 
               "Norte", "Norte", "Sul", "Sul", "Nordeste", "Sudeste", "Norte")
  )
  
  df <- df %>% 
    left_join(estados_regioes, by = "sigla")
  
  return(df)
}

construir_tabela_incidencia_por_semana <- function(df, var = "inc"){
  
  df <- df %>% 
    filter(as.numeric(substr(SE, 1, 4)) == ano_selecionado) %>% 
    filter(SE >= max(SE) - 3) %>% 
    arrange(SE) %>% 
    dplyr::select(codigo, SE, var) %>% 
    pivot_wider(names_from = SE, values_from = var) %>% 
    janitor::clean_names() %>% 
    mutate(codigo = as.character(codigo))
  
  return(df)
  
}


gerar_tabela_tendencia <- function(df, inc_obs_max = 10, inc_est_max = 10){
  
  df <- df %>% 
    janitor::clean_names() %>% 
    tibble()
  
  semana_labels <- colnames(df)[3:6]
  semana_labels <- substr(semana_labels, 6, 7)
  
  colnames(df) <- c("Regiao", "Sigla", 
                    "S1_A", "S2_A", "S3_A", "S4_A", "tendencia_A", 
                    "S1_B", "S2_B", "S3_B", "S4_B", "tendencia_B")
  
  tabela <- df %>% 
    gt(groupname_col = "Regiao") %>% 
    tab_options(table.font.size = 11) %>% 
    data_color(
      columns = 3:6,
      colors = scales::col_numeric(alpha = T,
                                   na.color = "#ffffff",
                                   c("#cffcb6",  "#f9fbc6", "#ffd0b5", "#ffc3c3"), 
                                   domain = range(0, inc_obs_max))
    ) %>% 
    data_color(
      columns = 8:11,
      colors = scales::col_numeric(alpha = T,
                                   na.color = "#ffffff",
                                   c("#cffcb6",  "#f9fbc6", "#ffd0b5", "#ffc3c3"), 
                                   domain = range(0, inc_est_max))
    ) %>% 
    cols_label(
      Regiao = "Região",
      Sigla = "Sigla",
      S1_A = semana_labels[1],
      S2_A = semana_labels[2],
      S3_A = semana_labels[3], 
      S4_A = semana_labels[4],
      tendencia_A = "Tendência",
      S1_B = semana_labels[1],
      S2_B = semana_labels[2],
      S3_B = semana_labels[3], 
      S4_B = semana_labels[4],
      tendencia_B = "Tendência"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = list(cell_fill(color = "#ebebeb"), cell_text(weight = "bold")),
      locations = cells_row_groups()
    ) %>% 
    fmt_number(
      columns = c(3:6, 8:11),
      decimals = 2
    ) %>% 
    cols_align(align = "center") %>% 
    tab_spanner(columns = 3:6, md("**Incidência observada**")) %>% 
    tab_spanner(columns = 8:11, md("**Incidência estimada**")) %>% 
    tab_style(
      style = cell_text(weight = "bold", color = "darkgreen"),
      locations = cells_body(
        columns = "tendencia_A", 
        rows = tendencia_A == "Decrescente"
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = "red"),
      locations = cells_body(
        columns = "tendencia_A", 
        rows = tendencia_A == "Crescente"
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = "black"),
      locations = cells_body(
        columns = "tendencia_A", 
        rows = tendencia_A == "Estável"
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold", color = "darkgreen"),
      locations = cells_body(
        columns = "tendencia_B", 
        rows = tendencia_B == "Decrescente"
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = "red"),
      locations = cells_body(
        columns = "tendencia_B", 
        rows = tendencia_B == "Crescente"
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = "black"),
      locations = cells_body(
        columns = "tendencia_B", 
        rows = tendencia_B == "Estável"
      )
    )
  
  return(tabela)
}


gerar_grafico_inc <- function(df){
  
  chart <-  df %>% 
    ggplot(aes(x = semana)) +
    geom_ribbon(aes(ymin = 0, ymax = P25, group = 1), fill = "#cffcb6") + 
    geom_ribbon(aes(ymin = P25, ymax = P50, group = 1), fill = "#f6fa9d") + 
    geom_ribbon(aes(ymin = P50, ymax = P75, group = 1), fill = "#fcb48b") + 
    geom_ribbon(aes(ymin = P75, ymax = P90, group = 1), fill = "#fa9393") +
    geom_ribbon(aes(ymin = P90, ymax = Inf, group = 1), fill = "#ff5959", alpha = 0.5) +
    geom_line(aes(y = ifelse(P25 > 0, P25, NA), group = 1), linetype = "dashed", color = "darkgreen", size = 0.5) + 
    geom_line(aes(y = P50, group = 1), linetype = "dashed",color = "gold", size = 0.5) +                       
    geom_line(aes(y = P75, group = 1), linetype = "dashed", color = "darkorange", size = 0.5) + 
    geom_line(aes(y = P90, group = 1), linetype = "dashed", color = "darkred", size = 0.5) +
    geom_line(data = df_modelo_dengue_nacional_atual, aes(y = inc, group = 1), color = "black", size = 0.75) +
    scale_x_continuous(breaks = seq(0, 52, by = 4)) + 
    theme_minimal() +                                         
    theme(
      legend.position = "top",                       
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = "Epiweek",
      y = "Incidência observada de dengue (por 100 mil hab.)"
    )
  
  return(chart)
}
