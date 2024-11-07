obter_siglas_codigos <- function(df, merge_by = "codigo"){
  
  df_uf = data.frame(
    regiao = c("Norte", "Norte", "Norte", "Norte", "Norte", "Norte", "Nordeste",
               "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste",
               "Nordeste", "Nordeste", "Centro-Oeste", "Centro-Oeste", "Centro-Oeste", "Centro-Oeste", "Sudeste", "Sul",
               "Sudeste", "Sudeste", "Sudeste", "Sul", "Sul", "Norte"),
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
    group_by(arbovirose, SE) %>%
    summarise(
      casos = sum(casos),
      casos_est = sum(casos_est),
      pop = sum(pop)
    ) %>%
    ungroup() %>% 
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
    ungroup() %>% 
    mutate(
      inc = casos / pop * 100000,
      incest = casos_est/pop * 100000
    ) %>%
    arrange(codigo, SE) #%>% 
  # group_by(codigo) %>% 
  # mutate(
  #   rt_1 = ifelse(lag(casos) > 0, casos/lag(casos), 0),
  #   rt_1_est = ifelse(lag(casos_est) > 0, casos_est/lag(casos_est), 0),
  #   rt_1 = ifelse(is.na(rt_1), 0, rt_1),
  #   rt_1_est = ifelse(is.na(rt_1_est), 0, rt_1_est)
  # ) %>% 
  # ungroup()
  
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
  
  # df <- df_dengue_chik %>% filter(arbovirose == "Dengue")
  
  result <- df %>%
    dplyr::select(regional_id, regional, macroregional_id, macroregional, SE, casos, casos_est, pop) %>%
    group_by(regional_id, SE) %>%
    reframe(
      casos = sum(casos),
      casos_est = sum(casos_est),
      pop = sum(pop),
      macroregional_id = unique(macroregional_id),
      regional = unique(regional),
      macroregional = unique(macroregional)
    ) %>%
    mutate(
      inc = casos / pop * 100000,
      incest = casos_est/pop * 100000,
      codigo = substr(macroregional_id, 1, 2),
      codigo =  as.character(codigo)
    ) %>%
    arrange(regional_id, macroregional_id, SE)
  
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
  
  # i = 11
  for(i in unique(tabela_UF$codigo)){
    
    pos = grep(i, tabela_UF$codigo)
    
    df2 <- obj_df %>% 
      filter(codigo == i) %>%
      arrange(SE)
    
    df2 <- df2 %>% filter(SE > 202400)
    semanas <- df2 %>% 
      filter(incest > tabela_UF %>% 
               filter(codigo == i) %>%
               pull(veryhigh)
      ) %>%
      pull(SE)
    
    tabela_UF$selimiaralto[pos] <- ifelse(length(semanas) == 0, NA, min(semanas))
    
    r <- Rt(obj_df[obj_df$codigo == tabela_UF$codigo[pos],], count = "casos_est", 
            gtdist = "normal", meangt = 3, sdgt = 1)
    
    tabela_UF$Rtmean[pos] <- mean(tail(r$Rt, n = 3))
    tabela_UF$secomp1[pos] <- sum(tail(r$lwr, n = 3) > 1)
    
    df2 <- df_estaduais %>% filter(codigo == tabela_UF$codigo[pos])
    tabela_UF$weekmax[pos] <-  df_estaduais$SE[which.max(df2$casos_est)]
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

obter_rt <- function(df, count_var, dist, mean, sd){
  
  result <- Rt(
    df,
    count = count_var, 
    gtdist = dist, meangt = mean, sdgt = sd
  )
  
  return(result)
}

# Revisar obter_classificacao_das_regionais
obter_classificacao_das_regionais <- function(df_regional, tab_regional){
  
  # df_regional = df_dengue_reg
  # tab_regional = tabela_reg_dengue
  
  obj_df_regional <- df_regional %>%
    arrange(regional_id, SE, codigo) %>% 
    data.frame() 
  
  tabela_reg_dengue = tabela_reg_dengue %>%
    arrange(regional_id, codigo) 
  
  se_min = max(obj_df_regional$SE) - 3
  
  se_min = 202400
  obj_df_regional <- obj_df_regional %>%
    filter(SE > se_min)
  
  tab_regional_final <- data.frame()
  for(cod in unique(obj_df_regional$codigo)){
    
    df_temp <- obj_df_regional %>% 
      filter(codigo == cod) 
    
    temp_tab_regional <- tab_regional %>% 
      filter(codigo == cod) %>% 
      dplyr::select(regional_id, veryhigh)
    
    if(nrow(df_temp) > 0 & nrow(temp_tab_regional) > 0){
      
      df_temp <- df_temp %>% 
        left_join(temp_tab_regional, by = "regional_id")
      
      semanas <- df_temp %>% 
        filter(incest > veryhigh) %>% 
        pull(SE)
      
      tab_regional$selimiaralto <- ifelse(length(semanas) == 0, NA, min(semanas))
      
      r <- Rt(
        df_temp,
        count = "casos_est", 
        gtdist = "normal", meangt = 3, sdgt = 1
      )
      
      temp_tab_regional <- tab_regional %>% 
        filter(codigo == cod) %>% 
        mutate(
          Rtmean = mean(tail(r$Rt, n = 3)),
          secomp1 = sum(tail(r$lwr, n = 3) > 1),
          weekmax =  df_temp$SE[which.max(df_temp$casos_est)]
        )
      
      tab_regional_final <- rbind(tab_regional_final, temp_tab_regional) 
      
    }
    
  }
  
  tab_regional_final <- tab_regional_final %>%
    mutate(status = case_when(
      Rtmean > 1.2 & nivel == 1 ~ "Alto em subida",
      Rtmean >= 0.9 & Rtmean < 1.2 & nivel == 1 ~ "Alto estável",
      Rtmean < 0.9 & nivel == 1 ~ "Alto em queda",
      nivel == 0 ~ "Baixo ou moderado",
      TRUE ~ NA # "Não se aplica"
    ),
    # status = ifelse(is.na(status), "Não se aplica", status),
    status = factor(status, levels = c("Alto em subida", "Alto estável", "Alto em queda", "Baixo ou moderado")) #, "Não se aplica"
    )
  
  return(tab_regional_final)
}


obter_rt_por_uf <- function(codigo_id, df_uf, tabela_uf, se_min = 202400) {
  
  temp_tabela_uf <- tabela_uf %>%
    filter(codigo == codigo_id)
  
  temp_obj <- df_uf %>%
    filter(codigo == codigo_id) %>%
    arrange(SE) %>% 
    data.frame()
  
  if(nrow(temp_obj) > 0 & nrow(temp_tabela_uf) > 0){
    
    temp_obj2 <- temp_obj %>%
      filter(SE > se_min)
    
    semanas <- temp_obj2 %>%
      filter(incest > temp_tabela_uf$veryhigh) %>%
      pull(SE)
    
    temp_tabela_uf$selimiaralto <- ifelse(length(semanas) == 0, NA, min(semanas, na.rm = T))
    
    r <- Rt(temp_obj, count = "casos_est", 
            gtdist = "normal", meangt = 3, sdgt = 1)
    
    temp_tabela_uf$Rtmean <- mean(tail(r$Rt, n = 3))
    temp_tabela_uf$secomp1 <- sum(tail(r$lwr, n = 3) > 1)
    
    temp_tabela_uf$weekmax <- temp_obj %>%
      slice_max(casos_est, n = 1) %>%
      slice(n()) %>% 
      pull(SE)
    
    temp_tabela_uf <- temp_tabela_uf %>%
      mutate(status = case_when(
        Rtmean > 1.2 & nivelNowcast == 1 ~ "Alto em subida",
        Rtmean >= 0.9 & Rtmean < 1.2 & nivelNowcast == 1 ~ "Alto estável",
        Rtmean < 0.9 & nivelNowcast == 1 ~ "Alto em queda",
        nivelNowcast == 0 ~ "Baixo ou moderado",
        TRUE ~ NA # "Não se aplica"
      ),
      # status = ifelse(is.na(status), "Não se aplica", status),
      status = factor(status, levels = c("Alto em subida", "Alto estável", "Alto em queda", "Baixo ou moderado")),
      tendencia = case_when(
        Rtmean > 1.2 ~ "Crescente",
        Rtmean > 1.1 & Rtmean <= 1.2 ~ "Crescente leve",
        Rtmean > 1 & Rtmean <= 1.1 ~ "Estável",
        Rtmean <= 1 ~ "Descrescente",
        TRUE ~ NA # "Não se aplica"
      ),
      tendencia = factor(tendencia, levels = c("Crescente", "Crescente leve", "Estável", "Descrescente"))
      )
    
    return(temp_tabela_uf)
    
  }
  
}

obter_rt_por_macroregiao <- function(macro_id, df_macro, tabela_macro, se_min = 202400) {
  
  # macro_id = unique(df_dengue_macro_se$macroregional_id)[1]
  # df_macro = df_dengue_macro_se
  # tabela_macro = tabela_dengue_macro
  # se_min = 202400
  
  temp_tabela_macro <- tabela_macro %>%
    filter(macroregional_id == macro_id)
  
  temp_obj <- df_macro %>%
    filter(macroregional_id == macro_id) %>%
    arrange(SE) %>% 
    data.frame()
  
  if(nrow(temp_obj) > 0 & nrow(temp_tabela_macro) > 0){
    
    temp_obj2 <- temp_obj %>%
      filter(SE > se_min)
    
    semanas <- temp_obj2 %>%
      filter(incest > temp_tabela_macro$veryhigh) %>%
      pull(SE)
    
    temp_tabela_macro$selimiaralto <- ifelse(length(semanas) == 0, NA, min(semanas, na.rm = T))
    
    r <- Rt(temp_obj, count = "casos_est", 
            gtdist = "normal", meangt = 3, sdgt = 1)
    
    temp_tabela_macro$Rtmean <- mean(tail(r$Rt, n = 3))
    temp_tabela_macro$secomp1 <- sum(tail(r$lwr, n = 3) > 1)
    
    temp_tabela_macro$weekmax <- temp_obj %>%
      slice_max(casos_est, n = 1) %>%
      slice(n()) %>% 
      pull(SE)
    
    temp_tabela_macro <- temp_tabela_macro %>%
      mutate(
        status = case_when(
          Rtmean > 1.2 & nivelNowcast == 1 ~ "Alto em subida",
          Rtmean >= 0.9 & Rtmean < 1.2 & nivelNowcast == 1 ~ "Alto estável",
          Rtmean < 0.9 & nivelNowcast == 1 ~ "Alto em queda",
          nivelNowcast == 0 ~ "Baixo ou moderado",
          TRUE ~ NA # "Não se aplica"
        ),
        status = factor(status, levels = c("Alto em subida", "Alto estável", "Alto em queda", "Baixo ou moderado")),
        tendencia = case_when(
          Rtmean > 1.2 ~ "Crescente",
          Rtmean > 1.1 & Rtmean <= 1.2 ~ "Crescente leve",
          Rtmean > 1 & Rtmean <= 1.1 ~ "Estável",
          Rtmean <= 1 ~ "Descrescente",
          TRUE ~ NA # "Não se aplica"
        ),
        tendencia = factor(tendencia, levels = c("Crescente", "Crescente leve", "Estável", "Descrescente"))
      )
    
    return(temp_tabela_macro)
  }
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

obter_mapa_risco_classif <- function(shape_mun, shape_state){
 
  g_map <- shape_mun %>% 
    ggplot() +
    geom_sf(aes(fill = status), color = "#ababab", show.legend=TRUE) +
    scale_fill_manual(
      values = c("#c93232", "#f2bd35", "yellow2", "#fefefe"),
      na.translate = F,
      # na.value = "#fefefe",
      drop = FALSE) +
    geom_sf(data = shape_state, fill = NA, color = "#4d4d4d", size = 3) +
    labs(fill = "Rt <= 1") +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      panel.grid.major.y  = element_line(linetype = "dotted",color = "grey", linewidth = 0.5),
      plot.title = element_text(size = 20),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18, hjust = 0.5),
      legend.position = "top",
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      strip.text.x = element_text(size = 18, colour = "black"), 
      strip.background = element_rect(fill = "white")
    )
  
  return(g_map)
}

gg_timeline_dots <- function(df, se_max = se_max){
  
  order <- df %>% 
    arrange(desc(inc)) %>% 
    distinct(sigla) %>% pull()
  
  df <- df %>% 
    mutate(sigla = factor(sigla, levels = order))
  
  g_chart <- ggplot() +
    geom_point(
      data = df %>% 
        filter(grupo == levels(df$grupo)[1]),
      color = 'grey70', size = 4,
      aes(x = sigla, y = inc), show.legend = T
    ) + 
    geom_point(
      data = df %>% 
        filter(grupo == levels(df$grupo)[2]),
      color = 'red', size = 3,
      aes(x = sigla, y = inc), show.legend = T
    ) + 
    geom_point(
      data = df %>% 
        filter(grupo == levels(df$grupo)[3]),
      color = 'orange', size = 2,
      aes(x = sigla, y = inc), show.legend = T
    ) + 
    theme(
      panel.background = element_blank(),
      panel.grid.major.y  = element_line(linetype = "dotted",color = "grey", linewidth = 0.5),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12,hjust = 0.5),
      legend.position = "bottom",
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(12),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      strip.text.x = element_text(size = 16, colour = "black"), 
      strip.background = element_rect(fill = "white")
    ) +
    labs(title = "",
         y = "Incidência por 100 mil habitantes",
         x = "",
         color = "")
  
  return(g_chart)
}

gg_inc_dengue_chikv <- function(df){
  
  df %>% 
    ggplot(aes(x = data, y = inc, fill = "Casos")) + 
    geom_bar(stat = "identity")+
    geom_line(aes(x = data, y = inc_est, colour =  "Estimativa Corrigida")) +
    scale_x_date(
      breaks = "1 month",
      minor_breaks = "1 week",
      date_labels = "%b\n%Y"
    ) +
    scale_fill_manual(values = "lightblue") +
    scale_colour_manual(values = "darkblue") +
    theme_light() +
    theme(
      legend.title = element_blank(), 
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 18),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      strip.text.x = element_text(size = 14, colour = "black"),
      strip.background = element_rect(fill = "white")
    ) +
    labs(
      x = "Mês/Ano",
      y = "Incidência (dengue + chikungunya) por 100 mil hab.") +
    facet_geo(~sigla, grid = "br_states_grid1", scales = "free_y")
}

add_arrows <- function(x, ref = 0){
  icon = ifelse(
    x > ref, "up", #&#129093 arrow-up
    ifelse(x < ref, 
           "down", #&#129095 arrow-down
           "right") #&#129094 
  )
  return(icon)
}

verificar_tendencia_regressao <- function(df) {
  
  tempos <- 1:(ncol(df)-1)
  
  calcular_tendencia <- function(valores) {
    dados <- data.frame(tempo = tempos, valor = valores)
    
    modelo <- lm(valor ~ tempo, data = dados)
    
    coef_angular <- coef(modelo)[2]
    
    return(add_arrows(coef_angular, ref = 0))
    
    # if (coef_angular > 0) {
    #   return("Crescente")
    # } else if (coef_angular < 0) {
    #   return("Decrescente")
    # } else {
    #   return("Estável")
    # }
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
  
  # df <- tabela_tendencia_por_uf_inc_observada_dengue %>%
  # left_join(tab_rt_uf %>% 
  #             filter(arbovirose == "Dengue"),
  #           by = "sigla") %>% 
  #   relocate(Rtmean, .after = sigla) %>% 
  #   dplyr::select(-arbovirose) %>% 
  #   bind_cols(tabela_tendencia_por_uf_inc_estimada_dengue %>% 
  #               dplyr::select(-c(regiao, sigla)))
  # df <- tab_tendencia_por_uf_inc_observada_dengue
  
  df <- df %>% 
    janitor::clean_names() %>% 
    tibble()
  
  semana_labels <- colnames(df)[4:12] #[4:7]
  semana_labels <- substr(semana_labels, 6, 7)
  
  colnames(df) <- c("Regiao", "UF", "Rt", "tendencia",
                    "S1_A", "S2_A", "S3_A", "S4_A", #"tendencia_A", 
                    "S1_B", "S2_B", "S3_B", "S4_B") #,"tendencia_B"
  
  tabela <- df %>% 
    gt(groupname_col = "Regiao") %>% #, row_group_as_column = TRUE
    data_color(
      columns = 5:8, #4:7
      colors = scales::col_numeric(alpha = T,
                                   na.color = "#ffffff",
                                   c("#cffcb6",  "#f9fbc6", "#ffd0b5", "#ffc3c3"), 
                                   domain = range(0, inc_obs_max))
    ) %>% 
    data_color(
      columns = 8:12, #9:12
      colors = scales::col_numeric(alpha = T,
                                   na.color = "#ffffff",
                                   c("#cffcb6",  "#f9fbc6", "#ffd0b5", "#ffc3c3"), 
                                   domain = range(0, inc_est_max))
    ) %>% 
    cols_label(
      Regiao = "Região",
      UF = "UF",
      Rt = "Rt",
      tendencia= "",
      S1_A = semana_labels[1],
      S2_A = semana_labels[2],
      S3_A = semana_labels[3], 
      S4_A = semana_labels[4],
      S1_B = semana_labels[1],
      S2_B = semana_labels[2],
      S3_B = semana_labels[3], 
      S4_B = semana_labels[4]#,
      # tendencia_B = ""
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
      columns = c(5:12), #4:7, 9:12
      decimals = 1
    ) %>% 
    cols_align(align = "center") %>% 
    tab_spanner(columns = 5:8, md("**Incidência observada**")) %>% 
    tab_spanner(columns = 9:12, md("**Incidência estimada**")) %>% 
    text_transform(
      locations = cells_body(columns = c(4)), 
      fn = function(x) {
        dplyr::case_when(
          x == "up" ~ md("{\\color{dark_red} \\uparrow}"),   # Seta para cima (vermelho)
          x == "down" ~ md("{\\color{dark_green} \\downarrow}"), # Seta para baixo (verde)
          x == "right" ~ md("{\\color{dark_blue} \\rightarrow}")  # Seta para a direita (dourado)
        )
      }
    )
  
  return(tabela)
}

gerar_tabela_tendencia_uf <- function(df){
  
  df <- tabela_dengue_chik_uf
  
  df <- df %>% 
    tibble() %>%
    arrange(regiao, arbovirose) %>%
    group_by(regiao, arbovirose) %>%
    mutate(
      show_arbovirose = if_else(row_number() == 1, arbovirose, ""),
      show_estado = if_else(row_number() > 1 & estado == lag(estado), "", estado)
    ) %>% 
    ungroup()
  
  tabela <- df %>%
    dplyr::select(-c(show_arbovirose, show_estado)) %>% 
    gt(groupname_col = "regiao") %>%
    cols_label(
      regiao = md("**Região**"),
      arbovirose = "",
      estado = md("**Estado**"),
      Rtmean = md("**Rt**"),
      tendencia = md("**Tendência**")
    ) %>%
    cols_align(align = "center") %>% 
    tab_style(
      style = list(cell_fill(color = "#bfbfbf"), cell_text(weight = "bold")),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = list(cell_fill(color = "#dbdbdb"), cell_text(weight = "bold")),
      locations = cells_row_groups()
    ) %>%
    fmt_number(
      columns = c(4), # Ajuste conforme necessário
      decimals = 2
    ) %>%
    text_transform(
      locations = cells_body(columns = c(arbovirose)),
      fn = function(x) df$show_arbovirose
    ) %>% 
    text_transform(
      locations = cells_body(columns = c(estado)),
      fn = function(x) df$show_estado
    ) %>% 
    tab_style(
      style = list(cell_fill(color = "#ebebeb")),
      locations = cells_body(
        rows = arbovirose == "Chikungunya"
      )
    ) %>%
    tab_style(
      style = list(cell_text(align = "left")),
      locations = cells_body(columns = c(arbovirose))
    ) %>% 
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "black",
        weight = px(2)
      ),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      ),
      locations = cells_body(rows = nrow(df))
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "top",
        color = "black",
        weight = px(3)
      ),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_row_groups()
    )

  return(tabela)
}

gerar_tabela_tendencia_macro <- function(df){
  
  df <- df %>% 
    tibble() %>%
    arrange(regiao, arbovirose) %>%
    group_by(regiao, arbovirose) %>%
    mutate(
      show_arbovirose = if_else(row_number() == 1, arbovirose, ""),
      show_estado = if_else(row_number() > 1 & estado == lag(estado), "", estado)
      ) %>% 
    ungroup()
  
  tabela <- df %>%
    dplyr::select(-c(show_arbovirose, show_estado)) %>% 
    gt(groupname_col = "regiao") %>%
    cols_label(
      regiao = md("**Região**"),
      arbovirose = "",
      estado = md("**Estado**"),
      macroregional = md("**Macroregional**"),
      Rtmean = md("**Rt**"),
      tendencia = md("**Tendência**")
    ) %>%
    cols_align(align = "center") %>% 
    tab_style(
      style = list(cell_fill(color = "#bfbfbf"), cell_text(weight = "bold")),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = list(cell_fill(color = "#dbdbdb"), cell_text(weight = "bold")),
      locations = cells_row_groups()
    ) %>%
    fmt_number(
      columns = c(5), # Ajuste conforme necessário
      decimals = 2
    ) %>%
    text_transform(
      locations = cells_body(columns = c(arbovirose)),
      fn = function(x) df$show_arbovirose
    ) %>% 
    text_transform(
      locations = cells_body(columns = c(estado)),
      fn = function(x) df$show_estado
    ) %>% 
    tab_style(
      style = list(cell_fill(color = "#ebebeb")),
      locations = cells_body(
        rows = arbovirose == "Chikungunya"
      )
    ) %>%
    tab_style(
      style = list(cell_text(align = "left")),
      locations = cells_body(columns = c(arbovirose))
    ) %>% 
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "black",
        weight = px(2)
      ),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      ),
      locations = cells_body(rows = nrow(df))
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "top",
        color = "black",
        weight = px(3)
      ),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_row_groups()
    ) %>% 
    as_latex()
  
  
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

get_map_mem_incidencia <- function(df_uf_inc, df_uf_mem, df_limiar_mem, max_se = 52){
  
  # df_uf_inc = df_dengue_uf
  # df_uf_mem = memUFsazonal
  # df_limiar_mem = memUFanual
  max_se = as.numeric(max_se)
  
  df_mem_uf_sazonal <- df_uf_mem %>% 
    rename(semana = SEe) %>% 
    dplyr::select(sigla, semana, preseason, epidemic, posseason) %>% 
    filter(semana >= (max_se - 10) & semana <= (max_se + 5))
  
  df_uf_ano_anterior <- df_uf_inc %>%
    filter(ano == max(ano, na.rm = T) - 1) %>% 
    dplyr::select(codigo, semana, inc, incest) %>% 
    set_names(c("codigo", "semana", "inc_ano_anterior", "inc_est_ano_anterior")) %>% 
    mutate(
      semana = as.numeric(semana),
      id = paste(codigo, semana, sep = "_")
    ) %>% 
    filter(semana >= (max_se - 10) & semana <= (max_se + 5)) 
  
  df_uf_ano_atual <- df_uf_inc %>%
    filter(ano == max(ano, na.rm = T)) %>% 
    dplyr::select(codigo, semana, inc, incest) %>% 
    set_names(c("codigo", "semana", "inc_ano_atual", "inc_est_ano_atual")) %>% 
    mutate(
      semana = as.numeric(semana),
      id = paste(codigo, semana, sep = "_")
    ) %>% 
    filter(semana >= (max_se - 10) & semana <= (max_se + 5)) %>% 
    dplyr::select(-c(codigo, semana))
  
  df_uf <- df_uf_ano_anterior %>% 
    left_join(df_uf_ano_atual, by = "id") %>% 
    mutate(
      codigo = as.character(codigo),
      semana = as.numeric(semana)
    ) %>% 
    obter_siglas_codigos(merge_by = "codigo") %>% 
    left_join(df_limiar_mem %>% 
                dplyr::select(codigo, veryhigh), by = "codigo") %>% 
    group_by(sigla) %>% 
    mutate(
      veryhigh = ifelse(
        veryhigh > 2 * max(inc_est_ano_anterior, na.rm = T) |
          veryhigh > 2 * max(inc_est_ano_atual, na.rm = T), NA_real_, veryhigh            
      )
    )
  
  g_map <- df_uf %>% 
    ggplot(aes(x = semana)) + 
    geom_hline(aes(yintercept = veryhigh), color = "black", linewidth = 1, linetype = "dashed")  +
    geom_ribbon(data = df_mem_uf_sazonal, mapping = aes(x = semana, ymin = preseason, ymax = posseason), fill = 'orange', alpha = 0.5) + 
    geom_ribbon(data = df_mem_uf_sazonal, mapping = aes(x = semana, ymin = preseason, ymax = epidemic), fill = 'yellow', alpha = 0.5) + 
    geom_ribbon(data = df_mem_uf_sazonal, mapping = aes(x = semana, ymin = preseason, ymax = epidemic), fill = 'gray', alpha = 0.5) +
    geom_line(aes(y = inc_est_ano_anterior), linewidth = 0.75, color = "blue", show.legend = T) +
    # geom_line(aes(y = inc_ano_anterior), linetype = "dashed", linewidth = 0.5, color = "darkblue", show.legend = T) +
    geom_line(aes(y = inc_est_ano_atual), linewidth = 0.75, color = "red", show.legend = T) +
    geom_line(aes(y = inc_ano_atual), linetype = "dashed", linewidth = 0.5, color = "darkred", show.legend = T) +
    theme_light() +
    theme(
      legend.title = element_blank(), 
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 18),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      strip.text.x = element_text(size = 14, colour = "black"),
      strip.background = element_rect(fill = "white")
    ) +
    labs(title = "", y = "Incidência", x = "Semana epidemiológica") +
    facet_geo(~sigla, grid = "br_states_grid1", scales = "free_y") 
  
  return(g_map)
  
}

get_map_mem_incidencia_br <- function(df_uf_inc, df_uf_mem, df_limiar_mem, max_se = 52){
  
  # df_uf_inc = df_chik_nacional
  # df_uf_mem = memBR_dengue_chik %>% filter(arbovirose == "Chikungunya")
  # df_limiar_mem = thresholds.tab
  # max_se = ultima_semana_chik
  max_se = as.numeric(max_se)
  
  df_mem_uf_sazonal <- df_uf_mem %>% 
    rename(semana = SEe) %>% 
    dplyr::select(semana, preseason, epidemic, posseason) 
  
  df_uf_ano_anterior <- df_uf_inc %>%
    mutate(
      ano = substr(SE, 1, 4),
      ano = as.numeric(ano),
      semana = substr(SE, 5, 6),
      semana = as.numeric(semana)
    ) %>% 
    filter(ano == (max(ano, na.rm = T) - 1)) %>% 
    dplyr::select(semana, inc, incest) %>% 
    set_names(c("semana", "inc_ano_anterior", "inc_est_ano_anterior"))
  
  df_uf_ano_atual <- df_uf_inc %>%
    mutate(
      ano = substr(SE, 1, 4),
      ano = as.numeric(ano),
      semana = substr(SE, 5, 6),
      semana = as.numeric(semana)
    ) %>% 
    filter(ano == max(ano, na.rm = T)) %>% 
    dplyr::select(semana, inc, incest) %>% 
    set_names(c("semana", "inc_ano_atual", "inc_est_ano_atual"))
  
  df_uf <- df_uf_ano_anterior %>% 
    left_join(df_uf_ano_atual, by = "semana") 
  
  veryhigh <- df_limiar_mem$veryhigh
  veryhigh <- ifelse(
    veryhigh > 2 * max(df_uf$inc_est_ano_anterior, na.rm = T) |
      veryhigh > 2 * max(df_uf$inc_est_ano_atual, na.rm = T), NA, veryhigh            
  )
  
  g_map <- df_uf %>% 
    ggplot(aes(x = semana)) + 
    geom_ribbon(data = df_mem_uf_sazonal, mapping = aes(x = semana, ymin = preseason, ymax = posseason), fill = 'orange', alpha = 0.5) + 
    geom_ribbon(data = df_mem_uf_sazonal, mapping = aes(x = semana, ymin = preseason, ymax = epidemic), fill = 'yellow', alpha = 0.5) + 
    geom_ribbon(data = df_mem_uf_sazonal, mapping = aes(x = semana, ymin = preseason, ymax = epidemic), fill = 'gray', alpha = 0.5) +
    geom_line(aes(y = inc_est_ano_atual), linewidth = 1, color = "red", show.legend = T) +
    geom_line(aes(y = inc_ano_atual), linetype = "dashed", linewidth = 0.75, color = "darkred", show.legend = T) +
    theme_light() +
    theme(
      legend.title = element_blank(), 
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 18),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      strip.text.x = element_text(size = 14, colour = "black"),
      strip.background = element_rect(fill = "white")
    ) +
    labs(title = "", y = "Incidência", x = "Semana epidemiolgica")
  
  
  df_uf2 <- df_uf %>% 
    filter(semana >= (max_se - 10) & semana <= (max_se + 5)) 
  
  df_mem_uf_sazonal2 <- df_mem_uf_sazonal %>% 
    filter(semana >= (max_se - 10) & semana <= (max_se + 5))
  
  g_map2 <- df_uf2 %>% 
    ggplot(aes(x = semana)) + 
    geom_ribbon(data = df_mem_uf_sazonal2, mapping = aes(x = semana, ymin = preseason, ymax = posseason), fill = 'orange', alpha = 0.5) +
    geom_ribbon(data = df_mem_uf_sazonal2, mapping = aes(x = semana, ymin = preseason, ymax = epidemic), fill = 'yellow', alpha = 0.5) +
    geom_ribbon(data = df_mem_uf_sazonal2, mapping = aes(x = semana, ymin = preseason, ymax = epidemic), fill = 'gray', alpha = 0.5) +
    geom_line(aes(y = inc_est_ano_atual), linewidth = 1, color = "red", show.legend = T) +
    geom_line(aes(y = inc_ano_atual), linetype = "dashed", linewidth = 0.75, color = "darkred", show.legend = T) +
    theme_cowplot() +
    theme(
      legend.title = element_blank(), 
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      axis.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      strip.background = element_rect(fill = "white"),
      axis.line.y = element_line(colour = "grey80"),
      axis.line.x = element_line(colour = "grey80")
    ) +
    labs(title = "", y = "", x = "")
  
  if(!is.na(veryhigh)){
    g_map <- g_map + 
      geom_hline(aes(yintercept = veryhigh), color = "black", linewidth = 1, linetype = "dashed") 
    
    g_map2 <- g_map2 + 
      geom_hline(aes(yintercept = veryhigh), color = "black", linewidth = 1, linetype = "dashed") 
  }
  
  final_plot <- ggdraw() +
    draw_plot(g_map) +
    draw_plot(g_map2, x = 0.50, y = 0.5, width = 0.5, height = 0.5)
  
  return(final_plot)
  
}

## Textos
texto_1 <- function(df, arbovirose){
  if(length(df) == 0){
    texto <- paste0("Nota-se que nenhuma UF esta apresentando a máxima incidência de ",arbovirose," neste ano.")
  }else{
    df = paste(df,collapse=", ")
    
    n_char_texto <- nchar(df) > 2
    
    if(n_char_texto == T){
      texto <- paste0(
        "As UFs ",
        substr(df, 1, nchar(df)-4),
        " e ",
        substr(df, nchar(df)-1, nchar(df)), " estão apresentando incidência máxima de ",arbovirose," nesta atual semana epidemiológica."
      )
    }else{
      texto <- paste0(df, " esta apresentando incidência máxima de ", arbovirose," nesta atual semana epidemiológica.")
    }
    
  }
  return(texto)
}
