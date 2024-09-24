obter_metricas_nacionais <- function(df){
  result <- df %>%
    group_by(SE) %>%
    summarise(casos = sum(casos),
              casos_est = sum(casos_est),
              casos_prov = sum(casprov),
              pop = sum(pop)) %>%
    arrange(SE)
  
  return(result)
}

obter_metricas_estaduais <- function(df){
  result <- df %>%
    mutate(UF = floor(municipio_geocodigo/100000)) %>%
    group_by(UF, SE) %>%
    summarise(casos = sum(casos),
              casos_est = sum(casos_est),
              pop = sum(pop)) %>%
    mutate(inc = casos / pop * 100000,
           incest = casos_est/pop * 100000) %>%
    arrange(UF, SE)
  
  return(result)
}

obter_metricas_estaduais_por_ano <- function(df){
  result <- df %>%
    mutate(UF = floor(municipio_geocodigo/100000)) %>%
    group_by(UF) %>%
    summarise(casos = sum(casos),
              casos_est = sum(casos_est))
  
  return(result)
}

obter_metricas_macrorregionais_por_semana <- function(df){
  result <- df %>% 
    mutate(UF = floor(municipio_geocodigo/100000)) %>%
    group_by(macroregional_id, SE) %>%
    summarise(casos = sum(casos),
              #casprov = sum(casprov),
              casos_est = sum(casos_est),
              pop = sum(pop),
              UF = unique(UF)) %>%
    mutate(inc = casos / pop * 100000,
           #incprov = casprov / pop * 100000,
           incest = casos_est/pop * 100000) %>%
    arrange(UF, SE)
  
  return(result)
}

obter_metricas_regionais_por_semana <- function(df){
  result <- df %>% 
    mutate(UF = floor(municipio_geocodigo/100000)) %>%
    group_by(regional_id, SE) %>%
    summarise(casos = sum(casos),
              #casprov = sum(casprov),
              casos_est = sum(casos_est),
              pop = sum(pop),
              macroregional_id = unique( macroregional_id),
              UF = unique(UF)) %>%
    mutate(inc = casos / pop * 100000,
           #incprov = casprov / pop * 100000,
           incest = casos_est/pop * 100000) %>%
    arrange(UF, macroregional_id, SE)
  
  return(result)
}

obter_tabela <- function(df, df_mem, se_max){
  
  df_total = df %>% obter_metricas_estaduais_por_ano()
  
  tabela_UF <- df_mem %>%
    left_join(UFs, join_by("nome" == "estado")) %>%
    select(sigla, codigo, nome, veryhigh) %>% 
    mutate(codigo = as.numeric(sigla)) %>%
    select(estado = nome, codigo, limiar = veryhigh) %>%
    left_join(df_total, by = join_by(codigo == UF))
  
  df_estaduais <- df %>% 
    obter_metricas_estaduais()
  
  df_se <- df_estaduais %>% 
    filter(SE == se_max) %>%
    select(UF, incest, inc)
  
  tabela_UF <- tabela_UF %>%
    left_join(df_se, join_by("codigo" == "UF")) %>% 
    mutate(
      nivelNowcast = as.numeric(incest > 1.2 * limiar)  
    )
  # sum(tabela_UF$nivel)  # numero de UFs acima do limiar epidemico
  
  ## Calcula Rt por UF
  obj_df <- df_estaduais %>%
    group_by(UF) %>%
    arrange(SE) %>% 
    data.frame()
  
  tabela_UF <- tabela_UF %>% 
    mutate(
      Rtmean = NA,
      secomp1 = NA,
      weekmax = NA
    )
  
  for(i in 1:length(tabela_UF$estado)){
    df2 <- obj_df[obj_df$UF == tabela_UF$codigo[i], ] %>%
      arrange(SE)
    df2 <- df2[df2$SE > 202400, ]
    semanas <- df2$SE[df2$incest > tabela_UF$limiar[i]]
    tabela_UF$selimiaralto[i] <- ifelse(length(semanas) == 0, NA, min(semanas))
    r <- Rt(obj_df[obj_df$UF == tabela_UF$codigo[i],], count = "casos_est", 
            gtdist = "normal", meangt = 3, sdgt = 1)
    tabela_UF$Rtmean[i] <- mean(tail(r$Rt, n = 3))
    tabela_UF$secomp1[i] <- sum(tail(r$lwr, n = 3) > 1)
    df2 <- df_estaduais %>% filter(UF == tabela_UF$codigo[i])
    tabela_UF$weekmax[i] <-  df_estaduais$SE[which.max(df2$casos_est)]
  }
  
  return(tabela_UF)
  
}

obter_pop_sob_risco <- function(df){
  df_result <- df %>% 
    group_by(uf) %>% 
    summarise(
      pop_total = sum(pop, na.rm = T),
      pop_risco = sum(poprisco, na.rm = T),
      prop_pop_risco = pop_risco/pop_total * 100
    ) %>% 
    arrange(prop_pop_risco)
  
  return(df_result)
}

obter_metricas_por_regional <- function(df_mem_anual, df_regional_por_se){
  
  df_result <- df_mem_anual %>% 
    select(uf, regional, regional_id = nome, veryhigh) %>%
    left_join(df_regional_por_se, by = "regional_id") %>% 
    mutate(
      nivel = as.numeric(incest > (1.2 * veryhigh)),
      poprisco = nivel * pop,
      Rtmean = NA,
      secomp1 = NA 
    )
  
  return(df_result)
  
}

obter_classificacao_das_regionais <- function(df_regional, tab_regional){
  
  obj_df_regional <- df_regional %>%
    group_by(regional_id) %>%
    arrange(SE) %>% 
    data.frame()
  
  
  for(i in 1:length(tab_regional$regional_id)){
    df_regi <- obj_df_regional[obj_df_regional$regional_id == tab_regional$regional_id[i], ]
    r <- Rt(obj_df_regional[obj_df_regional$regional_id == tab_regional$regional_id[i],], count = "casos_est", gtdist = "normal", meangt = 3, sdgt = 1)
    tab_regional$Rtmean[i] <- mean(tail(r$Rt, n = 3))
    tab_regional$secomp1[i] <- sum(tail(r$p1, n = 3) > 0.9)
  }
  
  tab_regional <- tab_regional %>%
    mutate(status = case_when(
      Rtmean > 1.2 & nivel == 1 ~ "Alto em subida",
      Rtmean >= 0.9 & Rtmean < 1.2 & nivel == 1 ~ "Alto estável",
      Rtmean < 0.9 & nivel == 1 ~ "Alto em queda",
      nivel == 0 ~ "Baixo ou moderado",
      TRUE ~ NA_character_  # para lidar com casos não cobertos
    ),
    status = factor(status, levels = c("Alto em subida", "Alto estável", "Alto em queda", "Baixo ou moderado"))
    )
  
  return(tab_regional)
}

gg_bar_pop_risco <- function(df){
  
  grafico <- df %>% 
    ggplot(aes(x = uf, y = prop_pop_risco)) +
    geom_bar(stat = "identity", fill = "orange") +
    geom_hline(yintercept = 50, color = "red", linetype = "dashed") +
    labs(title = "População em alto risco por estado", y = "%", x = "") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(grafico)
}

gg_map_risco_classif <- function(shape, se_max){
  ggplot() +
    geom_sf(data = shape, aes(fill = status)) +
    scale_fill_manual(values = c("orange4", "yellow2","orange2","white")) +
    labs(fill = "Rt <= 1") +
    ggtitle(se_max) +
    theme_minimal() 
}

