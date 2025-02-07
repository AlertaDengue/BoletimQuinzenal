obter_rt_por_regional <- function(regional_id, df_regional, tabela_regional, se_min = 202400) {
  
  temp_tabela_regional <- tabela_regional %>%
    filter(regional_id == regional_id)
  
  temp_obj <- df_regional %>%
    filter(regional_id == regional_id) %>%
    arrange(SE) %>% 
    data.frame()
  
  if(nrow(temp_obj) > 0 & nrow(temp_tabela_regional) > 0){
    
    temp_obj2 <- temp_obj %>%
      filter(SE > se_min)
    
    semanas <- temp_obj2 %>%
      filter(incest > temp_tabela_regional$veryhigh) %>%
      pull(SE)
    
    temp_tabela_regional$selimiaralto <- ifelse(length(semanas) == 0, NA, min(semanas, na.rm = T))
    
    r <- Rt(temp_obj, count = "casos_est", 
            gtdist = "normal", meangt = 3, sdgt = 1)
    
    temp_tabela_regional$Rtmean <- mean(tail(r$Rt, n = 3))
    temp_tabela_regional$secomp1 <- sum(tail(r$lwr, n = 3) > 1)
    
    temp_tabela_regional$weekmax <- temp_obj %>%
      slice_max(casos_est, n = 1) %>%
      slice(n()) %>% 
      pull(SE)
    
    temp_tabela_regional <- temp_tabela_regional %>%
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
    
    return(temp_tabela_regional)
  }
}


###### OK a partir daqui


