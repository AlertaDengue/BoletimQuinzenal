## Mapas e graficos de comparacao entre nowcast, forecast de curto prazo
## e forecast de longo prazo
# claudia, nov 2024

Obter os dados que gerar uma tabela para de now castingDF

library(geobr)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)


## Geometria do Brasil (geobr)

states <- read_state(
  year = 2020, 
  showProgress = FALSE
)

# atualizar a cada vez: 
weeknow = 47


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
  labs(title = "(A) modelo ensemble 23") +
  theme_minimal()  +
  theme(legend.position = "none")

m2 <- ggplot() +
  geom_sf(data = shape_anual, aes(fill = ens_inc24)) +
  scale_fill_viridis_c(option = "viridis", oob = scales::squish,
                       trans = "log",
                       limits = common_yrange,
                       breaks = c(100, 300, 1000, 10000),  # Define specific breakpoints
                       labels = c("100", "300", "1K", "10k"),
                       name = "Incidência acumulada") + 
  labs(title = "(A) modelo ensemble 24") +
  theme_minimal()  


m3 <- ggplot(data = shape_anual, aes(x = factor(code_state))) +
  geom_point(aes(y = ens_inc23), color = "black", shape = "_", size = 3) +
  geom_point(aes(y = ens_inc24), color = "black", shape = "_", size = 3) +
  geom_segment(aes(x = factor(code_state), xend = factor(code_state), 
                   y = ens_inc23, yend = ens_inc24), color = "black") +
  scale_y_log10() +  
  scale_x_discrete(labels = shapeNow$abbrev_state) +
  labs(x = "", y = "Incidência acumulada (x10^5)", 
       title = "(C) barra: faixa de incidência acumulada prevista pelos ensembles ") +
  theme_minimal() +
  geom_hline(yintercept = 300, color = "red", linetype = "dashed")


combined_plots123 <- (m1 | m2) /
  m3 + plot_layout(heights = c(1, 1.5)) +
  plot_annotation(title = "Previsões para a temporada 2024-25 (médias dos modelos)")
print(combined_plots123)

ggsave(filename = "figuras/forecast_maps.png")


# dados e modelos para a temporada ate agora ----


pred_se <- read.csv("data/ensemble_bayes_2025.csv") # forecast por semana
head(pred_se)

pred.casos.ac <- pred_se %>%
  mutate(se = epiweek(date)) %>%
  filter(se > 40 & se <= weeknow) %>%  ## check that in 2025!!!!!!
  group_by(state) %>%
  summarize(casos_e23 = sum(pred_ensemble_23),
            casos_e24 = sum(pred_ensemble_24))

head(pred.casos.ac)

# pegando os dados atuais, e nowcast (precisa de conexao) ----
if(exists(con)) {
  comando <- "SELECT municipio_geocodigo, \"SE\", casos, casprov, casos_est, pop, nivel 
FROM \"Municipio\".\"Historico_alerta\" WHERE \"SE\" > 202440"
  dd <- dbGetQuery(con, comando)  # conexao banco
  range(dd$SE)
  save(dd, file = "casos_atuais.RData")
  dbDisconnect(con)
}{message("carregar: dd <- tabela_historico com filtro SE > 202440")} 

load("casos_atuais.RData")

# agregando casos municipais, por UF, na temporada, ate agora
dd.uf <- dd %>% 
  filter(SE > 202440) %>%
  mutate(code_state = floor(municipio_geocodigo/10e4)) %>%
  group_by(code_state) %>%
  summarise(casos = sum(casos), # casos notificados
            casos_est = sum(casos_est), # casos not nowcasted
            pop = sum(pop)) %>%
  mutate(inc_prov = casos_est/pop * 1e5) %>% # incid not nowcasted 
  arrange(code_state)

head(dd.uf)

# para transformar em casos provaveis, vamos precisar usar o fator de correcao 
# baseado na prop de casos prov do ultimo periodo (Eduardo)

prop <- read.csv("data/prop_susp_prov.csv")

# juntando tudo no objeto shape

shapeNow <- states %>%
  left_join(pred.casos.ac, join_by(abbrev_state == state)) %>%
  left_join(dd.uf, join_by(code_state)) %>%
  left_join(prop, join_by(abbrev_state == state)) %>%
  mutate(casos_est_prov = round(casos_est * prop)) %>%
  arrange(code_state)

shapeNow$color <- ifelse(shapeNow$casos_est_prov < shapeNow$casos_e23 &
                               shapeNow$casos_est_prov < shapeNow$casos_e24,
                              "blue", "red")

# mapa de casos
m4 <- ggplot() +
  geom_sf(data = shapeNow, aes(fill = casos_est_prov)) +
  scale_fill_viridis_c(option = "viridis",
                       oob = scales::squish, 
                       trans = "log",
                       breaks = c(1, 10, 100, 1000, 10000),  # Define specific breakpoints
                       labels = c("1", "10", "100", "1k", "10k"),
                       name = "Casos prováveis") + 
  labs(title = "(A) Número estimado de casos prováveis") +
  theme_minimal()  

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
  theme_minimal()  

# grafico de pontos
m6 <- ggplot(data = shapeNow, aes(x = factor(code_state))) +
  geom_point(aes(y = casos_e23), color = "black", shape = "_", size = 3) +
  geom_point(aes(y = casos_e24), color = "black", shape = "_", size = 3) +
  geom_segment(aes(x = factor(code_state), xend = factor(code_state), 
                   y = casos_e23, yend = casos_e24), color = "black") +
  geom_point(aes(y = casos_est_prov), color = shapeNow$color, shape = 16, size = 2) +
  #geom_point(aes(y = casos_est), color = "blue", shape = 16, size = 2) +
  scale_y_log10() +  
  scale_x_discrete(labels = shapeNow$abbrev_state) +
  labs(x = "", y = "Casos Acumulados", 
       title = "(C) Casos prováveis estimados de dengue (semanas 40-47)") +
  theme_minimal()

combined_plots456 <- (m4 | m5) /
  m6 + plot_layout(heights = c(1, 1.5)) +
  plot_annotation(title = "Situação da dengue, SE 40-47 2024, usando nowcast.")
print(combined_plots456)

ggsave(filename = paste0("figuras/situacao_",weeknow,".png"))


## situacao prevista para as prox 3 semanas  

# previsoes geradas pelo modelo de short term forecast (Eduardo gera)
file = paste0("data/for_ensemble_47.csv")
short.pred <- read.csv(file) %>%
  mutate(se = epiweek(date)) 

(w3 <- unique(short.pred$se)) # verificando as semanas preditas

short.pred <- short.pred %>%  # agregando por UF
  group_by(state) %>%
  summarize(pred3w = sum(pred)) 
head(short.pred)

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
  labs(title = "(B) Incidência prevista para o período") +
  theme_minimal()  +
  theme(plot.margin = margin(5, 5, 5, 2)) # Top, right, bottom, left margins

m8 <- ggplot(data = shape_short, aes(x = factor(code_state))) +
  geom_point(aes(y = ens24w), color = "black", shape = "_", size = 3) +
  geom_point(aes(y = ens23w), color = "black", shape = "_", size = 3) +
  geom_segment(aes(x = factor(code_state), xend = factor(code_state), 
                   y = ens23w, yend = ens24w), color = "black") +
  geom_point(aes(y = pred3w), color = shape_short$color, shape = 16, size = 2) +
  scale_y_log10() +  
  labs(x = "", y = "Casos prováveis", 
       title = "(A) ponto = casos previstos; barra: casos esperados pelos ensembles" ) +
  scale_x_discrete(labels = shape_short$abbrev_state) +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10)) # Top, right, bottom, left margins

combined_plot78 <- m7 + m8 + 
  plot_layout(nrow = 2, heights = c(1.1,0.9)) + # Arrange plots in a single row, adjust ncol if needed
  plot_annotation(title = paste("Situação prevista para as proximas 3 semanas, SE:", 
                                min(w3), "a", max(w3)))

print(combined_plot78)
ggsave(filename = paste0("figuras/situacao_prevista_",min(w3),"-",max(w3),".png"))
