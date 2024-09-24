# Carregar o objeto alerta BR   administrador@65.21.204.98:/Storage/infodengue_data/alertasRData/
## (depois substituir pela API ou via túnel SSH no servidor? para gerar os gráficos é necessário dados desde 2018)

load("data/ale-BR-202437.RData")
load("data/cidades.RData")
cidregionais <- cids |> bind_rows() |>
    select(-cidade)

cidregionais <- cidregionais |> 
  left_join(UFs, by = c("uf"="estado"))


d <- d |> 
  left_join(cidregionais, by = c("cidade"="municipio_geocodigo"))

library(tidyverse)
library(ggplot2)

d <- d |>
    mutate(ano= floor(SE/100),
           sem = SE - ano*100)

N <- nrow(d)
lastSE <- d$SE[N]
esse_ano <- d$ano[N]
essa_se <- d$sem[N]
iniSE <- (esse_ano-1)*100+1 

##Fig1
casos_uf <- d %>%
  filter(sem <= essa_se) %>%
  group_by(codigo,ano,CID10) %>%
  mutate(casos = replace_na(casos, 0)) %>%
  summarise(
    nsem  = length(unique(sem)),
    casos = sum(casos),
    pop = sum(pop)/nsem,
    inc = casos/pop * 100000
  ) 

casos_uf <- transform(casos_uf,
                      CID10=factor(CID10,levels=c("A92.0","A90")))


fig1 <- casos_uf %>% 
  mutate(
    data = ifelse(ano == max(ano), max(ano), paste0(min(ano),"-",max(ano)-1)),
    ano = as.factor(ano)) %>% 
  ggplot(aes(x = reorder(codigo, inc, max), y = inc)) +
  geom_point(aes(color = data), size= 4) + 
  scale_color_manual(values=c('grey70','red'))+
  labs(title = "",
       y = "Incidência por 100 mil habitantes",
       x = "",
       color = "Ano") +
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16,hjust = 0.5),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        panel.grid.major.y  = element_line(linetype = "dotted",color = "grey", linewidth = 0.5))+
  facet_wrap(~CID10, ncol = 1,scales="free",labeller = labeller(CID10 =c("A92.0" = "Chikungunya","A90" = "Dengue")))  + 
  theme(strip.text.x = element_text(size = 18, colour = "black"), 
        strip.background = element_rect(fill = "white") )



##Fig3
library(geofacet)
library(AlertTools)

dd <- d |>
  filter(SE >= unique(SE)[length(unique(SE))-12]) |>
  group_by(SE,codigo) |>
  summarise(casos=sum(casos, na.rm = T),
            tcasesmed=sum(tcasesmed, na.rm = T),
            pop=sum(unique(pop)),
            uf=unique(uf)) |>
  mutate(inc=casos/pop*100000,
         inc_est=tcasesmed/pop*100000)

dd <- dd %>%
  mutate(
    data = SE2date(SE)$ini)   # transformar em data para o gráfico

cols <- c("Estimativa Corrigida"="darkblue","Casos"="lightblue")
fig3 <- ggplot(dd) + 
  geom_bar(stat = "identity", aes(x = data, y = inc,fill = "Casos"))+
  geom_line(aes(x = data, y = inc_est, colour= "Estimativa Corrigida")) +
  labs(y="Incidência (dengue + chikungunya) por 100 mil hab.", 
       x="SE/Ano",
       title = "",
       subtitle = " ")+
  scale_x_continuous(breaks = c(min(dd$data),min(dd$data)+21,min(dd$data)+42,min(dd$data)+63,max(dd$data)), 
                     labels = c(paste0(str_sub(unique(dd$SE)[1], start  = -2L),"/",str_sub(unique(dd$SE)[1], start  = 3L, end  = 4L)),
                                paste0(str_sub(unique(dd$SE)[4], start  = -2L),"/",str_sub(unique(dd$SE)[4], start  = 3L, end  = 4L)),
                                paste0(str_sub(unique(dd$SE)[7], start  = -2L),"/",str_sub(unique(dd$SE)[7], start  = 3L, end  = 4L)),
                                paste0(str_sub(unique(dd$SE)[10], start  = -2L),"/",str_sub(unique(dd$SE)[10], start  = 3L, end  = 4L)),
                                paste0(str_sub(unique(dd$SE)[13], start  = -2L),"/",str_sub(unique(dd$SE)[13], start  = 3L, end  = 4L))))+
  scale_fill_manual(values = "lightblue") +
  scale_colour_manual(values = "darkblue") +
  theme_light() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.text = element_text(size=14),
        axis.title = element_text(size=18),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))+
  facet_geo(~uf, grid = "br_states_grid1", scales = "free_y") + 
  theme(strip.text.x = element_text(size = 14, colour = "black"), strip.background = element_rect(fill = "white") )

