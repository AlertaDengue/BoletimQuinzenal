## Tabela resumo de situacao por UF, macro ou regional 
## 
library(tidyverse)
library(AlertTools)

ano <- 2024

## 1. carregar parametros MEM e list de cidades ----
load("data/mem2023.RData") # no AlertaDengueAnalise
## cidades (list de muns, regionais)
load("data/cidades.RData")

# 2.2 Opcao 2. carregar tabela de dados (depois substituir pela API)
# dengue
load("data/restab_den.RData")
dd <- restab_den %>%
  filter(SE > ano*100)
range(dd$SE)  # deve ter só esse ano

# chik
load("data/restab_chi.RData")
dc <- restab_chik %>%
  filter(SE > ano*100)
range(dc$SE) # deve ter só esse ano

# 3. Merge dos dados de casos com informacao territorial
dd <- dd %>%
  left_join(muns)

dc <- dc %>%
  left_join(muns)

# Agregacoes a nivel nacional, estadual e macroregiao ----

# Nacional total ----
# dengue
dd.fed <- dd %>%
  group_by(SE) %>%
  summarise(casos = sum(casos),
            casos_est = sum(casos_est),
            casos_prov = sum(casprov),
            pop = sum(pop)) %>%
  arrange(SE)

# chik  
dc.fed <- dc %>%
  group_by(SE) %>%
  summarise(casos = sum(casos),
            casos_est = sum(casos_est),
            casos_prov = sum(casprov),
            pop = sum(pop)) %>%
  arrange(SE)


# Estadual, por Semana ----

# dengue ----
dd.uf <- dd %>% 
  mutate(UF = floor(municipio_geocodigo/100000)) %>%
  group_by(UF, SE) %>%
  summarise(casos = sum(casos),
            casos_est = sum(casos_est),
            pop = sum(pop)) %>%
  mutate(inc = casos / pop * 100000,
         incest = casos_est/pop * 100000) %>%
  arrange(UF, SE)

# Estadual no ano ----

# dengue ----
tot  <-dd %>% 
  mutate(UF = floor(municipio_geocodigo/100000)) %>%
  group_by(UF) %>%
  summarise(casos = sum(casos),
            casos_est = sum(casos_est))

# Estadual, chik, casos por semana ----
dc.uf <- dc %>% 
  mutate(UF = floor(municipio_geocodigo/100000)) %>%
  group_by(UF, SE) %>%
  summarise(casos = sum(casos),
            casos_est = sum(casos_est),
            pop = sum(pop)) %>%
  mutate(inc = casos / pop * 100000,
         incest = casos_est/pop * 100000) %>%
  arrange(UF, SE)

# Estadual total chik no ano ----
totc  <-dc %>% 
  mutate(UF = floor(municipio_geocodigo/100000)) %>%
  group_by(UF) %>%
  summarise(casos = sum(casos),
            casos_est = sum(casos_est))


# Macroregional, por semana, dengue ----

dd.Macro <- dd %>% 
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

# Macroregional, por semana, chik ----

dc.Macro <- dc %>% 
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

# Regional, dengue  por semana ----

dd.Reg <- dd %>% 
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


# Regional, chik  por semana ----

dc.Reg <- dc %>% 
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


# Tabela Estadual, dengue  ----
(se <- max(dd.uf$SE))  # verificar se estamos analisando a semana certa

tabelaUF.den <- memUFanual %>%
  left_join(UFs, join_by("nome" == "estado")) %>%
  select(sigla, codigo, nome, veryhigh) %>% 
  mutate(codigo = as.numeric(sigla)) %>%
  select(estado = nome, codigo, limiar = veryhigh) %>%
  left_join(tot, by = join_by(codigo == UF))

# incidencia de dengue na ultima semana, para colocar na tabela UF
# incest = incidencia pelo nowcast; inc = inc pelos dados
dd.uf.se <- dd.uf %>% 
  filter(SE == se) %>%
  select(UF, incest, inc)
  
tabelaUF.den <- tabelaUF.den %>%
  left_join(dd.uf.se, join_by("codigo" == "UF"))

# incidencia nowcast dessa semana está muito acima do limiar?
# PS. fator 1.2 é para compensar um pouco pelo excesso de notificacao (suspeitos)
# em relacao ao provavel. A gente pode melhorar isso. 

tabelaUF.den$nivelNowcast <- as.numeric( tabelaUF.den$incest > 1.2*(tabelaUF.den$limiar))  
sum(tabelaUF.den$nivel)  # numero de UFs acima do limiar epidemico

# Calcula Rt por UF ----
# dengue
obj <- dd.uf %>%
  group_by(UF) %>%
  arrange(SE) 

obj <- as.data.frame(obj)

# indicadores referentes as ultimas 3 semanas
tabelaUF.den$Rtmean <- NA  # Rt medio das ultimas 3 semanas
tabelaUF.den$secomp1 <- NA # numero de semanas com Rt >1 nas ultimas 3 semanas

# indicador de pico
tabelaUF.den$weekmax <- NA # semana de pico de casos na UF 

# calculando Rt por UF
for(i in 1:27){
  dd.ufi <- obj[obj$UF == tabelaUF.den$codigo[i], ] %>%
    arrange(SE)
  dd.ufi <- dd.ufi[dd.ufi$SE > 202400, ]
  semanas <- dd.ufi$SE[dd.ufi$incest > tabelaUF.den$limiar[i]]
  tabelaUF.den$selimiaralto[i] <- ifelse(length(semanas) == 0, NA, min(semanas))
  r <- Rt(obj[obj$UF == tabelaUF.den$codigo[i],], count = "casos_est", 
          gtdist = "normal", meangt = 3, sdgt = 1)
  tabelaUF.den$Rtmean[i] <- mean(tail(r$Rt, n = 3))
  tabelaUF.den$secomp1[i] <- sum(tail(r$lwr, n = 3) > 1)
  dd.ufi <- dd.uf %>% filter(UF == tabelaUF.den$codigo[i])
  tabelaUF.den$weekmax[i] <-  dd.uf$SE[which.max(dd.ufi$casos_est)]
}


tabelaUF.den # essa tabela é a base para a tabela 2 do informe. 
# na tabela 2, 
#SE pico = weekmax
# nivel inc = alto se nivelNowcast = 1 (eu criei um nivel medio no boletim, de forma manual,
# mas aqui fica so alto , por enquanto)
# tendencia: é crescente se Rt > 1.2 e secomp = 2 ou 3; 
# estavel para crescente se  Rt >= 1.1 e a condicao acima nao ocorre
# estavel se 0.99 <= Rt < 1.1
# decrescente se Rt < 0.99
# % pop : ver mais abaixo (ainda nao esta nessa tabela)

# Incidencia de dengue a nivel de Regional de saude ----

tabelaReg <- memReganual %>%
  select(uf, regional, regional_id = nome, veryhigh) #veryhigh é o limiar epidemico

# nowcast de dengue na ultima semana
dd.Reg.se <- dd.Reg %>% 
  filter(SE == se) %>%
  select(regional_id, UF, pop, incest)

tabelaReg <- tabelaReg %>%
  left_join(dd.Reg.se, join_by("regional_id" == "regional_id"))

# regional está acima do limiar? (estamos com 26 municipios com dados faltantes - verificar)
tabelaReg$nivel <- as.numeric( tabelaReg$incest > (1.2 * tabelaReg$veryhigh)) 

# regionais que estão acima do limiar
sum(tabelaReg$nivel)

# % da populacao nacional vivendo em regionais acima do limiar
sum(tabelaReg$nivel * tabelaReg$pop)/sum(tabelaReg$pop)  

# populacao que vive em area de alto risco atual
tabelaReg$poprisco <- tabelaReg$nivel * tabelaReg$pop

# tabpoprisco ----
# calculando a populacao em regionais com alta incidencia, agregado por UF
# essa informacao entra na tabelaUF.den

tabpoprisco <- tapply(tabelaReg$poprisco, tabelaReg$uf, sum)/
  tapply(tabelaReg$pop, tabelaReg$uf, sum) * 100

tabpoprisco <- tabelaReg %>%
  group_by(uf) %>%
  summarise(poprisco = sum(poprisco),
            pop = sum(pop)) %>%
  mutate(popriscoprop = poprisco/pop * 100) %>%
  arrange(popriscoprop)

# essa figura a gente nao usa , mas poderia usar, 
# mostra a proporcao da pop vivendo em regioes de saude com alta notificacao de dengue
barplot(tabpoprisco$popriscoprop, names.arg = tabpoprisco$uf, las = 2, ylab = "%", 
        main = "pop em alto risco por estado", col = "orange")
abline(h = 50, col = 2, lty = 2)


## Figura das regionais com populacao em risco, 
# tbm nao estamos usando mas podemos usar

library(sf)
library(ggplot2)

shape <- st_read("shape/rs_450_RepCor1.shp")# shape de regionais
shape$primary.id <- as.numeric(shape$primary.id)
shape <- shape %>%
  left_join(tabelaReg, join_by("primary.id" == "regional_id"))

ggplot() +
  geom_sf(data = shape, aes(fill = as.character(nivel))) +
  scale_fill_manual(values = c("grey", "orange")) +
  labs(fill = "Alto") +
  ggtitle(se) +
  theme_minimal()  


# Calculo de Rt por regional ----
obj <- dd.Reg %>%
  group_by(regional_id) %>%
  arrange(SE) 

obj <- as.data.frame(obj)

# indicadores referentes as ultimas 3 semanas (semelhante ao feito pra UF)
tabelaReg$Rtmean <- NA # media do Rt nas ultimas semanas
tabelaReg$secomp1 <- NA # numero de semanas com Rt > 1

# calculo do Rt por regiao
for(i in 1:length(tabelaReg$regional_id)){
  dd.Regi <- obj[obj$regional_id == tabelaReg$regional_id[i], ]
  r <- Rt(obj[obj$regional_id == tabelaReg$regional_id[i],], count = "casos_est", gtdist = "normal", meangt = 3, sdgt = 1)
  tabelaReg$Rtmean[i] <- mean(tail(r$Rt, n = 3))
  tabelaReg$secomp1[i] <- sum(tail(r$p1, n = 3) > 0.9)
}

head(tabelaReg)

# variavel para o mapa da fig 4
tabelaReg$status <- NA
tabelaReg$status[tabelaReg$Rtmean > 1.2 & tabelaReg$nivel == 1] <- "alto em subida"
tabelaReg$status[tabelaReg$Rtmean >= 0.9 & tabelaReg$Rtmean < 1.2 & tabelaReg$nivel == 1] <- "alto estável"
tabelaReg$status[tabelaReg$Rtmean < 0.9 & tabelaReg$nivel == 1] <- "alto em queda"
tabelaReg$status[tabelaReg$nivel == 0] <- "baixo ou moderado"
table(tabelaReg$status, useNA = "ifany")

tabelaReg$status <- as.factor(tabelaReg$status)

# mapa status
shape <- st_read("../shape/rs_450_RepCor1.shp")# shape de regionais
shape$primary.id <- as.numeric(shape$primary.id)
shape <- shape %>%
  left_join(tabelaReg, join_by("primary.id" == "regional_id"))
# mapa
ggplot() +
  geom_sf(data = shape, aes(fill = status)) +
  scale_fill_manual(values = c("orange4", "yellow2","orange2","white")) +
  labs(fill = "Rt <= 1") +
  ggtitle(se) +
  theme_minimal()  # Adicione um tema, se desejar


