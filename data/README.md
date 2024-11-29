## Dados

Deixe uma breve explicação sobre os dados que serão utilizados, referenciando o nome do arquivo/tabela e o script em que ele é utilizado.

**dados**

restab_den.RData : tabela historico alerta para dengue atualizada semanalmente. 

restab_chik.RData : tabela historico alerta para chik atualizada semanalmente.

**estrutura administrativa**

cidades2024: dataframe com a divisao administrativa usada no Infodengue em 2024 


**limiares**

mem2024: limiares mem calculados para UF, Regionais e municipios (necessario para a tabela).  

mem2024chitemp: atualiza alguns limiares presentes em mem2024

mem2024temp: atualiza alguns limiares presentes em mem2024 (dengue)


**forecast tables**
 
prop_susp_prov: proporcao de casos provaveis dentre o universo de notificados, estimados para cada UF (autor: Eduardo)
 
predicoes_acumuladas.csv: incidencia acumulada calculada pelos ensembles, por UF

ensemble_bayes_2025.csv: incidencia semanal calculada pelos ensembles, por UF

for_ensemble47: forecast de curta duracao (3 semanas: 48 a 50), com dados da semana 47 (autor:Eduardo) 
