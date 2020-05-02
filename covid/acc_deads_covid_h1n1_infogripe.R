library(tidyverse)
library(skimr)
library(janitor)

# repo dos dados do infogripe https://gitlab.procc.fiocruz.br/mave/repo/tree/master/Dados/InfoGripe

##### Series Temporais

# Arquivo:
#   
#   serie_temporal_com_estimativas_recentes.csv
# 
# Conteúdo:
#   
#   data de publicação: data em que as análises foram geradas. As estimativas levam em conta os dados inseridos no sistema até o domingo da última semana epidemiológica apresentada.
#   UF: código da localidade (Brasil (0), UFs (11-53), Regiões geopolíticas (1-5), e Regional por perfil de atividade (1001-1004))
#   Unidade da Federação: nome da localidade (tanto UF quanto agregados de UFs)
#   Tipo: tipo de localidade (País, Estado, Região, Regional)
#   dado: total de síndrome respiratória aguda grave (srag), total de óbitos de SRAG (obito),
#   SRAG por influenza (sragflu), óbitos de SRAG por influenza (obitoflu), SRAG por COVID-19 (sragcovid), obitos de SRAG por COVID-19 (obitocovid)
#   escala: incidência por 100mil habitantes (incidência), casos
#   Ano epidemiológico: ano epidemiológico da semana de primeiros sintomas.
#   Semana epidemiológica: semana epidemiológica de primeiros sintomas.
#   Situação do dado: estável ou incompleto em função do padrão de inserção das notificações no sistema de informação.
#   'Total reportado até a última semana': total de notificações para a UF, dado, semana, e sexo correspondentes.
#   'limite inferior da estimativa': limite inferior do intervalo de confiança para a estimativa de casos recentes
#   'casos estimados': estimativa de casos recentes.
#   'limite superior da estimativa': limite inferior do intervalo de confiança para a estimativa de casos recentes
#   'Percentual em relação ao país'
#   'População': população total UF correspondente.

# Total de casos por semana e estimativas de casos recentes, para SRAG, SRAG por Influenza, SRAG por COVID-19,
# óbitos de SRAG, óbitos de SRAG por Influenza, e óbitos de SRAG por COVID-19.
gripe_raw <- read_delim("https://gitlab.procc.fiocruz.br/mave/repo/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes.csv",
                        delim = ";", locale = locale(decimal_mark = ","))

# clean up and type fixing
gripe_df <- gripe_raw %>% 
  clean_names() %>% 
  # parseia fatores
  mutate_at(vars(unidade_da_federacao, tipo, dado, escala, situacao_do_dado), factor) %>% 
  # deixa nomes mais claros
  rename( grupo = tipo, serie = dado )

# overview
gripe_df %>% 
  skim()

# unidade da federacao com 37 unique?
gripe_df$unidade_da_federacao %>% fct_unique() # UF, Regioes e Brasil

# outros fatores
gripe_df %>% count(grupo, sort = T)
gripe_df %>% count(serie, sort = T)
gripe_df %>% count(escala, sort = T)
gripe_df %>% count(situacao_do_dado, sort = T)

gripe_df %>% 
  glimpse()

gripe_df %>% 
  filter(grupo=="País", escala=="casos",
         serie %in% c("obito","obitocovid","obitoflu") ) %>% 
  # ajusta o ano para fator (para plotar por cor)
  rename( valor = total_reportado_ate_a_ultima_atualizacao ) %>% 
  mutate( year  = factor(ano_epidemiologico ) ) %>% 
  ggplot(aes(semana_epidemiologica, valor,
             group=year, color=year)) +
  geom_line( size=1) +
  labs(title="Mortes por Sars-Cov-2, H1N1 e Influenza",
       subtitle="Epidemias H1N1 em 2009 (sem vacina) e 2016 (com vacinação) \nCovid-19 em 2020 e anos de referência 2018 e 2019",
       caption = glue("fonte: Influenza do https://www.saude.gov.br/sinan via Fiocruz e\nCOVID-19 via Minist. Saude em https://covid.saude.gov.br obtidos em {date}", date=Sys.Date()),
       x="Semana a partir da 10a. morte",
       y="Mortes notificadas") +
  theme_minimal() +
  facet_wrap(.~serie)
  theme( plot.caption = element_text(hjust = 0) )
  
  
