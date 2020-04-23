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
                       delim = ";")

# clean up and type fixing
gripe_df <- gripe_raw %>% 
  clean_names() %>% 
  # parseia numeros com "," de decimal
  mutate_at(
    vars(total_reportado_ate_a_ultima_atualizacao, limite_inferior_da_estimativa,casos_estimados,
         limite_superior_da_estimativa, percentual_em_relacao_ao_pais),
    parse_number, locale=locale(decimal_mark = ",")
  ) %>% 
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

##### Dados Estratificados

# Arquivo:
#   
#   dado_semanais_faixa_etaria_sexo_virus.csv
# 
# Conteúdo:
#   
#   UF: código da localidade (Brasil (0), UFs (11-53), Regiões geopolíticas (1-5), e Regional por perfil de atividade (1001-1004))
#   Unidade da Federação: nome da localidade (tanto UF quanto agregados de UFs)
#   Tipo: tipo de localidade (País, Estado, Região, Regional)
#   dado: total de síndrome respiratória aguda grave (srag), total de óbitos de SRAG (obito),
#   SRAG por influenza (sragflu), óbitos de SRAG por influenza (obitoflu), SRAG por COVID-19 (sragcovid), obitos de SRAG por COVID-19 (obitocovid)
#   escala: incidência por 100mil habitantes (incidência), casos
#   Situação do dado: estável ou incompleto em função do padrão de inserção das notificações no sistema de informação.
#   sexo: masculino (M), feminino (F), ignorado (I), total (Total)
#   Ano epidemiológico: ano epidemiológico da semana de primeiros sintomas.
#   Semana epidemiológica: semana epidemiológica de primeiros sintomas.
#   'Total reportado até a última semana': total de notificações para a UF, dado, semana, e sexo correspondentes.
#   colunas '<2 anos', '0-4 anos', ... '60+ anos': estratificação por faixa etária. A coluna <2 anos só possui valores na escala 'casos'. Total em cada faixa etária para a UF, dado, semana, e sexo correspondentes.
#   colunas 'Testes positivos', 'Testes negativos', ..., 'Casos sem teste laboratorial': total de casos por situação dos exames laboratoriais, para a UF, dado, semana, e sexo correspondentes.
#   colunas 'Influenza A', 'Influenza B', 'SARS-CoV-2', ..., 'Adenovírus': total de casos com resultado positivo para cada vírus, para a UF, dado, semana, e sexo correspondentes. Não inclui todos os vírus testados.

# Dados por sexo do paciente, estratificado por idade ou situação do exame laboratorial.
dados_raw <- read_delim("https://gitlab.procc.fiocruz.br/mave/repo/raw/master/Dados/InfoGripe/valores_esperados_por_localidade.csv",
                        delim = ";", locale = locale(decimal_mark = ","))
