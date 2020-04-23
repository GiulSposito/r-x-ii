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

##### Valores Esperados

# Arquivo:
#   
#   valores_esperados_por_localidade.csv
# 
# Conteúdo:
#   
#   UF: código da localidade (Brasil (0), UFs (11-53), Regiões geopolíticas (1-5), e Regional por perfil de atividade (1001-1004))
#   Unidade da Federação: nome da localidade (tanto UF quanto agregados de UFs)
#   Tipo: tipo de localidade (País, Estado, Região, Regional)
#   dado: total de síndrome respiratória aguda grave (srag), total de óbitos de SRAG (obito),
#   SRAG por influenza (sragflu), óbitos de SRAG por influenza (obitoflu), SRAG por COVID-19 (sragcovid), obitos de SRAG por COVID-19 (obitocovid)
#   escala: incidência por 100mil habitantes (incidência), casos
#   Semana epidemiológica: semana epidemiológica de primeiros sintomas.
#   Situação do dado: estável ou incompleto em função do padrão de inserção das notificações no sistema de informação.
#   'corredor baixo': valor semanal considerado significativamente baixo com base no perfil histórico das temporadas regulares. Isto é, atipicamente baixo para a semana correspondente.
#   'corredor mediano': valor semanal esperado com base no perfil histórico das temporadas regulares. Isto é, valor esperado para a semana correspondente.
#   'corredor alto': valor semanal considerado significativamente alto com base no perfil histórico das temporadas regulares. Isto é, atipicamente alto para a semana correspondente.
#   'limiar pré-epidêmico': patamar que sinaliza inicio da temporada (início de surto epidêmico) com base no perfil histórico das temporadas regulares.
#   'intensidade alta': patamar que sinaliza valor significativamente alto com base no perfil histórico das temporadas regulares. Sinaliza um ano com pico considerado alto.
#   'intensidade muito alta': patamar que sinaliza valor extremamente alto com base no perfil histórico das temporadas regulares. Sinaliza um ano com pico considerado extremamente alto.

# Padrão esperado e limiares de atividade por localidade e conjunto de dados
esperados_raw <- read_delim("https://gitlab.procc.fiocruz.br/mave/repo/raw/master/Dados/InfoGripe/valores_esperados_por_localidade.csv",
                        delim = ";", locale = locale(decimal_mark = ","))

# clean up
esperados_df <- esperados_raw %>%
  clean_names() %>% 
  mutate_at(vars(unidade_da_federacao, tipo, dado, escala), factor) %>% 
  # se quiser manipular (tidy) os anos usados como "normais: sem epidemia"
  #separate_rows(temporadas_consideradas_regulares, sep = ",") %>% 
  # deixa nomes mais claros
  rename( grupo = tipo, serie = dado )

# overview
esperados_df %>% 
  skim()

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
dados_raw <- read_delim("https://gitlab.procc.fiocruz.br/mave/repo/raw/master/Dados/InfoGripe/dados_semanais_faixa_etaria_sexo_virus.csv",
                        delim = ";", locale = locale(decimal_mark = ","))

# clean up
dados_df <- dados_raw %>% 
  clean_names() %>% 
  mutate_if(is.character, factor) %>% 
  # duas colunas com NA foram importadas como logical, corrigindo
  mutate_at(vars(idade_desconhecida, x2_anos, x2_4_anos), as.numeric) %>% 
  # o ano_semana epidemiologica nao precisa ser fator
  # to do: a coluna esta vazia ou foi um erro de importacao
  mutate(ano_e_semana_epidemiologica=as.character(ano_e_semana_epidemiologica)) %>% 
  # deixa nomes mais claros
  rename( grupo = tipo, serie = dado )

# inspect
dados_df %>% 
  glimpse()

# overview
dados_df %>% 
  skim()
