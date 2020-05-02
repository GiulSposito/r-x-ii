library(tidyverse)
library(glue)

# Scrapping dos dados da Fiocruz (InfoGripe), anos de interesse
range <- c(2009:2020)
url <- "http://info.gripe.fiocruz.br/data/detailed/1/2/{year}/52/Brasil/weekly-incidence-curve"

# para cada ano, faz a requisição das notificações em formato CSV e importa
scrap <- range %>% 
  map(function(.x,.url){
    glue(.url, year=.x)
  }, .url=url) %>% 
  map(read_csv, na = "null")

# unificando os dataframes 
cases <- scrap %>%
  bind_rows() %>% 
  # cria uma coluna de ano para cada csv
  mutate( year = rep(range, each=57) ) %>% 
  select( year, everything() )

# para comparar, precisamos colocar os registros iniciando de uma mesma
# semana (adjweek), para isso vamos localizar nos anos, a semana em que há
# pelo menos 500 casos registrados
cases %>% 
  # dados de interesse
  select(year, epiweek, value, estimated_cases) %>% 
  # os dados de notificação são atualizados "no passado"
  # semanas de 2020 a partir da 14a. ainda estão com dados não confiáveis
  # mutate( value = ifelse( year==2020&epiweek>=13,estimated_cases,value ) ) %>% 
  group_by(year) %>% 
  # acumula
  mutate(acc_value=cumsum(value)) %>% 
  nest() %>% 
  mutate(
    # para cada conjunto de dado por ano
    data = map(data, function(.data){
      .data %>% 
        # cria uma "semana ajustada" a partir do 500o caso
        filter(acc_value>=100) %>% 
        mutate(adjweek = 1:nrow(.))
    })
  ) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  # ajusta o ano para fator (para plotar por cor)
  mutate( year  = factor(year) ) %>% 
  # pegar só o começo da epidemia
  filter( adjweek<=25 ) %>% 
  ggplot(aes(adjweek, acc_value, group=year, color=year)) +
  geom_line(size=1) +
  labs(title="Notificações de síndrome respiratória aguda grave (SRAG)",
       subtitle="Epidemias H1N1 em 2009 (sem vacina) e 2016 (com vacinação) \nc/ nCovid-19 em 2020 e anos de referência",
       caption = glue("fonte: www.saude.gov.br/sinan via Fiocruz em {date}", date=Sys.Date())) +
  theme_minimal() +
  theme( plot.caption = element_text(hjust = 0) )
