library(tidyverse)
library(glue)

# Scrapping dos dados da Fiocruz (InfoGripe), anos de interesse
range <- c(2009,2016,2019,2020)
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
  mutate( year = rep(range, each=54) ) %>% 
  select( year, everything() )

# para comparar, precisamos colocar os registros iniciando de uma mesma
# semana (adjweek), para isso vamos localizar nos anos, a semana em que há
# pelo menos 500 casos registrados
cases %>% 
  # dados de interesse
  select(year, epiweek, value, estimated_cases) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    # para cada conjunto de dado por ano
    data = map(data, function(.data){
      .data %>% 
        # acha a primeira semana com 500 registros
        filter(value>=500) %>% 
        filter(epiweek >= min(epiweek)) %>% 
        # cria uma "semana ajustada" a partir dela
        mutate( adjweek = 1:nrow(.) )
    })
  ) %>% 
  unnest(data) %>% 
  ungroup() %>%
  mutate(
    # ajusta o ano para fator (para plotar por cor)
    year  = factor(year),
    # os dados de notificação são atualizados "no passado"
    # semanas de 2020 a partir da 14a. ainda estão com dados não confiáveis
    # value = ifelse(year=="2020"&epiweek>=13,NA,value)
  ) %>% 
  # pegar só o começo da epidemia
  filter( adjweek<=10 ) %>% 
  ggplot(aes(adjweek, value, group=year, color=year)) +
  geom_line(size=1) +
  # plota também os dados de projeção de 2020 como pontilhado
  geom_line(aes(adjweek, estimated_cases), linetype="dotted", size=1, color="black", alpha=.5) +
  labs(title="Notificações de síndrome respiratória aguda grave (SRAG)",
       subtitle="Comparação entre as epidemias H1N1 em 2009 e 2016 \nc/ nCovid-19 em 2020 e um ano de referência 2019",
       caption = glue("fonte: www.saude.gov.br/sinan via Fiocruz em {date}", date=Sys.Date())) +
  theme_minimal() +
  theme( plot.caption = element_text(hjust = 0) )

         