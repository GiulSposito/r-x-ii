library(tidyverse)
library(glue)
library(lubridate)

## Dados: morte por influenza SINAN via InfoGripe da Fiocruz: http://info.gripe.fiocruz.br/

# Scrapping dos dados da Fiocruz (InfoGripe), anos de interesse
range <- c(2009,2016,2018,2019)
url <- "http://info.gripe.fiocruz.br/data/resumed/3/2/{year}/0/Brasil/weekly-incidence-curve"

# para cada ano, faz a requisição das mortes por "Influenza" em formato CSV e importa
influ_scrap <- range %>% 
  map(function(.x,.url){
    glue(.url, year=.x)
  }, .url=url) %>% 
  map(read_csv, na = "null")

# unificando os dataframes 
influ <- influ_scrap %>% 
  bind_rows() %>% 
  # cria uma coluna de ano para cada csv
  mutate( year = rep(range, each=57),
          type = ifelse(year==2009|year==2016,"H1N1","Influenza")) %>% 
  select( year, everything() )

## Mortes COVD-19 notificadas via MS: https://covid.saude.gov.br/
covid_csv <- read_delim("https://mobileapps.saude.gov.br/esus-vepi/files/unAFkcaNDeXajurGB7LChj8SgQYS2ptm/7f5ff801706b28fbdb4be818043c37a4_Download_COVID19_20200428.csv",
                    delim = ";")

# dados vem por dia e estado
covid <- covid_csv %>% 
  # cria uma "semana epidemiologica"
  mutate( epiweek = week(data)) %>% 
  group_by(epiweek) %>% 
  # soma os novos casos semanais
  summarise(value = sum(obitosNovos)) %>% 
  ungroup() %>% 
  arrange( epiweek ) %>%  
  # cria um "acumulado" e deixa o DF com as mesmas colunas do "influ"
  mutate( year=2020,
          acc_value = cumsum(value),
          type = "Sars-Cov-2" )

# para comparar, precisamos colocar os registros iniciando de uma mesma
# semana (adjweek), para isso vamos localizar nos anos, a semana em que há
# pelo menos X mortes registradas
influ %>% 
  # dados de interesse
  select(year, epiweek, value, type) %>% 
  group_by(year) %>% 
  # acumula por ano
  mutate(acc_value=cumsum(value)) %>% 
  bind_rows(covid) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    # para cada conjunto de dado por ano
    data = map(data, function(.data){
      .data %>% 
        # cria uma "semana ajustada" a partir do Xo caso
        filter(acc_value>=10) %>% 
        mutate(adjweek = 1:nrow(.))
    })
  ) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  # ajusta o ano para fator (para plotar por cor)
  mutate( year  = factor(year),
          type  = factor(type) ) %>% 
  ggplot(aes(adjweek, acc_value, group=year, color=year)) +
  geom_line(aes(linetype=type), size=1) +
  labs(title="Mortes por Sars-Cov-2, H1N1 e Influenza",
       subtitle="Epidemias H1N1 em 2009 (sem vacina) e 2016 (com vacinação) \nCovid-19 em 2020 e anos de referência 2018 e 2019",
       caption = glue("fonte: Influenza do https://www.saude.gov.br/sinan via Fiocruz e\nCOVID-19 via Minist. Saude em https://covid.saude.gov.br obtidos em {date}", date=Sys.Date()),
       x="Semana a partir da 10a. morte",
       y="Mortes notificadas") +
  theme_minimal() +
  theme( plot.caption = element_text(hjust = 0) )
