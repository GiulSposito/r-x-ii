library(tidyverse)
library(glue)

# Scrapping dos dados da Fiocruz (InfoGripe)
range <- 2015:2020
url <- "http://info.gripe.fiocruz.br/data/detailed/1/2/{year}/52/Brasil/weekly-incidence-curve"

# para cada ano, faz a requisição das notificações
scrap <- range %>% 
  map(function(.x,.url){
    glue(.url, year=.x)
  }, .url=url) %>% 
  map(read_csv, na = "null")

# unificando os dataframes 
cases <- scrap %>% 
  bind_rows() %>% 
  mutate( year = rep(range, each=54)) %>% 
  select(year, everything())

# plot
cases %>% 
  select(year, epiweek, value, estimated_cases) %>%
  filter(year>2016) %>%  #, epiweek<=15) %>% 
  mutate(year=factor(year)) %>% 
  ggplot(aes(epiweek, value, group=year)) +
  geom_line(aes(color=year), size=1) +
  labs(title="Notificações de síndrome respiratória aguda grave (SRAG)",
       subtitle = "fonte: www.saude.gov.br/sinan via Fiocruz",
       x="semana do ano",
       y="notificações SRAG") +
  theme_minimal()

