library(tidyverse)
library(skimr)
library(lubridate)
library(janitor)

# https://github.com/seade-R/dados-covid-sp
# 
# Repositório de dados sobre casos e óbitos decorrentes do COVID-19 nos municípios do Estado de São Paulo
# Apresentação e dados
# O SEADE mantém um painel de dados sobre casos e óbitos relacionados ao coronavírus no Estado de São Paulo a partir de dados oficiais da Secretaria de Estado da Saúde de São Paulo (SES). Os dados estão disponíveis em https://www.seade.gov.br/coronavirus/.

covid_raw <- read_delim("https://github.com/seade-R/dados-covid-sp/raw/master/data/dados_covid_sp.csv",
                        delim = ";")

covid_df <- covid_raw %>% 
  mutate(
    munic  = factor(munic),
    casos  = as.numeric(casos),
    obitos = as.numeric(obitos),
    date   = ymd(paste("2020",mes,dia, sep="/"))
  ) %>% 
  select(-dia, -mes) %>% 
  select(date, munic, codigo_ibge, everything())

covid_df %>% 
  skim()

covid_df %>% 
  filter(date==max(date, na.rm = T)) %>% 
  arrange(desc(obitos))

# ggmap
library(ggmap)
library(yaml) # used to not version the google's map key

# before use ggmap with google maps it's necessary
# read and register a Key for Google Map API
config <- yaml::read_yaml("./config/config.yml")
register_google(key=config$google_map_key)

# get the map area
bbox <- make_bbox(lon = covid_df$longitude, lat=covid_df$latitude, f = .1)
gmap <- get_map(location=bbox, source="google", maptype="terrain")

# plot the data of observations on revision date 2019-11-10
# of location with stains or sparse residuos
ggmap(gmap) +
  geom_point(data=filter(covid_df, date==max(date, na.rm = T)),
             aes(x=longitude, y=latitude, color=casos), size=2, alpha=.5) +
  scale_color_gradient2(low="salmon",mid = "red", high="darkred") +
  theme_void() +
  ggplot2::coord_fixed() 


library(gganimate)


anim <- ggmap(gmap) +
  geom_point(data=covid_df,
             aes(x=longitude, y=latitude, color=log(casos),
                 size=log(casos), group=munic), alpha=.5) +
  scale_color_gradient2(low="salmon",mid = "red", high="darkred") +
  theme_void() +
  theme(legend.position = "none") +
  ggplot2::coord_fixed() +
  transition_time(date) +
  shadow_trail() +
  labs(title = "Casos de COVID-19 em São Paulo", 
       subtitle = "{frame_time}",
       caption = "fonte: SEADE via https://github.com/seade-R/dados-covid-sp")
  
animate(anim, end_pause = 10)
anim_save("./covid/covid_cases_sp.gif")


