library(tidyverse)
library(lubridate)
library(gganimate)

# dados DB queimadas
dbq_raw <- read_csv("./queimadas/Focos_2020-01-01_2020-09-24.csv")

dbq <- dbq_raw %>% 
  mutate(datahora = ymd_hms(datahora))

dbq %>% 
  mutate( dia = floor_date(datahora, unit="days" ) ) %>% 
  filter(datahora >= ymd("20200801")) %>% 
  filter(dia == ymd(20200908)) %>% 
  glimpse()

# Além de monitorar a localização dos focos de calor, os satélites registram a intensidade da queimada.
# Medida como FRP (sigla em inglês para potência radiativa do fogo), essa variável apresentou 
# uma média alta em julho e, em agosto, sofreu uma escalada ainda mais acentuada.

# quais satelites tem frp?
dbq %>% 
  filter(!is.na(frp)) %>% 
  select(satelite) %>% 
  distinct()

# quais satelites com frp tem mais registros
dbq %>% 
  filter(!is.na(frp)) %>% 
  count(satelite, sort=T)

# tem os mesmos periodos e região?
dbq %>% 
  filter(!is.na(frp)) %>% 
  group_by(satelite) %>% 
  summarise(
    min_date = min(datahora), 
    max_date = max(datahora), 
    min_lat  = min(latitude), 
    max_lat  = max(latitude), 
    min_lon  = min(longitude), 
    max_lon  = max(longitude),
    registros = n()
  )

g <- dbq %>% 
  filter(satelite=="NPP-375",
         datahora %within% interval(ymd(20200915), ymd(20200916))) %>% 
  ggplot(aes(x=longitude, y=latitude, group=datahora)) +
  geom_point(aes(color=frp)) +
  scale_color_gradient(low="darkred",high="yellow") +
  theme_minimal()

g + 
  transition_time(datahora) +
  labs(title="Fire in Brazil",
       subtitle = "Date:  {frame_time} - source: IBAMA") +
  shadow_wake(wake_length = .05)
