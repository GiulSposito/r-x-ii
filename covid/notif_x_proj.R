


cases %>% 
  filter(year==2020, !is.na(value)) %>% 
  select(year, epiweek, value, estimated_cases) %>% 
  ggplot(aes(epiweek, value)) +
  geom_line(color="darkred", size=1) +
  geom_line(aes(epiweek, estimated_cases), linetype="dotted", size=1, alpha=.8) +
  labs(title="Notificações de SRAG",
       subtitle="Comparação entre notificados e projetados",
       caption = glue("fonte: www.saude.gov.br/sinan via Fiocruz em {date}", date=Sys.Date())) +
  theme_minimal() +
  theme( plot.caption = element_text(hjust = 0) )
