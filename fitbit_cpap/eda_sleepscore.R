library(tidyverse)
library(lubridate)
library(janitor)

setwd("C:/Projects/r-x-ii/fitbit_cpap")

sh <- read_csv("./data/raw/sleepyhead/SleepyHead_Username_Summary_2018-01-16_2020-08-16.csv") %>% 
  janitor::clean_names() %>% 
  set_names(paste0("cpap_", names(.)))


sc <- read_csv("./data/raw/sleep-score/sleep_score.csv") %>% 
  janitor::clean_names() %>% 
  set_names(paste0("fitbit_",names(.)))

glimpse(sh)
glimpse(sc)

sh %>% 
  count(cpap_date, sort = T)

sc %>% 
  mutate(fitbit_date = floor_date(fitbit_timestamp, unit = "days")) %>% 
  count(fitbit_date, sort=T)

sleep_df <- sc %>% 
  mutate(fitbit_date = floor_date(fitbit_timestamp, unit = "days")) %>% 
  inner_join(sh, by=c("fitbit_date"="cpap_date")) %>% 
  select(-fitbit_sleep_log_entry_id,-fitbit_timestamp, -fitbit_date, -cpap_start, -cpap_end) %>%
  mutate( cpap_total_time=as.numeric(cpap_total_time)/(60*60) ) %>% 
  # Excellent: 90-100
  # Good: 80-89
  # Fair: 60-79
  # Poor: Less than 60  
  mutate( fitbit_sleep_class = case_when(
    fitbit_overall_score >= 90 ~ "excellent",
    fitbit_overall_score >= 80 ~ "good",
    fitbit_overall_score >= 60 ~ "fair",
    fitbit_overall_score <  60 ~ "poor",
  )) %>% 
  mutate( fitbit_sleep_class = factor(fitbit_sleep_class, 
                                      levels=c("poor","fair","good","excellent"), 
                                      ordered=T )) %>% 
  select(-caret::nearZeroVar(.))


install.packages("summarytools")
install.packages("DataExplorer")

library(summarytools)
library(skimr)

skimr::skim(sleep_df)

freq(sleep_df, style="rmarkdown")
ctable(sleep_df, method="render")
dfSummary(sleep_df)


browser(dfSummary(sleep_df))

sleep_df %>% 
  select(-ends_with("score")) %>% 
  DataExplorer::plot_correlation()
