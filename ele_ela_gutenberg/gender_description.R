library(tidyverse)
library(tidytext)

books <- readRDS("./ele_ela_gutenberg/book_text.rds")

trigrams <- books %>% 
  unnest(text_lines) %>% 
  mutate(line_text = str_to_lower(line_text)) %>% 
  unnest_tokens(bigram, line_text, token = "ngrams", n=3, collapse = F)

trigrams_separated <- trigrams %>% 
  separate(bigram, c("gender","verb","description"), sep=" ")

saveRDS(trigrams_separated,"./ele_ela_gutenberg/book_trigrams.rds")

he_she_words <- trigrams_separated %>% 
  filter( gender %in% c("ele","ela", "eles","elas","elle","ella","elles","elllas") ) %>% 
  mutate( gender = str_remove(gender,"s") ) %>% 
  mutate( gender = str_replace(gender,"ll","l") ) %>% 
  filter( verb %in% c("é","eram", "são", "serão"))

he_she_words %>% 
  filter(!description %in% c(tm::stopwords("pt"),
                       "tôda","tambêm","ha","póde","afinal",
                       "elle","ella")) %>%
  mutate( stm_description = tm::stemDocument(description, "portuguese")) %>% 
  mutate( stm_description = tm::stemCompletion(stm_description, description, type = "prevalent")) %>% 
  count(gender, stm_description, sort=T) %>% 
  spread( gender, n, fill=0) %>% 
  mutate(
    total=ele+ela,
    ele = (ele+1)/sum(ele+1),
    ela = (ela+1)/sum(ela+1),
    log_ratio = log2(ela/ele),
    abs_ratio = abs(log_ratio)
  ) %>% 
  arrange(desc(log_ratio)) %>% 
  top_n(18, abs_ratio) %>% 
  mutate(stm_description=fct_reorder(stm_description, log_ratio)) %>% 
  ggplot(aes(x=stm_description, y=log_ratio, fill=log_ratio<0)) +
  geom_col() + 
  coord_flip() +
  theme_minimal()
