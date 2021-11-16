library(tidyverse)
library(tidytext)

books <- readRDS("./ele_ela_gutenberg/book_text.rds")

bigrams <- books %>% 
  unnest(text_lines) %>% 
  mutate(line_text = str_to_lower(line_text)) %>% 
  unnest_tokens(bigram, line_text, token = "ngrams", n=2, collapse = F)

bigrams_separated <- bigrams %>% 
  separate(bigram, c("word1","word2"), sep=" ")


saveRDS(bigrams_separated,"./ele_ela_gutenberg/book_bigrams.rds")

he_she_words <- bigrams_separated %>% 
  filter( word1 %in% c("ele","ela", "eles","elas") ) %>% 
  mutate( word1 = str_remove(word1,"s") )

he_she_words %>% 
  filter(!word2 %in% c(tm::stopwords("pt"),
                       "tôda","tambêm","ha","póde","afinal")) %>%
  mutate( stm_word2 = tm::stemDocument(word2, "portuguese")) %>% 
  mutate( stm_word2 = tm::stemCompletion(stm_word2, word2, type = "prevalent")) %>% 
  count(word1, stm_word2, sort=T) %>% 
  spread( word1, n, fill=0) %>% 
  mutate(
    total=ele+ela,
    ele = (ele+1)/sum(ele+1),
    ela = (ela+1)/sum(ela+1),
    log_ratio = log2(ela/ele),
    abs_ratio = abs(log_ratio)
  ) %>% 
  arrange(desc(log_ratio)) %>% 
  top_n(15, abs_ratio) %>% 
  mutate(stm_word2=fct_reorder(stm_word2, log_ratio)) %>% 
  ggplot(aes(x=stm_word2, y=log_ratio, fill=log_ratio<0)) +
  geom_col() + 
  coord_flip() +
  theme_minimal()

tm::stopwords("pt")
tm::removeNumbers()
tm::removePunctuation()
tm::stemDocument("portuguese")