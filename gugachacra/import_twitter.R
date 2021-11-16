# install.packages("twitteR")
library(yaml)
library(tidyverse)
library(rtweet)
library(tidytext)

## store api keys (these are fake example values; replace with your own keys)
cfg <- read_yaml("./gugachacra/twitter.yml")

## authenticate via web browser
token <- create_token(
  consumer_key = cfg$apiKey,
  consumer_secret = cfg$apiSecret,
  access_token = cfg$accessToken,
  access_secret = cfg$accessTokenSecret
)

# rtm <- search_tweets(
#   "@gugachacra", n = 5000, include_rts = FALSE
# )

rt <- get_timeline("gugachacra", n = 5000)

dtm <- rt %>% 
  select(status_id, text) %>% 
  # mutate( text = str_remove_all(text,
  # "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)")) %>%
  # # mutate( text_stm = tm::stemCompletion(text_stm, text, "prevalent"))
  mutate( text = tm::removeNumbers(text) ) %>%
  mutate( text = tm::removePunctuation(text) ) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords("pt"), by="word") %>%
  anti_join(get_stopwords(), by="word") %>%
  # mutate( word = tm::stemDocument(word, "portuguese")) %>% 
  select(status_id, word) %>% 
  filter( word != "" ) %>% 
  add_count(status_id, word) %>%
  cast_dtm(document = status_id, term = word, value = n)

library(topicmodels)

lda_model <- LDA(dtm, 5)

library(broom)

word_topics <- tidy(lda_model)

word_topics %>% 
  group_by(topic) %>% 
  top_n(8, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate( term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill=factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales="free") +
    scale_y_reordered()
