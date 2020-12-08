library(tidyverse)

gresps

groupCount <- function(g.resps){
  tb <- tibble(
    group_replies = list(g.resps)
  ) %>% 
    mutate( n.resps = map_int(group_replies, length)) %>% 
    mutate( prep.resp = map(group_replies, function(iresp){
      iresp %>% 
        strsplit("") %>% 
        unlist()
    })) %>% 
    mutate(
      count.resp = list(map_df(prep.resp,function(resps){
        tibble(reply=resps)
      })) )
  
  tb %>% 
    mutate(  count.resp = map(count.resp, ~count(.x, reply)) ) %>% 
    unnest(count.resp) %>% 
    filter(n==n.resps) %>% 
    select(reply, n, n.resps) %>% 
    return()
}

groupCount(c("abc"))  
