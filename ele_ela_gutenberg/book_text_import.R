library(tidyverse)

books <- readRDS("./ele_ela_gutenberg/books.rds")
book_files <- readRDS("./ele_ela_gutenberg/books_file.rds")

bks <- books %>% 
  select(book_id, author, title, subject, language) %>% 
  unnest(c(author, title, subject, language)) %>% 
  distinct() %>% 
<<<<<<< HEAD
  # filter( str_detect(str_to_lower(subject), "fiction"),
  #         language == "Portuguese" ) %>% 
=======
  filter( str_detect(str_to_lower(subject), "fiction"),
          language == "Portuguese" ) %>% 
>>>>>>> db22cc8a4f9d81d15df3328a0bfa9eb1f61b23e3
  inner_join(book_files, by="book_id")

book_lines <- bks %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.book){

    raw_text <- readLines(.book$filename, encoding = "UTF-8")
    
    text_lines <- raw_text %>% 
      str_c(" ") %>% 
      str_replace("^ $","<empty_line>") %>% 
      str_c(collapse = "") %>% 
      str_split("<empty_line>") %>% 
      unlist() %>% 
      str_trim()
    
    text_lines <- text_lines[text_lines!=""]
    
    tibble(line_text = text_lines) %>% 
      mutate(
        book_id = .book$book_id,
        lines = length(text_lines),
        line_number = row_number()
      ) %>% 
      nest(text_lines=c(line_number, line_text)) %>% 
      return()
    
    # .book %>%
    #   select(book_id) %>% 
    #   mutate( text = list(text_lines),
    #           lines = length(text_lines)) %>% 
    #   unnest(text) %>% 
    #   mutate( line_number = row_number() ) %>% 
    #   nest(text_lines=c(line_number, text)) %>% 
    #   return()
    
  })

book_lines %>% 
<<<<<<< HEAD
  saveRDS("./ele_ela_gutenberg/book_text.rds")
=======
  saveRDS("./ele_ela_gutenberg/fiction_book_text.rds")
>>>>>>> db22cc8a4f9d81d15df3328a0bfa9eb1f61b23e3
