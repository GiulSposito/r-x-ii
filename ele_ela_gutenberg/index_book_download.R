library(tidyverse)
library(glue)
library(rvest)
library(janitor)

# portuguese directory
url <- "https://www.gutenberg.org/browse/languages/pt"
index_page <- read_html(url)

# extracting links to books
book_links <- index_page %>% 
    html_nodes("body > div.container > div > div.pgdbbylanguage > ul > li > a") %>%
    as.character()

# creating a book dataset
books <- tibble(
    book_link = book_links
  ) %>% 
  mutate(
    # extract book number
    book_id = str_remove_all(str_extract(book_links,"\".+\""),"[^\\d]"),
    # extract book title
    book_title = str_remove_all(str_extract(book_links,">.+<"),"[<>]")
  ) %>% 
  relocate(book_id, book_title)


# for each book get the metadata info
books_info_values <- books$book_id %>% 
  map_df(function(.bid){
    
    # book download page
    meta_url <- glue("https://www.gutenberg.org/ebooks/{.bid}")
    meta_page <- read_html(meta_url)
    
    # there is two tables
    # #1 download liks 
    # #2 book info
    page_tables <- html_table(meta_page)
    
    # download links
    download_links <- page_tables[[1]] %>% 
      clean_names() %>% 
      select(format, url, size) %>% 
      mutate( format = make_clean_names(format) )
    
    text_link <- download_links %>% 
      filter(format=="plain_text_utf_8") %>% 
      pull(url)
    
    # extract info table
    book_info <-  page_tables[[2]] %>% 
      as_tibble() %>% 
      # it is a key-value table
      set_names(c("names","values")) %>% 
      # add meta_url
      bind_rows(
        tibble(
          names = c("book_page","download_links"),
          values = c(meta_url,text_link)
        )
      ) %>%  
      mutate(
        # clean key names
        names  = make_clean_names(names),
        # remove \r from the strings values 
        values = str_replace_all(values, "\\r"," "),
        # add book_id as register key
        book_id = as.integer(.bid)
      ) %>% 
      relocate(book_id)

    return(book_info)
    
  })

books_df <- books_info_values %>% 
  pivot_wider(names_from = "names", values_from="values")

saveRDS(books_df,"./ele_ela_gutenberg/books.rds")

books_files <- books_df %>% 
  select(book_id, download_links) %>% 
  unnest(download_links) %>% 
  distinct() %>% 
  # pull(download_links) %>% 
  # str_extract("[\\d\\-]+\\..*")
  # slice(1:50) %>% 
  split(1:nrow(.)) %>% 
  map_df(function(.book){
    filename <- paste0("./ele_ela_gutenberg/text/",str_extract(.book$download_links, "[\\d\\-]+\\..*"))
    download.file(.book$download_links, destfile = filename)
    tibble(
      book_id = .book$book_id,
      filename = filename
    ) %>% 
      return()
  })

saveRDS(books_files,"./ele_ela_gutenberg/books_file.rds")


books %>% 
  saveRDS("./ele_ela_gutenberg/books_dir.rds")
