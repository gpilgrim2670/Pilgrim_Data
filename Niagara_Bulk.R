library(SwimmeR)
library(rvest)
library(purrr)
library(dplyr)
library(stringr)

links_scrape <- function(url) {
  pg <- read_html(url)
  
  links <- html_attr(html_nodes(pg, "span a"), "href")
  
  
  links_good <- links %>%
    .[map_lgl(., str_detect, "\\.pdf$|.htm")] %>%
    .[!map_lgl(., str_detect, "\\.zip")] %>%
    .[!map_lgl(., str_detect, "\\.nsf")]
  
  links_good <- unlist(links_good)
  links_full <- paste0("http://www.teamunify.com", links_good)
  
  return(links_full)
}

links <- links_scrape("https://www.teamunify.com/team/eznslsc/page/times/2018-2019-results")

raw_results <- map(links, safely(read_results, otherwise = NA))

names(raw_results) <- links

discard_errors <- function(results) {
  element_extract <- function(lst, n) {
    sapply(lst, `[`, n)
  }
  scrape_test_full <- discard(results, ~ !is.null(.x$error))
  scrape_test_full <- element_extract(scrape_test_full, 1)
  return(scrape_test_full)
}

clean_results <- discard_errors(raw_results)

Niagara_2018_2019 <-
  map(
    clean_results,
    safely(Swim_Parse, otherwise = NA),
    typo = c("Greater Rochester Area YMCA --NI"),
    replacement = c("Greater Rochester Area YMCA-NI"),
    splits = TRUE
  )

Niagara_2018_2019 <- discard_errors(Niagara_2018_2019)

Niagara_2018_2019 <- data.table::rbindlist(Niagara_2018_2019, use.names = TRUE, idcol = "Source", fill = TRUE)

Niagara_2018_2019  <- Niagara_2018_2019  %>%
  mutate(Source = str_remove(Source, "\\.result\\.result")) %>% 
  mutate(Age = as.numeric(Grade)) %>% 
  select(Source, Place, Name, Age, everything()) %>% 
  select(-Grade)
