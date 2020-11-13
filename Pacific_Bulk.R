library(SwimmeR)
library(rvest)
library(purrr)
library(dplyr)
library(stringr)


links_scrape <- function(web_url, node = ".res a:nth-child(1)") {
  pg <- read_html(web_url)

  links <- html_attr(html_nodes(pg, node), "href")
  
  links_good <- links %>%
    .[map_lgl(., str_detect, "\\.pdf$|.htm")] %>%
    .[!map_lgl(., str_detect, "\\.zip")] %>%
    .[!map_lgl(., str_detect, "\\.nsf")]
  
  links_good <- unlist(links_good)
  
  return(links_good)
}

links <- links_scrape("http://www.pacswim.org/swim-meet-results")
links <- ifelse(str_detect(links, "http"), paste0("", links), paste0("http://www.pacswim.org", links))

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

return_errors <- function(results) {
  element_extract <- function(lst, n) {
    sapply(lst, `[`, n)
  }
  scrape_test_full <- discard(results, ~ is.null(.x$error))
  # scrape_test_full <- element_extract(scrape_test_full, 1)
  return(scrape_test_full)
}

clean_results <- discard_errors(raw_results)

PAC_2020 <- map(
  clean_results,
  safely(swim_parse, otherwise = NA),
  typo = c(
    "(?<=[:alpha:]) (?=[:digit:])",
    "DQY",
    "XDQ",
    "1\\d\\-0\\d"
  ),
  replacement = c(
    "   ",
    "DQ",
    "DQ",
    ""
  ))

PC_errors <- return_errors(PAC_2020)
# PC_errors[8] is interesting as something that should almost work

PAC_2020 <- discard_errors(PAC_2020)

PAC_2020 <- data.table::rbindlist(PAC_2020, use.names = TRUE, idcol = "Source", fill = TRUE)

PAC_2020  <- PAC_2020  %>%
  mutate(Source = str_remove(Source, "\\.result\\.result")) %>% 
  mutate(Age = as.numeric(Grade),
         Team = School) %>% 
  select(Source, Place, Name, Age, Team, everything()) %>% 
  select(-Grade, - School)