library(SwimmeR)
library(rvest)
library(purrr)
library(dplyr)
library(stringr)


links_scrape <- function(url, node = "span a") {
  pg <- read_html(url)
  # pg <- read_html("https://www.teamunify.com/team/eznslsc/page/times/2018-2019-results")
  
  # links <- html_attr(html_nodes(pg, "span a"), "href")
  links <- html_attr(html_nodes(pg, node), "href")
  
  links_good <- links %>%
    .[map_lgl(., str_detect, "\\.pdf$|.htm")] %>%
    .[!map_lgl(., str_detect, "\\.zip")] %>%
    .[!map_lgl(., str_detect, "\\.nsf")]
  
  links_good <- unlist(links_good)
  links_full <- ifelse(str_detect(links_good, "http"), paste0("", links_good), paste0("http://www.teamunify.com", links_good))
  
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

Niagara_2018_2019 <- map(
    clean_results,
    safely(Swim_Parse, otherwise = NA),
    typo = c(
      "Greater Rochester Area YMCA --NI",
      "(?<=[:alpha:]) (?=[:digit:])",
      "111 ",
      "Alexis/Lexi J",
      "DQY",
      "La Face, Isabella or Bella F",
      "Cunningham, Rhys*",
      "XDQ",
      " FUT ",
      "PENINSULA WAVE RIDERS SWIMMING-A"
    ),
    replacement = c(
      "Greater Rochester Area YMCA-NI",
      "   ",
      "III",
      "Alexis J",
      "DQ",
      "La Face, Isabella",
      "Cunningham, Rhys",
      "DQ",
      "",
      "PENINSULA WAVE RIDERS SWIMMING  "
    ))

Niagara_2018_2019 <- discard_errors(Niagara_2018_2019)

Niagara_2018_2019 <- data.table::rbindlist(Niagara_2018_2019, use.names = TRUE, idcol = "Source", fill = TRUE)

Niagara_2018_2019  <- Niagara_2018_2019  %>%
  mutate(Source = str_remove(Source, "\\.result\\.result")) %>% 
  mutate(Age = as.numeric(Grade),
         Team = School) %>% 
  select(Source, Place, Name, Age, everything()) %>% 
  select(-Grade, - School)

# Niagara_2018_2019 %>%
#   filter(str_detect(Name, "Kadlecik")) %>% 
#   View()

n_teams <- length(unique(Niagara_2018_2019$Team))

NI_test <- Niagara_2018_2019 %>% 
  mutate(Name = str_to_title(Name),
         Name = str_replace(Name, "(?<=[:alpha:]) [:upper:]$", "")) %>%
  mutate(Firstname = str_extract(Name, "(?<=,\\s)[[:alpha:]]*"),
         Lastname = str_extract(Name, "[[:alpha:]]*(?=,)")) %>% 
  na_if("") %>% 
  mutate(Firstname = case_when(is.na(Name) == FALSE & is.na(Firstname) == TRUE ~ str_split_fixed(Name, " ", n = 2)[,1],
                               is.na(Name) == TRUE | is.na(Firstname) == FALSE ~ Firstname),
         Lastname = case_when(is.na(Name) == FALSE & is.na(Lastname) == TRUE ~ str_split_fixed(Name, " ", n = 2)[,2],
                              is.na(Name) == TRUE | is.na(Lastname) == FALSE ~ Lastname),
         ID = case_when(is.na(Name) == FALSE ~ paste0(str_extract(Firstname, "^.{3,4}"), str_extract(Lastname, "^.{2,5}")),
                        is.na(Name) == TRUE ~ Team),
         Name = str_c(Firstname, Lastname, sep = " "))

# Niagara_2018_2019 %>%
#   filter(str_detect(Name, "Kadlecik")) %>% 
#   View()

# Niagara_2018_2019 %>% 
#   filter(str_detect(Name, "Verkleeren"))

NI_test %>% 
  filter(is.na(Name) == FALSE) %>% 
  group_by(ID) %>% 
  summarise(No_Swims = n(),
            Name = getmode(Name),
            Team = get_mode(Team),
            Age = mean(Age, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-ID) %>% 
  arrange(desc(No_Swims)) %>% 
  head(10)

# Niagara_2018_2019 %>% 
#   filter(is.na(Name) == FALSE) %>% 
#   group_by(Name) %>% 
#   summarise(No_Swims = n(),
#             Team = get_mode(Team),
#             Age = mean(Age, na.rm = TRUE)) %>% 
#   arrange(desc(No_Swims)) %>% 
#   head(10)

Niagara_2018_2019 %>%
  filter(str_detect(Name, "Baxter, Camden")) %>% 
  group_by(Name) %>% 
  summarise(No_Swim = n())

Niagara_2018_2019 %>% 
  mutate(Name = str_remove(Name, " [A-Z]$")) %>% 
  filter(str_detect(Name, "Baxter, Camden")) %>% 
  View()
  # filter(str_detect(Name, "Kadlecik")) %>% 
  # group_by(Name) %>% 
  # summarise(No_Swim = n())


#### 2009-2010 season ####
links <- links_scrape("https://www.teamunify.com/team/eznslsc/page/times/2009-2010-results")

raw_results <- map(links, safely(read_results, otherwise = NA))

names(raw_results) <- links

clean_results <- SwimmeR:::discard_errors(raw_results)

Niagara_2009_2010 <- map(
  clean_results,
  safely(swim_parse, otherwise = NA),
  typo = c(
    "Greater Rochester Area YMCA --NI",
    "(?<=[:alpha:]) (?=[:digit:])",
    "111 ",
    "Alexis/Lexi J",
    "DQY",
    "La Face, Isabella or Bella F",
    "Cunningham, Rhys*",
    "XDQ",
    " FUT ",
    "PENINSULA WAVE RIDERS SWIMMING-A"
  ),
  replacement = c(
    "Greater Rochester Area YMCA-NI",
    "   ",
    "III",
    "Alexis J",
    "DQ",
    "La Face, Isabella",
    "Cunningham, Rhys",
    "DQ",
    "",
    "PENINSULA WAVE RIDERS SWIMMING  "
  ))

Niagara_2009_2010 <- SwimmeR:::discard_errors(Niagara_2009_2010)

Niagara_2009_2010 <- data.table::rbindlist(Niagara_2009_2010, use.names = TRUE, idcol = "Source", fill = TRUE)

Niagara_2009_2010  <- Niagara_2009_2010  %>%
  mutate(Source = str_remove(Source, "\\.result\\.result")) %>% 
  mutate(Age = as.numeric(Grade),
         Team = School) %>% 
  select(Source, Place, Name, Age, everything()) %>% 
  select(-Grade, - School)

Niagara_2009_2010 <- Niagara_2009_2010 %>% 
  mutate(Name = str_to_title(Name),
         Name = str_replace(Name, "(?<=[:alpha:]) [:upper:]$", "")) %>%
  mutate(Firstname = str_extract(Name, "(?<=,\\s)[[:alpha:|']]*"),
         Lastname = str_extract(Name, "[[:alpha:]]*(?=,)")) %>% 
  na_if("") %>% 
  mutate(Firstname = case_when(is.na(Name) == FALSE & is.na(Firstname) == TRUE ~ str_split_fixed(Name, " ", n = 2)[,1],
                               is.na(Name) == TRUE | is.na(Firstname) == FALSE ~ Firstname),
         Lastname = case_when(is.na(Name) == FALSE & is.na(Lastname) == TRUE ~ str_split_fixed(Name, " ", n = 2)[,2],
                              is.na(Name) == TRUE | is.na(Lastname) == FALSE ~ Lastname),
         ID = case_when(is.na(Name) == FALSE ~ paste0(str_extract(Firstname, "^.{3,4}"), str_extract(Lastname, "^.{2,5}")),
                        is.na(Name) == TRUE ~ Team),
         Name = str_c(Firstname, Lastname, sep = " ")) %>% 
  select(Place, Name, Age, Team, Prelims_Time, Finals_Time, Event, DQ, Exhibition, everything())

df <- swim_parse(read_results("http://www.teamunify.com/eznslsc/UserFiles/Image/Meet%20Results/NiagaraLongCourseJO07232010Results.htm"))


df %>% 
  mutate(Age = as.numeric(Grade),
         Team = School) %>% 
  select(Place, Name, Age, everything()) %>% 
  select(-Grade, - School) %>% 
  mutate(Name = str_replace(Name, "(?<=[:alpha:]) [:upper:]$", "")) %>%
  mutate(Firstname = str_extract(Name, "(?<=,\\s)[[:alpha:]]*"),
         Lastname = str_extract(Name, "[[:alpha:]|-|']*(?=,)")) %>% 
  na_if("") %>% 
  mutate(Firstname = case_when(is.na(Name) == FALSE & is.na(Firstname) == TRUE ~ str_split_fixed(Name, " ", n = 2)[,1],
                               is.na(Name) == TRUE | is.na(Firstname) == FALSE ~ Firstname),
         Lastname = case_when(is.na(Name) == FALSE & is.na(Lastname) == TRUE ~ str_split_fixed(Name, " ", n = 2)[,2],
                              is.na(Name) == TRUE | is.na(Lastname) == FALSE ~ Lastname),
         ID = case_when(is.na(Name) == FALSE ~ paste0(str_extract(Firstname, "^.{3,4}"), str_extract(Lastname, "^.{2,5}")),
                        is.na(Name) == TRUE ~ Team),
         Name = str_c(Firstname, Lastname, sep = " ")) %>% 
  select(Place, Name, Age, Team, Prelims_Time, Finals_Time, Event, DQ, Exhibition, everything()) %>% 
  View()
