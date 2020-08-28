library(tidyverse)

devtools::install_github("gpilgrim2670/SwimmeR", build_vignettes = TRUE, force = TRUE)
library(SwimmeR)

### Texas
real_time_links <- function(base, event_numbers) {
  event_numbers <- sprintf("%02d", as.numeric(event_numbers))
  links <- map(base, paste0, event_numbers, ".htm")
  links <- unlist(links, recursive = FALSE)
  return(links)
}

base_6A <-
  "https://www.uiltexas.org/tournament-results/swimming-diving/2020/6a/200214F0"

TX_Links_6A <-
  real_time_links(base = base_6A,
                  event_numbers = 1:24)

TX_Results_6A <-
  map(TX_Links_6A, read_results, node = "pre") %>% # map SwimmeR::read_results over the list of links
  map(swim_parse,
      typo = c("\\s\\d{1,2}\\s{2,}"),
      replacement = c(" ")) %>%
  bind_rows() %>% # bind together results from each link
  # select(Name, School, Finals_Time, Event) %>% # only the columns we need
  mutate(State = "TX",
         Division = "6A")# add column for state since we'll be combining results with OH

base_5A <-
  "https://www.uiltexas.org/tournament-results/swimming-diving/2020/5a/200214F0"

TX_Links_5A <-
  real_time_links(base = base_5A,
                  event_numbers = 1:24)

TX_Results_5A <-
  map(TX_Links_5A, read_results, node = "pre") %>% # map SwimmeR::read_results over the list of links
  map(swim_parse,
      typo = c("\\s\\d{1,2}\\s{2,}"),
      replacement = c(" ")) %>%
  bind_rows() %>% # bind together results from each link
  # select(Name, School, Finals_Time, Event) %>% # only the columns we need
  mutate(State = "TX",
         Division = "5A") # add column for state since we'll be combining results with OH

TX_Results <- bind_rows(TX_Results_5A, TX_Results_6A)

write.csv(TX_Results, "TX_States_2020.csv")

#### California
base <- "http://www.results.teamunify.com/clov/2019/CIFSTATEMEET/190510F0" # base url
event_numbers <- 1:24 # sequence of numbers, total of 24 evetns across boys and girls
event_numbers <- str_pad(event_numbers, width = 2, side = "left", pad = "0") # add leading zeros to single digit numbers
CA_Links <- paste0(base, event_numbers, ".htm") # paste together base urls and sequence of numbers (with leading zeroes as needed)

CA_Results <- map(CA_Links, read_results, node = "pre") %>% # map SwimmeR::read_results over the list of links
  map(swim_parse) %>% 
  bind_rows() %>% # bind together results from each link
  # select(Name, School, Finals_Time, Event) %>% # only the columns we need
  mutate(State = "CA") # add column for state since we'll be combining results with GA

CA_Results <- CA_Results %>% 
  mutate(Grade = case_when(is.na(Grade) == TRUE & str_detect(School, "^Fr|^So|^Jr|^Sr") == TRUE ~ str_extract(School, "^Fr|^So|^Jr|^Sr"),
                           TRUE ~ Grade),
         School = case_when(is.na(Grade) == TRUE & str_detect(School, Grade) == TRUE ~ str_remove(School, Grade),
                            TRUE ~ School),
         School = str_remove(School, "^Sr |^Jr |^So |^Fr "),
         Grade = str_to_upper(Grade),
         School = trimws(School))

write.csv(CA_Results, "CA_States_2019.csv")


#### NY ####
NY_Boys <- "http://www.nyhsswim.com/Results/Boys/2020/NYS/Single.htm"
NY_Girls <- "http://nyhsswim.com/Results/Girls/2019/NYS/Single.htm"

NY_Links <- c(NY_Boys, NY_Girls)
NY_Avoid <- c("Federation", "NYS Fed", "NYSPHSAA", "NYS Meet Rec")

NY_Results <- map(c(NY_Links), Read_Results, node = "pre") %>% 
  map(Swim_Parse, avoid = NY_Avoid) %>% 
  set_names(c("NY_Boys", "NY_Girls")) %>% 
  bind_rows(.id = "Source")

NY_Results <- NY_Results %>%
  mutate(
    State = str_split_fixed(Source, "_", n = 2)[, 1],
    Gender = str_split_fixed(Source, "_", n = 2)[, 2]
  ) %>%
  select(-Source)

write.csv(NY_Results, "NY_States_2020.csv")

#### PA ######
PA_Boys_3A <- "http://www.paswimming.com/19_20/results/state/PIAA_3_A_boys_states_Results.htm"
PA_Girls_3A <- "http://www.paswimming.com/19_20/results/state/PIAA_3_A_girls_states_Results.htm"
PA_Girls_3A_Diving <- "http://www.piaa.org/assets/web/documents/2019_3a_girls_f_dive_results.htm"
PA_Boys_2A <- "http://www.paswimming.com/18_19/results/states/Results/2_A_Boys_Results_2019.htm"
PA_Girls_2A <- "http://www.paswimming.com/18_19/results/states/Results/2_A_Girls_Results_2019.htm"

PA_Links <- c(PA_Boys_3A, PA_Girls_3A, PA_Girls_3A_Diving, PA_Boys_2A, PA_Girls_2A)
PA_Avoid <- c("PIAA", "NFHS", "NF Hon. Rol")

PA_Results <- map(c(PA_Links), Read_Results, node = "pre") %>% 
  map(Swim_Parse, avoid = PA_Avoid) %>% 
  set_names(c("PA_Boys_3A", "PA_Girls_3A", "PA_Girls_3A", "PA_Boys_2A", "PA_Girls_2A")) %>% 
  bind_rows(.id = "Source")

PA_Results <- PA_Results %>%
  mutate(
    State = str_split_fixed(Source, "_", n = 3)[, 1],
    Gender = str_split_fixed(Source, "_", n = 3)[, 2],
    Division = str_split_fixed(Source, "_", n = 3)[, 3],
  ) %>%
  select(-Source) %>% 
  mutate(Name = str_to_title(Name),
         School = str_to_title(School))

write.csv(PA_Results, "PA_States_2020.csv")

#### Florida ####
base_4A <- "https://www.fhsaa.org/sites/default/files/orig_uploads/sports/swimming-diving/archives/2019-20/state/4A/191115F0"

base_3A <- "https://www.fhsaa.org/sites/default/files/orig_uploads/sports/swimming-diving/archives/2019-20/state/3A/191116F0"

base_2A <- "https://www.fhsaa.org/sites/default/files/orig_uploads/sports/swimming-diving/archives/2019-20/state/191108F0"

base_1A <- "https://www.fhsaa.org/sites/default/files/orig_uploads/sports/swimming-diving/archives/2019-20/state/1A/191109F0"

real_time_links <- function(base, event_numbers) { # function to make list of results links
  event_numbers <-
    sprintf("%02d", as.numeric(event_numbers)) # adds leading zeros where needed on event numbers
  links <-
    map(base, paste0, event_numbers, ".htm") # combines link base with event numbers
  links <- unlist(links, recursive = FALSE)
  return(links)
}

FL_Links <-
  real_time_links(base = c(base_4A, base_3A, base_2A, base_1A),
                  event_numbers = 1:24)

FL_Results <-
  map(FL_Links, read_results, node = "pre") %>% # map SwimmeR::read_results over the list of links
  map(swim_parse)%>% 
  set_names(c(rep("4A", 24), rep("3A", 24), rep("2A", 24), rep("1A", 24))) %>% 
  bind_rows(.id = "Division") %>% 
  mutate(State = "FL")

write.csv(FL_Results, "FL_States_2020.csv")

#### Illinois ####
IL_Boys_Link <- "https://www.ihsa.org/archive/swb/2019-20/StateResults.pdf"
IL_Girls_Link <-
  "https://www.ihsa.org/Portals/0/prelim%20results.pdf"

IL_Results_Raw <-
  map(c(IL_Girls_Link, IL_Boys_Link), read_results)

IL_Results <-
  map(c(IL_Girls_Link, IL_Boys_Link), read_results) %>% # map SwimmeR::read_results over the list of links
  map(
    swim_parse,
    avoid = c("IHSA", "NFHS", "POOL", "NATIONAL"),
    typo = c(      "\\s+(\\d{1,2})\\s{2,}",
                   "Sr ",
                   "Jr ",
                   "So ",
                   "Fr ",
                   "Waubonsie   Valley",
                   "\\s{1,}\\("),
    replacement = c(      "\\1 ",
                          "SR ",
                          "JR ",
                          "SO ",
                          "FR ",
                          "Waubonsie Valley",
                          " \\(")
  ) %>%
  bind_rows() %>% # bind together results from each link
  mutate(State = "IL")

write.csv(IL_Results, "IL_States_2020.csv")


#### Ohio

Ohio_DI_Link <-
  "https://ohsaaweb.blob.core.windows.net/files/Sports/Swimming-Diving/2019-20/State%20Tournament/Division%20I/2020DivisionISwimmingFinalsResults.pdf"

OH_DI <- swim_parse(
  read_results(Ohio_DI_Link),
  typo = c(# fix issue with some class designation strings
    "SR\\s{2,}",
    "JR\\s{2,}",
    "SO\\s{2,}",
    "FR\\s{2,}"),
  replacement = c("SR ",
                  "JR ",
                  "SO ",
                  "FR ")
)

OH_DI_Diving_Link <-
  "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/OH_DI_Diving_2020.txt"

OH_DI_Diving <-
  read_delim(url(OH_DI_Diving_Link), delim = "\t") %>%
  tidyr::fill(Event, .direction = "down") %>%
  select(-c(Semis_Time, Order)) %>%
  na_if("Cut")

Ohio_DII_Link <-
  "https://ohsaaweb.blob.core.windows.net/files/Sports/Swimming-Diving/2019-20/State%20Tournament/Division%20II/2020DivisionIISwimmingFinalsResults.pdf"

OH_DII_raw <- read_results(Ohio_DII_Link)

OH_DII <- swim_parse(read_results(Ohio_DII_Link),
                     typo = c("SR  ", "JR  ", "SO  ", "FR  ", " q "),
                     replacement = c("SR ", "JR ", "SO ", "FR ", ""),
                     avoid = c("State Tour\\:",
                               "State Record\\:",
                               "Pool\\:"))

OH_Results <- bind_rows(OH_DI, OH_DII, OH_DI_Diving) %>%
  mutate(State = "OH",
         Division = str_to_title(str_extract(Event, " DIVISION \\d")),
         Event = str_remove(Event, " DIVISION \\d")) %>% 
  filter(str_detect(School, "Tour\\:") == FALSE,
         str_detect(School, "\\*") == FALSE,
         str_detect(School, "#") == FALSE,
         str_detect(School, "@") == FALSE,
         str_detect(School, "Record:") == FALSE)

write.csv(OH_Results, "OH_States_2020.csv")
