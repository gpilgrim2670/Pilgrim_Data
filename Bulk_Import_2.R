library(SwimmeR)
library(rvest)
library(purrr)
library(dplyr)
library(stringr)

links_scrape_section <- function(url) {
  pg <- read_html(url)
  
  links <- html_attr(html_nodes(pg, ".xspLinkViewColumn"), "href")
  
  
  links_good <- links %>%
    .[map_lgl(., str_detect, "http")] %>%
    .[map_lgl(., str_detect, "Meet")] %>%
    .[!map_lgl(., str_detect, "nyhsswim")] %>% 
    .[!map_lgl(., str_detect, "\\.zip")] %>%
    .[!map_lgl(., str_detect, "\\.nsf")]
  
  links_good_single <-
    map(links_good, str_replace, "Meet.htm", "Single.htm")
  links_good_single <-
    map(links_good, str_replace, "Meet.htm", "Single.htm")
  links_good_single <- unlist(links_good_single)
  
  return(links_good_single)
}

links_scrape_states <- function(url) {
  pg <- read_html(url)
  
  links <- html_attr(html_nodes(pg, ".xspLinkViewColumn"), "href")
  
  
  links_good <- links %>%
    .[map_lgl(., str_detect, "http")] %>%
    .[map_lgl(., str_detect, "Meet")] %>%
    .[!map_lgl(., str_detect, "\\.zip")] %>%
    .[!map_lgl(., str_detect, "\\.nsf")] %>% 
    .[map_lgl(., str_detect, "nyhsswim")]
  
  links_good_single <-
    map(links_good, str_replace, "Meet.htm", "Single.htm")
  links_good_single <-
    map(links_good, str_replace, "Meet.htm", "Single.htm")
  links_good_single <- unlist(links_good_single)
  
  return(links_good_single)
}

### Section Bulk Import ###
section_list_boys <- c("http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec1Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec2Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec3Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec4Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec5Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec6Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec7Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec8Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec9Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec10Boys.xsp",
                       "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec11Boys.xsp")

section_list_girls <- c("http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec1Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec2Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec3Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec4Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec5Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec6Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec7Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec8Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec9Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec10Girls.xsp",
                        "http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec11Girls.xsp")

#### Boys Sections ####
links_boys <- unlist(map(section_list_boys, links_scrape_section))

#### Boys States ####
links_boys_states <- links_scrape_states("http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec4Boys.xsp")

#### Girls Sections ####
links_girls <- unlist(map(section_list_girls, links_scrape_section))

#### Girls States ####
links_girls_states <- links_scrape_states("http://www.swimdata.info/NYState/MeetResultsRepository.nsf/Sec4Girls.xsp")

#### All Results ####
links_all <-
  c(links_boys,
    links_boys_states)
    # links_girls,
    # links_girls_states)

Raw_Results <- map(links_all, safely(Read_Results, otherwise = NA), node = "pre")

# Raw_Results_Test_Names <- map(Raw_Results_Test, ~{names(.) <-  links_all; .})

# Raw_Results_Test <- Raw_Results
# Raw_Results_Test_Names <- map(Raw_Results_Test, ~{names(.) <-  links_all; .})
names(Raw_Results) <- links_all


# for(i in seq_along(Raw_Results)) {
#   names(Raw_Results[[i]])[names(Raw_Results[[i]]) != 'error'] <-  links_all[i]}

discard_errors <- function(results) {
  element_extract <- function(lst, n) {
    sapply(lst, `[`, n)
  }
  scrape_test_full <- discard(results, ~ !is.null(.x$error))
  scrape_test_full <- element_extract(scrape_test_full, 1)
  return(scrape_test_full)
}

Clean_Results <- discard_errors(Raw_Results)

# read_with_progress <- function(filename){
#   pb$tick()$print()
#   df <- read_csv(filename)
#   # you can add additional operations on data_read, or 
#   # decide on entirely different task that this function should do.
# }
# # create the progress bar with a dplyr function. 
# pb <- progress_estimated(length(Clean_Results))
# res <- file_list %>%
#   map_df(~read_with_progress(.))

Full_Results <-
  map(
    Clean_Results,
    safely(Swim_Parse, otherwise = NA),
    splits = TRUE,
    relay_swimmers = TRUE,
    typo = c(
      "Greece  Athena",
      "  [A-Z]  ",
      # "(?<=[:alpha:]) (?=[:digit:])",
      "Newburgh Free  9",
      "FAYETTEVILLE MAN  ",
      "CICERO NORTH SYR  ",
      " - ",
      "Vineland  \\(Boy\\'s\\)",
      "\\(Kp\\)",
      "\\(Mc\\)",
      "\\(P",
      "  Psal",
      " Brian\\t A",
      "Williamsville E ",
      " B-AAB",
      "Section  X I",
      "Mexico  -B",
      "Nottingham  -A",
      "Bronxville  High School",
      "A A",
      ",  CT",
      ",  MA",
      "-1NORTH ROCKL",
      "QUEENSBURY  HIGH",
      "Sugrue_Neuendorf,",
      "dos Santos-Ilker,",
      "Finkelman-Mahoney,",
      "(?<=[:alpha:]) (?=[:digit:])",
      "Crane, Brian\\s{1,}A",
      "Kenneth Stein\\s{1,}IV"
    ),
    replacement = c(
      "Greece Athena",
      "",
      # "   ",
      "Newburgh Free-9",
      "FAYETTEVILLE MAN ",
      "CICERO NORTH SYR ",
      "-",
      "Vineland",
      "",
      "",
      "",
      "-Psal",
      "Brian A",
      "Williamsville East ",
      "B-AAB",
      "Section XI",
      "Mexico",
      "Nottingham",
      "Bronxville",
      "AA",
      "-CT",
      "-MA",
      "1-NORTH ROCKL",
      "QUEENSBURY",
      "Sugrue, Neuendorf",
      "Dos Santos-Ilker",
      "Finkelman, Mahoney",
      "   ",
      "Crane, Brian A",
      "Kenneth Stein IV"
    )
  )

Full_Results_2 <- discard_errors(Full_Results)

Full_Results_2 <- data.table::rbindlist(Full_Results_2, use.names = TRUE, idcol = "Source", fill = TRUE)

Full_Results_2  <- Full_Results_2  %>%
  mutate(
    Meet = case_when(
      str_detect(Source, "nyhsswim") == TRUE ~ paste("States", str_extract(Source, "\\d{4}"), sep = " "),
      str_detect(Source, "section\\d{1,3}swim") == TRUE ~ paste(
        str_split_fixed(Source, "/", n = 9)[, 7],
        str_split_fixed(Source, "/", n = 9)[, 8],
        str_extract(Source, "\\d{4}"),
        sep = " "
      )
    ),
    Meet = str_replace(Meet, " Single.htm.result", ""),
    Source = str_remove(Source, "\\.result\\.result"))

saveRDS(Full_Results_2, file = "NYS_Boys_Results_12022020.rds")
# saveRDS(Raw_Results, file = "Raw_NYS_Results_11062020.rds")

df <-
  swim_parse(
    read_results("http://www.nyhsswim.com/Results/Boys/2001/NYS/Single.htm"),
    typo = c(
      "Byram H/Valhalla/W Lake",
      "Ardley/Hastings/Wdlds -",
      "John Jay CrossRiver - S",
      "Plattsburgh - Section 7",
      "Comsewogue - Section 11",
      "Burnt Hills - Section 2",
      "- Section \\d{0,2}",
      " ?- ?Sectio",
      " - Chsaa",
      "- Chs",
      " ?- ?Sec"
    ),
    replacement = c(
      "Byram H/Valhalla/W Lake  ",
      "Ardley/Hastings/Wdlds  ",
      "John Jay CrossRiver  ",
      "Plattsburgh  ",
      "Comsewogue  ",
     "Burnt Hills  ",
     "  ",
     "  ",
     "  ",
     "  ",
     "  "
    )
  )
