library(tidyverse)
library(readr)

Full_Results_3 <- read_csv("Full_NYS_Results_0_2_0.csv")

results_DF_working <- Full_Results_3 %>% 
  mutate(
    Year = as.numeric(str_extract(Meet, "\\d{4}")),
    Section = str_extract(Meet, "Sec[[:alnum:]]*"),
    Section = case_when(Section == "Sectional" ~ "Sec6",
                        Section != "Sectional" ~ Section),
    Section = as.numeric(str_replace(Section, "Sec", "")),
    Event = str_to_title(Event),
    Event = str_replace_all(Event, "Division [[:alpha:]] ", ""),
    Event_Sex = str_split_fixed(Event, " ", n = 2)[,1],
    Event_Sex = case_when(str_detect(Event_Sex, "Men") == TRUE ~ "Boys",
                          str_detect(Event_Sex, "Women") == TRUE ~ "Girls",
                          str_detect(Event_Sex, "Women") == FALSE & str_detect(Event_Sex, "Men") == FALSE ~ Event_Sex),
    Event = str_replace(Event, "Medley Relay", "MR"),
    Event = str_replace(Event, "Free ", "Freestyle "),
    Event = str_replace(Event, "Freestyle Relay", "FR")) %>% 
  filter(str_detect(Event, "Diving"))

all_names <- results_DF_working %>% 
  mutate(Name = str_to_title(Name),
         Name = str_replace(Name, " [:upper:]$", "")) %>% 
  mutate(Firstname = str_extract(Name, "(?<=,\\s)[[:alpha:]]*"),
         Lastname = str_extract(Name, "[[:alpha:]]*(?=,)")) %>% 
  na_if("") %>% 
  mutate(Firstname = case_when(is.na(Name) == FALSE & is.na(Firstname) == TRUE ~ str_split_fixed(Name, " ", n = 2)[,1],
                               is.na(Name) == TRUE | is.na(Firstname) == FALSE ~ Firstname),
         Lastname = case_when(is.na(Name) == FALSE & is.na(Lastname) == TRUE ~ str_split_fixed(Name, " ", n = 2)[,2],
                              is.na(Name) == TRUE | is.na(Lastname) == FALSE ~ Lastname),
         ID = case_when(is.na(Name) == FALSE ~ paste0(str_extract(Firstname, "^.{3,4}"), str_extract(Lastname, "^.{2,5}")),
                        is.na(Name) == TRUE ~ School),
         Name = str_c(Firstname, Lastname, sep = " "))

getmode <- function(x) {
  unique_x <- unique(x[!is.na(x)])
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

all_sections <- all_names %>%
  mutate(School = str_to_title(School),
         School = str_replace_all(School, " - ", "-"),
         School = trimws(School)) %>%
  mutate(
    Section = case_when(
      str_detect(Meet, "States") == TRUE ~ as.character(str_extract(School, "\\d{1,3}")),
      str_detect(Meet, "States") == TRUE & str_detect(School, "^C-[:alpha:]{2,}") == TRUE ~ "Catholic",
      str_detect(Meet, "States") == TRUE & str_detect(School, "^P-[:alpha:]{2,}") == TRUE ~ "PSAL",
      str_detect(Meet, "States") == TRUE & str_detect(School, "^PSAL-") == TRUE ~ "PSAL",
      str_detect(Meet, "States") == FALSE ~ as.character(Section)
    ),
    Section = case_when(is.na(Section) == TRUE & str_detect(School, "^Chsaa-") == TRUE ~ "Catholic",
                        is.na(Section) == TRUE & str_detect(School, "^Ch-") == TRUE ~ "Catholic",
                        # is.na(Section) == TRUE & str_detect(School, "^C-") == TRUE ~ "Catholic", # To agressive, gets C-C (Churchville Chili)
                        is.na(Section) == TRUE & str_detect(School, "^Cs-") == TRUE ~ "Catholic",
                        is.na(Section) == TRUE & str_detect(School, "-Chsaa$") == TRUE ~ "Catholic",
                        is.na(Section) == TRUE & str_detect(School, "-Ch$") == TRUE ~ "Catholic",
                        is.na(Section) == TRUE & str_detect(School, "-Iv$") == TRUE ~ "4",
                        is.na(Section) == TRUE & str_detect(School, "-Vi$") == TRUE ~ "6",
                        is.na(Section) == TRUE & str_detect(School, "-X$") == TRUE ~ "10",
                        is.na(Section) == TRUE & str_detect(School, "-Xi$") == TRUE ~ "1",
                        is.na(Section) == TRUE & str_detect(School, "-Psal$") == TRUE ~ "PSAL",
                        is.na(Section) == TRUE & str_detect(School, "^P-") == TRUE ~ "PSAL",
                        is.na(Section) == TRUE & str_detect(School, "^Ps-") == TRUE ~ "PSAL",
                        is.na(Section) == TRUE & str_detect(School, "^Psal") == TRUE ~ "PSAL",
                        is.na(Section) == FALSE ~ Section)
    # Section = as.numeric(Section)
  ) %>%
  mutate(School = str_replace(School, "\\d+", ""),
         School = str_replace_all(School, "^-", ""),
         School = str_replace(School, "-$", ""),
         School = str_replace(School, "-Zz$", ""),
         School = str_replace(School, "-Ni$", ""),
         School = str_replace(School, "-Da$", ""),
         School = str_replace(School, "-Mr$", ""),
         School = str_replace(School, "-Ad$", ""),
         School = str_replace(School, "-Us$", ""),
         School = str_replace(School, "-Ne$", ""),
         School = str_replace(School, "-Ma$", ""),
         School = str_replace(School, "Central High School", ""),
         School = str_replace(School, "Central School", ""),
         School = str_replace(School, "Senior High School", ""),
         School = str_replace(School, "High School", ""),
         School = str_replace(School, "Boys", ""),
         School = str_replace(School, "Girls", ""),
         School = str_replace(School, "Swimming", ""),
         School = str_replace(School, "Varsity", ""),
         School = str_replace(School, "Swim Team", ""),
         School = str_replace(School, "Swim", ""),
         School = str_replace(School, "^Chsaa-", ""),
         School = str_replace(School, "-Chsaa$", ""),
         School = str_replace(School, "^Ch-", ""),
         School = str_replace(School, "-Ch$", ""),
         School = str_replace(School, "-Sect\\.$", ""),
         School = str_replace(School, "-Sect\\:*$", ""),
         School = str_replace(School, "-Iv$", ""),
         School = str_replace(School, "-Vi$", ""),
         School = str_replace(School, "-X$", ""),
         School = str_replace(School, "-Xi$", ""),
         School = str_replace_all(School, "/", "-"),
         School = str_replace(School, "^P-", ""),
         School = str_replace(School, "^Ps-", ""),
         School = str_replace(School, "^Psal-", ""),
         School = case_when(str_detect(School, "^[:upper:]-[:alpha:]{2,}") == TRUE ~ str_replace(School, "^[:upper:]-", ""),
                            str_detect(School, "^[:upper:]-[:alpha:]{2,}") == FALSE ~ School),
         School = trimws(School)
  ) %>%
  group_by(ID, School) %>%
  fill(Section, .direction = "downup") %>%
  ungroup() %>%
  dplyr::group_by(School, Meet) %>%
  arrange(School, Meet) %>%
  tidyr::fill(Section, .direction = "updown") %>%
  ungroup() %>% 
  group_by(ID, Year) %>%
  mutate(Section = case_when(is.na(Section) == TRUE ~ getmode(Section),
                             is.na(Section) == FALSE ~ Section)) %>%
  ungroup() %>%
  dplyr::group_by(School, Meet) %>%
  arrange(School, Meet) %>%
  tidyr::fill(Section, .direction = "updown") %>%
  ungroup() %>% 
  mutate(School = case_when(str_detect(School, "\\:") == TRUE ~ str_replace(School, "-Sec.*|-Ps.*", ""),
                            TRUE ~ School),
         School = str_replace(School, "-Sect.*", ""))


all_sections_clean <- all_sections %>% 
  mutate(Meet = str_replace(Meet, "\\.result", "")) %>% 
  mutate(Source = str_replace(Source, "\\.result", "")) %>% 
  select(Name, ID, Place, Grade, School, Finals_Time, Event, Meet, Year, Section, Event_Sex, Source) %>% 
  mutate(Section = case_when(Section == 1 ~ "I",
                             Section == 2 ~ "II",
                             Section == 3 ~ "III",
                             Section == 4 ~ "IV",
                             Section == 5 ~ "V",
                             Section == 6 ~ "VI",
                             Section == 7 ~ "VII",
                             Section == 8 ~ "VIII",
                             Section == 9 ~ "IX",
                             Section == 10 ~ "X",
                             Section == 11 ~ "XI"))

all_state_divers <- all_sections_clean %>% 
  group_by(Year, ID) %>% 
  filter(any(str_detect(Meet, "States"))) %>% 
  group_by(Year, ID) %>%
  unique() %>% 
  mutate(Season_Best = max(as.numeric(Finals_Time[str_detect(Meet, "States", negate = TRUE)])),
         State_Score = max(as.numeric(Finals_Time[str_detect(Meet, "States")]), na.rm = TRUE)) %>% 
  filter(str_detect(Meet, "States")) %>% 
  ungroup() %>% 
  filter(Place <= 20) %>% 
  group_by(Year, ID) %>%
  filter(is.infinite(Season_Best) == FALSE,
         Season_Best >= 395) %>% 
  summarise(Place = min(Place),
            Season_Best = max(Season_Best),
            State_Score = max(State_Score),
            Section = getmode(Section),
            Performance_Ratio = State_Score/Season_Best,
            Performance = State_Score - Season_Best)

all_state_divers_sum <- all_state_divers %>%
  ungroup() %>% 
  # group_by(Section, Year) %>%
  group_by(Section) %>%
  filter(Year >= 2015) %>% 
  summarise(Performance = round(mean(Performance, na.rm = TRUE),2),
            `Performance Ratio` = round(mean(Performance_Ratio, na.rm = TRUE), 2)) %>%
  arrange(Performance)

all_state_divers %>%
  ungroup() %>% 
  # group_by(Section, Year) %>%
  group_by(Section) %>%
  filter(Year >= 2015) %>% 
  summarise(Performance = round(mean(Performance, na.rm = TRUE),2),
            `Performance Ratio` = round(mean(Performance_Ratio, na.rm = TRUE), 2)) %>%
  arrange(Performance) %>%
  flextable::flextable() %>% 
  flextable::autofit()

df_centroids_perf <- df_centroids %>% 
  left_join(all_state_divers_sum, by = c("ind" = "Section")) %>% 
  mutate(ind = paste0(ind, ": ", `Performance Ratio`),
         naughty = case_when(is.na(Performance) ~ "N",
                             TRUE ~ "Y"))

ggplot() +
  geom_polygon(
    data = df_2,
    mapping = aes(
      x = x,
      y = y,
      group  = fips,
      fill = Section
    ),
    color = "white"
  ) +
  ggrepel::geom_label_repel(data = df_centroids_perf,
                            aes(
                              label = ind,
                              x = x,
                              y = y,
                              color = naughty
                            ),
                            size = 3.5) +
  theme_void() +
  scale_color_manual(values = c("green", "red")) +
  theme(legend.position = "none") +
  viridis::scale_fill_viridis(discrete = TRUE) +
  labs(title = "New York State Sections")
