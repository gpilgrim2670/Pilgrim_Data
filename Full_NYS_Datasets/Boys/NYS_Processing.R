library(tidyverse)
library(readxl)
library(SwimmeR)

results_DF <- boys_results %>% 
  # mutate(X = NULL) %>% 
  rbind(boys_results_states) %>% 
  rbind(girls_results) %>% 
  rbind(girls_results_states)

# results_DF_working <- results_DF %>% #Might be missing section 10
results_DF_working <- all_results %>% 
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
    Event = str_replace(Event, "Freestyle Relay", "FR"),
    Distance = as.numeric(str_extract(Event, "\\d+")),
    Stroke = str_split_fixed(Event, " ", n = 4)[,4]) %>% 
  filter(str_detect(Stroke, "Butterfly|Backstroke|Breaststroke|Freestyle|Relay|MR|Medley|FR|Im$|\\bIm\\b|\\bDiving\\b|Dive") == TRUE,
         Distance %in% c(50, 100, 200, 400, 500)) %>% 
  mutate(Stroke = unlist(map(str_extract_all(Stroke, "Butterfly|Backstroke|Breaststroke|Freestyle|Relay|MR|Medley|FR|Im$|\\bIm\\b|\\bDiving\\b|Dive"), ~ str_c(., collapse=" ")), recursive = FALSE),
    Event = str_c(Distance, Stroke, sep = " "),
    Event = str_replace(Event, "Relay Relay", "Relay"),
    Event = str_replace(Event, "Freestyle Relay", "FR"),
    Event = str_replace(Event, "Medley Relay", "MR")
    ) %>% 
  filter(Event %in% c("50 Freestyle", "100 Freestyle", "200 Freestyle", "500 Freestyle", "100 Backstoke", "100 Breaststroke", "100 Butterfly", "200 Im", "200 MR", "200 FR", "400 FR")) %>% 
  # mutate(
  #        # Source = NULL
  #        ) %>% 
  mutate(Event = str_replace(Event, "Im", "IM"),
         Stroke = str_replace(Stroke, "Im", "IM"))

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

Schools <-
  read_csv(
    "Schools.csv",
    col_types = cols(Section = col_character(),
                     Section_Suppliment = col_character())
  ) %>% 
  filter_all(any_vars(complete.cases(.)))  

Schools_2 <-
  read_csv(
    "Schools_2.csv",
    col_types = cols(Section = col_character(),
                     Section_Sup_2 = col_character())
  ) %>% 
  filter_all(any_vars(complete.cases(.)))  

all_sections <- all_sections %>% 
  mutate(School = str_replace(School, "\\? ", "")) %>% 
  na_if("")

all_schools_csv <- all_sections %>% 
  select(School, Section) %>% 
  full_join(Schools, by = c("School", "Section")) %>% 
  full_join(Schools_2, by = c("School", "Section", "School_Full")) %>% 
  unique()

write.csv(all_schools_csv, "all_schools.csv", row.names = FALSE)

all_schools <- all_sections %>%
  ungroup() %>%
  left_join(Schools, by = c("School" = "School", "Section" = "Section")) %>%
  # mutate(Section.y = NULL) %>%
  # rename(Section = Section.x) %>%
  group_by(ID, Year) %>%
  mutate(
    School_Full = case_when(is.na(School_Full) == TRUE ~ School,
                            TRUE ~ School_Full),
    School_Full = School_Full[which.max(str_length(School_Full))]
  ) %>%
  ungroup() %>%
  mutate(
    ID = case_when(ID == School ~ School_Full,
                   TRUE ~ ID),
    School = School_Full,
    School_Full = NULL
  ) %>%
  left_join(Schools_2, by = c("School" = "School", "Section" = "Section")) %>%
  mutate(Section = case_when(is.na(Section) == TRUE ~ Section_Sup_2,
                             TRUE ~ Section)) %>%
  # group_by(ID, Year) %>%
  mutate(
    School = case_when(is.na(School_Full) == TRUE ~ School,
                            TRUE ~ School_Full),
    # School = School[which.max(str_length(School))]
  ) %>%
  # group_by(ID, Year) %>% 
  # mutate(School = School[which.max(str_length(School))]) %>% 
  # ungroup() %>%
  mutate(
    ID = case_when(Stroke %in% c("FR", "MR") ~ School,
                   TRUE ~ ID),
  # School = School_Full,
  School_Full = NULL
  ) %>%
  group_by(School, Year) %>%
  tidyr::fill(Section, .direction = "updown") 

# NA_School_List <- as.list(Schools_2 %>%
# filter(is.na(School_Full)) %>%
# select(School))
# NA_School_List <- NA_School_List[1]
# NA_School_List <- unlist(NA_School_List, recursive = FALSE)
# NA_School_List
# 
# boys_schools %>% 
#   ungroup() %>% 
#   select(School, School_Full, Section, Source) %>% 
#   filter(School %in% NA_School_List) %>% 
#   unique() %>% 
#   View()
# 
# boys_schools %>%
#   mutate(Section = getmode(Section)) %>%
#   select(School, Section, Year) %>% 
#   unique() %>% 
#   View()
# 
# boys_schools %>% 
#   ungroup() %>% 
#   select(School, Source, Section) %>% 
#   unique() %>% 
#   View()

all_sections %>% 
  ungroup() %>% 
  left_join(Schools, by = c("School" = "School", "Section" = "Section")) %>% 
  select(School, School_Full, Section) %>% 
  unique() %>% 
  write.csv("Schools_2.csv")
              

# boys_sections %>% 
#   select(School, Section) %>%
#   # filter(is.na(Section) == TRUE) %>% 
#   unique() %>% 
#   View()

all_grades <- all_schools %>% 
  mutate(Grade = case_when(
    Grade == "12" ~ "SR",
    Grade == "11" ~ "JR",
    Grade == "10" ~ "SO",
    Grade == "9" ~ "FR",
    Grade == "8" ~ "8th",
    Grade == "7" ~ "7th",
    # Grade == "0\\d" ~ as.character((2000 + as.numeric(Grade)) - Year),
    TRUE ~ Grade)) %>% # Add something for dealing with "06", "05" etc.
  na_if("") %>% 
  mutate(Grade = replace(Grade, Grade %!in% c("FR", "SO", "JR", "SR", "8th", "7th"), NA),
         Grade = case_when(
           Grade == "7th" ~ 7,
           Grade == "8th" ~ 8,
           Grade == "FR" ~ 9,
           Grade == "SO" ~ 10,
           Grade == "JR" ~ 11,
           Grade == "SR" ~ 12
         )) %>% 
  group_by(ID, School) %>% #Need to fix schools first
  mutate(
    Grade = coalesce(Grade, first(Grade[!is.na(Grade)]) - (first(Year[!is.na(Grade)]) - Year))
  )

library(readxl)
library(SwimmeR)
State_Cuts <- readxl::read_excel("State_Cuts.xlsx")

State_Cuts <- State_Cuts %>% 
  mutate(NYS_Cut_sec = sec_format(NYS_Cut),
         AAC_Cut_sec = sec_format(AAC_Cut),
         AA_Cut_sec = sec_format(AA_Cut)) %>% 
  mutate(Stroke = case_when(Stroke == "Free" ~ "Freestyle",
                            Stroke == "Fly" ~ "Butterfly",
                            Stroke == "Back" ~ "Backstroke",
                            Stroke == "Breast" ~ "Breaststroke",
                            TRUE ~ Stroke),
         Event = paste(Distance, Stroke, sep = " "),
         Event = str_replace(Event, "^NA ", ""),
         
         Stroke = NULL,
         Distance = NULL)

State_Cuts_Long <- State_Cuts %>% 
  select(Year, Event, Sex, NYS_Cut_sec, AAC_Cut_sec, AA_Cut_sec) %>% 
  pivot_longer(cols = c(NYS_Cut_sec, AAC_Cut_sec, AA_Cut_sec), names_to = "Cut", values_to = "Cut_Time")
  # mutate(Stroke = case_when(Stroke == "Free" ~ "Freestyle",
  #                           Stroke == "Fly" ~ "Butterfly",
  #                           Stroke == "Back" ~ "Backstroke",
  #                           Stroke == "Breast" ~ "Breaststroke",
  #                           TRUE ~ Stroke),
  #        Event = paste(Distance, Stroke, sep = " "),
  #        Event = str_replace(Event, "^NA ", ""),
  #        
  #        Stroke = NULL,
  #        Distance = NULL)

all_grades %>%
  filter(
    # str_detect(Meet, "States") == TRUE,
    str_detect(Event, "100 Butterfly") == TRUE) %>%
  mutate(Finals_sec = sec_format(Finals_Time)) %>% 
  group_by(ID, School, Year) %>%
  summarise(Best_Time = min(Finals_sec, na.rm = TRUE)) %>%
  filter(Best_Time > 42,
         Best_Time < 80) %>% 
  # View()
  ggplot(aes(x = as.factor(Year), y = Best_Time)) +
  geom_violin(position = position_dodge(1)) +
  geom_line(
    data = State_Cuts_Long %>% filter(Event == "100 Butterfly", Sex == "M"),
    aes(x = as.factor(Year), y = Cut_Time, group = Cut, color = Cut)
  ) +
  # geom_jitter(alpha = 0.6, width = 0.1, height = 0) +
  # ylim(c(43, 100)) +
  theme_bw()

State

all_test <- all_grades %>%
  # left_join(State_Cuts %>% filter(Sex == "M") %>% select(-Sex), by = c("Year", "Event")) %>% 
  mutate(Finals_sec = sec_format(Finals_Time)) %>% 
  group_by(ID, Year, School, Event) %>% 
  summarise(Best_Time = min(Finals_sec, na.rm = TRUE)) %>% 
    left_join(State_Cuts %>% filter(Sex == "M") %>% select(-Sex), by = c("Year", "Event")) %>% 
    mutate(State = case_when(Best_Time <= NYS_Cut_sec ~ "Y",
                             Best_Time > NYS_Cut_sec ~ "N"),
           AAC = case_when(Best_Time <= AAC_Cut_sec ~ "Y",
                     Best_Time > AAC_Cut_sec ~ "N"),
           AA = case_when(Best_Time <= AA_Cut_sec ~ "Y",
                          Best_Time > AA_Cut_sec ~ "N")) %>% 
  group_by(Year) 
  
all_percent_cuts <- all_test %>%
  ungroup() %>%
  group_by(ID, Year, School) %>% 
  # View()
  mutate(State = any(State == "Y", na.rm = TRUE),
         AAC = any(AAC == "Y", na.rm = TRUE),
         AA = any(AA == "Y", na.rm = TRUE)) %>% 
  select(ID, Year, School, State, AAC, AA) %>% 
  unique() %>% 
  # View()
  ungroup() %>% 
  group_by(Year) %>% 
  summarise(NYS_Porp = round(100 * sum(State == TRUE, na.rm = TRUE) / n(), 2),
            AAC_Porp = round(100 * sum(AAC == TRUE, na.rm = TRUE) / n(), 2),
            AA_Porp = round(100 * sum(AA == TRUE, na.rm = TRUE) / n(), 2))

all_grades %>% 
  filter(
    # str_detect(Meet, "States") == TRUE,
    str_detect(Event, "100 Butterfly") == TRUE,
    between(Year, 2014, 2019)) %>%
  mutate(Finals_sec = sec_format(Finals_Time)) %>% 
  group_by(ID, School, Meet, Year) %>%
  summarise(Best_Time = min(Finals_sec, na.rm = TRUE)) %>%
  filter(Best_Time > 42,
         Best_Time < 80) %>% 
  # View()
  ggplot(aes(x = as.factor(Year), y = Best_Time)) +
  geom_violin(position = position_dodge(1)) +
  geom_line(
    data = State_Cuts_Long %>% filter(Event == "100 Butterfly", Sex == "M", between(Year, 2014, 2019)),
    aes(x = as.factor(Year), y = Cut_Time, group = Cut, color = Cut)
  ) +
  # geom_jitter(alpha = 0.6, width = 0.1, height = 0) +
  # ylim(c(43, 100)) +
  theme_bw()
