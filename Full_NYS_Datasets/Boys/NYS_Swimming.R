library(tidyverse)
library(here)
library(SwimmeR)
library(readxl)
library(data.table)

# All_2019_Path <- here::here("2019_allresults_birthdate.csv")
# All_2019 <- read_csv(All_2019_Path, col_names = FALSE)

#### Reading In Files ####
result_files <- list.files(path = here::here("Meet_Results"), pattern = "*.csv", full.names = T)

read_files <- function(x) {
  x %>%
    read_csv(col_names = FALSE, na = c("---"))
}

results <- map(result_files, read_files)

#### Cleaning Data ####
results_clean <- function(x) {
  x <- x %>%
    select_if( ~ sum(!is.na(.)) > 0)
  x_times <- x %>% 
    select(-c(seq(1,19,1)))
  x_details <- x %>% 
    select(c(seq(1,19,1)))
  
  x_times <- as.data.frame(t(apply(x_times, 1, function(y) {
    return(c(y[!is.na(y)], y[is.na(y)]))
  })))
  x_times <- x_times %>% 
    select_if( ~ sum(!is.na(.)) > 0)
  
  full <- cbind(x_details, x_times)
  full <- full %>% 
    select(c(seq(1,29,1)))
  
  return(full)

}

results_cleaned <- map(results, results_clean)

All_Results_Raw <- rbindlist(results_cleaned, use.names = FALSE)

#### Formatting Data ####
All_Results <- All_Results_Raw %>% 
  mutate(
    X1 = NULL,
    X2 = NULL,
    X5 = NULL,
    X9 = NULL,
    X10 = NULL,
    X11 = NULL,
    X12 = NULL,
    X20 = NULL
  ) %>%
  select(
    Name = X13,
    School = X14,
    Section = X15,
    Time = X16,
    Leadoff = X17,
    DQ = X18,
    PF = X19,
    Distance = X21,
    Stroke = X22,
    Place = X23,
    Meet_Date = X24,
    Meet_Name = X25,
    Split_50 = V1,
    Split_100 = V2,
    Split_150 = V3,
    Split_200 = V4,
    Split_250 = V5,
    Split_300 = V6,
    Split_350 = V7,
    Split_400 = V8,
    split_450 = V9,
    Split_500 = V10
  ) %>%
  mutate(
    Meet_Date = str_replace_all(Meet_Date, "/", "-"),
    Meet_Date = as.Date(Meet_Date, tz = "EST", format = "%m-%d-%Y"),
    Meet_Name = str_to_title(Meet_Name),
    Distance = na_if(Distance, "1 Meter"),
    Distance = as.integer(Distance),
    Time = str_remove_all(Time, "Y"),
    Time = str_remove_all(Time, "X"),
    Time = trimws(Time),
    DQ = case_when(DQ == "DQ" ~ "DQ",
                   Time == "DQ" ~ "DQ",
                   Time == "NS" ~ "NS"),
    Time = replace(Time, Time == "NS", NA),
    Time = replace(Time, Time == "DQ", NA),
    Diving_Score = case_when(str_detect(Stroke, "Dive") == TRUE ~ Time),
    Time = case_when(
      str_detect(Stroke, "Dive") == TRUE ~ "61",
      str_detect(Stroke, "Dive") == FALSE ~ Time
    ),
    Time = replace(Time, as.numeric(Time) > 60, NA)
  ) %>%
  # filter(is.na(Time) == FALSE) %>%
  mutate(
    Time_sec = sec_format(Time),
    Sex = case_when(
      str_detect(Name, " F ") == TRUE ~ "F",
      str_detect(Name, " F$") == TRUE ~ "F",
      str_detect(Name, " M ") == TRUE ~ "M",
      str_detect(Name, " M$") == TRUE ~ "M"
    ),
    Name = str_remove(Name, " F "),
    Name = str_remove(Name, " M "),
    Grade = str_extract(Name, "(Yr: ..)"),
    Grade = str_remove(Grade, "Yr: "),
    Grade = str_remove(Grade, "t"),
    Grade = str_remove(Grade, "\\)"),
    Grade = case_when(
      Grade == "FR" ~ 9,
      Grade == "SO" ~ 10,
      Grade == "JR" ~ 11,
      Grade == "SR" ~ 12,
      is.character(Grade) == TRUE ~ as.numeric(Grade)
    ),
    Name = str_remove(Name, "  \\(Yr: ..\\)"),
    Name = str_remove(Name, "  \\(Yr: .\\)"),
    Age = str_extract(Name, " \\(\\d\\d\\) "),
    Age = str_remove(Age, "\\) "),
    Age = str_remove(Age, " \\("),
    Age = as.numeric(Age),
    Age = replace(Age, Age < 12, NA),
    Name = str_remove(Name, " \\(\\d\\d\\) "),
    Birthday = as.Date(str_extract(Name, "\\d.*\\d"), format = "%m/%d/%Y", tz = "est"),
    Meet_Age = Meet_Date - Birthday,
    Name = str_remove(Name, "\\d.*\\d"),
    # Name = trimws(Name),
    Name = str_remove(Name, " M$"),
    Name = str_remove(Name, " F$"),
    Name = str_remove(Name, " \\(\\d\\)"),
    Name = str_remove(Name, "\\) "),
    Name = str_remove(Name, " \\("),
    Name = str_replace(Name, "A J", "Aj"),
    # Fixes issue with "A J Blake" vs. "Aj Blake"
    Name = trimws(Name),
    ID = paste0(str_extract(Name, "^.{3}"), str_extract(Name, ".{5}$"))
  )


###### Dealing With Sections ######
All_Results_Meets <- All_Results %>%
  mutate(Meet_Name_Corrected = Meet_Name) %>% 
  mutate(Meet_Name_Corrected = str_replace(Meet_Name_Corrected, "2011  Girls Swimming And Diving Class C", "2011 Girls Swimming And Diving Section V Class C"),
         Meet_Name_Corrected = str_replace(Meet_Name_Corrected, " Girls Swimming And Diving Class ", " Girls Swimming And Diving Section V Class ")) %>% 
  mutate(Meet_Name_Corrected = case_when(Meet_Name == "Class A" ~ "2015 Girls Section 4 Class A",
                               Meet_Name == "Class C" ~ "2015 Girls Section 4 Class C",
                               Meet_Name == "2014 Class A Sectionals" ~ "2014 Girls Section 4 Class A",
                               Meet_Name == "2016 Class A Sectionals" ~ "2016 Girls Section 4 Class A",
                               Meet_Name == "2016 Class B Sectionals" ~ "2016 Girls Section 4 Class B",
                               Meet_Name == "2016 Class C Sectionals" ~ "2016 Girls Section 4 Class C",
                               Meet_Name == "Boys Sectional Championships" ~ "Boys Section 6 Championships",
                               Meet_Name != "Class A" &
                                 Meet_Name != "Class C" &
                                 Meet_Name != "Boys Sectional Championships" &
                                 Meet_Name != "2014 Class A Sectionals" &
                                 Meet_Name != "2016 Class A Sectionals" &
                                 Meet_Name != "2016 Class B Sectionals" &
                                 Meet_Name != "2016 Class C Sectionals" ~ Meet_Name_Corrected)) %>%
  mutate(Section_Working = str_extract(Meet_Name_Corrected, "Section [[:alnum:]]*"),
         Section_Working = case_when(is.na(Section_Working) == FALSE ~ Section_Working,
                                     is.na(Section_Working) == TRUE ~ str_extract(Meet_Name_Corrected, "Sec [[:alnum:]]*"),
                                     is.na(Section_Working) == TRUE ~ str_extract(Meet_Name_Corrected, "Sect [[:alnum:]]*")),
         Section_Working = str_remove(Section_Working, "Section "),
         Section_Working = str_remove(Section_Working, "Sec "),
         Section_Working = str_remove(Section_Working, "Sect "),
         Section_Working = case_when(Section_Working == "1" ~ "1",
                                     Section_Working == "2" ~ "2",
                                     Section_Working == "3" ~ "3",
                                     Section_Working == "3" ~ "3",
                                     Section_Working == "3" ~ "3",
                                     Section_Working == "4" ~ "4",
                                     Section_Working == "5" ~ "5",
                                     Section_Working == "6" ~ "6",
                                     Section_Working == "7" ~ "7",
                                     Section_Working == "8" ~ "8",
                                     Section_Working == "9" ~ "9",
                                     Section_Working == "10" ~ "10",
                                     Section_Working == "11" ~ "11",
                                     Section_Working == "I" ~ "1",
                                     Section_Working == "II" ~ "2",
                                     Section_Working == "III" ~ "3",
                                     Section_Working == "IV" ~ "4",
                                     Section_Working == "V" ~ "5",
                                     Section_Working == "VI" ~ "6",
                                     Section_Working == "VII" ~ "7",
                                     Section_Working == "VIII" ~ "8",
                                     Section_Working == "IX" ~ "9",
                                     Section_Working == "X" ~ "10",
                                     Section_Working == "XI" ~ "11",
                                     Section_Working == "Ii" ~ "2",
                                     Section_Working == "Iii" ~ "3",
                                     Section_Working == "Iv" ~ "4",
                                     Section_Working == "Vi" ~ "6",
                                     Section_Working == "Vii" ~ "7",
                                     # Section_working == "ViiX" ~ "7",
                                     Section_Working == "Viii" ~ "8",
                                     Section_Working == "Ix" ~ "9",
                                     Section_Working == "Xi" ~ "11",
                                     Section_Working == "One" ~ "1",
                                     Section_Working == "Two" ~ "2",
                                     Section_Working == "Three" ~ "3",
                                     Section_Working == "Four" ~ "4",
                                     Section_Working == "Five" ~ "5",
                                     Section_Working == "Six" ~ "6",
                                     Section_Working == "Seven" ~ "7",
                                     Section_Working == "Eight" ~ "8",
                                     Section_Working == "Nine" ~ "9",
                                     Section_Working == "Ten" ~ "10",
                                     Section_Working == "Eleven" ~ "11",
                                     Section_Working == "Twelve" ~ "12"),
         Section_Working = as.numeric(Section_Working),
         Section = str_extract(Section, "[[\\d*]]")) %>%
  select(Meet_Name, Meet_Name_Corrected, Section_Working) %>%
  unique()

All_Results <- All_Results %>%
  left_join(All_Results_Meets, by = "Meet_Name") %>%
  mutate(Section = str_extract(Section, "[[\\d*]]"),
         Section = as.numeric(Section),
         Section = case_when(is.na(Section) == FALSE ~ Section,
                             is.na(Section) == TRUE ~ Section_Working),
         Meet_Name = Meet_Name_Corrected,
         Meet_Name_Corrected = NULL,
         Meet_Type = case_when(str_detect(Meet_Name, "NYS") == TRUE & str_detect(Meet_Name, "Sec") == FALSE ~ "State",
                               str_detect(Meet_Name, "Sec") == TRUE ~ "Sectional",
                               str_detect(Meet_Name, "Class") == TRUE ~ "Sectional")) %>%
  group_by(ID, School) %>%
  fill(Section, .direction = "downup") %>%
  ungroup() %>%
  dplyr::group_by(School, Meet_Name) %>%
  arrange(School, Meet_Name) %>%
  tidyr::fill(Section, .direction = "updown") %>%
  ungroup()

# All_2019_Cleaned_Test %>%
#   select(School, Section, Meet_Name) %>%
#   unique() %>%
#   View()

####### Dealing With Team Names ########
# unique(All_2019_Cleaned$School)

files <- list.files(path = here::here("Team_Codes"), pattern = "*.xlsx", full.names = T)

read_team_codes <- function(x) {
  x %>%
    read_excel(col_names = FALSE)
}

team_codes <- map(files, read_team_codes)

code_file_names <- str_split_fixed(string = files, pattern = "/", n = 5)[,5]
code_file_names <- str_split_fixed(string = code_file_names, pattern = "_", n = 3)[,3]
code_file_names <- str_replace(code_file_names, ".xlsx", "")
code_file_names <- str_split_fixed(code_file_names, pattern = "_", n = 2)[,2]

names(team_codes) <- code_file_names
team_codes_df <- rbindlist(team_codes, use.names = TRUE, idcol = "Section")

Teams_DF <- team_codes_df %>%
  select(Section, School_Full = 3, School_Code = 4)%>%
  mutate(Section = as.numeric(Section),
         School_Code = str_replace(School_Code, "[:punct:]", ""))

All_Results_Section <- All_Results %>%
  ungroup() %>%
  mutate(Sec_Extract = as.numeric(str_match(School, "[:digit:]*")),
         Section = case_when(is.na(Section) == TRUE & is.na(Sec_Extract) == FALSE ~ Sec_Extract,
                             is.na(Section) == TRUE & is.na(Sec_Extract) == TRUE ~ Section,
                             is.na(Section) == FALSE & is.na(Section_Working) == FALSE ~ Section_Working),
         School = map(str_extract_all(School, "[:alpha:]+"), ~ str_c(., collapse="")),
         School = unlist(School),
         Section = replace(Section, Section == 10, 7),
         Meet_Year = as.numeric(format(as.Date(Meet_Date, format = "%d/%m/%Y"), "%Y"
         ))) %>% 
  group_by(School, Meet_Name) %>% 
  # mutate(Section = case_when(str_detect(Meet_Name, "Class") == TRUE ~ fill(Section, .direction = "downup"),
  #                            str_detect(Meet_Name, "Class") == FALSE ~ Section)) %>% 
  fill(Section, .direction = "downup") %>%
  ungroup() %>% 
  left_join(Teams_DF, by = c("Section", "School" = "School_Code"))
  # group_by(ID, School_Full, Meet_Year) %>% 
  # fill(Section, .direction = "downup")


#### For Troubleshooting Section Naming Within Sectional Meets ####
All_Results_Section %>%
  filter(Meet_Name == "2018 Girls Swimming and Diving Class A",
         Stroke == "Fly") %>%
  select(Name,
         School,
         Distance,
         Stroke,
         Time_sec,
         Time,
         Section,
         Meet_Type,
         Section_Working) %>%
  View()

All_Results_Section %>% 
  filter(School == "IHCA") %>% 
  select(Section, Meet_Name) %>% 
  unique() %>% 
  View()

All_Results_Section %>%
  filter(is.na(School_Full) == TRUE,
         Section == 10
         ) %>%
  select(School, Section, Name, Meet_Name, Meet_Date) %>%
  unique() %>%
  arrange(School) %>%
  View()



#### State Cuts ####
State_Cuts <- read_excel("State_Cuts.xlsx")

State_Cuts <- State_Cuts %>% 
  mutate(NYS_Cut_sec = sec_format(NYS_Cut),
         AAC_Cut_sec = sec_format(AAC_Cut),
         AA_Cut_sec = sec_format(AA_Cut))

State_Cuts_Long <- State_Cuts %>% 
  select(Year, Stroke, Distance, Sex, NYS_Cut_sec, AAC_Cut_sec, AA_Cut_sec) %>% 
  pivot_longer(cols = c(NYS_Cut_sec, AAC_Cut_sec, AA_Cut_sec), names_to = "Cut", values_to = "Cut_Time")

All_Results_Section %>%
  filter(
    Section == 4,
         Sex == "M",
         Distance == 100,
         Stroke == "Fly") %>%
  mutate(Meet_Year = as.numeric(format(
    as.Date(Meet_Date, format = "%d/%m/%Y"), "%Y"
  ))) %>%
  group_by(ID, Meet_Year, Distance, Stroke, School_Full, Section) %>%
  summarise(Best_Time = min(Time_sec, na.rm = TRUE)) %>%
  filter(Best_Time > 40,
         Meet_Year >= 2009) %>% 
  # View()
  ggplot(aes(x = as.factor(Meet_Year), y = Best_Time)) +
  geom_violin(position = position_dodge(1)) +
  geom_line(
    data = State_Cuts_Long %>% filter(Stroke == "Fly", Distance == 100, Sex == "M"),
    aes(x = as.factor(Year), y = Cut_Time, group = Cut, color = Cut), alpha = 0.6
  ) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0) +
  ylim(c(43, 100)) +
  theme_bw()


All_Results_Section %>%
  filter(
    School_Full == "Lansing",
    Sex == "M",
    Distance == 100,
    Stroke == "Fly") %>%
  mutate(Meet_Year = as.numeric(format(
    as.Date(Meet_Date, format = "%d/%m/%Y"), "%Y"
  ))) %>%
  group_by(ID, Meet_Year, Distance, Stroke, School_Full, Section) %>%
  summarise(Best_Time = min(Time_sec, na.rm = TRUE)) %>%
  filter(Best_Time > 40,
         Meet_Year >= 2009) %>% 
  # View()
  ggplot(aes(x = as.factor(Meet_Year), y = Best_Time)) +
  geom_violin(position = position_dodge(1)) +
  geom_line(
    data = State_Cuts_Long %>% filter(Stroke == "Fly", Distance == 100, Sex == "M"),
    aes(x = as.factor(Year), y = Cut_Time, group = Cut, color = Cut), alpha = 0.6
  ) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0) +
  # ylim(c(43, 100)) +
  theme_bw()

ggplot() +
  geom_line(
    data = State_Cuts %>% filter(Stroke == "Fly", Distance == 100, Sex == "F"),
    aes(x = as.factor(Year), y = AAC_Cut_sec, group = as.factor(Year), size = 3, color = "red"))

#### Work In Progress ####
All_2019_Team_Codes %>%
  filter(Sex == "F") %>%
  group_by(School) %>%
  select(School, Section) %>%
  unique() %>%
  View()

# All_2019_Cleaned %>%
# group_by(ID) %>%
# select(ID, Name) %>%
# unique() %>%
# View()


All_2019_Cleaned %>%
  filter(Stroke == "Fly",
         Distance == 100,
         is.na(DQ) == TRUE,
         Sex == "M") %>%
  ggplot(aes(y = Time_sec, x = Age, color = as.factor(Age))) +
  geom_violin() +
  geom_jitter(position = position_jitter(width = 0.08), alpha = 0.5, size = 3) +
  scale_y_continuous(labels = scales::trans_format("identity", mmss_format)) +
  theme_bw()
