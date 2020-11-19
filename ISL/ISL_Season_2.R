library(swimmeR)
library(purrr)

ISL_1 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_16102020_Budapest_Match_1.pdf"
ISL_2 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_18102020_Budapest_Match_2.pdf"
ISL_3 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_24102020_Budapest_Match_3.pdf"
ISL_4 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_26102020_Budapest_Match_4.pdf"
ISL_5 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_30102020_Budapest_Match_5.pdf"
ISL_6 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_01112020_Budapest_Match_6.pdf"
ISL_7 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_05112020_Budapest_Match_7.pdf"
ISL_8 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_05112020_Budapest_Match_8.pdf"
ISL_9 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_09112020_Budapest_Match_9.pdf"
ISL_10 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_09112020_Budapest_Match_10.pdf"
ISL_Semi_1 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_14112020_Budapest_Semi_1.pdf"
ISL_Semi_2 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_15112020_Budapest_Semi_2.pdf"

ISL_Season_2 <- c(ISL_1, ISL_2, ISL_3, ISL_4, ISL_5, ISL_6, ISL_7, ISL_8, ISL_9, ISL_10, ISL_Semi_1, ISL_Semi_2)
ISL_Season_2 <- map(ISL_Season_2, read_results)
ISL_Season_2 <- map(ISL_Season_2, swim_parse_ISL)

ISL_names <- c(paste("Match", seq(1:10), sep = "_"), paste("Semi", seq(1:2), sep = "_"))

names(ISL_Season_2) <- ISL_names

ISL_Season_2 <- bind_rows(ISL_Season_2, .id = 'Source')


ISL_Season_2 %>% 
  group_by(Team) %>% 
  summarise(Total_Points = sum(Points, na.rm = TRUE)/4) %>% 
  arrange(desc(Total_Points))

ISL_Season_2 %>% 
  filter(is.na(Name) == FALSE) %>% 
  group_by(Name) %>% 
  summarise(Total_Points = sum(Points, na.rm = TRUE),
            Team = unique(Team)) %>% 
  arrange(desc(Total_Points)) %>% 
  head(5)
