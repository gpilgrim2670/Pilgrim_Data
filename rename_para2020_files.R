library(dplyr)
library(stringr)

old.names <- list.files(path = "Paralympics2020/to_rename/", pattern = ".pdf")
# old.names <- list.files(path=Paralympics2020/test/, pattern=".pdf")
new.names <- old.names %>% 
  str_remove("-_SWM_C73[:upper:]\\d") %>% 
  str_replace_all("\\-{1,}", "_") %>% 
  str_replace_all("_\\.", "\\.") %>% 
  str_replace_all("(?<=R)(\\d{5})", "_\\1") %>% 
  str_replace_all("__", "_") %>% 
  str_remove_all("_000100")

file.rename(from = paste0("Paralympics2020/to_rename/", old.names), to = paste0("Paralympics2020/to_rename/", new.names))
