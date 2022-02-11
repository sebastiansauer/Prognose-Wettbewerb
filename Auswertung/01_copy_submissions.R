library(tidyverse)
library(magrittr)





# Read submissions:
submissions_raw <- list.files(path = raw_path,
                          full.names = TRUE,
                          pattern = ".csv$",
                          recursive = TRUE) 

file.copy(submissions_raw, subm_path)



submissions_folders <- list.dirs(path = raw_path,
                                 full.names = FALSE,
                                 recursive = T)
submissions_folders

write_rds(submissions_folders, file = paste0(results_path, "submissions_folders.rds"))

subm_last_names <- 
  submissions_folders %>% 
  str_match(" (\\w+-?\\w*)_(?:\\d)") %>% 
  `[`(, 2) %>% 
  purrr::discard(is.na)

subm_last_names

write_rds(subm_last_names, file = paste0(results_path,"subm_last_names.rds"))


