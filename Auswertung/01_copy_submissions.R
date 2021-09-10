library(tidyverse)
library(magrittr)


raw_path <- "Rohdaten/"

# Parse submssions:
submissions_raw <- list.files(path = raw_path,
                          full.names = TRUE,
                          pattern = ".csv$",
                          recursive = TRUE) 

file.copy(submissions_raw, "Submissions")



submissions_folders <- list.dirs(path = raw_path,
                                 full.names = FALSE,
                                 recursive = )
submissions_folders

subm_last_names <- 
  submissions_folders %>% 
  str_match(" (\\w+-?\\w*)_(?:\\d)") %>% 
  `[`(, 2) %>% 
  purrr::discard(is.na)

subm_last_names

write_rds(subm_last_names, file = "subm_last_names.rds")


