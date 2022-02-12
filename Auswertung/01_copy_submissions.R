library(tidyverse)
library(magrittr)





# Read submissions, how man CSV files?
submissions_raw_full <- list.files(path = raw_path,
                          full.names = TRUE,
                          pattern = csv_types,
                          recursive = TRUE) 

submissions_raw_full

length(submissions_raw_full)

# copy in folder for processing submissions:
file.copy(from = submissions_raw_full, to = subm_path,
          overwrite = TRUE)


# here are the names of the submissions:
# how many FOLDERS?
submissions_raw <- list.dirs(path = raw_path,
                             full.names = FALSE,
                             recursive = T)
submissions_raw
length(submissions_raw)

# save result to disk:
write_rds(submissions_raw, file = paste0(results_path, "submissions_raw.rds"))

subm_raw_last_names <- 
  submissions_raw %>% 
  str_match(" (\\w+-?\\w*)_(?:\\d)") %>% 
  `[`(, 2) %>% 
  purrr::discard(is.na) %>% 
  str_to_lower()

subm_raw_last_names

write_rds(subm_raw_last_names, file = paste0(results_path,"subm_raw_last_names.rds"))


# Check for differences in raw submissions and processed submissions:

submissions_processed <- list.files(path = subm_path,
                                    full.names = FALSE,
                                    pattern = ".csv$",
                                    recursive = TRUE)

submissions_processed_last_names <-
  submissions_processed %>% 
  str_to_lower() %>% 
  str_match("[a-z]+")

submissions_processed_last_names


"graz" %in% submissions_processed_last_names



