
library(tidyverse)
library(magrittr)
library(berryFunctions)
library(R.utils)
library(tidymodels)
library(numform)


source("funs/parse_names.R")
source("funs/prep_csv.R")
source("funs/comp_r2_submissions.R")
source("funs/grade_em.R")


solution_df <- read_csv(solution_df_path)



# create df for test sample:
test_df <- 
  solution_df %>% 
  select(id, y)


# write results:

#write_csv(test_df, paste0(results_path, "test_sample.csv"))



# compute error value for each submission:
d_error <- comp_error_submissions(path_to_submissions = subm_path)

write_rds(x = d_error, file = paste0(results_path,"d_error.rds"))
#d_error <- read_rds(file = paste0(results_path,"d_error.rds"))

d_error2 <-
  d_error %>% 
  select(id, last_name, first_name, matrikelnummer, npreds, error_value)


# grading sequence:
# THIS LINE IS IMPORTANT! 
# Here we decide which performance is "good" and which means "fail"
mae_seq <- c(Inf, seq(from = 1.3, to = 1.1, length.out = 10), 0)


d_grades <- 
  d_error2 %>% 
  mutate(grade_f = grade_em(x = error_value,
                          breaks12 = mae_seq,
                          reverse = TRUE)) %>% 
  mutate(grade = as.numeric(as.character(grade_f)))


# 
# d_grades <- 
#   d_error %>% 
#   mutate(grade_f = cut_interval(error_value, 
#                                n = 10, 
#                                labels = c(
#                                  4, 3.7,
#                                  3.3, 3, 2.7,
#                                  2.3, 2, 1.7,
#                                  1.3, 1))) %>% 
#   mutate(grade = as.numeric(as.character(grade_f)))

write_csv(d_grades, file = paste0(results_path, "d_grades.rds"))
write_rds(d_grades, file = paste0(results_path, "d_grades.rds"))
#d_grades <- read_rds(file = paste0(results_path, "d_grades.rds"))
