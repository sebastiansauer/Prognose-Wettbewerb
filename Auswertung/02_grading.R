
library(tidyverse)
library(magrittr)
library(berryFunctions)
library(R.utils)
library(tidymodels)
library(numform)


source("funs/parse_names.R")
source("funs/prep_csv.R")
source("funs/comp_r2_submissions.R")


# create df for test sample:
test_df <- 
  solution_df %>% 
  select(id, punkte) %>% 
  rename(lebenszufriedenheit = punkte)

write_csv(test_df, "test_sample.csv")


# compute r2 for each submission:
d_r2 <- comp_r2_submissions()

d_grades <- 
  d_r2 %>% 
  mutate(grade_f = cut_interval(r2_vec, 
                               n = 10, 
                               labels = c(
                                 4, 3.7,
                                 3.3, 3, 2.7,
                                 2.3, 2, 1.7,
                                 1.3, 1))) %>% 
  mutate(grade = as.numeric(as.character(grade_f)))


write_rds(d_grades, file = "d_grades.rds")
