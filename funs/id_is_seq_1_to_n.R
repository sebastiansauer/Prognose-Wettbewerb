library(tidyverse)
library(targets)
tar_load("submissions_prepped")

out <- 
submissions_prepped |> 
  mutate(id_is_seq_1_to_n = map_lgl(data, function(df) identical(df$id, 1:88)))



out <- 
  submissions_prepped |> 
  mutate(id_is_seq_1_to_n = map_lgl(data, ~ identical(.x$id, 1:88)))
