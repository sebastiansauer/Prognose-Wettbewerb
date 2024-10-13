library(tidyverse)
library(teachertools)

late_subm_path <- "/Users/sebastiansaueruser/github-repos/_other/Prognose-Wettbewerb-SeS/exams/qm1-2024-sose/late/Gaza_Nicole_00170045_Prognose.csv"

late_subm_df <- read.csv(late_subm_path)

solution_df <- read_csv("/Users/sebastiansaueruser/github-repos/_other/Prognose-Wettbewerb-SeS/exams/qm1-2024-sose/data/d_control.csv")







yardstick::rmse_vec(
  truth = solution_df$reaction_time,
  estimate = late_subm_df$pred)





main_df <- 
bann_file_path |> 
  sanitize_pred_csv(paths$subm_proc,
                    filename_with_path = TRUE,
                    add_filename = TRUE,
                    alternative_name_for_pred = "reaction_time") |> 
  add_solution_col(solution_df, 
                   name_output_var = "reaction_time") |> 
  build_main_df() |> 
  add_rmse_col()

