# QM1, 23-SoSe

library(targets)  
library(tarchetypes)  # tar_files
library(tidyverse)


# source functions:
funs_paths <- list.files(path = "funs",
                         full.names = TRUE,
                         pattern = ".R")
map(funs_paths, source)
source("debugging.R")


# set packages to be available for all targets:
tar_option_set(packages = c("tidyverse"))


# define target steps list:
list(
  
  # define paths:
  tar_target(paths, def_paths()),
  tar_target(names_processed, process_submissions(paths)),
  tar_files(submissions_copied, 
             list.files(path = paste0(here::here(),"/", paths$subm_proc),
                        full.names = TRUE,
                        pattern = paths$csv_pattern,
                        recursive = TRUE)),
  tar_target(submissions_prepped,
             submissions_copied |> 
               prep_csv(path_to_submissions = NULL) |> 
               write_csv(file = submissions_copied),
             pattern = map(submissions_copied)),

  tar_target(names_raw, parse_names_raw(paths)),
  tar_target(names_diff, diff_names(names_raw = names_raw, 
                                    names_processed = names_processed)),
  tar_target(grades_thesholds_file, "Daten/grades_thresholds.csv", format = "file"),
  tar_target(grades_thresholds, read.csv(grades_thesholds_file)),
  tar_target(performance_students, 
             comp_error_submissions(path_to_submissions = paths$subm_proc,
                                    name_output_var = "count",
                                    path_to_train_data = paths$train_df,
                                    path_to_test_data = paths$solution_df,
                                    error_fun = yardstick::rmse,
                                    verbose = TRUE,
                                    start_id = 1),
             packages = "teachertools"),
  tar_target(grade_scheme, set_names(grades_thresholds$threshold, grades_thresholds$grade)),
  tar_target(no_shows_file, paths$no_shows_file, format = "file"),
  tar_target(no_shows, read_csv(no_shows_file, col_types = "cccd")),
  tar_target(grades, assign_grade(performance_students, var = "error_value", grading_scheme = grade_scheme),
             packages = "teachertools"),
  tar_target(grades_thresholds_plot, ggtexttable(grades_thresholds, rows = NULL), packages = "ggpubr"),
  tar_target(notenliste, grades |> 
               select(last_name, first_name, id, grade, bemerkung = error_value) %>% 
               mutate(bemerkung = round(bemerkung, 2)), packages = "dplyr"),
  tar_target(no_shows_added, notenliste |> bind_rows(no_shows)),
  tar_target(grades_plot, plot_grade_distribution(no_shows_added),
             packages = "teachertools"),
  tar_target(notenliste_xslx, no_shows_added |> writexl::write_xlsx("notenliste.xlsx"))
)
