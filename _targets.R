# QM1, 23-SoSe

library(targets)  
library(tarchetypes)  # tar_files
library(tidyverse)
library(docstring)  # oxygen documentation in funs


# source functions:
funs_paths <- list.files(path = "funs",
                         full.names = TRUE,
                         pattern = ".R")
map(funs_paths, source)



# set packages to be available for all targets:
tar_option_set(packages = c("tidyverse"))


# define target steps list:
list(
  
  # define paths:
  tar_target(paths, def_paths()),
  
  # copy submiesions:
  tar_target(names_processed, copy_submissions(paths)),
  
  # # get students names and ids:
  # tar_target(submissions_names, get_names_from_submissions(paths$proc)),
 
  # sanitize submissions files:
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

  # get student names and ids:
  tar_target(names_raw, parse_names_raw(paths)),
  
  # check if some names got lost (differences between Moodle and course list of students):
  # tar_target(names_diff, diff_names(names_raw = names_raw, 
  #                                   names_processed = names_processed)),
  
  # define thresholds for grades:
  tar_target(grades_thesholds_file, "Daten/grades_thresholds.csv", format = "file"),
  tar_target(grades_thresholds, read.csv(grades_thesholds_file)),
  
  # compute model performance for all students:
  tar_target(performance_students, 
             comp_error_submissions(path_to_submissions = paths$subm_proc,
                                    name_output_var = "count",
                                    path_to_train_data = paths$train_df,
                                    path_to_test_data = paths$solution_df,
                                    error_fun = yardstick::rmse,
                                    verbose = TRUE,
                                    start_id = 1),
             packages = "teachertools"),
  
  # define grading schemes:
  tar_target(grade_scheme, set_names(grades_thresholds$threshold, grades_thresholds$grade)),
  
  # find out no-shows (student who did not show up for the examen):
  tar_target(no_shows_file, paths$no_shows_file, format = "file"),
  tar_target(no_shows, read_csv(no_shows_file, col_types = "cccd")),
  
  # assign grades:
  tar_target(grades, assign_grade(performance_students, var = "error_value", grading_scheme = grade_scheme),
             packages = "teachertools"),
  tar_target(grades_thresholds_plot, ggtexttable(grades_thresholds, rows = NULL), packages = "ggpubr"),
  
  # write grades to excel file:
  tar_target(notenliste, grades |> 
               select(last_name, first_name, id, grade, bemerkung = error_value) %>% 
               mutate(bemerkung = round(bemerkung, 2)), packages = "dplyr"),
  tar_target(no_shows_added, notenliste |> bind_rows(no_shows)),
  tar_target(notenliste_xslx, no_shows_added |> writexl::write_xlsx("notenliste.xlsx")),
  
  # plot distribution of grades:
  tar_target(grades_plot, plot_grade_distribution(no_shows_added),
             packages = "teachertools"),
  tar_target(notenschwellen_plot, plot_notenschwellen(grade_scheme),
             packages = "teachertools")
)
