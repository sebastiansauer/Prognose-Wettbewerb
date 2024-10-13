library(targets)  
library(tarchetypes)  # tar_files
library(tidyverse)


# source functions:
funs_paths <- list.files(path = "funs",
                         full.names = TRUE,
                         pattern = "\\.R")
map(funs_paths, source)



# set packages to be available for all targets:
tar_option_set(packages = c("tidyverse"))


# define target steps list:
list(
  
  # define paths, and watch the file for changes:
  tar_file(paths_file, "exams/qm1-2024-sose-nachkorrektur/paths.yml"),  # enter here path of the current exam
  tar_target(paths, build_paths(paths_file), packages = "yaml"),
  
  # import solution data:
  tar_file(solution_file, paths$solution_df),
  tar_target(solution_df, read_csv(solution_file)),
  
  # define thresholds for grades:
  tar_file(grades_thresholds_file, paths$grades_thresholds),
  tar_target(grades_thresholds, read_csv(grades_thresholds_file)),
  

  # watch raw submissions:
  tar_files(submissions_raw,
            list.files(
              path = paste0(here::here(),"/", paths$subm_raw),
              full.names = TRUE,
              pattern = paths$csv_pattern,
              recursive = TRUE)),
  
  # copy submissions:
  tar_target(submissions_csv_files, copy_submissions(paths, submissions_raw)),
  
  # # get students names and ids:
  # tar_target(submissions_names, get_names_from_submissions(paths$proc)),
 
  # watch list of submission files:
  tar_files(submissions_copied, 
             list.files(path = paste0(here::here(),"/", paths$subm_proc),
                        full.names = TRUE,
                        pattern = paths$csv_pattern,
                        recursive = TRUE)),
  
  # sanitize submissions files in a loop:
  tar_target(submissions_prepped,
             submissions_copied |> 
               sanitize_pred_csv(paths$subm_proc,
                                 filename_with_path = TRUE,
                                 add_filename = TRUE,
                                 alternative_name_for_pred = paths$name_outcome_variable) |> 
               #write_csv(file = submissions_copied),
               add_solution_col(solution_df, name_output_var = paths$name_outcome_variable) |> 
               build_main_df() |> 
               add_rmse_col(),

             pattern = map(submissions_copied), # loop through all submission files
             
             packages = c("teachertools")
             ),
  
  # posthoc fixes and controls:
  tar_target(add_id_seq_check,
             submissions_prepped |>  
               mutate(id_is_seq_1_to_n = map_lgl(data, function(df) identical(df$id, 1:88)))
  ),
  

  # define grading schemes:
  tar_target(grade_scheme, 
             set_names(grades_thresholds$threshold, grades_thresholds$grade)),
  
  # find out no-shows (student who did not show up for the exam, watching the file):
  tar_target(no_shows_file, paths$no_shows_file, format = "file"),
  tar_target(no_shows, read_csv(no_shows_file, col_types = "cccd")),
  
  # assign grades:
  tar_target(grades, 
             assign_grade(add_id_seq_check, 
                          var = "error_value", 
                          grading_scheme = grade_scheme),
             packages = "teachertools"),
  tar_target(grades_thresholds_plot, 
             ggtexttable(grades_thresholds, rows = NULL), packages = "ggpubr"),
  tar_target(grades_thresholds_plot_disk,
             grades_thresholds_plot |> ggsave(file = paste0(paths$exam_root, "/Notenschwellen.png"))),
  
  # write grades to excel file:
  tar_target(notenliste, grades |> 
               select(last_name, first_name, id, grade, bemerkung = error_value) %>% 
               mutate(bemerkung = round(bemerkung, 2)), 
             packages = "dplyr"),
  tar_target(no_shows_added, notenliste |> bind_rows(no_shows)),
  tar_target(notenliste_xslx, 
             no_shows_added |> 
               write_xlsx(path = paths$notenliste_temp_file),
             packages = "writexl"),
  
  # plot distribution of grades and errors:
  tar_target(grades_plot, plot_grade_distribution(notenliste),
             packages = "teachertools"),
  tar_target(grades_plot_png, 
             grades_plot |> ggsave(filename = paste0(paths$exam_root, "/Notenverteilung.png"))),
  tar_target(error_plot,
             no_shows_added |> 
               select(error = bemerkung) |> 
               ggdensity(x = "error", add = "mean", rug = TRUE, fill = "lightgrey"),
             packages = "ggpubr"),
  tar_target(error_plot_png,
             error_plot |> ggsave(filename = paste0(paths$exam_root, "/Fehlerverteilung.png"))),
  
  # copy into university's grading document, joining using id (Matrikelnummer):
  tar_target(official_grading_list,
             read_xlsx(paths$notenliste_template) |> 
               mutate(id = as.character(id)) |> 
               mutate(id = str_pad(id, width = 8, side = "left", pad = "0")) |> 
               full_join(no_shows_added, by = "id"),
             packages = "readxl"),
  tar_target(official_grading_list_file,
             official_grading_list |> 
               write_xlsx(paste0(paths$exam_root, "/grading_list_official.xlsx")),
             packages = c("writexl"))
  
)
