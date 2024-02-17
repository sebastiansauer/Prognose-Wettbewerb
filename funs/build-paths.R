build_paths <- function(paths_file){
  
  paths <- read_yaml(paths_file)
  paths_raw <- read_yaml(paths_file)
  
  paths$subm_raw <- paste0(paths_raw$exam_root, paths_raw$subm_raw)
  paths$subm_test <- paste0(paths_raw$exam_root, paths_raw$subm_test)
  paths$subm_proc <- paste0(paths_raw$exam_root, paths_raw$subm_proc)
  paths$solution_df <- paste0(paths_raw$exam_root, paths_raw$solution_df)
  paths$train_df <- paste0(paths_raw$exam_root, paths_raw$train_df)
  paths$notenliste_template <- paste0(paths_raw$exam_root, paths_raw$notenliste_template)
  paths$no_shows_file <- paste0(paths_raw$exam_root, paths_raw$no_shows_file)
  paths$grades_thresholds <- paste0(paths_raw$exam_root, paths_raw$grades_thresholds)
  
  return(paths)
}