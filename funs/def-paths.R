

def_paths <- function() {
  
  
  
  
  #  paths ---------------------------------------------
  
  path <- list()
  
  
  path$csv_pattern <- ".csv$|.CSV$|.Csv$|.CSv$|.csV$|.cSV$|.cSV$"
  
  # Remember to put  a trailing slash to each path!
  
  path$subm_raw <- "submissions-raw"  # raw, unprocessed submissions, from Moodle
  
  path$subm_test <-  "submissions-test"  # test  data (without solution, y) for students
  
  path$results <- "Noten"  # ?
  
  path$subm_proc <- "/submissions-processed"  # processed submissions
  
  path$solution_df <- "Daten/d_control.csv"  # control data (with solution, y) for teacher
  
  # But no trailing slash to a file name:
  path$train_df <- "Daten/d_train.csv"  # tran data
  
  path$notenliste_template <-  "Notenliste-leer.xlsx"  # template for grading list
  
  path$no_shows_file <- "Daten/no_shows.csv"
  
  
  # add absolute path:
  # path2 <- 
  #   path |> 
  #   map( ~ paste0(here::here(), .x))
  
  path$subm_files_w_path <- list.files(path = path$subm_raw,
                                       full.names = TRUE,
                                       pattern = path$csv_pattern,
                                       recursive = TRUE) 
  
  return(path)
  
}