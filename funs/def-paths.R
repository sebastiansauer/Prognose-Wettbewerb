

def_paths <- function() {
  
  
  
  
  #  paths ---------------------------------------------
  
  path <- list()
  
  
  path$csv_pattern <- ".csv$|.CSV$|.Csv$|.CSv$|.csV$|.cSV$|.cSV$"
  
  # Remember to put  a trailing slash to each path!
  
  path$subm_raw <- "submissions-raw"  # raw, unprocessed submissions, from Moodle
  
  path$subm_test <-  "submissions-test"  
  
  path$results <- "Noten"
  
  path$subm_proc <- "submissions-processed"  # processed submissions
  
  path$solution_df <- "Daten/d_control.csv"
  
  # But no trailing slash to a file name:
  path$train_df <- "Daten/d_train.csv"
  
  path$notenliste_template <-  "Notenliste-leer.xlsx"
  
  
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