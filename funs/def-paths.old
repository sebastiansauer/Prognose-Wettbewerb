

def_paths <- function() {
  
  
  
  
  #  paths ---------------------------------------------
  
  paths <- yaml::read_yaml("paths.yml")
  
  
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