
parse_names_raw <- function(path) {
  
  #' parse students names
  #' returns names and matrikelnummers of raw submissions files
  #' @params path path to submissions raw file from moodle
  
  path_subm_files <- list.files(path = path$subm_raw,
                                full.names = FALSE,
                                pattern = path$csv_pattern,
                                recursive = TRUE) 
  path_subm_files
  
  subm_names <- 
    path_subm_files %>% 
    str_to_lower() |> 
    str_match("(\\w+-?\\w*) (\\w+)(?:_\\d)")  |>   # First name possibly with dash then lastname but do not capture "underscore some digits" afterwards
    as_tibble(.name_repair = "unique")
  
names(subm_names) <- c("name-raw", "firstname", "lastname")


  
  subm_matrikelnummers <-
    path_subm_files %>% 
    str_remove(".*/") |> 
    str_remove(".csv$") |> 
    str_extract("\\d+") 
  
  subm_names <-
    subm_names |> 
    mutate(matrikelnummer = subm_matrikelnummers)
  
  return(subm_names)
  
}
