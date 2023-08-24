
parse_names_raw <- function(paths) {
  #' parse students names returns names and matrikelnummers of raw submissions files
  #' @param name description path path to submissions raw file from moodle
  #' @return sanitized data frame of individual submission. See details.
  #' @export
  #'
  
  
  
  
  
  
  get_matrikelnumbers <- function(path_subm_files) {
    path_subm_files %>%
      stringr::str_remove(".*/") |>  # rm all path until last slash
      stringr::str_remove("\\.csv$") |>  # rm file type
      stringr::str_extract("\\d+")
  }
  
  
  
  path_subm_files <- list.files(path = paths$subm_raw,
                                full.names = FALSE,
                                pattern = paths$csv_pattern,
                                recursive = TRUE) 
  
  
  out <- 
    path_subm_files %>%
    stringr::str_to_lower() |>
    stringr::str_match("(\\w+-?\\w*) (\\w+)(?:_\\d)") |>   # First name possibly with dash then lastname but do not capture "underscore some digits" afterwards
    tibble::as_tibble(.name_repair = "unique") |> 
    mutate(matrikelnummer = get_matrikelnumbers(path_subm_files))
  
  names(out) <- c("names-raw", "lastname", "firstname", "matrikelnummer")
  
  
}


