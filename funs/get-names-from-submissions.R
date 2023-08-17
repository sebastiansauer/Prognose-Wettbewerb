get_names_from_submissions <- function (path_of_submissions, verbose = TRUE) {
  

  full_path_to <- paste0(here::here(), "/", path_of_submissions, "/")
  
  
  # Get last names of each submission:
  
  subm_processed_files <- 
    list.files(path = full_path_to,
               full.names = FALSE,
               recursive = FALSE) 
  
  
  # subm_processed_files |> 
  #   str_match("(\\w+-?\\w*)_(?:\\d)")
  # 
  subm_names <- 
    subm_processed_files %>% 
    # First name possibly with dash then lastname but do not capture "underscore some digits" afterwards:
    str_match("(\\w+-?\\w*)_(?:\\d)") %>%  
    `[`(, 2) %>% 
   discard(is.na) %>% 
    str_to_lower() %>% 
    # if two underscores, replace the second with a blank, as only one underscore is permitted:
    str_replace(pattern = "(_)(\\w+)(_)", replacement = "\\1\\2 ")
  
  subm_matrikelnummers <-
    subm_processed_files %>% 
    str_match("(?:\\w+-?\\w*)_(\\d+)") %>%  
    `[`(, 2) %>% 
    discard(is.na) %>% 
    str_to_lower()
  
  
  subm_ids <-
    tibble(
      name = subm_names) |> 
    separate_wider_delim(name, delim = "_", names = c("firstname", "lastname")) |> 
  mutate(matrikelnummer  = subm_matrikelnummers)
      
  return(subm_ids)
 
}
