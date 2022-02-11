parse_last_names <- function(submission){
  # Parse last names
  
  last_name <- 
    str_to_lower(submission) %>% 
    str_extract("(^.+)_(.+)_.*(\\d+)") %>%   
    str_split(pattern = "_") %>% 
    map_chr(~magrittr::extract(.,1)) 
  
  return(last_name)
}


parse_first_names <- function(submission){
  
  # Parse first names:
  
  first_name <- 
    str_to_lower(submission) %>%
    str_extract("(^.+)_(.+)_.*(\\d+)") %>%   
    str_split(pattern = "_") %>% 
    map_chr(~magrittr::extract(.,2)) 
  return(first_name)
}

parse_matrikelnummer <- function(submission) {
  
  matrikelnummer <- 
    str_to_lower(submission) %>%
    str_extract("(^.+)_(.+)_.*(\\d+)") %>%  
    str_split(pattern = "_") %>%
    str_extract("\\d+") %>% 
    str_pad(width = 8, pad = "0")
  
  return(matrikelnummer)
  
}





# submissions[176] %>% 
# str_to_lower(submissions) %>%
#   str_extract("(^.+)_(.+)_.*(\\d+)") %>%  
#   str_split(pattern = "_")
