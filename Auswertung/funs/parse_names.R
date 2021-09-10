parse_last_names <- function(submissions){
  # Parse last names
  
  last_name <- 
    str_to_lower(submissions) %>% 
    str_extract("(^.+)_(.+)_.*(\\d+)") %>%   
    str_split(pattern = "_") %>% 
    map_chr(~magrittr::extract(.,1)) 
  
  return(last_name)
}


parse_first_names <- function(submissions){
  
  # Parse first names:
  
  first_name <- 
    str_to_lower(submissions) %>%
    str_extract("(^.+)_(.+)_.*(\\d+)") %>%   
    str_split(pattern = "_") %>% 
    map_chr(~magrittr::extract(.,2)) 
  return(first_name)
}

parse_matrikelnummer <- function(submissions) {
  
  matrikelnummer <- 
    str_to_lower(submissions) %>%
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
