diff_names <- function(names_raw, names_processed) {
  
  out <- 
  names_raw |> 
    full_join(names_processed, by = "matrikelnummer") |> 
    rename(firstname = firstname.x,
           lastname = lastname.x)

  return(out)  
    
}