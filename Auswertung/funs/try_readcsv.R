
try_readcsv <- function(file, verbose = FALSE) {
  
  # import csv file
  x <- data.table::fread(file, header = TRUE)
  
  # if more than 2 columns, only select first and last one:
  if (ncol(x) > 2) {
    x <- x %>% 
      select(1, last_col())
  }
  
  if (ncol(x) == 1) stop("only 1 column!")
  
  names(x) <- c("id", "pred")
  
  x <-
    x %>% 
    mutate(across(where(is.character), 
                  .fns = ~ str_replace_all(., pattern = ",", 
                                                                replacement = ".")))
  
  if (verbose == TRUE) {print("Ncol: "); print(ncol(x))}
  return(x)
  
} 
