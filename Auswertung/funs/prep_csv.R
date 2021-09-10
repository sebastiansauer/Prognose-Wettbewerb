prep_csv <- function(file, 
                     path_subm = "submissions/",
                     max_row = 146,
                     verbose = FALSE){
  
  if (verbose) print(paste0("Now processing: ", file))
  
  x <- data.table::fread(paste0(path_subm, file), header = TRUE)
  
  names(x) <- tolower(names(x))
  
  names_pred <- str_extract(names(x), "pred") %>% 
    purrr::discard(is.na)
  
  names_idx <- str_which(names(x), "pred")[1]
  
  names(x)[names_idx] <- "pred"
  
  if ("vorhersage" %in% names(x)) {
    x <- 
      x %>% 
      rename(pred = vorhersage)
  }
  
  if ("prediction" %in% names(x)) {
    x <- 
      x %>% 
      rename(pred = prediction)
  }

  if ("lebenszufriedenheit" %in% names(x) & !("pred" %in% names(x))) {
    x <- 
      x %>% 
      rename(pred = lebenszufriedenheit)
  }  
  
  
  if (all(c("id", "pred") %in% names(x))) {
    x <-
      x %>% 
      select(id, pred)
  } else {
  x <- 
    x %>% 
    select(1, last_col())
    # assuming first col is id, second col is pred
    if (length(names(x)) == 2) names(x) <- c("id", "pred")  
  }
  
  # make sure the names are "id" and "pred":
  if (length(names(x)) == 1) {
    names(x) <- "pred"
    x <-
      x %>% add_column(
        id = 251:(251+nrow(x)-1))
  }
  
  x$id <- as.integer(x$id)
 
  
  # if "pred" has a comma as delimiter, fix it:
  x <-
    x %>% 
    mutate(across(where(is.character), 
                  .fns = ~ str_replace_all(., pattern = ",", 
                                           replacement = "."))) %>% 
    mutate(across(where(is.character),
                  .fns = as.numeric))
  
  
  # if id is all NA, set it with the sequence from 251 to max row:
  if (all(is.na(x$id))) x$id <- 251:(251+nrow(x)-1)
  if (any(x$id[1:10] != 251:260)) x$id <- 251:(251+nrow(x)-1)
 
  
  # filter max n=`max_row` rows
  x <-
    x %>% 
    slice(1:max_row)
  
  
solution_df <- read_csv("/Users/sebastiansaueruser/github-repos/Vorhersagewettbewerb/SoSe_2021/Bachelor-Vorhersagewettbewerb/Vorverarbeitung/Variante-ses/Kontrolldaten.csv", 
                        show_col_types = FALSE)
  
  x_joined <-
    solution_df %>% 
    select(id, punkte) %>% 
    left_join(x,
              by = "id") %>% 
    rename(lebenszufriedenheit = punkte)
  
  x_joined <-
    x_joined %>%
    mutate(pred = replace_na(pred, mean(pred, na.rm = TRUE)))
 
  if (verbose == TRUE) {print("Ncol: "); print(ncol(x_joined))}
  
  if (verbose) print(paste0("Finished processing: ", file))
  
  return(x_joined)
}

#undebug(prep_csv)
# df <- prep_csv(name5)
# 
# mean(df$pred, na.rm = TRUE)
# 
# df <-
#   df %>%
#   mutate(pred = replace_na(pred, 8.44))
# 
# df %>% 
#   rsq(truth = lebenszufriedenheit,
#       estimate = pred)
