prep_csv <- function(submission_file, 
                     path_to_submissions = "submissions/",
                     max_row = 146,
                     start_id = 601,
                     verbose = FALSE){
  
  if (verbose) print(paste0("Now processing: ", submission_file))
  
  x <- data.table::fread(paste0(path_to_submissions, submission_file), header = TRUE)
  
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
        id = start_id:(start_id+nrow(x)-1))
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
  
  
  # if id is all NA, set it with the sequence from id_start to to (start_id+max_row):
  if (all(is.na(x$id))) x$id <- start_id:(start_id+nrow(x)-1)
  if (any(x$id[1:10] != start_id:(start_id+9))) x$id <- start_id:(start_id+nrow(x)-1)
 
  
  # filter max n=`max_row` rows
  x <-
    x %>% 
    slice(1:max_row)
  
  
solution_df <- read_csv(solution_df_path, 
                        show_col_types = FALSE)
  
  x_joined <-
    solution_df %>% 
    select(id, y) %>% 
    left_join(x,
              by = "id") 
  
  x_joined <-
    x_joined %>%
    mutate(pred = replace_na(pred, mean(pred, na.rm = TRUE)))
 
  if (verbose == TRUE) {print("Ncol: "); print(ncol(x_joined))}
  
  if (verbose) print(paste0("Finished processing: ", submission_file))
  
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
