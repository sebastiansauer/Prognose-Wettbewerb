prep_csv <- function(submission_file, 
                     path_to_submissions = "submissions/",
                     path_to_train_data,
                     path_to_test_data,
                     max_row = 146,
                     start_id = 601,
                     verbose = TRUE){
  
  if (verbose) cat("This if function `prep_csv` speaking.\n")
  
  if (verbose) print(paste0("Now processing: ", submission_file))
  
  x <- data.table::fread(paste0(path_to_submissions, submission_file), header = TRUE)
  
  stopifnot(any(class(x) == "data.frame"))
  
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
  # x2 <-
  #   x %>% 
  #   mutate(across(where(is.character), 
  #                 .fns = ~ str_replace_all(., pattern = ",", 
  #                                          replacement = "."))) %>% 
  #   mutate(across(where(is.character),
  #                 .fns = as.numeric))
  
  
  x2 <-
    x %>% 
    mutate(across(where(is.character),
                  ~ readr::parse_number(., 
                                      locale = locale(decimal_mark = ",",
                                                      grouping_mark = "."))))
  
  
  # if id is all NA, set it with the sequence from id_start to to (start_id+max_row):
  if (all(is.na(x2$id))) x2$id <- start_id:(start_id+nrow(x)-1)
  if (any(x2$id[1:10] != start_id:(start_id+9))) x2$id <- start_id:(start_id+nrow(x)-1)
 
  
  # filter max n=`max_row` rows
  x2 <-
    x2 %>% 
    slice(1:max_row)
  
  
  if (verbose) {
    cat("Now reading test (control/solution) df file.\n")
    cat(paste0("Assuming this path/file name: ", path_to_test_data, "\n"))
  }
  solution_df <- read_csv(solution_df_file, 
                        show_col_types = FALSE)
  
  stopifnot(any(class(solution_df) == "data.frame"))
  stopifnot(any(names(solution_df) == "y"))
  
  x_joined <-
    solution_df %>% 
    select(id, y) %>% 
    left_join(x2,
              by = "id") 
  
  
  n_na <-
    x2 %>% 
    summarise(na_n = sum(is.na(pred)),
              na_prop = na_n/n()) 
  
  if (verbose) cat("Sum of NA in predictions: ", n_na$na_n, "\n")
  if (verbose) cat("Proportion of NA in predictions: ", n_na$na_prop, "\n")
  
  if (n_na$na_prop[1] > .5) cat("Warning: More than 50% NA in prediction. \n")
  if (n_na$na_prop[1] > .9) cat("Warning: More than 90% NA in prediction. \n")
  if (n_na$na_prop[1] > .99) {
    cat("Warning: More than 99% NA in prediction.\n")
    cat("Setting all NA to mean of y in train data set.\n")
    train_df <- read_csv(path_to_train_data,
                         show_col_types = FALSE)
    stopifnot(any(class(train_df) == "data.frame"))
    stopifnot(any(names(train_df) == "y"))
    x2 <-
      x2 %>% 
      mutate(pred = replace_na(pred, mean(train_df$y, na.rm = TRUE)))
  }
  
  if (verbose) cat("Replacing NA with mean of y.\n")
  x_joined <-
    x_joined %>%
    mutate(pred = replace_na(pred, mean(pred, na.rm = TRUE)))
  
  if (verbose == TRUE) {
    cat(paste0("Ncol: ", ncol(x_joined), "\n"))
    }
  
  if (verbose) print(paste0("Finished processing: ", submission_file, "\n"))
  
  return(x_joined)
}
