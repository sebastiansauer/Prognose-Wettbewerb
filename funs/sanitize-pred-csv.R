#' sanitize_pred_csv
#'
#' Sanitizes a submission csv file for subsequent computation of prediction error
#'
#' Takes a submission file as as input,
#' and irons out some glitches so that the prediction error can be computed
#' in a subsequent step (not part of this function).
#' Each single submissions consists of a number of submissions.
#' The submssions are expected to be a data frame with two columns:
#' 1) id: the unique id of each prediction (integer, positive)
#' 2) pred: the predicted value for each instance
#' Note that the id must be shared knowledge,
#' i.e., the student must submit an id that is used by the teacher,
#' otherwise the predictions cannot be matched.
#' If the two column names are not "id" and "pred", the function
#' assumes the first and last column to be the relevant ones.
#' If only one column is provided as submission data frame,
#' the function assumes that the column contains the predictions. An id column is
#' added in this case.

#' Any missing values in the prediction will be filled with a given value,
#' provided the argument `replace_na_preds` is not NULL.
#' The submission does not necessarily be of a standard csv format, as
#' `data.table::fread` is able to guess formats. However, for the train and test data file, a standard csv file is expected (comma as deliminators, US centric locale).
#'
#' The function returns a data frame with the the columns mentioned above (id, pred).
#' The returned dataframes contains some attributes: (1) "comments_to_student", where hints about gross errors are mentioned, (2) "fail" to indicate pass/fail of the test, here if gross errors (eg., no data submitted) are present, (3), "na_prop": the proportion of na in the prediction column.
#' Note that no file is written to disk.
#'
#' @param submission_file name of csv file with predictions (chr)
#' @param path_to_submissions path to submission folder with submission files (chr)
#' @param replace_na_preds  value to replace NA predictions (dbl)
#' @param max_row how many rows should be prepared maximally (int)?
#' @param start_id number of the first id (int)
#' @param alternative_name_for_pred alternative names for prediction column (str)
#' @param name_id_var name of the id variable (chr)?
#' @param name_pred_column name of the columns with the predictions (chr)?
#' @param verbose more output (lgl)?
#' @param filename_with_path comes the filename of the submission file with (or without) path? (lgl, FALSE)
#' @param add_filename should the name of the submission file should be added as a column? (lgl, FALSE)
#'
#' @return sanitized data frame of individual submission. See details.
#' @export
#'
#' @examples
#' \dontrun{prep_csv(my_submission_file, path_train, path_test, max_row = 500, start_id = 800)}
sanitize_pred_csv <- function(submission_file,
                         path_to_submissions = "submissions/",
                         replace_na_preds = NULL,
                         alternative_name_for_pred = NULL,
                         max_row = NULL,
                         start_id = 1,
                         name_id_var = "id",
                         name_pred_column = "pred",
                         filename_with_path = FALSE,
                         add_filename = FALSE,
                         verbose = TRUE){
  
  # in some cases, the student will fail, eg., if no data has been submitted
  student_fails <- NA
  comment_to_student <- "Feel free to use the control data provided to check the validity of your grade."
  
  
  if (verbose) cat("This is function `sanitize_pred_csv` speaking.\n")
  
  # Make sure the paths are ending with a slash:
  if (!is.null(path_to_submissions)) {
    if (!stringr::str_detect(path_to_submissions, "/$"))
      path_to_submissions <- stringr::str_c(path_to_submissions, "/")
    if (verbose) cat(paste0("Path to submissions is: ", path_to_submissions, "\n"))
  }
  
  # start id must be positive:
  stopifnot(start_id > 0)
  
  # all column names are assumed lower cases:
  name_pred_column <- tolower(name_pred_column)
  
  # if no start id is given, assume 1 (function default):
  if (is.null(start_id)) start_id <- 1
  
  
  
  
  
  # Should the following part be moved to a own function,
  # such as `is_valid_csv`?
  
  

# read submission file ----------------------------------------------------

  
  # read and prepare a submission file:
  if (verbose) print(paste0("*****Now processing: ", submission_file, "*****"))
  
  
  # accomodate if file names come without or with path:
  if (!filename_with_path) x <- data.table::fread(paste0(path_to_submissions, submission_file), header = TRUE)
  if (filename_with_path) x <- data.table::fread(submission_file, header = TRUE)
  
  
  stopifnot(any(class(x) == "data.frame"))
  if (verbose){
    cat(paste0("Dimension of submissions file: ", stringr::str_c(dim(x), collapse = "; ")))
    cat("\n")
    cat(paste0("Column names of submission file: ", stringr::str_c(names(x), collapse = "; ")))
  }
  
  # stop if the submission is not a data frame:
  stopifnot(any(class(x) == "data.frame"))
  if (verbose){
    cat(paste0("Dimension of submissions file: ", stringr::str_c(dim(x), collapse = "; ")))
    cat("\n")
    cat(paste0("Column names of submission file: ", stringr::str_c(names(x), collapse = "; ")))
  }
  
  if(ncol(x) < 2) warning("Less than two columns detected!")
  
  # all column names to lower:
  names(x) <- base::tolower(base::names(x))
  
  

  
  # find column with predictions:
  names_pred <-
    stringr::str_extract(names(x), tolower(name_pred_column))  |>
    purrr::discard(is.na)
  
  # this regex chooses the first column that contains "pred" in its name:
  names_idx <- stringr::str_which(names(x), name_pred_column)[1]
  
  names(x)[names_idx] <- "pred"  # will not fail if `names_idx` is NA.
  
  # There are certain commonly mistakes, ie wrong y col names for "pred",
  # which we will rectify here:
  if ("vorhersage" %in% names(x)) {
    x <-
      x |>
      dplyr::rename(pred = vorhersage)
  }
  
  if ("prediction" %in% names(x)) {
    x <-
      x |>
      dplyr::rename((pred = prediction))
  }
  
  # if there's a column named after the outcome var,  but not column "pred":
  if (!is.null(alternative_name_for_pred)) {
    
    if (tolower(alternative_name_for_pred) %in% names(x) & !(name_pred_column %in% names(x))) {
      x <-
        x |>
        # ... then rename this column to "pred"
        dplyr::rename(pred = {{alternative_name_for_pred}})
    }
  }
  
  # remove punctuation from names of submission files:
  names(x) <- stringr::str_remove_all(names(x), '[[:punct:]]')
  
  
  # remove the following unneeded columns:
  remove_these_cols <- c("v1", "", " ", "...1")
  x <-
    x |>
    dplyr::select(-tidyselect::any_of(remove_these_cols))
  
  
  # if NO (zero) columns are present, invent the two canonical ones:
  if ((ncol(x) == 0)) {
    x <-
      tibble::tibble(
        id = c(NA_integer_),
        pred = c(NA_real_)
      )
    student_fails <- TRUE
    comment_to_student <- "No columns detected in submission CSV file."
  }
  
  
  # if only 1 column is present, assume it is the prediction column:
  # and add an id column
  if ((ncol(x) == 1) & (nrow(x) > 0)) {
    names(x) <- "pred"
    x$id <- start_id:(start_id + nrow(x) - 1)
  }
  
  # if "pred" and "id" are existing columns, select only those two:
  if (all(c("id", "pred") %in% names(x))) {
    x <-
      x |>
      dplyr::select(id, pred)
  } else {
    # otherwise select the first and the last column as a heuristic, hoping to get the correct ones.
    x <-
      x |>
      dplyr::select(1, tidyselect::last_col())
    # assuming first col is id, second col is pred
    if (length(names(x)) == 2) names(x) <- c("id", "pred")
  }
  
  # make sure the names are "id" and "pred":
  if ((length(names(x)) == 1) & (nrow(x) > 1)) {
    names(x) <- "pred"
    x <-
      x |>
      tibble::add_column(
        id = start_id:(start_id + nrow(x) - 1))
  }
  
  x$id <- as.integer(x$id)  # will _not_ fail if "id" is not present
  
  
  # if two columns are present, but "id" and "pred" are not the names
  
  #if ((length(names(x)) == 2))
    
    
  # Intervene if no data have been submitted:
  if (nrow(x) < 2) {
    comment_to_student <- "Submission file has 1 or 0 detected row only."
    student_fails <- TRUE
    if (verbose) cat(comment_to_student)
    cat("\n")
    x <-
      tibble::tibble(
        id = c(NA_integer_),
        pred = c(NA_real_)
      )
  }
  
  
  
  
  # transform character to number:
  x2 <-
    x |>
    dplyr::mutate(dplyr::across(tidyselect:::where(is.character),  # note the tripple colons
                                ~ readr::parse_number(.,
                                                      locale = readr::locale(decimal_mark = ",",
                                                                             grouping_mark = "."))))
  
  
  # make sure column "pred" is of type numeric, not integer:
  x2 <-
    x2 |>
    dplyr::mutate(pred = as.numeric(pred))
  
  
  # if id is all NA, set it with the sequence from id_start to to (start_id+max_row):
  if (all(is.na(x2$id))) x2$id <- start_id:(start_id + nrow(x) - 1)
  
  # buggy:
  # if (nrow(x2) > 9) {
  #   if (any(x2$id[1:10] != start_id:(start_id + 9))) x2$id <- start_id:(start_id + nrow(x) - 1)
  # }
  
  
  # filter max n=`max_row` rows, if `max_row` is not NULL:
  if (!is.null(max_row)) {
    x2 <-
      x2 |>
      dplyr::slice(1:max_row)
  }
  
  
  
  
  # compute na statistics:
  n_na <-
    x2 |>
    dplyr::summarise(
      na_n = sum(is.na(pred)),
      na_prop = na_n/dplyr::n())
  
  if (verbose) cat("Sum of NA in predictions: ", n_na$na_n, "\n")
  if (verbose) cat("Proportion of NA in predictions: ", n_na$na_prop, "\n")
  
  if (n_na$na_prop[1] > .5) cat("Warning: More than 50% NA in prediction. \n")
  if (n_na$na_prop[1] > .9) cat("Warning: More than 90% NA in prediction. \n")
  if (n_na$na_prop[1] > .99) {
    cat("Warning: More than 99% NA in prediction.\n")
    cat("Setting all NA to mean of y (output variable) in train data set.\n")
  }
  
  
  
  if (!is.null(replace_na_preds)) {
    x2 <-
      x2 |>
      mutate(!!name_pred_column := ifelse(is.na(!!sym(name_pred_column)), replace_na_preds, !!sym(name_pred_column)))
  }
  
  
  if (add_filename) {
    x2 <-
      x2 |> 
      mutate(filename = str_extract(submission_file, "[^/]+$"))
    if (verbose) print(paste0("Filename has been added as a column. \n"))
  }
  
  
  # finalizing:
  
  out_df <- x2
  
  if (verbose == TRUE) {
    cat(paste0("Ncol of processed submission df: ", ncol(out_df), "\n"))
  }
  
  if (verbose) print(paste0("Finished preprocessing: ", submission_file, "\n"))
  
  
  # Add some metadata to the results:
  attr(out_df, "comments_to_student") <- comment_to_student
  attr(out_df, "failed") <- student_fails
  attr(out_df, "na_prop") <- n_na$na_prop[1]
  attr(out_df, "filename") <- submission_file
  
  
  return(out_df)
}


