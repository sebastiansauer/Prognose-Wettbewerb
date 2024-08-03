#' prep_csv3
#'
#' Prepares a submission csv file for subsequent computation of prediction error
#'
#' Takes a submission file as as input, as well as the train and test data files,
#' and irons out some glitches so that the prediction error can be computed
#' in a subsequent step (not part of this function).
#' Each single submissions consists of a number of submissions.
#' The submossions are expected to be a data frame with two columns:
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
#' The output variable will be renamed to "y", in order to simplify the downstream work.
#' Any missing values in the submission will be filled with the (arithmetic) mean.
#' The submission does not necessarily be of a standard csv format, as
#' `data.table::fread` is able to guess formats. However, for the train and test data file, a standard csv file is expected (comma as deliminators, US centric locale).
#' Column names of submissions files are transformed to lower case.
#'
#' The function returns a data frame with the the columns mentioned above (id, pred).
#' The returned dataframes contains some attributes: (1) "comments_to_student", where hints about gross errors are mentioned, (2) "fail" to indicate pass/fail of the test, here if gross errors (eg., no data submitted) are present, (3), "na_prop": the proportion of na in the prediction column.
#'
#' @param submission_file name of csv file with predictions (chr)
#' @param path_to_submissions path to submission folder with submission files (chr)
#' @param path_to_submissions path to test (solution) data
#' @param max_row how many rows should be prepared maximally (int, defaults to NULL)?
#' @param start_id number of the first id (int, defaults to 1)
#' @param name_output_var name of the variable to be predicted (chr, defaults to "y")
#' @param name_id_var name of the id variable (chr, defaults to "id")?
#' @param name_pred_column name of the columns with the predictions (chr, defaults to "pred")?
#' @param verbose more output (lgl, defaults to TRUE)?
#'
#' @return sanitized data frame of individual submission. See details.
#' @export
#'
#' @examples
#' \dontrun{prep_csv(my_submission_file, path_train, path_test, max_row = 500, start_id = 800)}
join_pred_solution <- function(submission_file,
                      path_to_submissions = "submissions/",
                      path_to_test_data,
                      max_row = NULL,
                      start_id = 1L,
                      name_output_var = "y",
                      name_id_var = "id",
                      name_pred_column = "pred",
                      verbose = TRUE){
  
  # in some cases, the student will fail, eg., if no data has been submitted:
  student_fails <- FALSE
  comment_to_student <- "Feel free to use the control data provided to check the validity of your grade."
  
  
  if (verbose) cat("This is function `prep_csv` speaking.\n")
  
  # Make sure the paths are ending with a slash:
  if (!stringr::str_detect(path_to_submissions, "/$"))
    path_to_submissions <- stringr::str_c(path_to_submissions, "/")
  if (verbose) cat(paste0("Path to submissions is: ", path_to_submissions, "\n"))
  
  stopifnot(start_id > 0)
  
  if (is.null(start_id)) start_id <- 1
  
  
  
  if (verbose) {
    cat("Now reading test (control/solution) df file.\n")
    cat(paste0("Assuming this path/file name: ", path_to_test_data, "\n"))
  }
  
  # import test/solution df file:
  #stopifnot(file.exists(path_to_test_data))  # appears not to work for remote files
  solution_df <- readr::read_csv(path_to_test_data,
                                 show_col_types = FALSE)
  if (verbose) dplyr::glimpse(solution_df)
  cat("\n")
  stopifnot(any(class(solution_df) == "data.frame"))
  
  
  
  # Should the following part be moved to a own function,
  # such as `is_valid_csv`?
  
  # read and prepare a submission file
  if (verbose) print(paste0("*****Now processing: ", submission_file, "*****"))
  
  # assumiing that submissions file is withouts path:
  x <- data.table::fread(paste0(path_to_submissions, submission_file), header = TRUE)
  
  # assumiing that submissions file is with path:
  #x <- data.table::fread(submission_file, header = TRUE)
  
  
  stopifnot(any(class(x) == "data.frame"))
  if (verbose){
    cat(paste0("Dimension of submissions file: ", stringr::str_c(dim(x), collapse = "; ")))
    cat("\n")
    cat(paste0("Column names of submission file: ", stringr::str_c(names(x), collapse = "; ")))
  }
  
  if(ncol(x) < 2) warning("Less than two columns detected!")
  
  # all names to lower:
  names(x) <- base::tolower(base::names(x))
  
  # remove punctuation from names of submission files:
  names(x) <- stringr::str_remove_all(names(x), '[[:punct:]]')
  
  
  
  
  # find column with predictions:
  names_pred <-
    stringr::str_extract(names(x), name_pred_column)  |>
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
  
  # if there's a column named after the outcome var,
  # but not column "pred":
  if (name_output_var %in% names(x) & !(name_pred_column %in% names(x))) {
    x <-
      x |>
      # ... then rename this column to "pred"
      dplyr::rename(pred = {{name_output_var}})
  }
  
  
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
  
  if ((length(names(x)) == 2))
    
    
    # Intervene if no data have been submitted:
    if (nrow(x) == 1) {
      comment_to_student <- "Submission file has 1 detected row only."
      student_fails <- TRUE
      if (verbose) cat(comment_to_student)
      cat("\n")
      x <-
        tibble::tibble(
          id = c(NA_integer_),
          pred = c(NA_real_)
        )
    }
  
  if (nrow(x) == 0) {
    comment_to_student <- "Submission file has zero detected rows."
    student_fails <- TRUE
    if (verbose) cat(comment_to_student)
    x <-
      tibble::tibble(
        id = c(NA_integer_),
        pred = c(NA_real_)
      )
    cat("\n")
    if (verbose) cat("One empty row has been added.")
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
  if (nrow(x2) > 9) {
    if (any(x2$id[1:10] != start_id:(start_id + 9))) x2$id <- start_id:(start_id + nrow(x) - 1)
  }
  
  
  # filter max n=`max_row` rows, if `max_row` is not NULL:
  if (!is.null(max_row)) {
    x2 <-
      x2 |>
      dplyr::slice(1:max_row)
  }
  
  
  
  
  # Rename the output variable to "y" in solution_df:
    solution_df <-
    solution_df |>
    dplyr::rename(y = {{name_output_var}})
  
  
  # add id column in solution df if not present:
    if (!("id" %in% names(solution_df))) solution_df$id <- start_id:(start_id + nrow(solution_df) - 1)
  
  
  # Make sure "id" is of type integer:
  solution_df <-
    solution_df |>
    dplyr::mutate(id = as.integer(id)) |> 
    dplyr::select(id, y)
  
  if (!any(names(solution_df) == "y")) {
    cat(paste0("Names of columns in solution_df: "), names(solution_df))
    stop("`y` not among column names of solution df!")
  }
  
  
  # join solution data with submission data:
  x_joined <-
    x2 |>
      dplyr::left_join(solution_df,
                     by = "id")
  
  
  # make sure y is numeric, not integer:
  if (typeof(x_joined$y) == "integer") x_joined$y <- as.numeric(x_joined$y)
  if (typeof(x_joined$y) == "character") stop("y must not be of type `character`!")
  
  
  # this is the returned object (if not NAs are replaced by mean):
  out <- x_joined
  

  
  # if (replace_na_by_mean){
  #
  # # reading training data, only to get the mean y-value:
  # if (verbose) cat("Now reading train data. Expecting standard CSV.\n")
  # #stopifnot(file.exists(path_to_train_data))
  # train_df <- readr::read_csv(path_to_train_data,
  #                             show_col_types = FALSE)
  # stopifnot(any(class(train_df) == "data.frame"))
  # stopifnot(any(names(train_df) == name_output_var))
  #
  # # Rename outcome variable in train data to "y":
  # train_df <-
  #   train_df |>
  #   dplyr::rename(y = {{name_output_var}}) |>
  #   dplyr::mutate(y = as.numeric(y))
  #
  #
  #
  # # x2 <-
  # #   x2 |>
  # #   dplyr::mutate(pred = tidyr::replace_na(pred, base::mean(train_df$y, na.rm = TRUE)))
  #
  #
  # if (verbose) cat("Replacing NA with mean of y (the output variable).\n")
  # x_joined2 <-
  #   x_joined |>
  #   dplyr::mutate(pred = tidyr::replace_na(pred, base::mean(train_df$y, na.rm = TRUE)))
  #
  # out <-
  #   x_joined2
  # }
  #
  # if (verbose == TRUE) {
  #   cat(paste0("Ncol of processed submission df: ", ncol(out), "\n"))
  # }
  #
  # if (verbose) print(paste0("Finished preprocessing: ", submission_file, "\n"))
  #
  #
  # # Add some metadata to the results:
  # attr(out, "comments_to_student") <- comment_to_student
  # attr(out, "failed") <- student_fails
  # attr(out, "na_prop") <- n_na$na_prop[1]
  
  
  return(out)
}


