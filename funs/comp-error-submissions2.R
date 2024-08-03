#' comp_error_submission2
#'
#' Computes prediction error for student submissions
#'
#' Given a number of csv files with predictions (and id),
#' this function returns the prediction error (such as MAE or RMSE) for each prediction (row)
#' The "control data" is the test data including the "solution", ie., to variable to
#' be predicted. The submissions csv files will be sanitized (using `prep_csv`)
#' before further processing (however it is okay to insert sanitized csv files).
#'
#'
#' @param path_to_submissions path  and file (CSV) with predictions (character)
#' @param error_fun which error fun to use (mae, rmse, ..., defaults to rmse), possibly from the tidymodels ecoverse
#' @param path_to_submissions path to submission folder with processed submission files (chr)
#' @param path_to_train_data  path to train data (chr)
#' @param path_to_test_data path to CONTROL (test) data file (with solution/y), regular csv file expected  (Chr)
#' @param max_row how many rows should be prepared maximally (int, defaults to NULL)?
#' @param max_row how many rows should be prepared maximally (int, defaults to NULL)?
#' @param start_id number of the first id (int, defaults to 1)
#' @param name_output_var name of the variable to be predicted (chr, defaults to "y")
#' @param name_id_var name of the id variable (chr, defaults to "id")?
#' @param name_pred_column name of the columns with the predictions (chr, defaults to "pred")?
#' @param verbose more output (lgl, defaults to TRUE)?
#'
#' @return tibble with prediction error value
#' @export
#'
#' @examples
#' \dontrun{comp_error_submissions(mypath)}
comp_error_submissions2 <- function(
    prepped_pred_data,
    path_to_submissions = "Submissions/",
    verbose = TRUE,
    path_to_test_data,
    max_row = NULL,
    start_id = 1,
    name_output_var = "y",
    name_id_var = "id",
    name_pred_column = "pred",
    error_fun = yardstick::rmse) {
  
  
  # import solution data:
  

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
  

  # Rename the output variable to "y" in solution_df:
  solution_df <-
   solution_df |>
   dplyr::rename(y = {{name_output_var}})


  # add id column in solution df if not present:
  if (!("id" %in% names(solution_df))) solution_df$id <- start_id:(start_id + nrow(solution_df) - 1)

  # Make sure "id" is of type integer:
  solution_df <-
    solution_df |>
    dplyr::mutate(id = as.integer(id))

  if (!any(names(solution_df) == "y")) {
    cat(paste0("Names of columns in solution_df: "), names(solution_df))
    stop("`y` not among column names of solution df!")
  }


  # join solution data with submission data:
  x_joined <-
    solution_df |>
    dplyr::select(id, y) |>
    dplyr::left_join(prepped_pred_data,
                     by = "id")


  # make sure y is numeric, not integer:
  if (typeof(x_joined$y) == "integer") x_joined$y <- as.numeric(x_joined$y)
  if (typeof(x_joined$y) == "character") stop("y must not be of type `character`!")

  # compute predictive quality/prediction error:
  if (verbose) print("Now computing test set error for each submission.")
  d_error_fun <-
    x_joined |>
    dplyr::mutate(error_coef = purrr::map(data,
                                          ~error_fun(truth = y,
                                                     estimate = pred,
                                                     data = .x)))
  # set `error_fun <- mae` during debugging!
  
  options(scipen = 4)
  d_error_fun2 <-
    d_error_fun |>
    dplyr::mutate(error_value = purrr::map_dbl(error_coef,
                                               ".estimate"))
  
  if (verbose) print("Computing test set error for each submission is now completed.")
  
  
  if (verbose) cat("Finished `comp_error_submissions`.\n")
  return(d_error_fun2)
  
  
}
