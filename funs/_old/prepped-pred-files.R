prep_pred_files <- function(
    path_to_submissions = "Submissions/",
    path_to_test_data,
    verbose = TRUE,
    max_row = NULL,
    start_id = 1,
    name_output_var = "y",
    name_id_var = "id",
    name_pred_column = "pred"
) {
  
  
  if (verbose) cat("This is function `comp_error_submissions()` speaking.\n")
  
  # Parse submissions:
  #tar_load(submissions_processed)
  submissions_processed <- list.files(
    path = path_to_submissions,
    full.names = FALSE,
    pattern = "\\.csv$|\\.CSV$|\\.Csv$|\\.CSv$|\\.csV$|\\.cSV$|\\.cSV$",
    ignore.case = TRUE,
    recursive = TRUE)
  
  Encoding(submissions_processed) <- "utf8"
  
  if (verbose) cat(paste0("Number of CSV files to be processed: ", length(submissions_processed), "\n"))
  
  # Make sure the paths are ending with a slash:
  if (!stringr::str_detect(path_to_submissions, "/$"))
    path_to_submissions <- stringr::str_c(path_to_submissions, "/")
  if (verbose) cat(paste0("Path to submissions is: ", path_to_submissions, "\n"))
  
  stopifnot(start_id > 0)
  
  
  
  
  # parse names and Matrikelnummers to df:
  d <-
    tibble::tibble(id_seq = 1:length(submissions_processed)) |>
    dplyr::mutate(csv_file_name = stringr::str_conv(submissions_processed, "utf8")) |>
    dplyr::mutate(csv_file_name = berryFunctions::convertUmlaut(csv_file_name)) |>
    dplyr::mutate(last_name = parse_last_names(csv_file_name),
                  first_name = parse_first_names(csv_file_name)) |>
    dplyr::mutate(id = parse_matrikelnummer(csv_file_name))
  
  
  # add nrow of preds to df:
  if (verbose) print("Now counting data lines per prediction data (submission) file.")
  d2 <-
    d |>
    dplyr::mutate(npreds = purrr::map_dbl(submissions_processed,
                                          ~ R.utils::countLines(paste0(path_to_submissions, .x))) - 1)
  
  
  
  # add column names of pred data file to df:
  if (verbose) print("Now extracting col names from csv file with prediction data.")
  d2a <-
    d2 |>
    dplyr::mutate(colnames_pred_file = purrr::map_chr(
      .x = submissions_processed,
      .f = ~ data.table::fread(paste0(path_to_submissions, .x), nrows = 1) |>
        names() |>
        stringr::str_c(collapse = " - ")))
  
  
  #debug(prep_csv)
  # parse prediction data to df:
  # CALL PREP_CSV:
  if (verbose) print("Now starting to parse csv files with prediction data.")
  d3 <-
    d2a |>
    #slice(1) |>
    dplyr::mutate(data = purrr::map(
      .x = submissions_processed,
      # prep_csv() is from `{teachertools}`:
      .f = ~ prep_csv3(submission_file =  .x,
                      path_to_submissions = path_to_submissions,
                      path_to_test_data = path_to_test_data,
                      max_row = max_row,
                      start_id = start_id,
                      name_output_var = name_output_var,
                      name_id_var = name_id_var,
                      name_pred_column = name_pred_column,
                      verbose = verbose)))
  
  if (verbose) print("Parsing of prediction data (submissions) completed.")
  
  return(d3)
  
  
  
}
