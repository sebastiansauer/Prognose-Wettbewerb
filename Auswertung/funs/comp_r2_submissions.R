comp_error_submissions <- function(path_to_submissions = "Submissions/", 
                                   verbose = TRUE,
                                   error_fun = mae) {
  
  source("funs/parse_names.R")
  source("funs/prep_csv.R")
  
  if (verbose) cat("This is function `comp_error_submissions` speaking.\n")
  
  # Parse submissions:
  submissions <- list.files(path = path_to_submissions,
                            full.names = FALSE,
                            pattern = csv_types,
                            recursive = TRUE) 
  
  Encoding(submissions) <- "utf8"
  
  if (verbose) cat(paste0("Number of CSV files to be processed: ", length(submissions), "\n"))
  
  
  # parse names and Matrikelnummer to df:
  d <-
    tibble(id = 1:length(submissions)) %>% 
    mutate(csv_file_name = str_conv(submissions, "utf8")) %>% 
    mutate(csv_file_name = convertUmlaut(csv_file_name)) %>% 
    mutate(last_name = parse_last_names(csv_file_name),
           first_name = parse_first_names(csv_file_name)) %>% 
    mutate(matrikelnummer = parse_matrikelnummer(csv_file_name))
  
  
  # add nrow of preds to df:
  if (verbose) print("Now counting data lines per pred data file.")
  d2 <-
    d %>% 
    mutate(npreds = map_dbl(submissions,
                            ~ countLines(paste0(subm_path, .x))) - 1)
  
  
  
  # add column names of pred data file to df:
  if (verbose) print("Now extracting col names from csv file with prediction data.")
  d2a <-
    d2 %>% 
    mutate(colnames_pred_file = map_chr(.x = submissions,
                                        .f = ~ data.table::fread(paste0(subm_path, .x),
                                                                 nrows = 1) %>% 
                                          names() %>% 
                                          str_c(collapse = " - ")))
  
  
  #debug(prep_csv)
  # parse prediction data to df:
  if (verbose) print("Now parsing csv files with prediction data.")
  d3 <- 
    d2a %>% 
    #slice(1) %>% 
    mutate(data = map(
      .x = submissions,
      .f = ~ prep_csv(submission_file =  .x,
                      path_to_submissions = subm_path,
                      path_to_test_data = solution_df_file,
                      path_to_train_data = train_df_file,
                      verbose = TRUE)))
  
  
  if (verbose) print("Now computing test set error.")
  d4 <-
    d3 %>% 
    mutate(error_coef = map(data,
                    ~error_fun(truth = y,
                         estimate = pred,
                         data = .x))) 
  # set `error_fun <- mae` during debugging!
  
  options(scipen = 4)
  d5 <-
    d4 %>% 
    mutate(error_value = map_dbl(error_coef,
                            ".estimate"))
  
  return(d5)
}
