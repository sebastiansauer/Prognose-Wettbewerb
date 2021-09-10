comp_r2_submissions <- function(subm_path = "Submissions/") {
  
  source("funs/parse_names.R")
  source("funs/prep_csv.R")
  
  # Parse submissions:
  submissions <- list.files(path = subm_path,
                            full.names = FALSE,
                            pattern = ".csv$",
                            recursive = TRUE) 
  
  Encoding(submissions) <- "utf8"
  
  
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
                            ~ countLines(paste0(subm_path, .x))))
  
  
  
  # add column names of pred data file to df:
  if (verbose) print("Now extracting col names from csv file with prediction data.")
  d2a <-
    d2 %>% 
    mutate(colnames_pred_file = map_chr(.x = submissions,
                                        .f = ~ data.table::fread(paste0(subm_path, .x),
                                                                 nrows = 1) %>% 
                                          names() %>% 
                                          str_c(collapse = " - ")))
  
  
  # parse prediction data to df:
  if (verbose) print("Now parsing csv files with prediction data.")
  d3 <- 
    d2a %>% 
    #slice(1) %>% 
    mutate(data = map(
      .x = submissions,
      .f = ~ prep_csv(file =  .x,
                      verbose = TRUE)))
  
  
  if (verbose) print("Now computing r2.")
  d4 <-
    d3 %>% 
    mutate(r2 = map(data,
                    ~rsq(truth = lebenszufriedenheit,
                         estimate = pred,
                         data = .x))) 
  
  
  options(scipen = 4)
  d5 <-
    d4 %>% 
    mutate(r2_vec = map_dbl(r2,
                            ".estimate"))
  
  return(d5)
}
