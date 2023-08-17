process_submissions <- function (paths, verbose = TRUE) {
  
  path_subm_files <- list.files(path = paths$subm_raw,
                                full.names = TRUE,
                                pattern = paths$csv_pattern,
                                recursive = TRUE) 
  
  if (verbose) cat(paste0("Number of raw submissions found: ", length(path_subm_files)))
  
  if (!(file.exists(paths$subm_proc))) {
    if (verbose) cat("Creating folder for processed submissions")
    dir.create(paths$subm_proc)
  }
  
  full_path_from <- paste0(here::here(), "/", paths$subm_raw, "/")
  full_path_to <- paste0(here::here(), "/", paths$subm_proc, "/")
  
  stopifnot(file.exists(full_path_from))
  stopifnot(file.exists(full_path_to))
  
  copy_subm_files_succeeded <- 
    file.copy(from = paths$subm_files_w_path, 
              to = full_path_to,
              overwrite = TRUE)
  stopifnot(copy_subm_files_succeeded)
  
  if (verbose) cat(paste0(length(paths$subm_files_w_path), " submission files have been copied from raw folder to processed folder."))
  
  
  # Get last names of each submission:
  
  subm_processed_files <- 
    list.files(path = full_path_to,
               full.names = FALSE,
               recursive = FALSE) 
  
  
  # subm_processed_files |> 
  #   str_match("(\\w+-?\\w*)_(?:\\d)")
  # 
  subm_names <- 
    subm_processed_files %>% 
    # First name possibly with dash then lastname but do not capture "underscore some digits" afterwards:
    str_match("(\\w+-?\\w*)_(?:\\d)") %>%  
    `[`(, 2) %>% 
   discard(is.na) %>% 
    str_to_lower() %>% 
    # if two underscores, replace the second with a blank, as only one underscore is permitted:
    str_replace(pattern = "(_)(\\w+)(_)", replacement = "\\1\\2 ")
  
  subm_matrikelnummers <-
    subm_processed_files %>% 
    str_match("(?:\\w+-?\\w*)_(\\d+)") %>%  
    `[`(, 2) %>% 
    discard(is.na) %>% 
    str_to_lower()
  
  
  subm_ids <-
    tibble(
      name = subm_names) |> 
    separate_wider_delim(name, delim = "_", names = c("firstname", "lastname")) |> 
  mutate(matrikelnummer  = subm_matrikelnummers)
      
  return(subm_ids)
 
}
