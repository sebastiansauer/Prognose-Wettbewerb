copy_submissions <- function (paths, submissions_raw, verbose = TRUE) {
  
  #' Copy submission files 
  #' Copy submission files from raw data folder to destination folder
  #' 
  #' Note that relative paths are expected
  #' 
  #' @param paths list with paths (object from path yml file)
  #' @param verbose print details (defaults to TRUE)?
  
  # path_subm_files <- list.files(path = paths$subm_raw,
  #                               full.names = TRUE,
  #                               pattern = paths$csv_pattern,
  #                               recursive = TRUE) 
  

  # if destination folder does not exist, create it:
  if (!(file.exists(paths$subm_proc))) {
    dir.create(paths$subm_proc)
    if (verbose) print("Destination folder did not exist, I created it.")
  }
  
  # create absolute paths:
  full_path_from <- paste0(here::here(), "/", paths$subm_raw, "/")
  full_path_to <- paste0(here::here(), "/", paths$subm_proc, "/")
  
  # stopifnot(file.exists(full_path_from))
  # stopifnot(file.exists(full_path_to))
  
  paths$subm_files_w_full_path <- submissions_raw
  
  # paths$subm_files_w_full_path <- list.files(
  #   path = paths$subm_raw,
  #   full.names = TRUE,
  #   pattern = paths$csv_pattern,
  #   recursive = TRUE) 

  # copy:
  copy_subm_files_succeeded <- 
    file.copy(from = paths$subm_files_w_full_path, 
              to = full_path_to,
              overwrite = TRUE)
  if (verbose) print(paste0("In total, ", length(submissions_raw), 
                            " CSV submission files have been copied to destination folder."))
  stopifnot(copy_subm_files_succeeded)
  

  # return list of copied files:
  submissions_copied <-
    list.files(path = paths$subm_files_w_full_path,
             full.names = TRUE,
             pattern = paths$csv_pattern,
             recursive = TRUE)
}
