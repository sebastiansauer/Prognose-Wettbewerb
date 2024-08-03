add_solution_col <- function(submission_df, 
                             solution_df, 
                             name_output_var = "y") {
  
  

  solution_df <-
    solution_df |>
    dplyr::rename(y = {{name_output_var}})
  
  # add id column in solution df if not present:
  if (!("id" %in% names(solution_df))) solution_df$id <- start_id:(start_id + nrow(solution_df) - 1)
  
  solution_df_short <- 
    solution_df |>
    dplyr::mutate(id = as.integer(id)) |> 
    dplyr::select(id, y)
  
  if (!any(names(solution_df) == "y")) {
    cat(paste0("Names of columns in solution_df: "), names(solution_df))
    stop("`y` not among column names of solution df!")
  }
  
  x_joined <-
    submission_df |>
    dplyr::left_join(solution_df_short,
                     by = "id")
  
  
}