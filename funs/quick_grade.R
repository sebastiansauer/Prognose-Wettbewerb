

#' quickly grade a prediction contest submission
#' 
#' Note that no sanitizing of the csv file is taking place.
#' All that happens is that truth and estimate csv files are loaded
#' and then their correspondence as to RMSE is returned.
#' Note that "id" and "pred" are the assumed columns in the estimate csv.
#' An "id" column ranging from 1 to nrow() is added to the truth csv.
#' The name of the outcome variable column must be supplied.
#'
#' @param df_estimate_path path and file name of estimate csv file (character)
#' @param df_truth_path path and file name of truth csv file (character)
#' @param name_outcome_var name of outcome variable (character)
#'
#' @return numeric value of rmse performance (tibble)
#' @export
#'
#' @examples
#' quick_grade(estimate_file, truth_file, outcome_var)
quick_grade <- function(df_estimate,
                        df_truth,
                        name_outcome_variable) { 

  
  df_truth <-
    df_truth|> 
    dplyr::mutate(id = 1:n())
  
  df_merged <-
    df_truth |> 
    # dplyr::select(id, all_of(name_outcome_variable)) |> 
    dplyr::select(id, {{name_outcome_variable}}) |> 
    dplyr::left_join(df_estimate, by = "id")
  
  performance_rmse <- 
    yardstick::rmse(df_merged,
                    truth = count,
                    estimate = pred)
  
  return(performance_rmse)
  
}
