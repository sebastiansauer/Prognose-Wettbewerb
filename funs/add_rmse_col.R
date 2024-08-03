add_rmse_col <- function(main_df){
  
  main_df |> 
    dplyr::mutate(rmse_coef = purrr::map(data,
                                         ~yardstick::rmse(truth = y,
                                                          estimate = pred,
                                                          data = .x))) |> 
    dplyr::mutate(error_value = purrr::map_dbl(rmse_coef,
                                        ".estimate"))
  
  
}