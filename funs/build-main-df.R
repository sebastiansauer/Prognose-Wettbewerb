build_main_df <- function(submission_df, vars_to_nest = c("id", "pred", "y")){
  
  submission_df |> 
  rename(csv_file_name = filename) |> 
    nest(data = all_of(vars_to_nest)) |> 
    mutate(csv_file_name = stringr::str_conv(csv_file_name, "utf8")) |>
    mutate(csv_file_name = berryFunctions::convertUmlaut(csv_file_name)) |>
    mutate(last_name = parse_last_names(csv_file_name),
                  first_name = parse_first_names(csv_file_name)) |>
    mutate(id = parse_matrikelnummer(csv_file_name)) |> 
    mutate(npreds = map_dbl(.x = data,
                                          .f = nrow)) |> 
    mutate(colnames_pred_file = map_chr(.x = data,
                                                      .f = ~ str_c(names(data), collapse = " - ")))
  
}