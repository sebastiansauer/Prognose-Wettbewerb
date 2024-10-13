
build_grading_list <- function(grading_template_path, grades){
  
  grading_template <-
    read_xlsx(grading_template_path)
  
  if ("Mat.nummer" %in% colnames(grading_template)) {
    grading_template <- grading_template %>%
      rename(id = `Mat.nummer`)
  }
  
  out <-
    grading_template |> 
    mutate(id = as.character(id)) |> 
    mutate(id = str_pad(id, width = 8, side = "left", pad = "0")) |> 
    full_join(no_shows_added, by = "id") |> 
    filter(!if_all(everything(), is.na))  # remove all empty rows
  
  return(out)
}