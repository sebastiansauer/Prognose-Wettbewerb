# define constants
# such as paths etc.

library(teachertools)

# Grades scheme -----------------------------------------------------------


grades_scheme <- get_grade_scheme()
grades_scheme
#length(grades_scheme)



# Div ---------------------------------------------------------------------



# 
# path$subm_files_wo_path <- list.files(path = path$subm_proc,  # processed!
#                                       full.names = FALSE,
#                                       pattern =  csv_pattern,
#                                       recursive = TRUE)


# test if all exist:
path %>% 
  map(file.exists) %>% 
  simplify() %>% 
  all()



# Names and IDs of submissions ----------------------------------------------------

subm <- list()

subm$names <- 
  path$subm_files_wo_path %>% 
  map_chr(parse_last_names)



subm$ids <- 
  path$subm_files_wo_path %>% 
  map_chr(parse_matrikelnummer) 




# Grade particulars -------------------------------------------------------



# Add individual "ad-hoc" changes to computed grade:



nrow(grade_thresholds)


grades_particulars <-
  tribble(
    ~matrikelnummer, ~grade_change, ~comment,
    "00159390", 2, "Stoff des Unterrichts wurde unzureichend verwendet"
  )




# Grade thresholds --------------------------------------------------------



grade_thresholds <-
  tibble::tribble(
    ~threshold, ~grade,
    0,    0.7,
    14250L,      1,
    15000L,    1.3,
    16000L,    1.7,
    17000L,      2,
    18100L,    2.3,
    18500L,    2.7,
    19000L,      3,
    20000L,    3.3,
    21000L,    3.7,
    22000L,      4,
    Inf,         5
  )




# if "id" is not a column in the test set, create it:
if (!"id" %in% names(solution_df)) {
  solution_df <-
    solution_df %>% 
    mutate(id = row_number() + start_id_testset -1) %>% 
    relocate(id, .before = 1)
}




# Error function ----------------------------------------------------------



error_fun <- yardstick::rmse  # using package {yardstick}
error_fun_name <- "rmse"





# Name of output variable (DV) --------------------------------------------


dv_name <- "count"
id_name <- "id"

vars_for_testset <- c(id_name, dv_name)




