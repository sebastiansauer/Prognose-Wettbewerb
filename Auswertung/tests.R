# Tests


library(testthat)


test_that("CSV-Files have been read",
          {expect_gt(length(submissions), 200)})


test_that("Alle Nachnamen in den Ordnern finden sich in der Notenliste",
          {}
          )

subm_last_names %>% 
  setdiff(d_grades$last_name)



"weissler" %in% d_grades$last_name
d_grades$last_name[d_grades$first_name == "selin"]
"selin" %in% d_grades$first_name
"00158303" %in% d_grades$matrikelnummer
