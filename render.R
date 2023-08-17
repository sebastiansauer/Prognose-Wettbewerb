#library(tidyverse)
library(targets)
tar_make()

verbose <- TRUE

tar_load(path)
tar_load(names_processed)
tar_load(names_raw)
tar_load(names_diff)
tar_load(grades_thresholds)
tar_load(maes)
tar_load(grades)
tar_load(grades_plot)
tar_load(grades_thresholds_plot)
tar_load(notenliste)
tar_load(no_shows)
