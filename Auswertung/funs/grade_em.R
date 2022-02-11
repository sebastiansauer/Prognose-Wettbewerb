grade_em <- function(x, breaks12, reverse = FALSE){
  # input: 9 threshold values, giving the grade  thresholds 4 to 1.3
  

  grades_scheme <- c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1)  # length 11
  
  if (reverse) grades_scheme <- rev(grades_scheme)
  
  grades <- cut(x, breaks = breaks12, labels = grades_scheme)
}




#x <- c(1,2,3)
#cut(x, breaks = 2, labels = c("low", "high"))


# cut(c(1.1, 1.6, 0.9),
#     breaks = mae_seq,
#     labels = grades_scheme)
