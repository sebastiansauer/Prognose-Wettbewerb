
library(gt)
library(hrbrthemes)


grades_file_path <- paste0(results_path, "d_grades.rds")
file.exists(grades_file_path)

d_grades <- read_rds(file = grades_file_path)


bestehensquote <- 
  nrow(d_grades[d_grades$grade <= 4, ]) / nrow(d_grades) 

bestehensquote


grades_p_hist <- 
  d_grades %>% 
  ggplot(aes(x = grade_f)) +
  geom_bar() +
  geom_vline(xintercept = mean(d_grades$grade),
             linetype = "dashed") +
  annotate("label", x = mean(d_grades$grade),
           y = 1,
           label = paste0("MW: ", numform::round2(mean(d_grades$grade),
                                                  digits = 2))) +
  labs(title = "Notenverteilung QM1, SoSe 22",
       caption = paste0("n = ", nrow(d_grades), "; Bestehensquote: ", round(bestehensquote, 2)),
       x = "Notenstufen",
       y = "Anzahl") +
  #scale_x_continuous() +
  scale_y_continuous(breaks = seq(0,15,5)) +
  theme_ipsum_rc() 
grades_p_hist

ggsave(plot = grades_p_hist, 
       filename = paste0(results_path, "/grades_p_hist.png"))


# grades_p_boxplot <- 
#   d_grades %>% 
#   ggplot(aes(x = 1, y = grade)) +
#   geom_boxplot() +
#   geom_jitter(width = 0.1, alpha = .2) +
#   scale_x_continuous(labels = NULL, guide = NULL) +
#   labs(title = "Notenverteilung QM1, SoSe 22",
#        caption = paste0("n = ", nrow(d_grades)))
# 
# grades_p_boxplot
# 
# ggsave(plot = grades_p_boxplot,
#        file = "figs/grades_p_boxplot.png")

d_grades_sum <- 
  d_grades %>% 
  summarise(grade_mean = mean(grade),
            grade_md = median(grade),
            grade_sd = sd(grade),
            grade_mad = mad(grade),
            grade_iqr = IQR(grade)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Kennwert",
               values_to = "Wert") %>% 
  mutate(Wert = round2(Wert, digits = 2))


d_grades_sum_gt <- 
  d_grades_sum %>% 
  gt()

d_grades_sum_gt

gtsave(d_grades_sum_gt, file = "figs/d_grades_sum_gt.png")

# plot distribution of r2.



# R2 intervals and grades matching:


cut_interval(d_r2$r2_vec, 
             n = 10) %>% 
  table() %>% 
  names() -> interval_borders

grades <- c(
  4, 3.7,
  3.3, 3, 2.7,
  2.3, 2, 1.7,
  1.3, 1)


matching_r2_grades <- 
  tibble(interval_borders = interval_borders,
         grades = grades)

matching_r2_grades_gt <- 
  matching_r2_grades %>% 
  gt()

gtsave(matching_r2_grades_gt, file = "figs/matching_r2_grades_gt.png")
