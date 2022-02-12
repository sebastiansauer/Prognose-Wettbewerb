library(tidyverse)
library(writexl)
library(janitor)
library(rio)

#notenliste_vorlage <- read_csv("Notenliste_Vorlage.csv")
d_grades <- read_rds(file = paste0(results_path, "d_grades.rds"))



participants_results_df <- import(notenliste_template_path,
                                  skip  = 20)  %>% # skip the first few lines
  remove_empty("rows") %>% 
  clean_names() %>% 
  rename(nachname = "name") %>% 
  select(-c(bewertung, bemerkung))


names(d_grades)



# Merge template (Vorlage) with grade sheet -------------------------------


d_grades2 <-
  d_grades %>% 
  select(matrikelnummer, grade, last_name, first_name, id, error_value) %>% 
  rename(mat_nummer = matrikelnummer,
         Name = last_name,
         Vorname = first_name,
         Bewertung = grade) %>% 
  mutate(Bemerkung = paste0(error_fun_name, " im Test-Sample: ", round(error_value, 3))) %>% 
  select(-c(Name, Vorname, id, error_value))



notenliste <-
  participants_results_df %>% 
  full_join(d_grades2, by = "mat_nummer")


glimpse(notenliste)



# Manuel changes to grades ------------------------------------------------

# ... 


# Write to disk -----------------------------------------------------------




write_csv(notenliste, paste0(results_path, "notenliste.csv"))
write_xlsx(notenliste,  paste0(results_path, "notenliste.xlsx"))
