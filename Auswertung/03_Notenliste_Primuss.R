library(tidyverse)
library(writexl)

notenliste_vorlage <- read_csv("Notenliste_Vorlage.csv")
d_grades <- read_rds(file = "d_grades.rds")

names(d_grades)



# Merge template (Vorlage) with grade sheet -------------------------------


d_grades2 <-
  d_grades %>% 
  select(matrikelnummer, grade, last_name, first_name, id, r2_vec) %>% 
  rename(Mat.nummer = matrikelnummer,
         Name = last_name,
         Vorname = first_name,
         Bewertung = grade) %>% 
  mutate(Bemerkung = paste0("R2 im Test-Sample: ", round(r2_vec, 3))) %>% 
  select(-c(Name, Vorname, id, r2_vec))


notenliste_vorlage2 <-
  notenliste_vorlage %>% 
  select(-c(Bewertung, Bemerkung))

names(notenliste_vorlage2)

notenliste <-
  notenliste_vorlage2 %>% 
  full_join(d_grades2, by = "Mat.nummer")


glimpse(notenliste)



# Manuel changes to grades ------------------------------------------------

# ... 


# Write to disk -----------------------------------------------------------




write_csv(notenliste, "notenliste.csv")
write_xlsx(notenliste, path = "notenliste.xlsx")
