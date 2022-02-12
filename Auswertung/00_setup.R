# define constants
# such as paths etc.



# Grades scheme -----------------------------------------------------------


grades_scheme <- c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1) 


# Raw data (submissions) path ---------------------------------------------

raw_path <- "/Users/sebastiansaueruser/Google Drive/Lehre/Lehre_AKTUELL/2021-WiSe/QM1/Prüfung/Submissions-raw/"

# check if path exists:
dir.exists(raw_path)



# Submission processed path -----------------------------------------------

subm_path <- "/Users/sebastiansaueruser/Google Drive/Lehre/Lehre_AKTUELL/2021-WiSe/QM1/Prüfung/Submissions-processed/"

dir.exists(subm_path)


# Results (objects) path --------------------------------------------------

results_path <- "/Users/sebastiansaueruser/Google Drive/Lehre/Lehre_AKTUELL/2021-WiSe/QM1/Prüfung/Ergebnisse/"

# check if path exists:
dir.exists(results_path)




# control (test) data -----------------------------------------------------

solution_df_file <- "/Users/sebastiansaueruser/github-repos/Vorhersagewettbewerb/WiSe_2021/ses-predcontest-wise21/raw-data/d-control.csv"


file.exists(solution_df_file)




# train data --------------------------------------------------------------

train_df_file <- "/Users/sebastiansaueruser/github-repos/Vorhersagewettbewerb/WiSe_2021/ses-predcontest-wise21/raw-data/d-train.csv"

file.exists(train_df_file)







# Notenliste --------------------------------------------------------------


notenliste_template_path <- "/Users/sebastiansaueruser/Google Drive/Lehre/Lehre_AKTUELL/2021-WiSe/QM1/Prüfung/Quantitative_Methoden_I_AWM_016_Notenliste_09-02-2022.xlsx"






# Error function ----------------------------------------------------------



error_fun <- mae
error_fun_name <- "mae"





# Div ---------------------------------------------------------------------



csv_types <- ".csv$|.CSV$|.Csv$|.CSv$|.csV$|.cSV$|.cSV$"
