source("handle_data.R")


# SETUP LOCUS COLUMNS
locus_column_names <- c("G10L - 1", "G10L - 2", "MU05 - 1", "MU05 - 2", "MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2", 
                        "MU23 - 1", "MU23 - 2", "MU50 - 1", "MU50 - 2", "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2")

locus_columns <- c("G10L", "G10L.1", "Mu05", "Mu05.1", "Mu09", "Mu09.1", "Mu10", "Mu10.1",
                   "Mu23", "Mu23.1", "Mu50", "Mu50.1", "Mu51", "Mu51.1", "Mu59", "Mu59.1")

names(locus_columns) <- locus_column_names

# LOAD THE DATA
data <- load_data(file_path = "~/Downloads/AC_allKorr.csv", index_column = "SEP", locus_columns = locus_columns, "Individ",
                  meta_columns = c(date = "Funnetdatum", north = "Nord", east = "Ost", gender = "Kon"))

str(data)

## CREATE EXAMPLE DATA
locus_data <- c("110", "112", "143", "145", "123", "127", "164", "170", "150", "150", "230", "248", "184", "186", "128", "132")
names(locus_data) <- c("MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2", "MU05 - 1", "MU05 - 2", "MU23 - 1", "MU23 - 2", 
                       "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2", "G10L - 1", "G10L - 2", "MU50 - 1", "MU50 - 2")

new_data <- create_new_data(index = "SEP123", multilocus = locus_data, meta = c(date = "2020-06-29", north = "7096503", east = "644381", gender = "Hane"))

