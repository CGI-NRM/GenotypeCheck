#' Import and format data
#'
#' @param file The path to the file to be imported
#'
#' @return A table with the relevant columns from the file
#' @export
#'
#' @examples
#' data <- import_data("raw_data.csv")
import_data <- function(file) {
  raw_data <- read.csv(file = file, header = TRUE, sep = ",", na.strings = c("NA"))
  multilocus_names <- c("G10L", "G10L.1", "Mu05", "Mu05.1", "Mu09", "Mu09.1", "Mu10", "Mu10.1",
                        "Mu23", "Mu23.1", "Mu50", "Mu50.1", "Mu51", "Mu51.1", "Mu59", "Mu59.1")
  data <- raw_data %>%
    select("SEP", "Funnetdatum", "Nord", "Ost", "Kon", "Individ", all_of(multilocus_names))
  data
}

#' Convert the data to a allelematch dataset
#'
#' @param data The relevant data that has been imported
#'
#' @return A allelematch dataset with the relevant index and locus column
#' @export
#'
#' @examples
#' am_data <- get_allelematch_dataset(data)
get_allelematch_dataset <- function(data) {
  am_data <- amDataset(data, indexColumn = "SEP", ignoreColumn = c("Funnetdatum", "Nord", "Ost", "Kon", "Individ"), missingCode = "NA")
  am_data
}

#' TODO:
#' - Check what the diffrences are between calculated data and given data
#' - Match for exact match
#' - Give close

library(tidyverse)
library(allelematch)

data <- import_data("R/AC_allKorr.csv")
am_data <- get_allelematch_dataset(data)

alleleMismatch <- 3
am_unique <- amUnique(am_data, alleleMismatch = alleleMismatch)

indexes <- c()
individ_id <- c()
multilocuses <- c()
ind <- 0

search_data <- data.frame()

for (pair in am_unique$pairwise) {
  multilocus <- paste0(unname(split(pair$match$multilocus, rep(1:nrow(pair$match$multilocus), ncol(pair$match$multilocus)))))


  search_data <- search_data %>%
    rbind(list(index = pair$match$index, multilocus = pair$match$multilocus))
  individ_id <- individ_id %>%
    c(rep(ind, length(pair$match$index)))
  ind <- ind + 1
  indexes <- indexes %>%
    c(pair$match$index)
  multilocuses <- multilocuses %>%
    c(unname(split(pair$match$multilocus, rep(1:nrow(pair$match$multilocus), ncol(pair$match$multilocus)))))
}

search_data <- list(index = indexes, multilocus = multilocuses, individ_id = individ_id)

write.csv(search_data, file = "R/out.csv", sep = ",")
