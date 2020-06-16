library(tidyverse)
library(allelematch)

#' Import and format data
#' @description To be replaced by the user chosing the relevant columns
#'
#' @param file The path to the file to be imported
#' @param index_column The name or index of the column containing the indexes
#' @param additional_data A vector with the names or indexes to the columns that contain the date, north, east, gender, and any preset id:s in that order.
#' @param locus_names A vector with the name or indexes to the columns that contain the genotypes
#'
#' @return A table with the relevant columns from the file
#' @export
#'
#' @examples
#' \dontrun{
#' data <- import_data("raw_data.csv")
#' }
import_data <- function(file, index_column, additional_data, locus_names) {
  # Read the data from the file
  raw_data <- read.table(file = file, header = TRUE, sep = ",", na.strings = c("NA"))
  # Select only the columns we are intressted in
  data <- raw_data %>%
    select(all_of(index_column), as.vector(unlist(additional_data)), all_of(locus_names))
  # Return the resulting table
  data
}


#' Convert The Data to a Allelematch Dataset
#' @description To be rewritten to take a table with only index and locus data, split in the \code{\link{import_data}} function.
#' Now ignores the irrelevant data for this step, can be connected by their index later. A wrapper for the
#' \code{\link{amDataset}} function.
#'
#' @param data The relevant data that has been imported
#' @param index_column The name or indexe to the column that contain the index of the data
#' @param ignore_columns A vector with the names or indexes to the columns that are to be ignored by the 'allelematch' packet. These are the date, nord, east, gender and preset individual, in that order. TODO: Remove the preset individual, it should be used earlier.
#'
#' @return A allelematch dataset with the relevant index and locus column
#' @export
#'
#' @examples
#' \dontrun{
#' am_data <- create_allelematch_dataset(data)
#' }
create_allelematch_dataset <- function(data, index_column, ignore_columns) {
  # Create the allelematch dataset
  am_data <- allelematch::amDataset(data, indexColumn = index_column, ignoreColumn = as.vector(unlist(ignore_columns)), missingCode = "000")
  # Retrun the allelematch dataset
  am_data
}

#' Load Data and Group Into Individuals
#' @description TODO: Keep the multimatch data
#'
#' @param file_path A path to the csv file
#' @param index_column The name or index to the colmn containing the indexes for the data
#' @param additional_data_columns A named list with the names or indexes to the date, north, east, gender and preset_ind colmns.
#' @param locus_columns A vector with the names or indexes to the columns containing the locus data
#' @param allele_mismatch A value for how many allele mismatchs are to be allowed and still count like a match
#'
#' @return A list with $index, $multilocus, and $individ_id.
#' @export
#'
#' @examples
#' \dontrun{
#' search_data <- create_search_data("data.csv")
#' }
create_search_data <- function(file_path, index_column, additional_data_columns, locus_columns, allele_mismatch) {
  data <- import_data(file_path, index_column = index_column, additional_data = additional_data_columns, locus_names = locus_columns)
  rownames(data) <- pull(data, index_column)

  am_data <- create_allelematch_dataset(data, index_column = index_column, ignore_columns = additional_data_columns)
  am_unique <- amUnique(am_data, alleleMismatch = allele_mismatch)

  ind <- 0
  search_data <- data.frame(index = character(), multilocus = character(), individ_id = character())

  for (pair in am_unique$pairwise) {
    multilocus_combined <- apply(pair$match$multilocus, 1, combine_multilocus)

    search_data <- rbind(search_data, list(index = pair$match$index, multilocus = multilocus_combined, individ_id = rep(as.character(ind), length(pair$match$index))))
    ind <- ind + 1
  }

  if (!is.null(additional_data_columns$preset_ind)) {
    for (ind in 1:length(search_data$index)) {
      new_id <- pull(data[search_data$index[[ind]],], additional_data_columns$preset_ind)
      search_data$individ_id[[ind]] <- new_id
    }
  }

  list(search_data, am_unique$multipleMatches$index, list(index = am_unique$unclassified$index, multilocus = am_unique$unclassified$multilocus))
}

#' Combine Multiple Locus and Assure Constant Width
#'
#' @param locus A vector with all the locus in string format.
#'
#' @return A long string with all locus combined, padding zeros making ever locus three characters long.
#' @export
#'
#' @examples
#' \dontrun{
#' multilocus <- c("182", "180", "152", "152")
#' multilocus_combined <- combine_multilocus(multilocus)
#' }
#' \dontrun{
#' multilocus_combined <- apply(multilocus_matrix, 1, combine_multilocus)
#' }
#'
combine_multilocus <- function(locus) {
  paste0(formatC(as.numeric(locus), width = 3, flag = "0", format = "d"), collapse = "")
}
