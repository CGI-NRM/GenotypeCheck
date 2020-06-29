library(dplyr)
library(allelematch)
library(readxl)
library(readODS)


library(poppr)
library(adegenet)
library(phangorn)
library(ape)

locus_names <- c("G10L", "G10L.1", "Mu05", "Mu05.1", "Mu09", "Mu09.1", "Mu10", "Mu10.1",
                "Mu23", "Mu23.1", "Mu50", "Mu50.1", "Mu51", "Mu51.1", "Mu59", "Mu59.1")

locus_names_known <- c("G10L - 1", "G10L - 2", "MU05 - 1", "MU05 - 2", "MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2",
                       "MU23 - 1", "MU23 - 2", "MU50 - 1", "MU50 - 2", "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2")

data <- import_data("data/AC_allKorr.csv", "SEP", c(), locus_names)

new_data <- data.frame(index = data$index)
for (i in 1:(length(locus_names) / 2)) {
  new_data <- cbind(new_data, paste(data[,locus_names_known[2 * i - 1]], data[,locus_names_known[2 * i]], sep = " "))
}

colnames(new_data) <- c("index", locus_names_known[c(TRUE, FALSE)])
rownames(new_data) <- new_data$index
new_data <- new_data %>% select(locus_names_known[c(TRUE, FALSE)])

gendata <- as.genclone(df2genind(new_data, sep = " ", ploidy = 2, ncode = 3))

raw_dist <- function(x){
  dist(genind2df(x, usepop = FALSE))
}
xdis <- raw_dist(gendata)
plot.phylo(upgma(xdis))
# mlg.filter(gendata, distance = xdis) <- 1 + .Machine$double.eps^0.5
mlg.filter(gendata, distance = bruvo.dist, replen = c(2, 2, 2, 2, 1, 2, 2, 2)) <- 0.6
# bruvo.dist(gendata, replen = c(2, 2, 2, 2, 1, 2, 2, 2))
mlg.table(gendata)



#' Import and format data
#' @description To be replaced by the user chosing the relevant columns
#'
#' @param file The path to the file to be imported
#' @param index_column The name or index of the column containing the indexes
#' @param additional_data A vector with the names or indexes to the columns that contain the date, north, east, gender, and any preset id:s in that order.
#' @param locus_names A vector with the name or indexes to the columns that contain the genotypes
#' @param sheet The index or name to the sheet desired from the file. Does noting if a csv file is loaded.
#'
#' @return A table with the relevant columns from the file
#' @export
#'
#' @examples
#' \dontrun{
#' data <- import_data("raw_data.csv")
#' }
import_data <- function(file, index_column, additional_data, locus_names, sheet = 1) {
  # Read the data from the file depending on the file type
  if (endsWith(file, ".xls") | endsWith(file, ".xlsx")) {
    raw_data <- readxl::read_excel(path = file, sheet = sheet, na = c("NA"), col_names = TRUE)
  } else if (endsWith(file, "ods")) {
    raw_data <- readODS::read_ods(path = file, col_names = TRUE, na = "NA", sheet = sheet)
  } else {
    raw_data <- read.table(file = file, header = TRUE, sep = ",", na.strings = c("NA"))
  }

  # Select only the columns we are intressted in
  data <- raw_data %>%
    select(all_of(index_column), as.vector(unlist(additional_data)), all_of(locus_names))

  # Rename all column to be the names we know (index, north, south, gender etc) insted of the colmn names from the file
  locus_names_known <- c("G10L - 1", "G10L - 2", "MU05 - 1", "MU05 - 2", "MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2", "MU23 - 1", "MU23 - 2", "MU50 - 1", "MU50 - 2", "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2")
  colnames(data) <- c("index", names(additional_data), locus_names_known)
  # Make the rows indexable by index
  rownames(data) <- data$index
  # Return the table
  data
}


#' Convert The Data to a Allelematch Dataset
#' @description To be rewritten to take a table with only index and locus data, split in the \code{\link{import_data}} function.
#' Now ignores the irrelevant data for this step, can be connected by their index later. A wrapper for the
#' \code{\link{amDataset}} function.
#'
#' @param data The relevant data that has been imported
#' @param ignore_columns A vector with the names or indexes to the columns that are to be ignored by the 'allelematch' packet. These are the date, nord, east, gender and preset individual, in that order. TODO: Remove the preset individual, it should be used earlier.
#'
#' @return A allelematch dataset with the relevant index and locus column
#' @export
#'
#' @examples
#' \dontrun{
#' am_data <- create_allelematch_dataset(data)
#' }
create_allelematch_dataset <- function(data, ignore_columns) {
  # If ignore_columns is empty we have no columns to ignore and change it therefor to NULL for it not te be read as a character in the amDataset function.
  if (identical(ignore_columns, character(0))) {
    ignore_columns <- NULL
  }
  # Create the allelematch dataset
  am_data <- allelematch::amDataset(data, indexColumn = "index", ignoreColumn = as.vector(unlist(ignore_columns)), missingCode = "000")
  # Retrun the allelematch dataset
  am_data
}

#' Load Data and Group Into Individuals
#' @description TODO: Keep the multimatch data
#'
#' @param data A dateframe with index, all meta-data and locus
#' @param am_data An allelematch dataset, converted from the data containing the index and locus
#' @param allele_mismatch A value for how many allele mismatchs are to be allowed and still count like a match
#'
#' @return A list with the search_data, which is a list with $index, $multilocus, and $individ_id, a list with the $index of the samples that matched multiple individuals, and a list with the $index and $multilocus of the samples were unclassified.
#' @export
#'
#' @examples
#' \dontrun{
#' search_data <- create_search_data("data.csv")
#' }
#create_search_data <- function(file_path, index_column, additional_data_columns, locus_columns, allele_mismatch) {
create_search_data <- function(data, am_data, allele_mismatch) {
  # Group the samples together to form individuals
  am_unique <- allelematch::amUnique(am_data, alleleMismatch = allele_mismatch)

  ind <- 0
  # If a override id is specified, get the largest override id and add ind from that
  if (!is.null(data$preset_ind)) {
    # Convert everything to a number
    preset_num <- as.numeric(data[,"preset_ind"])
    # Remove everything that is not a number, our generated ind cannot accidentaly assume the same value
    preset_num <- preset_num[!is.na(preset_num)]
    # Start right after the biggest one found
    ind_biggest <- max(preset_num) + 1
    # If all override_ids were NA or not numbers, we can count from zero like usual
    if (is.na(ind_biggest)) {
      ind_biggest <- 0
    }
  }

  # Go through the data and create a large data.frame with all the prevoius samples, adding a column for the individ_id
  search_data <- data.frame(index = character(), multilocus = character(), individ_id = character())

  for (pair in am_unique$pairwise) {
    multilocus_combined <- apply(pair$match$multilocus, 1, combine_multilocus)

    search_data <- rbind(search_data, list(index = pair$match$index, multilocus = multilocus_combined, individ_id = rep(as.character(ind), length(pair$match$index))))
    ind <- ind + 1
  }

  # Add a empty column for the override data to be read from the file or created by the user later
  search_data <- cbind(search_data, list(override_id = rep(NA, length(search_data$index))))

  # If a override_id column is specified, write it to the new column
  if (!is.null(data$preset_ind)) {
    search_data$override_id <- data[search_data$index,"preset_ind"]
  }

  # The multiple matches that have been handled by the user previously and is now in the file
  multiple_matches_filter <- duplicated(search_data$index) & duplicated(search_data$override_id) & !is.na(search_data$override_id)

  multiple_matches <- am_unique$multipleMatches$index
  # Only keep the multiple matches indexes that have not been handled
  multiple_matches <- multiple_matches[!(multiple_matches %in% search_data$index[multiple_matches_filter])]

  search_data <- search_data[!multiple_matches_filter,]

  # Return all we want, the data (meta-data), search_data (index, multilocus and the id to group them together)
  list(search_data, multiple_matches, list(index = am_unique$unclassified$index, multilocus = am_unique$unclassified$multilocus))
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
combine_multilocus <- function(locus) {
  # Convert it to a number, add leading 0es if needed to reach length 3 and paste with collapse to create a long string
  locus %>%
    as.numeric() %>%
    formatC(width = 3, flag = "0", format = "d") %>%
    paste0(collapse = "", sep = " ")
}

#' Simplify the ID getting process
#' @description Return the override id if there is one, otherwise return the individ_id
#'
#' @param row The row for which the id wishes to be taken
#'
#' @return The relevant id
#' @export
#'
#' @examples
#' \dontrun{
#' # Get the id for the sample at the 17th row
#' id <- get_id(search_data[[17,]])
#' }
get_id <- function(row) {
  # read the override id first
  id <- row$override_id
  # if the override id is NA, use the regular id instead
  id[is.na(id)] <- row$individ_id[is.na(id)]
  id
}

#' Generate Allelematch Profile Plot
#'
#' @param am_dataset A allelematch dataset to examine the optimal mismatch value for
#'
#' @return The plotdata to show the user what the program thinks is the optimal mismatch value
#' @export
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' output$plot <- renderPlot({
#'   generate_allelematch_plot(am_data)
#' })
#' }
generate_allelemtach_profile_plot <- function(am_dataset) {
  # Generate the plot
  allelematch::amUniqueProfile(am_dataset, doPlot = TRUE)
}

#' Handle Multimatch By User
#' @description This function takes
#'
#' @param search_data The search data, a dateframe with indexes, locuses and ids
#' @param multiple_matches A list with the indexes that currently matches to multiples ids in the search_data
#' @param multimatch_index The index of the sample that is to get a specified id
#' @param new_id The new id that the specified id and all entries in the same group will get
#'
#' @return A list with the updated search_data and multiple_matches
#' @export
#'
#' @examples
#' \dontrun{
#' c(search_data, multiple_match) %<-% handle_multimatch
#'            (search_data, multiple_matches, "SEP123", "B31")
#' }
handle_multimatch <- function(search_data, multiple_matches, multimatch_index, new_id) {
  # Remove the sample from the list of samples that have multiple matches, it has been corrected by the user
  multiple_matches <- multiple_matches[multiple_matches != multimatch_index]
  # Remove all duplicated of the specific sample, the order doesnt matter as override id is the important parameter for the id
  # We can remove the one with the correct id and set the override id of another without problem
  search_data <- search_data[!(search_data$index == multimatch_index & duplicated(search_data$index)),]
  # Set the sample (now no duplicates, only one left) override id to be the id specified by the user
  search_data$override_id[search_data$index == multimatch_index] <- new_id
  # Set every sample that is in the same group to have a override id, maybe not necessary but to ensure the order generated by
  # allelelmatch doesnt change and would therefor place the user "controlled" one in a then different group
  search_data$override_id[get_id(search_data) == new_id & !(search_data$index %in% multiple_matches)] <- new_id

  # Return the updated data
  list(search_data, multiple_matches)
}

#' Title
#'
#' @param data The old data, containing all samples that have already been handeled, or been choosen not to be handleh
#' @param new_data The new data, the meta-data for the new sample, containing the same columns as the old data, index, locus, date, gender, etc
#' @param additional_data_columns The names of the columns containing the meta-data for the \code{\link{create_allelematch_dataset}} to be able to create a new am_dataset
#' @param allele_mismatch The mismatch (numbers of alleles that are allowed to differ between individuals) to use for this grouping, user could be intrested in being more strict or more loose, up to them
#'
#' @return Returns only the new data grouped into search_data format, which of them that matched multiple and if some of them were unclassified
#' @export
#'
#' @examples
match_new_data <- function(data, new_data, additional_data_columns, allele_mismatch) {
  # Combine the data under each other, create a big data.frame
  # print(head(data))
  # print(head(new_data))
  combined_data <- dplyr::bind_rows(data[,colnames(data) %in% colnames(new_data)], new_data)
  # Create an amDataset to be able to run the grouping on all of the data
  am_data <- create_allelematch_dataset(data = combined_data, ignore_columns = additional_data_columns)
  # Get the combined data for all samples, every index and group, including which fitted into multiple and which were unclassified
  c(combined_search_data, combined_multiple_matches, combined_unclassified) %<-% create_search_data(data = combined_data,
              am_data = am_data, allele_mismatch = allele_mismatch)
  # Return the data gathered but filter to only keep the entries whos indexes are not in the "old" data. Thereby only passing on the new data but sorted with individ_id, if they matched multiple and if they were unclassified
  list(combined_search_data[!combined_search_data$index %in% data$index,],
              combined_multiple_matches[!combined_multiple_matches %in% data$index],
              combined_unclassified[!combined_unclassified$index %in% data$index])
}
