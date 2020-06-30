library(dplyr)
library(readxl)
library(readODS)

load_data <- function(file_path, index_column, locus_columns, individ_column, meta_columns, na_strings = c("NA", "-99", "000"), sheet = 1) {
    if (endsWith(file_path, ".xls") | endsWith(file_path, ".xlsx")) {
        raw_data <- readxl::read_excel(path = filepath, col_names = TRUE, na = na_strings, sheet = sheet)
    } else if (endsWith(file_path, ".ods")) {
        raw_data <- readODS::read_ods(path = file_path, col_names = TRUE, na = na_strings, sheet = sheet)
    } else {
        raw_data <- read.table(file = file_path, header = TRUE, na.strings = na_strings, sep = ",")
    }

    if (raw_data %>% select(all_of(index_column)) %>% duplicated() %>% sum() >= 1) {
        warning("The indexes are not a unique identifier.")
    }

    meta_data <- data.frame(raw_data %>% select(all_of(index_column), all_of(meta_columns), all_of(individ_column)))
    colnames(meta_data) <- c("index", names(meta_columns), "individ")
    rownames(meta_data) <- meta_data$index

    locus_data <- data.frame(raw_data %>% select(all_of(locus_columns))) %>%
        apply(c(1, 2), as.numeric)
    colnames(locus_data) <- c(names(locus_columns))
    rownames(locus_data) <- meta_data$index

    data <- list(multilocus = locus_data, meta = meta_data, locus_column_names = names(locus_columns), meta_column_names = c("index", names(meta_columns), "individ"))
    data
}

create_new_data <- function(index, multilocus, meta, na_strings = c("NA", "-99", "000")) {
    multilocus[multilocus %in% na_strings] <- NA

    locus_data <- data.frame(as.list(as.numeric(multilocus)))
    colnames(locus_data) <- c(names(multilocus))
    rownames(locus_data) <- c(index)

    meta_data <- data.frame(index, as.list(meta), NA)
    colnames(meta_data) <- c("index", names(meta), "individ")
    rownames(meta_data) <- c(index)

    ndata <- list(multilocus = locus_data, meta = meta_data, locus_column_names = names(multilocus), meta_column_names = c("index", names(meta), "individ"), distances = c(NULL))
    ndata
}

sanity_check_new_data <- function(new_data, data) {
    range <- data.frame(as.list(apply(data$multilocus, 2, min, na.rm = TRUE))) %>%
        rbind(data.frame(as.list(apply(data$multilocus, 2, max, na.rm = TRUE))))

    colnames(range) <- colnames(data$multilocus)

    test_values <- data.frame(as.list(new_data$multilocus))
    colnames(test_values) <- names(new_data$multilocus)
    range <- range %>% rbind(test_values)

    rownames(range) <- c("min", "max", "current")

    outside_range <- range["current",] > range["max",] | range["current",] < range["min",]

    if (outside_range %>% any()) {
        return(paste("Some values are outside the range of the rest of the dataset, ensure that this is intended and that the locuses are in the correct order. The problematic locuses are:", paste(colnames(range)[outside_range], collapse = ", ")))
    }

    return("No problems were found with the new data.")
}

# A list with two vectors, each containing the locuses of the different samples
# Example:
# multilocus1 <- c(110, 112, 132, 128)
# multilocus2 <- c(96, 108, 118, 112)
# res <- dist_euclidian(list(multilocus1, multilocus2))
dist_euclidian <- function(multilocus1, multilocus2) {
    sqrt(sum((multilocus1 - multilocus2) ^ 2, na.rm = TRUE))
}

# Apply to distances of new_data to ensure this data is not combined with the wrong new_data
# Example:
# new_data$distances <- calculate_new_data_distances(new_data, data, dist_euclidian) 
calculate_new_data_distances <- function(new_data, data, distance_function) {

    distances <- mapply(distance_function, split(data$multilocus, seq(nrow(data$multilocus))), split(new_data$multilocus, 1))
    names(distances) <- rownames(data$multilocus)

    distances
}

match_new_data <- function(new_data, threshold) {
    if (is.null(new_data$distances)) {
        warning("The new data needs to get the distances assigned to it, use the 'calculate_new_data_distances' function")
        return(NULL)
    }

    names(new_data$distances[new_data$distances <= threshold])
}

merge_new_data <- function(new_data, data, new_data_id) {
    data$multilocus <- data$multilocus %>% rbind(new_data$multilocus)
    new_data$meta$individ <- new_data_id
    data$meta <- data$meta %>% rbind(new_data$meta)

    data
}

