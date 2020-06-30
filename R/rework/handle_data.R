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

    if (raw_data %>% select(index_column) %>% duplicated() %>% sum() >= 1) {
        warning("The indexes are not a unique identifier.")
    }

    meta_data <- data.frame(raw_data %>% select(index_column, all_of(meta_columns), individ_column))
    colnames(meta_data) <- c("index", names(meta_columns), "individ")
    rownames(meta_data) <- meta_data$index

    locus_data <- data.frame(raw_data %>% select(all_of(locus_columns)))
    colnames(locus_data) <- c(names(locus_columns))
    rownames(locus_data) <- meta_data$index

    data <- list(multilocus = locus_data, meta = meta_data, locus_column_names = names(locus_columns), meta_column_names = c("index", names(meta_columns), "individ"))
    
    data
}

create_new_data <- function(index, multilocus, meta, na_strings = c("NA", "-99", "000")) {
    multilocus[multilocus %in% na_strings] <- NA

    locus_data <- data.frame(as.list(multilocus))
    colnames(locus_data) <- c(names(multilocus))
    rownames(locus_data) <- c(index)

    meta_data <- data.frame(index, as.list(meta), NA)
    colnames(meta_data) <- c("index", names(meta), "individ")
    rownames(meta_data) <- c(index)

    ndata <- list(multilocus = locus_data, meta = meta_data, locus_column_names = names(multilocus), meta_column_names = c("index", names(meta), "individ"))
    ndata
}

sanity_check_new_data <- function(new_data, data) {
    range <- data.frame(list(apply(data$multilocus, 2, min, na.rm = TRUE)))
    #, apply(data$multilocus, 2, max, na.rm = TRUE))
    # rownames(range) <- c("min", "max")

    # range <- list(min = apply(data$multilocus, 2, min, na.rm = TRUE), max = apply(data$multilocus, 2, max, na.rm = TRUE))

    range
}

match_new_data_point <- function() {

}
