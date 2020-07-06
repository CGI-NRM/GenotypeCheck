library(dplyr)
# library(tidyselect)
library(readxl)
library(readODS)

load_data <- function(file_path, index_column, locus_columns, individ_column = NA, meta_columns, na_strings = c("NA", "-99", "000"), sheet = 1) {
    if (endsWith(file_path, ".xls") | endsWith(file_path, ".xlsx")) {
        raw_data <- readxl::read_excel(path = file_path, col_names = TRUE, na = na_strings, sheet = sheet)
    } else if (endsWith(file_path, ".ods")) {
        raw_data <- readODS::read_ods(path = file_path, col_names = TRUE, na = na_strings, sheet = sheet)
    } else {
        raw_data <- read.table(file = file_path, header = TRUE, na.strings = na_strings, sep = ",", stringsAsFactors = FALSE)
    }

    if (raw_data %>% select(all_of(index_column)) %>% duplicated() %>% sum() >= 1) {
        warning("The indexes are not a unique identifier.")
    }

    if (is.na(individ_column)) {
        meta_data <- data.frame(raw_data %>% select(all_of(index_column), all_of(meta_columns)), NA)
    } else {
        meta_data <- data.frame(raw_data %>% select(all_of(index_column), all_of(meta_columns), all_of(individ_column)))
    }
    colnames(meta_data) <- c("index", names(meta_columns), "individ")
    rownames(meta_data) <- meta_data$index

    # apply(c(1, 2) <- is this needed? 
    locus_data <- data.frame(raw_data %>% select(all_of(locus_columns))) %>%
        apply(1:2, as.numeric)
    colnames(locus_data) <- c(names(locus_columns))
    rownames(locus_data) <- meta_data$index
    locus_data <- locus_data[,sort(colnames(locus_data))]

    data <- list(multilocus = locus_data, meta = meta_data, locus_column_names = names(locus_columns), meta_column_names = c("index", names(meta_columns), "individ"), multilocus_names = "index")
    data
}

create_new_data <- function(index, multilocus, meta, na_strings = c("NA", "-99", "000")) {
    multilocus[multilocus %in% na_strings] <- NA

    locus_data <- data.frame(as.list(as.numeric(multilocus)))
    colnames(locus_data) <- c(names(multilocus))
    rownames(locus_data) <- c(index)
    locus_data <- locus_data[,sort(colnames(locus_data))]

    meta_data <- data.frame(index, as.list(meta), NA)
    colnames(meta_data) <- c("index", names(meta), "individ")
    rownames(meta_data) <- c(index)

    ndata <- list(multilocus = locus_data, meta = meta_data, locus_column_names = names(multilocus), meta_column_names = c("index", names(meta), "individ"), distances = list(distances = c(NULL), names_type = c(NULL)))
    ndata
}

create_new_data_batch <- function(file_path, index_column, locus_columns, meta_columns, na_strings = c("NA", "-99", "000"), sheet = 1) {
    load_data(file_path = file_path, index_column = index_column, locus_columns = locus_columns, individ_column = NA, meta_columns = meta_columns, na_strings = na_strings, sheet = sheet)
}

sanity_check_new_data <- function(new_data, data) {
    problems <- c()

    range <- data.frame(as.list(apply(data$multilocus, 2, min, na.rm = TRUE))) %>%
        rbind(data.frame(as.list(apply(data$multilocus, 2, max, na.rm = TRUE))))

    colnames(range) <- colnames(data$multilocus)
    rownames(range) <- c("min", "max")

    for (i in seq(nrow(new_data$multilocus))) {
        test_values <- new_data$multilocus[i,]

        names(test_values) <- colnames(new_data$multilocus)
        # Rearrange to match the correct locus with each other
        test_values <- test_values[colnames(range)]
        outside_range <- test_values > range["max",] | test_values < range["min",]
        outside_range[is.na(outside_range)] <- FALSE
        if (any(outside_range, na.rm = TRUE)) {
            problems <- c(problems, paste(new_data$meta$index[i], "had some values outside the expected range. The problematic locuses are:", paste(names(test_values)[outside_range], collapse = ", ")))
        }
    }

    if (length(problems) >= 1) {
        problems <- c("Some values are outside of the range of the rest of the dataset, ensure that this is inteded and that the locuses are in the correct order.", problems)
    }

    for (i in seq(nrow(new_data$multilocus))) {
        if (sum(is.na(new_data$multilocus[i,])) > 4) {
            problems <- c(problems, paste(new_data$meta$index[i], "There are more than 4 points of missing data, this will lead to large uncertenty when matching against the dataset"))
        }
    }

    if (length(problems) == 0) {
        return("No problems were found with the new data.")
    } else {
        return(problems)
    }
}

# calculate_individ_centers <- function(data) {
#     individs <- aggregate(data$multilocus, list(data$meta$individ), mean)
#     rownames(individs) <- individs[,1]
#     individs[,1] <- NULL

#     # Speeds up operations
#     individs <- apply(individs, 1:2, as.integer)

#     list(multilocus = individs, multilocus_names = "individ")
# }

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

    distances <- list()
    combined_data <- rbind(data$multilocus, new_data$multilocus)
    data_rows <- split(combined_data, seq(nrow(combined_data)))

    for (ndata_row in seq(nrow(new_data$multilocus))) {
        distance <- mapply(distance_function, data_rows, split(new_data$multilocus, seq(nrow(new_data$multilocus)))[ndata_row])
        names(distance) <- rownames(combined_data)
        distance <- distance[names(distance) != new_data$meta$index[ndata_row]]
        distances <- append(distances, list(distance))
    }

    names(distances) <- new_data$meta$index

    list(distances = distances, names_type = data$multilocus_names)
}

combine_multilocus <- function(locus) {
    locus[is.na(locus)] <- 0
    locus %>% formatC(width = 3, flag = "0", format = "d") %>%
        paste0(collapse = " ")
}

generate_user_choice_data_frame <- function(possible_matches, new_data, data, ind) {
    individuals <- unique(data$meta[possible_matches[[ind]]$ids, "individ"])
    ids <- data$meta[data$meta$individ %in% individuals, "index"]
    ids <- unique(c(ids, possible_matches[[ind]]$ids))
    ids <- ids[ids != ind]

    df <- data.frame(index = c(ind, ids))

    combined_data <- rbind(new_data$multilocus, data$multilocus)

    multi <- combine_multilocus(new_data$multilocus[ind,])
    multi <- rbind(multi, data.frame(multilocus = apply(combined_data, 1, combine_multilocus)[ids]))

    distance <- c(NA, new_data$distances$distances[[ind]][ids])
    names(distance)[1] <- ind

    indi <- c(NA, data$meta[ids,"individ"])
    names(indi)[1] <- ind
    if (!is.na(data$meta[ind, "individ"])) {
        indi[1] <- data$meta[ind, "individ"]
    }

    df <- cbind(df, multi, distance, indi)
    df <- df[order(df$distance, na.last = FALSE),]
    colnames(df) <- c("index", "multilocus", "locus distance", "individual")
    rownames(df) <- df$index

    df
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# NOT DONE - NEED HANDELING OF MULTIPLE THINGS
generate_threshold_plot <- function(new_data, data) {
    min_dist <- min(new_data$distances$distances, na.rm = TRUE)
    max_dist <- max(new_data$distances$distances, na.rm = TRUE)

    pl <- list(thre = min_dist + (max_dist - min_dist) * seq(0, 1, 0.01))
    pl
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

match_new_data <- function(new_data, threshold) {
    if (is.null(new_data$distances$distances)) {
        warning("The new data needs to get the distances assigned to it, use the 'calculate_new_data_distances' function")
        return(NA)
    }

    possible_matches <- list()
    for (new_ind in new_data$meta$index) {
        possible_matches <- append(possible_matches, list(list(ids = names(new_data$distances$distances[[new_ind]][new_data$distances$distances[[new_ind]] <= threshold]), 
            id_type = new_data$distances$names_type)))
    }
    names(possible_matches) <- new_data$meta$index
    possible_matches
}

extract_one_index_from_batch <- function(batch, index) {
    list(multilocus = batch$multilocus[index,], meta = batch$meta[index,], locus_column_names = batch$locus_column_names, 
        meta_column_names = batch$meta_column_names, multilocus_names = batch$multilocus_names)
}

merge_new_data <- function(new_data, data, new_data_id) {
    if (is.na(new_data_id)) {
        return(list(data = data, success = FALSE))
    }

    if (new_data$meta$index[1] %in% data$meta$index) {
        return(list(data = data, success = FALSE))
    }

    df_multi <- data.frame(as.list(new_data$multilocus))
    rownames(df_multi) <- new_data$meta$index
    colnames(df_multi) <- colnames(data$multilocus)
    data$multilocus <- data$multilocus %>% rbind(df_multi)
    new_data$meta$individ <- new_data_id
    data$meta <- data$meta %>% rbind(new_data$meta)

    list(data = data, success = TRUE)
}

