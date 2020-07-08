library(dplyr)
library(readxl)
library(readODS)

load_data <- function(file_path, index_column, locus_columns, individ_column = NA, meta_columns, na_strings = c("NA", "-99", "000"), sheet = 1) {
    if (endsWith(file_path, ".xls") | endsWith(file_path, ".xlsx")) {
        raw_data <- readxl::read_excel(path = file_path, col_names = TRUE, na = na_strings, sheet = sheet)
    } else if (endsWith(file_path, ".ods")) {
        raw_data <- readODS::read_ods(path = file_path, col_names = TRUE, na = na_strings, sheet = sheet)
    } else if (endsWith(file_path, ".db")) {
        raw_data <- load_data_sqlite(file_path)
    } else {
        raw_data <- read.table(file = file_path, header = TRUE, na.strings = na_strings, sep = ",", stringsAsFactors = FALSE)
    }

    if (raw_data %>% dplyr::select(dplyr::all_of(index_column)) %>% duplicated() %>% sum() >= 1) {
        warning("The indexes are not a unique identifier.")
    }

    if (is.na(individ_column)) {
        meta_data <- data.frame(raw_data %>% dplyr::select(dplyr::all_of(index_column), dplyr::all_of(meta_columns)), NA)
    } else {
        meta_data <- data.frame(raw_data %>% dplyr::select(dplyr::all_of(index_column), dplyr::all_of(meta_columns), dplyr::all_of(individ_column)))
    }
    colnames(meta_data) <- c("index", names(meta_columns), "individ")
    rownames(meta_data) <- meta_data$index

    # apply(c(1, 2) <- is this needed? 
    locus_data <- data.frame(raw_data %>% dplyr::select(dplyr::all_of(locus_columns))) %>%
        apply(1:2, as.numeric)
    colnames(locus_data) <- c(names(locus_columns))
    rownames(locus_data) <- meta_data$index
    locus_data <- locus_data[,sort(colnames(locus_data))]

    combined_locus_data <- apply(locus_data, 1, combine_multilocus)

    data <- list(multilocus = locus_data, meta = meta_data, combined_locus_data = combined_locus_data, locus_column_names = names(locus_columns), 
        meta_column_names = c("index", names(meta_columns), "individ"), multilocus_names = "index")
    data
}

# TODO : Work on this
load_data_sqlite <- function(file_path, table = "Bears") {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), file_path)
    query <- sprintf("SELECT * FROM %s", table)

    raw_data <- RSQLite::dbGetQuery(db, query)
    RSQLite::dbDisconnect(db)

    raw_data
}

create_new_data <- function(index, multilocus, meta, na_strings = c("NA", "-99", "000", "0")) {
    multilocus[multilocus %in% na_strings] <- NA

    multilocus[multilocus %in% na_strings] <- NA
    locus_data <- data.frame(as.list(as.numeric(multilocus)))
    colnames(locus_data) <- c(names(multilocus))
    rownames(locus_data) <- c(index)
    locus_data <- locus_data[,sort(colnames(locus_data))]

    combined_locus_data <- combine_multilocus(locus_data[1, ])
    names(combined_locus_data) <- index

    meta_data <- data.frame(index, as.list(meta), NA)
    colnames(meta_data) <- c("index", names(meta), "individ")
    rownames(meta_data) <- c(index)

    meta_data$north <- as.numeric(meta_data$north)
    meta_data$east <- as.numeric(meta_data$east)

    ndata <- list(multilocus = locus_data, meta = meta_data, combined_locus_data = combined_locus_data, locus_column_names = names(multilocus), meta_column_names = c("index", names(meta), "individ"), 
        distances = list(distances = c(NULL), names_type = c(NULL)))
    ndata
}

create_new_data_batch <- function(file_path, index_column, locus_columns, meta_columns, na_strings = c("NA", "-99", "000"), sheet = 1) {
    load_data(file_path = file_path, index_column = index_column, locus_columns = locus_columns, individ_column = NA, meta_columns = meta_columns, na_strings = na_strings, sheet = sheet)
}

sanity_check_new_data <- function(new_data, data) {
    problems <- c()

    locus_range <- apply(data$multilocus, 2, range, na.rm = TRUE)

    colnames(locus_range) <- colnames(data$multilocus)
    rownames(locus_range) <- c("min", "max")

    for (i in seq(nrow(new_data$multilocus))) {
        test_values <- new_data$multilocus[i,]

        names(test_values) <- colnames(new_data$multilocus)
        # Rearrange to match the correct locus with each other
        test_values <- test_values[colnames(locus_range)]
        outside_range <- test_values > locus_range["max",] | test_values < locus_range["min",]
        outside_range[is.na(outside_range)] <- FALSE
        if (any(outside_range, na.rm = TRUE)) {
            problems <- c(problems, paste(new_data$meta$index[i], "had some values outside the expected range. The problematic locuses are:", paste(names(test_values)[outside_range], collapse = ", ")))
        }
    }

    if (length(problems) >= 1) {
        problems <- c("Some values are outside the range of the rest of the dataset, ensure that this is inteded and that the locuses are in the correct order.", problems)
    }

    for (i in seq(nrow(new_data$multilocus))) {
        if (sum(is.na(new_data$multilocus[i,])) > 4) {
            problems <- c(problems, paste(new_data$meta$index[i], "There are more than 4 points of missing data, this will lead to large uncertenty when matching against the dataset"))
        }
    }

    for (new_ind in new_data$meta$index) {
        if (new_ind %in% data$meta$index) {
            problems <- c(problems, paste(new_ind, " already exists in the loaded data. Be aware that the program cannot handle this and it may lead to crashes and/or weird behaviour."))
        }
    }

    if (length(problems) == 0) {
        return("No problems were found with the new data.")
    } else {
        return(problems)
    }
}

# A list with two vectors, each containing the locuses of the different samples
# Example:
# multilocus1 <- c(110, 112, 132, 128)
# multilocus2 <- c(96, 108, 118, 112)
# res <- dist_euclidian(list(multilocus1, multilocus2))
dist_euclidian <- function(multilocus1, multilocus2) {
    sqrt(sum((multilocus1 - multilocus2) ^ 2, na.rm = TRUE))
}

dist_manhattan <- function(multilocus1, multilocus2) {
    sum(abs(multilocus1 - multilocus2), na.rm = TRUE)
}

dist_maximum <- function(multilocus1, multilocus2) {
    max(abs(multilocus1 - multilocus2), na.rm = TRUE)
}

dist_num_mismatches <- function(multilocus1, multilocus2) {
    sum(!(multilocus1 == multilocus2))
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

    distances
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

    combined_data <- c(new_data$combined_locus_data, data$combined_locus_data)

    multi <- combined_data[ind]
    multi <- c(multi, combined_data[ids])

    distance <- c(NA, new_data$distances[[ind]][ids])
    names(distance)[1] <- ind

    indi <- c(NA, data$meta[ids,"individ"])
    names(indi)[1] <- ind
    if (!is.na(data$meta[ind, "individ"])) {
        indi[1] <- data$meta[ind, "individ"]
    }

    df <- cbind(df, data.frame(multilocus = multi), distance, indi)
    df <- df[order(df$distance, na.last = FALSE),]
    colnames(df) <- c("index", "multilocus", "locus distance", "individual")
    rownames(df) <- df$index

    df
}

# EASY TO SPEED UP; REMOVE LOPS TODO::: TODO
generate_threshold_plot <- function(new_data, data) {
    min_dist <- min(unlist(new_data$distances))
    # max_dist <- max(unlist(new_data$distances))
    max_dist <- max(unlist(lapply(new_data$meta$index, function(ind) {
        sum(sort(new_data$distances[[ind]])[1:2], na.rm = TRUE)
    })))

    temp_thres <- min_dist + (max_dist - min_dist) * seq(0, 1, 0.01)
    matches <- data.frame(lapply(temp_thres, function(thres) { 
        nums <- data.frame(lapply(new_data$meta$index, function(ind) {
            ids <- names(new_data$distances[[ind]])[new_data$distances[[ind]] <= thres]
            ids <- ids[ids %in% data$meta$index]
            num_matches <- length(unique(data$meta[ids, "individ"]))
            c(num_matches >= 2, num_matches == 0)
        }))

        c(sum(unlist(nums[1,])), sum(unlist(nums[2,])))
    }))

    plot(x = temp_thres, y = matches[1,], xlab = "Threshold", ylab = "Number Of New Samples", col = "red", type = "l", lwd = 2)
    lines(x = temp_thres, y = matches[2,], col = "blue", lwd = 2)
    legend(x = "right", y = (min(temp_thres) + max(temp_thres)) / 2, legend = c("Non-matches", "Multiple Matches"), col = c("blue", "red"), lty = 1, pch = 16)
}

match_new_data <- function(new_data, threshold) {
    if (is.null(new_data$distances)) {
        warning("The new data needs to get the distances assigned to it, use the 'calculate_new_data_distances' function")
        return(NULL)
    }

    possible_matches <- list()
    for (new_ind in new_data$meta$index) {
        possible_matches <- append(possible_matches, list(list(ids = names(new_data$distances[[new_ind]][new_data$distances[[new_ind]] <= threshold]))))
    }
    names(possible_matches) <- new_data$meta$index
    possible_matches
}

extract_one_index_from_batch <- function(batch, index) {
    list(multilocus = batch$multilocus[index,], meta = batch$meta[index,], locus_column_names = batch$locus_column_names, 
        meta_column_names = batch$meta_column_names, multilocus_names = batch$multilocus_names)
}

merge_new_data <- function(new_data, data, new_data_id) {
    if (is.na(new_data_id) | is.null(new_data_id)) {
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
