source("handle_data.R")


# SETUP LOCUS COLUMNS
locus_column_names <- c("G10L - 1", "G10L - 2", "MU05 - 1", "MU05 - 2", "MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2", 
                        "MU23 - 1", "MU23 - 2", "MU50 - 1", "MU50 - 2", "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2")

locus_columns <- c("G10L", "G10L.1", "Mu05", "Mu05.1", "Mu09", "Mu09.1", "Mu10", "Mu10.1",
                   "Mu23", "Mu23.1", "Mu50", "Mu50.1", "Mu51", "Mu51.1", "Mu59", "Mu59.1")

names(locus_columns) <- locus_column_names

# LOAD THE DATA


# -x-x-x-x-x-x-x-x-x-x-x-x-x-
#  - - - - - - - - - - - - - 
# ---------------------------
#
# NOTE: h1 and h2 are just the original file split into two, one with aprox 100 entries and the other with the rest, these have been linked in the mail as they are for testing only
#
# ---------------------------
#  - - - - - - - - - - - - -
# -x-x-x-x-x-x-x-x-x-x-x-x-x-


data <- load_data(file_path = "~/Downloads/AC_allKorr_h1.csv", index_column = "SEP", locus_columns = locus_columns, individ_column = "Individ",
                  meta_columns = c(date = "Funnetdatum", north = "Nord", east = "Ost", gender = "Kon"))

# Option to test one new data point or load a file with multiple
nda <- "one"
# nda <- "mul"

if (identical(nda, "one")) {
    ## CREATE EXAMPLE DATA ONE POINT
    locus_data <- c("110", "112", "143", "145", "123", "127", "164", "170", "150", "150", "230", "248", "184", "186", "128", "132")
    names(locus_data) <- c("MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2", "MU05 - 1", "MU05 - 2", "MU23 - 1", "MU23 - 2", 
                           "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2", "G10L - 1", "G10L - 2", "MU50 - 1", "MU50 - 2")

    new_data <- create_new_data(index = "SEP123", multilocus = locus_data, meta = c(date = "2020-06-29", north = "7096503", east = "644381", gender = "Hane"))

    sanity_message <- sanity_check_new_data(new_data, data)
    print(sanity_message)
    # Display sanite message to user

    # Calculate the center of each individ, this is optional way of doing things, might be better as an individ could expand othervise
    centers <- calculate_individ_centers(data)

    new_data$distances <- calculate_new_data_distances(new_data, data, dist_euclidian)

    # TODO:: Generate plot to allow user to choose a fitting threshold value
    # threshold_plot <- generate_threshold_plot(new_data)

    threshold <- 20

    possible_matches <- match_new_data(new_data, threshold)

    # Check what kind of data possible matches gave us, either indexes of close samples or close individs
    if (identical(possible_matches[[new_data$meta$index]]$id_type, "index")) {
        new_id <- data$meta[possible_matches[[new_data$meta$index]]$ids[3], "individ"]
    } else if (identical(possible_matches[[new_data$meta$index]]$id_type, "individ")) {
        new_id <- possible_matches[[new_data$meta$index]]$ids[3]
    }

    # Display the options to the user, then let them choose

    merged_data <- merge_new_data(new_data, data, new_id)

} else if (identical(nda, "mul")) {
    # Load a file with aprox 100 new data entries
    new_data_batch <- create_new_data_batch("~/Downloads/AC_allKorr_h2.csv", index_column = "SEP", locus_columns = locus_columns, 
        meta_columns = c(date = "Funnetdatum", north = "Nord", east = "Ost", gender = "Kon"))

    # Generates the sanity check, currently only tests if all values are in range and amonut of missing data
    sanity_message <- sanity_check_new_data(new_data_batch, data)
    print(sanity_message)

    # Calculate the center of each individ, this is optional way of doing things, might be better as an individ could expand othervise
    centers <- calculate_individ_centers(data)

    new_data_batch$distances <- calculate_new_data_distances(new_data_batch, centers, dist_euclidian)

    # TODO:: Generate plot to allow user to choose a fitting threshold value - work with batch new
    # threshold_plot <- generate_threshold_plot(new_data)

    threshold <- 20

    possible_matches <- match_new_data(new_data_batch, threshold)

    # We now merge in the data with the "big" data, we use the first match, this does not mean the closest but is a random match within the threshold

    merged_data <- data

    for (ind in new_data_batch$meta$index) {
        # Check what kind of data possible matches gave us, either indexes of close samples or close individs
        if (identical(possible_matches[[ind]]$id_type, "index")) {
            new_id <- data$meta[possible_matches[[ind]]$ids[1], "individ"]
        } else if (identical(possible_matches[[ind]]$id_type, "individ")) {
            new_id <- possible_matches[[ind]]$ids[1]
        }

        # Display the options to the user, then let them choose which id to give

        merged_data <- merge_new_data(extract_one_index_from_batch(new_data_batch, ind), merged_data, new_id)$data
    }
}
