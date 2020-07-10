#' Workflow example
#'
#' @description Do not run this function, copy it to a file and configure it to your environment. Use the test data bear_data_part1.csv, bear_data_part2.csv and bear_data_all.csv or create your own data. Information on constraints and configuration of new data exists on `https://github.com/CGI-NRM/GenotypeCheck`.
workflow_example <- function() {

    # SETUP LOCUS COLUMNS
    locus_column_names <- c("G10L_1", "G10L_2", "MU05_1", "MU05_2", "MU09_1", "MU09_2", "MU10_1", "MU10_2",
                            "MU23_1", "MU23_2", "MU50_1", "MU50_2", "MU51_1", "MU51_2", "MU59_1", "MU59_2")

    locus_columns <- c("G10L_1", "G10L_2", "MU05_1", "MU05_2", "MU09_1", "MU09_2", "MU10_1", "MU10_2",
                       "MU23_1", "MU23_2", "MU50_1", "MU50_2", "MU51_1", "MU51_2", "MU59_1", "MU59_2")

    names(locus_columns) <- locus_column_names

    data <- load_data(load_raw_data(file_path = "path/to/main/data_file.csv"), index_column = "index", locus_columns = locus_columns, individ_column = "individ",
                      meta_columns = c(date = "date", north = "north", east = "east", gender = "gender", date_changed = "date_changed", confirmed_dead = "confirmed_dead"))

    # Option to test one new data point or load a file with multiple
    nda <- "one"
    # nda <- "mul"

    if (identical(nda, "one")) {
        locus_data <- c("110", "112", "143", "145", "123", "127", "164", "170", "150", "150", "230", "248", "184", "186", "128", "132")
        names(locus_data) <- locus_column_names

        new_data <- create_new_data(index = "SEP123", multilocus = locus_data, meta = c(date = "2020-06-29", north = "7096503", east = "644381", gender = "Hane", confirmed_dead = "No"))
    } else if (identical(nda, "mul")) {
        new_data <- create_new_data_batch(load_raw_data(file_path = "path/to/new/data/to/be/matched.csv"), index_column = "index", locus_columns = locus_columns,
            meta_columns = c(date = "date", north = "north", east = "east", gender = "gender", confirmed_dead = "confirmed_dead"))
    }

    sanity_message <- sanity_check_new_data(new_data = new_data, data = data)
    print(sanity_message)
    # Display sanite message to user

    new_data$distances <- calculate_new_data_distances(new_data = new_data, data = data, distance_function = dist_euclidian)

    # Generate plot to allow user to choose a fitting threshold value
    # generate_threshold_plot(new_data, data)

    threshold <- 10

    possible_matches <- match_new_data(new_data = new_data, threshold = threshold)

    merged_data <- data

    for (ind in new_data$meta$index) {
        new_id <- data$meta[possible_matches[[ind]]$ids[1], "individ"]

        merged_data <- merge_new_data(new_data = extract_one_index_from_batch(batch = new_data, index = ind), data = merged_data, new_data_id = new_id)$data
    }

}
