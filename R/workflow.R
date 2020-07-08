# SETUP LOCUS COLUMNS
locus_column_names <- c("G10L - 1", "G10L - 2", "MU05 - 1", "MU05 - 2", "MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2",
                        "MU23 - 1", "MU23 - 2", "MU50 - 1", "MU50 - 2", "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2")

locus_columns <- c("G10L_1", "G10L_2", "MU05_1", "MU05_2", "MU09_1", "MU09_2", "MU10_1", "MU10_2",
                        "MU23_1", "MU23_2", "MU50_1", "MU50_2", "MU51_1", "MU51_2", "MU59_1", "MU59_2")

names(locus_columns) <- locus_column_names

data <- load_data(load_raw_data(file_path = "~/code/r/GenotypeCheck/data/bear_data_part1.csv"), index_column = "index", locus_columns = locus_columns, individ_column = "individ",
                  meta_columns = c(date = "date", north = "north", east = "east", gender = "gender"))

# Option to test one new data point or load a file with multiple
# nda <- "one"
nda <- "mul"

if (identical(nda, "one")) {
    locus_data <- c("110", "112", "143", "145", "123", "127", "164", "170", "150", "150", "230", "248", "184", "186", "128", "132")
    names(locus_data) <- c("MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2", "MU05 - 1", "MU05 - 2", "MU23 - 1", "MU23 - 2",
                           "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2", "G10L - 1", "G10L - 2", "MU50 - 1", "MU50 - 2")

    new_data <- create_new_data(index = "SEP123", multilocus = locus_data, meta = c(date = "2020-06-29", north = "7096503", east = "644381", gender = "Hane"))
} else if (identical(nda, "mul")) {
    new_data <- create_new_data_batch(load_raw_data(file_path = "~/code/r/GenotypeCheck/data/bear_data_part2.csv"), index_column = "index", locus_columns = locus_columns,
        meta_columns = c(date = "date", north = "north", east = "east", gender = "gender"))
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

