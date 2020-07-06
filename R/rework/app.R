source("handle_data.R")

SWEREF99 <- sp::CRS("+init=epsg:3006")
WGS84 <- sp::CRS("+init=epsg:4326")

locus_column_names <- c("G10L - 1", "G10L - 2", "MU05 - 1", "MU05 - 2", "MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2", 
                        "MU23 - 1", "MU23 - 2", "MU50 - 1", "MU50 - 2", "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2")

ui <- shiny::fluidPage(

    shiny::titlePanel("Match Genotype"),

    shiny::actionButton(inputId = "save", label = "Save Changes"),
    shiny::actionButton(inputId = "export", label = "Export New Individuals"),
    shiny::tags$hr(),

    shiny::tabsetPanel(
        shiny::tabPanel(
            title = "Load Dataset", value = "load_dataset",
            shiny::fluidRow(
                shiny::column(width = 12,
                    shiny::h3("Dataset"),
                    shiny::sidebarLayout(
                        sidebarPanel = shiny::sidebarPanel(width = 2,
                            shiny::fileInput(inputId = "data_file", "Choose Data File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".xls", ".xlsx", ".ods")),
                            shiny::uiOutput(outputId = "load_data_sheet"),
                            shiny::uiOutput(outputId = "load_data_choice"),
                            shiny::uiOutput(outputId = "load_data_locuses"),
                            shiny::uiOutput(outputId = "load_data_button")
                        ),
                        mainPanel = shiny::mainPanel(
                            DT::dataTableOutput(outputId = "dataset_table")
                        )
                    )
                )
            )
        ),
        shiny::tabPanel(
            title = "Match New Data", value = "match_new_data_large",
            shiny::fluidRow(
                shiny::column(width = 12,
                    shiny::h4(shiny::textOutput(outputId = "load_data_hint")),
                    shiny::tabsetPanel(
                        shiny::tabPanel(
                            title = "Load", value = "load_new_data",
                            shiny::fluidRow(
                                shiny::column(width = 12,
                                    shiny::sidebarLayout(
                                        sidebarPanel = shiny::sidebarPanel(width = 3,
                                            shiny::radioButtons(inputId = "load_data_type", label = "Load Data Type", choices = c("Single/Manual" = "single", "Multiple/File" = "file"), 
                                                selected = "single", inline = TRUE),
                                            shiny::conditionalPanel(condition = "input.load_data_type == 'single'",
                                                shiny::textInput(inputId = "load_new_index", label = "Index: "),
                                                shiny::splitLayout(
                                                    shiny::numericInput(inputId = "load_new_locus_1", label = "G10L - 1", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_2", label = "G10L - 2", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_3", label = "MU05 - 1", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_4", label = "MU05 - 2", value = 0, min = 0)
                                                ),
                                                shiny::splitLayout(
                                                    shiny::numericInput(inputId = "load_new_locus_5", label = "MU09 - 1", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_6", label = "MU09 - 2", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_7", label = "MU10 - 1", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_8", label = "MU10 - 2", value = 0, min = 0)
                                                ),
                                                shiny::splitLayout(
                                                    shiny::numericInput(inputId = "load_new_locus_9", label = "MU23 - 1", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_10", label = "MU23 - 2", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_11", label = "MU50 - 1", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_12", label = "MU50 - 2", value = 0, min = 0)
                                                ),
                                                shiny::splitLayout(
                                                    shiny::numericInput(inputId = "load_new_locus_13", label = "MU51 - 1", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_14", label = "MU51 - 2", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_15", label = "MU59 - 1", value = 0, min = 0),
                                                    shiny::numericInput(inputId = "load_new_locus_16", label = "MU59 - 2", value = 0, min = 0)
                                                ),
                                                shiny::numericInput(inputId = "load_new_north", label = "North: ", value = 0, min = 0),
                                                shiny::numericInput(inputId = "load_new_east", label = "East: ", value = 0, min = 0),
                                                shiny::dateInput(inputId = "load_new_date", label = "Date: "),
                                                shiny::textInput(inputId = "load_new_gender", label = "Gender: "),
                                                shiny::textOutput(outputId = "current_gender_indicators_used"),
                                                shiny::tags$hr(),
                                                shiny::actionButton(inputId = "compile_single_new_data", label = "Compile New Data")
                                            ),
                                            shiny::conditionalPanel(condition = "input.load_data_type == 'file'",
                                                shiny::fileInput(inputId = "new_data_file", "Choose Data File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", 
                                                    ".csv", ".xls", ".xlsx", ".ods")),
                                                shiny::uiOutput(outputId = "load_new_data_sheet"),
                                                shiny::uiOutput(outputId = "load_new_data_choice"),
                                                shiny::uiOutput(outputId = "load_new_data_locuses"),
                                                shiny::uiOutput(outputId = "load_new_data_button")
                                            )
                                        ),
                                        mainPanel = shiny::mainPanel(
                                            shiny::h4(shiny::textOutput(outputId = "sanity_check")),
                                            shiny::textOutput(outputId = "sanity_message"),
                                            shiny::tags$hr(),
                                            DT::dataTableOutput(outputId = "new_data_datatable")
                                        )
                                    )
                                )
                            )
                        ),
                        shiny::tabPanel(
                            title = "Match", value = "match_new_data",
                            shiny::fluidRow(
                                shiny::column(width = 12,
                                    shiny::h4(shiny::textOutput(outputId = "load_data_before_match")),
                                    shiny::sidebarLayout(
                                        sidebarPanel = shiny::sidebarPanel(width = 3,
                                            shiny::selectInput(inputId = "distance_function", label = "Distance Function", choices = c("Eucilidian Distance" = "euc"), selected = "euc"),
                                            shiny::actionButton(inputId = "generate_distances", label = "Generate New Data Distances"),
                                            shiny::textOutput(outputId = "distances_done_message"),
                                            shiny::tags$hr(),
                                            shiny::conditionalPanel(condition = "input.generate_threshold_plot > 0",
                                                shiny::plotOutput(outputId = "threshold_plot")
                                            ),
                                            shiny::tags$hr(),
                                            shiny::actionButton(inputId = "generate_threshold_plot", label = "Generate Threshold Plot"),
                                            shiny::tags$hr(),
                                            shiny::numericInput(inputId = "match_threshold", label = "Distance Threshold To Match", value = 0, min = 0),
                                            shiny::actionButton(inputId = "match_new_against_data", label = "Match Against Data"),
                                            shiny::textOutput(outputId = "you_need_to_calculate_distances")
                                        ),
                                        mainPanel = shiny::mainPanel(
                                            shiny::div(id = "show_matches")
                                        )
                                    )
                                )
                            )
                        ),
                        shiny::tabPanel(
                            title = "Merge And View Details", value = "merge_new_data_and_view_details",
                            shiny::fluidRow(
                                shiny::column(width = 12,
                                    shiny::tags$br(),
                                    shiny::h4(shiny::textOutput(outputId = "load_data_and_generate_distances_merge_tab")),
                                    shiny::sidebarLayout(
                                        sidebarPanel = shiny::sidebarPanel(width = 3,
                                            shiny::textOutput(outputId = "number_of_one_matches"),
                                            shiny::tags$br(),
                                            shiny::actionButton(inputId = "merge_one_individ_new_data", label = "Merge The New Data That Only Matched One Individual"),
                                            shiny::textOutput(outputId = "merge_one_return_message"),
                                            shiny::tags$hr(),
                                            shiny::textOutput(outputId = "number_of_zero_matches"),
                                            shiny::tags$br(),
                                            shiny::actionButton(inputId = "merge_zero_distance_data", label = "Merge The Data That Matched With Zero Distance"),
                                            shiny::textOutput(outputId = "merge_zero_return_message"),
                                            shiny::tags$hr(),
                                            shiny::textInput(inputId = "show_details_for_new_data", label = "View Details For New Data: "),
                                            shiny::tags$hr(),
                                            shiny::conditionalPanel(condition = "input.show_details_for_new_data != ''",
                                                shiny::uiOutput(outputId = "merge_new_individ_id")
                                            ),
                                            shiny::actionButton(inputId = "merge_specific", label = "Merge Sample Into Dataset"),
                                            shiny::tags$br(),
                                            shiny::textOutput(outputId = "merge_return_message"),
                                        ),
                                        mainPanel = shiny::mainPanel(
                                            shiny::conditionalPanel(condition = "input.show_details_for_new_data != ''",
                                                shiny::h4(shiny::textOutput(outputId = "merge_info_index")),
                                                DT::dataTableOutput(outputId = "merge_info_data_table"),
                                                leaflet::leafletOutput(outputId = "merge_info_map")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {

    data <- NULL
    new_data <- NULL
    possible_matches <- NULL

    output$load_data_hint <- shiny::renderText("You Need To Load The Dataset Before You Can Match New Data Against It")
    output$load_data_before_match <- shiny::renderText("You Need To Load Some New Data Before You Can Match It Against The Dataset") 
    output$load_data_and_generate_distances_merge_tab <- shiny::renderText("You Need To Load Data And Match New Data Against It Before You Can Use This Tab")

    shiny::observeEvent(input$data_file, {
        shiny::req(input$data_file)

        output$load_data_choice <- shiny::renderUI({
            if (endsWith(input$data_file$datapath, ".xls") | endsWith(input$data_file$datapath, ".xlsx") | endsWith(input$data_file$datapath, ".ods")) {
                shiny::tagList(
                    shiny::textInput(inputId = "load_data_sheet", label = "Sheet: "),
                    shiny::actionButton(inputId = "load_data_sheet_done", label = "Sheet Entered")
                )
            }
        })
    })

    shiny::observeEvent(input$new_data_file, {
        shiny::req(input$new_data_file)

        output$load_new_data_choice <- shiny::renderUI({
            if (endsWith(input$new_data_file$datapath, ".xls") | endsWith(input$new_data_file$datapath, ".xlsx") | endsWith(input$new_data_file$datapath, ".ods")) {
                shiny::tagList(
                    shiny::textInput(inputId = "load_new_data_sheet", label = "Sheet: "),
                    shiny::actionButton(inputId = "load_new_data_sheet_done", label = "Sheet Entered")
                )
            }
        })
    })

    shiny::observeEvent({input$new_data_file
                  input$load_new_data_sheet_done
                  1
                  }, {
        shiny::req(input$new_data_file)

        if (endsWith(input$new_data_file$datapath, ".xls") | endsWith(input$new_data_file$datapath, ".xlsx") | endsWith(input$new_data_file$datapath, ".ods")) {
            shiny::req(input$load_new_data_sheet)
        }

        headers <- load_file_headers(input$new_data_file$datapath, input$load_new_data_sheet)

        output$load_new_data_choice <- shiny::renderUI({
            shiny::tagList(
                shiny::selectInput(inputId = "load_new_data_choice_index_col", label = "Index Column: ", choices = headers, selected = "SEP", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_date_col", label = "Date Column: ", choices = headers, selected = "Funnetdatum", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_gender_col", label = "Gender Column: ", choices = headers, selected = "Kon", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_north_col", label = "North Column: ", choices = headers, selected = "Nord", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_east_col", label = "East Column: ", choices = headers, selected = "Ost", multiple = FALSE)
            )
        })

        output$load_new_data_locuses <- shiny::renderUI({
            shiny::tagList(
                shiny::selectInput(inputId = "load_new_data_choice_locus_1", label = "G10L - 1", choices = headers, selected = "G10L", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_2", label = "G10L - 2", choices = headers, selected = "G10L.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_3", label = "MU05 - 1", choices = headers, selected = "MU05", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_4", label = "MU05 - 2", choices = headers, selected = "MU05.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_5", label = "MU09 - 1", choices = headers, selected = "MU09", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_6", label = "MU09 - 2", choices = headers, selected = "MU09.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_7", label = "MU10 - 1", choices = headers, selected = "MU10", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_8", label = "MU10 - 2", choices = headers, selected = "MU10.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_9", label = "MU23 - 1", choices = headers, selected = "MU23", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_10", label = "MU23 - 2", choices = headers, selected = "MU23.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_11", label = "MU50 - 1", choices = headers, selected = "MU50", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_12", label = "MU50 - 2", choices = headers, selected = "MU50.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_13", label = "MU51 - 1", choices = headers, selected = "MU51", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_14", label = "MU51 - 2", choices = headers, selected = "MU51.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_15", label = "MU59 - 1", choices = headers, selected = "MU59", multiple = FALSE),
                shiny::selectInput(inputId = "load_new_data_choice_locus_16", label = "MU59 - 2", choices = headers, selected = "MU59.1", multiple = FALSE)
            )
        })

        output$load_new_data_button <- shiny::renderUI({
            shiny::actionButton(inputId = "load_new_data_button", label = "Load New Data")
        })
    })

    shiny::observeEvent({input$data_file
                  input$load_data_sheet_done
                  1
                  }, {
        shiny::req(input$data_file)

        if (endsWith(input$data_file$datapath, ".xls") | endsWith(input$data_file$datapath, ".xlsx") | endsWith(input$data_file$datapath, ".ods")) {
            shiny::req(input$load_data_sheet)
        }

        headers <- load_file_headers(input$data_file$datapath, input$load_data_sheet)

        output$load_data_choice <- shiny::renderUI({
            shiny::tagList(
                shiny::selectInput(inputId = "load_data_choice_index_col", label = "Index Column: ", choices = headers, selected = "SEP", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_date_col", label = "Date Column: ", choices = headers, selected = "Funnetdatum", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_gender_col", label = "Gender Column: ", choices = headers, selected = "Kon", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_north_col", label = "North Column: ", choices = headers, selected = "Nord", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_east_col", label = "East Column: ", choices = headers, selected = "Ost", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_individ_col", label = "Individ Column: ", choices = headers, selected = "Individ", multiple = FALSE)
            )
        })

        output$load_data_locuses <- shiny::renderUI({
            shiny::tagList(
                shiny::selectInput(inputId = "load_data_choice_locus_1", label = "G10L - 1", choices = headers, selected = "G10L", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_2", label = "G10L - 2", choices = headers, selected = "G10L.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_3", label = "MU05 - 1", choices = headers, selected = "MU05", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_4", label = "MU05 - 2", choices = headers, selected = "MU05.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_5", label = "MU09 - 1", choices = headers, selected = "MU09", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_6", label = "MU09 - 2", choices = headers, selected = "MU09.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_7", label = "MU10 - 1", choices = headers, selected = "MU10", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_8", label = "MU10 - 2", choices = headers, selected = "MU10.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_9", label = "MU23 - 1", choices = headers, selected = "MU23", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_10", label = "MU23 - 2", choices = headers, selected = "MU23.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_11", label = "MU50 - 1", choices = headers, selected = "MU50", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_12", label = "MU50 - 2", choices = headers, selected = "MU50.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_13", label = "MU51 - 1", choices = headers, selected = "MU51", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_14", label = "MU51 - 2", choices = headers, selected = "MU51.1", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_15", label = "MU59 - 1", choices = headers, selected = "MU59", multiple = FALSE),
                shiny::selectInput(inputId = "load_data_choice_locus_16", label = "MU59 - 2", choices = headers, selected = "MU59.1", multiple = FALSE)
            )
        })

        output$load_data_button <- shiny::renderUI({
            shiny::actionButton(inputId = "load_data_button", label = "Load Data")
        })
    })

    load_file_headers <- function(file_path, sheet) {
        if (endsWith(file_path, ".xls") | endsWith(file_path, ".xlsx")) {
            raw_data <- readxl::read_excel(path = file_path, col_names = TRUE, sheet = sheet) 
        } else if (endsWith(file_path, ".ods")) {
            raw_data <- readODS::read_ods(path = file_path, col_names = TRUE, sheet = sheet)
        } else {
            raw_data <- read.table(file = file_path, header = TRUE, sep = ",")
        }

        colnames(raw_data)
    }

    shiny::observeEvent(input$load_data_button, {
        locus_columns <- c(input$load_data_choice_locus_1, input$load_data_choice_locus_2, input$load_data_choice_locus_3, input$load_data_choice_locus_4, 
                          input$load_data_choice_locus_5, input$load_data_choice_locus_6, input$load_data_choice_locus_7, input$load_data_choice_locus_8, 
                          input$load_data_choice_locus_9, input$load_data_choice_locus_10, input$load_data_choice_locus_11, input$load_data_choice_locus_12,
                          input$load_data_choice_locus_13, input$load_data_choice_locus_14, input$load_data_choice_locus_15, input$load_data_choice_locus_16)

        names(locus_columns) <- locus_column_names

        data <<- load_data(file_path = input$data_file$datapath, index_column = input$load_data_choice_index_col, locus_columns = locus_columns, individ_column = input$load_data_choice_individ_col,
                           meta_columns = c(date = input$load_data_choice_date_col, north = input$load_data_choice_north_col, east = input$load_data_choice_east_col, 
                           gender = input$load_data_choice_gender_col), sheet = input$load_data_sheet)

        update_main_table()
        
        output$load_data_hint <- shiny::renderText("")

        output$current_gender_indicators_used <- shiny::renderText(paste0("The current genders used are: ", paste(data$meta$gender[!duplicated(data$meta$gender) & !is.na(data$meta$gender)], collapse = ", ")))
    })

    update_main_table <- function() {
        output$dataset_table <- DT::renderDataTable(options = list(pageLength = 30, lengthMenu = c(30, 50, 100, 250), scrollX = TRUE), rownames = FALSE, filter = "top", {
            combined_multilocus <- apply(data$multilocus, 1, combine_multilocus)
            df <- data.frame(multilocus = combined_multilocus)
            rownames(df) <- names(combined_multilocus)
            df <- cbind(index = data$meta$index, df, data$meta[colnames(data$meta) != "index"])
            df
        })
    }

    shiny::observeEvent(input$load_new_data_button, {

        locus_columns <- c(input$load_new_data_choice_locus_1, input$load_new_data_choice_locus_2, input$load_new_data_choice_locus_3, input$load_new_data_choice_locus_4, 
                          input$load_new_data_choice_locus_5, input$load_new_data_choice_locus_6, input$load_new_data_choice_locus_7, input$load_new_data_choice_locus_8, 
                          input$load_new_data_choice_locus_9, input$load_new_data_choice_locus_10, input$load_new_data_choice_locus_11, input$load_new_data_choice_locus_12,
                          input$load_new_data_choice_locus_13, input$load_new_data_choice_locus_14, input$load_new_data_choice_locus_15, input$load_new_data_choice_locus_16)

        names(locus_columns) <- locus_column_names

        new_data <<- create_new_data_batch(file_path = input$new_data_file$datapath, index_column = input$load_new_data_choice_index_col, locus_columns = locus_columns, 
            meta_columns = c(date = input$load_new_data_choice_date_col, 
            north = input$load_new_data_choice_north_col, east = input$load_new_data_choice_east_col, gender = input$load_new_data_choice_gender_col), sheet = input$load_new_data_sheet)

        output$new_data_datatable <- DT::renderDataTable(options = list(pageLength = 30, lengthMenu = c(30, 50, 100, 250), scrollX = TRUE), rownames = FALSE, filter = "top", {
            combined_multilocus <- apply(new_data$multilocus, 1, combine_multilocus)
            df <- data.frame(multilocus = combined_multilocus)
            rownames(df) <- names(combined_multilocus)
            df <- cbind(index = new_data$meta$index, df, new_data$meta[colnames(new_data$meta) != "index"])
            df
        })

        output$load_data_before_match <- shiny::renderText("")
        output$sanity_message <- shiny::renderText(paste(sanity_check_new_data(new_data, data), collapse = " : "))
        output$sanity_check <- shiny::renderText("Sanity Check")
        output$distances_done_message <- shiny::renderText("")
    })

    shiny::observeEvent(input$compile_single_new_data, {
        locus_data <- c(input$load_new_locus_1, input$load_new_locus_2, input$load_new_locus_3, input$load_new_locus_4, input$load_new_locus_5, input$load_new_locus_6, 
                     input$load_new_locus_7, input$load_new_locus_8, input$load_new_locus_9, input$load_new_locus_10, input$load_new_locus_11, input$load_new_locus_12, 
                     input$load_new_locus_13, input$load_new_locus_14, input$load_new_locus_15, input$load_new_locus_16)

        names(locus_data) <- locus_column_names
        
        new_data <<- create_new_data(input$load_new_index, multilocus = locus_data, 
            meta = c(date = as.character(input$load_new_date), north = input$load_new_north, east = input$load_new_east, gender = input$load_new_gender))

        output$new_data_datatable <- DT::renderDataTable(options = list(pageLength = 30, lengthMenu = c(30, 50, 100, 250), scrollX = TRUE), rownames = FALSE, filter = "top", {
            combined_multilocus <- apply(new_data$multilocus, 1, combine_multilocus)
            df <- data.frame(multilocus = combined_multilocus)
            rownames(df) <- names(combined_multilocus)
            df <- cbind(index = new_data$meta$index, df, new_data$meta[colnames(new_data$meta) != "index"])
            df
        })

        output$load_data_before_match <- shiny::renderText("")
        output$sanity_message <- shiny::renderText(paste(sanity_check_new_data(new_data, data), collapse = " : "))
        output$sanity_check <- shiny::renderText("Sanity Check")
        output$distances_done_message <- shiny::renderText("")
    })

    shiny::observeEvent(input$generate_distances, {
        distance_function <- dist_euclidian
        if (identical(input$distance_function, "euc")) {
            distance_function <- dist_euclidian
        }
        new_data$distances <<- calculate_new_data_distances(new_data, data, dist_euclidian)
        output$distances_done_message <- shiny::renderText("Distances Calculated")
    })

    shiny::observeEvent(input$generate_threshold_plot, {
        shiny::req(data)
        shiny::req(new_data)
        shiny::req(new_data$distances$distances)

        output$threshold_plot <- shiny::renderPlot({
            generate_threshold_plot(new_data, data)
        })
    })

    shiny::observeEvent(input$match_new_against_data, {
        if (is.null(data) | is.null(new_data)) {
            output$you_need_to_calculate_distances <- shiny::renderText("You need to load a dataset and some new data first.")
        }
        shiny::req(data)
        shiny::req(new_data)
        if (is.null(new_data$distances)) {
            output$you_need_to_calculate_distances <- shiny::renderText("You need to calculate the distances before you can match the new data.")
        }
        shiny::req(new_data$distances)

        output$you_need_to_calculate_distances <- shiny::renderText("")

        possible_matches <<- match_new_data(new_data, input$match_threshold)

        lapply(new_data$meta$index, function(ind) {
            if (length(possible_matches[[ind]]$ids) <= 1) {
                shiny::insertUI(selector = "#show_matches", where = "afterEnd", ui = shiny::h5("No matches were found in the dataset within the selected threshold. 
                    This could be a new individual or the threshold could be to low."))
            } else {
                shiny::insertUI(selector = "#show_matches", where = "afterEnd", ui = DT::dataTableOutput(outputId = paste0("SHOW_", ind)))
                output[[paste0("SHOW_", ind)]] <- DT::renderDataTable(options = list(scrollX = TRUE, dom = "ltip"), rownames = FALSE, {
                    generate_user_choice_data_frame(possible_matches, new_data, data, ind)
                })
            }
            shiny::insertUI(selector = "#show_matches", where = "afterEnd", ui = shiny::h4(paste0("Showing Matches For: ", ind)))
            shiny::insertUI(selector = "#show_matches", where = "afterEnd", ui = shiny::tags$hr())
        })

        output$load_data_and_generate_distances_merge_tab <- shiny::renderText("")
        output$number_of_one_matches <- shiny::renderText(paste0("There are ", sum(unlist(lapply(possible_matches, function(x) { length(unique(data$meta[x$ids, "individ"])) == 1 }))), 
            " new data-points that matched only one individual in the original data."))
        output$number_of_zero_matches <- shiny::renderText(paste0("There are ", sum(unlist(lapply(new_data$distances$distances, function(x) { min(x) == 0 }))), 
            " new data-points that matched an existing sample from the original data with zero distance."))
    })

    shiny::observeEvent(input$show_details_for_new_data, {
        shiny::req(input$show_details_for_new_data)
        if (!(input$show_details_for_new_data %in% new_data$meta$index)) {
            return()
        }

        ind <- input$show_details_for_new_data

        output$merge_info_index <- shiny::renderText(paste0("Showing Details for: ", ind))
        output$merge_info_data_table <- DT::renderDataTable(options = list(scrollX = TRUE), rownames = FALSE, {
            generate_user_choice_data_frame(possible_matches, new_data, data, ind)
        })

        output$merge_info_map <- leaflet::renderLeaflet({

            individuals <- unique(data$meta[possible_matches[[ind]]$ids, "individ"])
            ids <- data$meta[data$meta$individ %in% individuals, "index"]
            ids <- ids[ids != ind]

            coords <- list(lng = c(new_data$meta[ind, "east"], data$meta[ids, "east"]), lat = c(new_data$meta[ind, "north"], data$meta[ids, "north"]))
            p1 <- sp::SpatialPoints(coords = coords, proj4string = SWEREF99)
            p2 <- sp::coordinates(sp::spTransform(p1, WGS84))

            dates <- c(new_data$meta[ind, "date"], data$meta[ids, "date"])
            individs <- c("CURRENT", data$meta[ids, "individ"])
            labels_with_br <- paste0("Index: ", c(ind, ids), "<br>", " Date: ", dates, "<br>", " Individual: ", individs)
            labels <- paste0("Index: ", c(ind, ids), " Date: ", dates, " Individual: ", individs)

            leaflet::leaflet() %>%
                leaflet::addProviderTiles(provider = leaflet::providers$OpenStreetMap,
                                          options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
                leaflet::addPopups(lng = p2[,"lng"], lat = p2[,"lat"], popup = individs, options = leaflet::popupOptions(closeButton = TRUE)) %>%
                leaflet::addMarkers(lng = p2[,"lng"], lat = p2[,"lat"], label = labels, popup = labels_with_br)
        })

        output$merge_new_individ_id <- shiny::renderUI({
            choices <- c(unique(data$meta[possible_matches[[ind]]$ids, "individ"]))
            choices <- choices[!is.na(choices)]
            combined_data <- rbind(data$meta, new_data$meta)
            nrm_ids <- combined_data$individ[startsWith(combined_data$individ, "NRM_")]
            if (sum(!is.na(nrm_ids)) == 0) {
                max_nrm_num_id <- 0
            } else {
                max_nrm_num_id <- max(as.numeric(gsub("^.*?_", "", nrm_ids)), na.rm = TRUE)
            }
            choices <- c(choices, paste0("NRM_", max_nrm_num_id + 1))
            shiny::selectInput(inputId = "merge_new_individ_id_select", label = "New Individ-Id", choices = choices)
        })
    })

    shiny::observeEvent(input$merge_one_individ_new_data, {
        merged_data <<- data

        one_individ_match <- unlist(lapply(new_data$meta$index, function(ind) {
            individs <- unique(data$meta[possible_matches[[ind]]$ids, "individ"])
            individs <- individs[!is.na(individs)]
            length(individs) == 1
        }))

        lapply(new_data$meta$index[one_individ_match], function (ind) {
            individs <- unique(data$meta[possible_matches[[ind]]$ids, "individ"])
            new_individ <- individs[!is.na(individs)][1]

            merged_data <<- merge_new_data(extract_one_index_from_batch(new_data, ind), merged_data, new_individ)$data
        })

        data <<- merged_data

        lapply(new_data$meta$index, function(ind) {
            output[[paste0("SHOW_", ind)]] <- DT::renderDataTable(options = list(scrollX = TRUE, dom = "ltip"), rownames = FALSE, {
                df <- generate_user_choice_data_frame(possible_matches, new_data, data, ind)
                df
            })
        })

        update_main_table()

        output$merge_one_return_message <- shiny::renderText("Done")
    })

    shiny::observeEvent(input$merge_zero_distance_data, {
        merged_data <<- data

        have_zero <- unlist(lapply(new_data$distances$distances, function(x) { sort(x)[1] == 0 }))
        lapply(new_data$meta$index[have_zero], function(ind) {
            indexes <- names(new_data$distances$distances[[ind]][new_data$distances$distances[[ind]] == 0])
            new_individ <- data$meta[indexes, "individ"] 
            new_individ <- new_individ[!is.na(new_individ)][1]

            merged_data <<- merge_new_data(extract_one_index_from_batch(new_data, ind), merged_data, new_individ)$data
        })

        data <<- merged_data

        lapply(new_data$meta$index, function(ind) {
            output[[paste0("SHOW_", ind)]] <- DT::renderDataTable(options = list(scrollX = TRUE, dom = "ltip"), rownames = FALSE, {
                df <- generate_user_choice_data_frame(possible_matches, new_data, data, ind)
                df
            })
        })

        update_main_table()

        output$merge_zero_return_message <- shiny::renderText("Done")
    })

    shiny::observeEvent(input$merge_new_individ_id, {
        if (!identical(input$merge_new_individ_id_select, "") & !(input$merge_new_individ_id_select %in% data$meta$individ)) {
            output$merge_return_message <- shiny::renderText("The new inidivid-id was not found among the existing individuals. This will create a new individ, make sure this is what you want to do.")
        } else {
            output$merge_return_message <- shiny::renderText("")
        }
    })

    shiny::observeEvent(input$merge_specific, {
        shiny::req(input$show_details_for_new_data)
        if (!(input$show_details_for_new_data %in% new_data$meta$index)) {
            output$merge_return_message <- shiny::renderText("The specified index is not found in the new data.")
            return()
        }
        shiny::req(input$merge_new_individ_id_select)

        ind <- input$show_details_for_new_data

        merged_data <<- merge_new_data(extract_one_index_from_batch(new_data, input$show_details_for_new_data), data, input$merge_new_individ_id_select)
        data <<- merged_data$data

        lapply(rev(new_data$meta$index), function(ind) {
        # lapply(new_data$meta$index[data$meta[new_data$meta$index, "individ"] %in% data$meta[ind, "individ"]], function (ind) {
            output[[paste0("SHOW_", ind)]] <- DT::renderDataTable(options = list(scrollX = TRUE, dom = "ltip"), rownames = FALSE, {
                df <- generate_user_choice_data_frame(possible_matches, new_data, data, ind)
                df
            })
        })


        if (any(merged_data$success)) {
            output$merge_return_message <- shiny::renderText("New data successfully merged with data.")
        } else {
            output$merge_return_message <- shiny::renderText("There was an error trying to merge the new data.")
        }

        update_main_table()
        shiny::updateTextInput(session, inputId = "show_details_for_new_data", value = "")
    })

    shiny::observeEvent(input$save, {
        system(sprintf("cp %s %s", input$data_file$datapath, paste0("~/Downloads/backup_", stringr::str_replace_all(format(Sys.time(), format = "", tz = ""), "[: -]", "_"), "_", input$data_file$name)))

        write.csv(
            x = cbind(data$meta, data$multilocus),
            file = paste0("~/Downloads/", input$data_file$name),
            row.names = FALSE, quote = FALSE
        )
    })

    shiny::observeEvent(input$export, {
        write.csv(
            x = cbind(data$meta[new_data$meta$index %in% data$meta$index,], new_data$multilocus[new_data$meta$index %in% data$meta$index,]),
            file = paste0("~/Downloads/data_export_", stringr::str_replace_all(format(Sys.time(), format = "", tz = ""), "[: -]", "_"), "_", input$data_file$name),
            row.names = FALSE, quote = FALSE
        )
    })
}

shiny::shinyApp(ui = ui, server = server)

