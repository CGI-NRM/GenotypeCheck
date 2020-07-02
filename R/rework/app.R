source("handle_data.R")

locus_column_names <- c("G10L - 1", "G10L - 2", "MU05 - 1", "MU05 - 2", "MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2", 
                        "MU23 - 1", "MU23 - 2", "MU50 - 1", "MU50 - 2", "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2")

ui <- shiny::fluidPage(

    shiny::titlePanel("Match Genotype"),

    shiny::tabsetPanel(
        shiny::tabPanel(
            title = "Load Dataset", value = "load_dataset",
            shiny::fluidRow(
                shiny::column(width = 12,
                    shiny::h3("Dataset"),
                    shiny::sidebarLayout(
                        sidebarPanel = shiny::sidebarPanel(width = 3,
                            shiny::fileInput(inputId = "data_file", "Choose Data File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".xls", ".xlsx", ".ods")),
                            shiny::uiOutput(outputId = "load_data_sheet"),
                            shiny::uiOutput(outputId = "load_data_choice"),
                            shiny::uiOutput(outputId = "load_data_locuses"),
                            shiny::uiOutput(outputId = "load_data_button")
                        ),
                        mainPanel = shiny::mainPanel(
                            DT::dataTableOutput("dataset_table")
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
                                            shiny::radioButtons(inputId = "load_data_type", label = "Load Data Type", choices = c("Single/Manual" = "single", "Multiple/File" = "file"), selected = "single", inline = TRUE),
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
                                                shiny::fileInput(inputId = "new_data_file", "Choose Data File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".xls", ".xlsx", ".ods")),
                                                shiny::uiOutput(outputId = "load_new_data_sheet"),
                                                shiny::uiOutput(outputId = "load_new_data_choice"),
                                                shiny::uiOutput(outputId = "load_new_data_locuses"),
                                                shiny::uiOutput(outputId = "load_new_data_button")
                                            )
                                        ),
                                        mainPanel = shiny::mainPanel(
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
                                    shiny::selectInput(inputId = "distance_function", label = "Distance Function", choices = c("Eucilidian Distance" = "euc"), selected = "euc"),
                                    shiny::actionButton(inputId = "generate_distances", label = "Generate New Data Distances")
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

    data <- list()
    new_data <- list()

    output$load_data_hint <- shiny::renderText("You Need To Load The Dataset Before You Can Match New Data Against It")
    output$load_data_before_match <- shiny::renderText("You Need To Load Some New Data Before You Can Match It Against The Dataset") 

    observeEvent(input$data_file, {
        req(input$data_file)

        output$load_data_choice <- shiny::renderUI({
            if (endsWith(input$data_file$datapath, ".xls") | endsWith(input$data_file$datapath, ".xlsx") | endsWith(input$data_file$datapath, ".ods")) {
                shiny::tagList(
                    shiny::textInput(inputId = "load_data_sheet", label = "Sheet: "),
                    shiny::actionButton(inputId = "load_data_sheet_done", label = "Sheet Entered")
                )
            }
        })
    })

    observeEvent(input$new_data_file, {
        req(input$new_data_file)

        output$load_new_data_choice <- shiny::renderUI({
            if (endsWith(input$new_data_file$datapath, ".xls") | endsWith(input$new_data_file$datapath, ".xlsx") | endsWith(input$new_data_file$datapath, ".ods")) {
                shiny::tagList(
                    shiny::textInput(inputId = "load_new_data_sheet", label = "Sheet: "),
                    shiny::actionButton(inputId = "load_new_data_sheet_done", label = "Sheet Entered")
                )
            }
        })
    })

    observeEvent({input$new_data_file
                  input$load_new_data_sheet_done
                  1
                  }, {
        req(input$new_data_file)

        if (endsWith(input$new_data_file$datapath, ".xls") | endsWith(input$new_data_file$datapath, ".xlsx") | endsWith(input$new_data_file$datapath, ".ods")) {
            req(input$load_new_data_sheet)
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

    observeEvent({input$data_file
                  input$load_data_sheet_done
                  1
                  }, {
        req(input$data_file)

        if (endsWith(input$data_file$datapath, ".xls") | endsWith(input$data_file$datapath, ".xlsx") | endsWith(input$data_file$datapath, ".ods")) {
            req(input$load_data_sheet)
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

    observeEvent(input$load_data_button, {

        locus_columns <- c(input$load_data_choice_locus_1, input$load_data_choice_locus_2, input$load_data_choice_locus_3, input$load_data_choice_locus_4, 
                          input$load_data_choice_locus_5, input$load_data_choice_locus_6, input$load_data_choice_locus_7, input$load_data_choice_locus_8, 
                          input$load_data_choice_locus_9, input$load_data_choice_locus_10, input$load_data_choice_locus_11, input$load_data_choice_locus_12,
                          input$load_data_choice_locus_13, input$load_data_choice_locus_14, input$load_data_choice_locus_15, input$load_data_choice_locus_16)

        names(locus_columns) <- locus_column_names

        data <<- load_data(file_path = input$data_file$datapath, index_column = input$load_data_choice_index_col, locus_columns = locus_columns, individ_column = input$load_data_choice_individ_col,
                           meta_columns = c(date = input$load_data_choice_date_col, north = input$load_data_choice_north_col, east = input$load_data_choice_east_col, gender = input$load_data_choice_gender_col))

        output$dataset_table <- DT::renderDataTable(options = list(pageLength = 30, lengthMenu = c(30, 50, 100, 250)), rownames = FALSE, filter = "top", {
            combined_multilocus <- apply(data$multilocus, 1, combine_multilocus)
            df <- data.frame(multilocus = combined_multilocus)
            rownames(df) <- names(combined_multilocus)
            df <- cbind(index = data$meta$index, df, data$meta[colnames(data$meta) != "index"])
            df
        })

        output$load_data_hint <- shiny::renderText("")

        output$current_gender_indicators_used <- shiny::renderText(paste0("The current genders used are: ", paste(data$meta$gender[!duplicated(data$meta$gender) & !is.na(data$meta$gender)], collapse = ", ")))
    })

    observeEvent(input$load_new_data_button, {

        locus_columns <- c(input$load_new_data_choice_locus_1, input$load_new_data_choice_locus_2, input$load_new_data_choice_locus_3, input$load_new_data_choice_locus_4, 
                          input$load_new_data_choice_locus_5, input$load_new_data_choice_locus_6, input$load_new_data_choice_locus_7, input$load_new_data_choice_locus_8, 
                          input$load_new_data_choice_locus_9, input$load_new_data_choice_locus_10, input$load_new_data_choice_locus_11, input$load_new_data_choice_locus_12,
                          input$load_new_data_choice_locus_13, input$load_new_data_choice_locus_14, input$load_new_data_choice_locus_15, input$load_new_data_choice_locus_16)

        names(locus_columns) <- locus_column_names

        new_data <<- create_new_data_batch(file_path = input$new_data_file$datapath, index_column = input$load_new_data_choice_index_col, locus_columns = locus_columns, meta_columns = c(date = input$load_new_data_choice_date_col, 
            north = input$load_new_data_choice_north_col, east = input$load_new_data_choice_east_col, gender = input$load_new_data_choice_gender_col))

        output$new_data_datatable <- DT::renderDataTable(options = list(pageLength = 30, lengthMenu = c(30, 50, 100, 250)), rownames = FALSE, filter = "top", {
            combined_multilocus <- apply(new_data$multilocus, 1, combine_multilocus)
            df <- data.frame(multilocus = combined_multilocus)
            rownames(df) <- names(combined_multilocus)
            df <- cbind(index = new_data$meta$index, df, new_data$meta[colnames(new_data$meta) != "index"])
            df
        })

        output$load_data_before_match <- shiny::renderText("")
    })

    observeEvent(input$compile_single_new_data, {
        locus_data <- c(input$load_new_locus_1, input$load_new_locus_2, input$load_new_locus_3, input$load_new_locus_4, input$load_new_locus_5, input$load_new_locus_6, 
                     input$load_new_locus_7, input$load_new_locus_8, input$load_new_locus_9, input$load_new_locus_10, input$load_new_locus_11, input$load_new_locus_12, 
                     input$load_new_locus_13, input$load_new_locus_14, input$load_new_locus_15, input$load_new_locus_16)

        names(locus_data) <- locus_column_names
        
        new_data <<- create_new_data(input$load_new_index, multilocus = locus_data, meta = c(date = as.character(input$load_new_date), north = input$load_new_north, east = input$load_new_east, gender = input$load_new_gender))

        output$new_data_datatable <- DT::renderDataTable(options = list(pageLength = 30, lengthMenu = c(30, 50, 100, 250)), rownames = FALSE, filter = "top", {
            combined_multilocus <- apply(new_data$multilocus, 1, combine_multilocus)
            df <- data.frame(multilocus = combined_multilocus)
            rownames(df) <- names(combined_multilocus)
            df <- cbind(index = new_data$meta$index, df, new_data$meta[colnames(new_data$meta) != "index"])
            df
        })

        output$load_data_before_match <- shiny::renderText("")
    })

    observeEvent(input$generate_distances, {
        distance_function <- dist_euclidian
        if (identical(input$distance_function, "euc")) {
            distance_function <- dist_euclidian
        }
        new_data$distances <<- calculate_new_data_distances(new_data, data, dist_euclidian)
    })
}

shiny::shinyApp(ui = ui, server = server)
