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
                        sidebarPanel = shiny::sidebarPanel(
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
            title = "Match New Data", value = "match_new_data",
            shiny::fluidRow(
                shiny::column(width = 12,
                    "temp",
                )
            )
        )
    )
)

server <- function(input, output, session) {

    data <- list()
    
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
        print(data)
    })
}

shiny::shinyApp(ui = ui, server = server)
