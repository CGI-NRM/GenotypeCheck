library("shiny")
library("zeallot")
library("GenotypeCheck")

# The standard locus names, mostly for faster testing
locus_names <- c("G10L", "G10L.1", "Mu05", "Mu05.1", "Mu09", "Mu09.1", "Mu10", "Mu10.1",
                 "Mu23", "Mu23.1", "Mu50", "Mu50.1", "Mu51", "Mu51.1", "Mu59", "Mu59.1")

# Define UI
ui <- fluidPage(

  # App title
  titlePanel("Match Genotype"),

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      # Input: Select a file
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),


      # Input: Checkbox if file has header
      checkboxInput("header", "Header", TRUE),

      tags$hr(),
      # Select allele mismatch value
      textInput(inputId = "alleleMismatchValue", label = "Allowed Allele-mismatch", value = "3"),

      h4("Type the column name of the specified columns."),
      h5("If header is deseleted, type the indexes of the columns."),


      # Select Index Column
      textInput(inputId = "indexColumnName", label = "Index Column", value = "SEP"),

      # All user to specify colmn-names/indexes for
      textInput(inputId = "dateColumnName", label = "Date Column", value = "Funnetdatum"),
      textInput(inputId = "northColumnName", label = "North Column", value = "Nord"),
      textInput(inputId = "eastColumnName", label = "East Column", value = "Ost"),
      textInput(inputId = "genderColumnName", label = "Gender Column", value = "Kon"),
      textInput(inputId = "presetIndColumnName", label = "PresetIndividual Column", value = ""),

      # Select Locus Columns
      textInput(inputId = "locusColumnNames", label = "Locus Columns (separated by ',')", value = paste0(locus_names, collapse = "", sep = ",")),

      # Parse Data
      actionButton(inputId = "groupIndividuals", label = "GROUP INDIVIDUALS"),

      tags$hr(),
      tags$hr(),

      # Display some result data to the user, TODO: allow the user to handle these exceptations
      textOutput(outputId = "amtMultipleMatches"),
      textOutput(outputId = "amtUnclassified"),

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      tableOutput("contents")

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  # Define defaults, unnecessary as they get defined by the default values from the ui
  additional_data <- list(date="Funnetdatum", north="Nord", east="Ost", gender="Kon", preset_ind="")

  # Define empty lists for all the data objects
  search_data <- list(index = character(), multilocus = character(), individ_id = character())
  multiple_matches <- list(index = character())
  unclassified <- list(index = character(), multilocus = character())

  # Run allelematch and all GenotypeChecks the surrounding code when the click of the button
  observeEvent(input$groupIndividuals, {
    groupIndividuals()
  })

  groupIndividuals <- function() {
    # Render the table of all sample data
    output$contents <- renderTable({
      req(input$file1)

      # Read the locus data from the ui
      locus_columns <- strsplit(input$locusColumnNames, ",")[[1]]

      # Read all of columns for the additional data from the ui
      additional_data <<- list(date = input$dateColumnName, north = input$northColumnName, east = input$eastColumnName, gender = input$genderColumnName, preset_ind = input$presetIndColumnName)
      # If the user does not specify column they get removed here to not have empty objects later that mess things up
      additional_data <<- additional_data[additional_data != ""]

      index_column <- input$indexColumnName

      # Convert the numbers if header is deselected (and we are handeling columnindexes instead of columnnames)
      if (!input$header) {
        locus_columns <- sapply(locus_columns, as.numeric)
        additional_data <<- sapply(additional_data, as.numeric)
        index_column <- as.numeric(index_column)
      }

      # Unpack the different data returned by our wrapper of allelematch into temp variables
      c(search_data_temp, multiple_matches_temp, unclassified_temp) %<-% create_search_data(input$file1$datapath, index_column, additional_data, locus_columns, as.numeric(input$alleleMismatchValue))

      # Change the session (server) data from the temp data
      search_data <<- search_data_temp
      multiple_matches <<- multiple_matches_temp
      unclassified <<- unclassified_temp

      # Show the indexes, multilocus and individual id data to the user
      return(search_data)
    })

    # Display the amount of problematic data. TODO: Allow user to handle this data and tell the program what to do
    output$amtMultipleMatches <- renderText(
      paste0("There were: ", length(multiple_matches), " samples that matched multiple individuals.")
    )

    output$amtUnclassified <- renderText(
      paste0("There were: ", length(unclassified$index), " samples that were unclassified.")
    )
  }
}

shinyApp(ui = ui, server = server)
