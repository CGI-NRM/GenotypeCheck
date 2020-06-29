library(shiny)
library(DT)
library(zeallot)
library(leaflet)
library(rgdal)
#library(GenotypeCheck)
#source("import_data.R")

# The standard locus names, mostly for faster testing
locus_names <- c("G10L", "G10L.1", "Mu05", "Mu05.1", "Mu09", "Mu09.1", "Mu10", "Mu10.1",
                 "Mu23", "Mu23.1", "Mu50", "Mu50.1", "Mu51", "Mu51.1", "Mu59", "Mu59.1")

# Define coordinates system to convert from SWEREF99 (which the data is in) to WGS84 to render with leaflet
SWEREF99 <- CRS("+init=epsg:3006")
RT90 <- CRS("+init=epsg:3021")
WGS84 <- CRS("+init=epsg:4326")
UTM32N <- CRS("+init=epsg:32632")

# Define UI
ui <- shiny::fluidPage(

  # App title
  shiny::titlePanel("Match Genotype"),

  # Sidebar layout with input and output definitions
  shiny::sidebarLayout(

    # Sidebar panel for inputs
    shiny::sidebarPanel(width = 3,

      # Input: Select a file
      shiny::fileInput("file1", "Choose Data File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xls", ".xlsx", ".ods")),

      # Input: Checkbox if file has header
      shiny::checkboxInput("header", "Header", TRUE),

      # Input: The name of the sheet to be loaded
      shiny::textInput(inputId = "sheet", label = "Sheet name"),

      shiny::tags$hr(),
      # Select allele mismatch value
      shiny::numericInput(inputId = "alleleMismatchValue", label = "Allowed Allele-mismatch", value = 3, min = 0, step = 1),
      # If the user asks for the plot, generate it and show it
      shiny::conditionalPanel(condition = "input.generateAllelematchProfile >= 1",
        plotOutput(outputId = "allelematchProfilePlot"),
      ),
      shiny::actionButton(inputId = "generateAllelematchProfile", "Generate Mismatch Plot"),
      shiny::h4("Type the column name of the specified columns."),
      shiny::h5("If header is deseleted, type the indexes of the columns."),


      # Select Index Column
      shiny::textInput(inputId = "indexColumnName", label = "Index Column", value = "SEP"),

      # All user to specify colmn-names/indexes for
      shiny::textInput(inputId = "dateColumnName", label = "Date Column", value = "Funnetdatum"),
      shiny::textInput(inputId = "northColumnName", label = "North Column", value = "Nord"),
      shiny::textInput(inputId = "eastColumnName", label = "East Column", value = "Ost"),
      shiny::textInput(inputId = "genderColumnName", label = "Gender Column", value = "Kon"),
      shiny::textInput(inputId = "presetIndColumnName", label = "PresetIndividual Column", value = "Individ"),

      # Select Locus Columns
      shiny::textInput(inputId = "locusColumnNames", label = "Locus Columns (separated by ',')", value = paste0(locus_names, collapse = "", sep = ",")),

      # Parse Data
      shiny::actionButton(inputId = "groupIndividuals", label = "GROUP INDIVIDUALS"),

      shiny::tags$hr(),

      # Display some result data to the user
      shiny::textOutput(outputId = "amtMultipleMatches"),
      shiny::textOutput(outputId = "amtUnclassified"),
    ),

    # Main panel for displaying outputs
    mainPanel = shiny::mainPanel(
      shiny::tabsetPanel(id = "rightOperationTabset",
        shiny::tabPanel(title = "Dataset", value = "dataset",
              # Output: Data file
              DT::dataTableOutput("contents")
                        ),
        shiny::tabPanel(title = "Handle Multiple Matches And Unclassified Samples", value = "handle_multiple_matches_and_unclassified_samples",
              # Allow the user to select and handle all of the multiple matches that occured
              shiny::div(shiny::h4("Handle Multiple Matches")),

              shiny::numericInput(inputId = "multipleMatchIndex", label = "View Details (Index of Multiple Matched Sample): ", value = 0, min = 1, step = 1),
              DT::dataTableOutput("multipleMatchesTable"),

              # TODO:: Allow the user to handle these (similar to matching new data)
              shiny::div(shiny::h4("Handle Unclassified Samples")),

              DT::dataTableOutput("unclassifiedTable"),

              # Panel for handeling multiple matched data, will probably be similar to the panel for matching new data
              shiny::conditionalPanel("input.multipleMatchIndex != ''",
                               # "Showing Multimatch data for SEP123123" <-- Example
                               shiny::h4(textOutput("multiMatchDataFor")),
                               shiny::h5("The sample matched the following individuals: "),
                               # Desired: map beside data, now it jumps down because of size, not that important
                               shiny::sidebarLayout(
                                 sidebarPanel = shiny::sidebarPanel(width = 9,
                                  # Render the ones that were similar
                                   DT::dataTableOutput("multipleMatchedSingle"),
                                 ),
                                 mainPanel = shiny::mainPanel(
                                   # render the map for the user to have all data when deciding which individual to add it to
                                   leafletOutput(outputId = "multiMatchMap"),
                                 ),
                               ),
                               # User choose and add to a group of samples/individual - information
                               h5("If this ID is one of the listed above the sample will be added to that group of sample/individual, if not, the sample will create a new individual IF the new ID does not already exist, make sure it is unique if that is the desired action."),
                               # Text box to type new id, either create new group or create a override id for every sample in that group
                               shiny::textInput(inputId = "multipleMatchFix", label = "Set ID/Individual to group: "),
                               shiny::actionButton(inputId = "multipleMatchFixConfirm", label = "Confirm/Save to data"),
                               shiny::tags$hr(),
                               ),
                  ),
        # Tab for loading and testing new data
          shiny::tabPanel(title = "Test New Data", value = "test_new_data",
                          # Choose wheter to write a single sample or load a file with multiple
                          shiny::radioButtons(inputId = "new_data_mode", label = "Add/Test new data by: ",
                                              choices = c(Single = "single", Multiple = "multiple"), selected = "single"),
                          # Show options for loading a single data point
                          shiny::conditionalPanel(condition = "input.new_data_mode == 'single'",
                                                  shiny::textInput(inputId = "new_data_index", label = "Index: "),
                                                  shiny::dateInput(inputId = "new_data_date", label = "Date: "),
                                                  shiny::textInput(inputId = "new_data_north", label = "North: "),
                                                  shiny::textInput(inputId = "new_data_east", label = "East: "),
                                                  shiny::h5(shiny::textOutput(outputId = "currentGenderStyle")),
                                                  shiny::textInput(inputId = "new_data_gender", label = "Gender: "),
                                                  shiny::h5("Make sure the order is the same as the rest of the data, in alignment with the order given to the right."),
                                                  shiny::textInput(inputId = "new_data_locus", label = "Locus (separated by ' '):")
                                           ),
                          # If multiple is choosen, open those options
                          shiny::conditionalPanel(condition = "input.new_data_mode == 'multiple'",

                                            shiny::h5("Using column-names from the panel on the left, make sure they match the given file."),
                                            # Allow user to load a file with the data
                                            shiny::fileInput(inputId = "new_data_file", label = "Choose Data File",
                                                  multiple = FALSE,
                                                  accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv", ".xls", ".xlsx", ".ods")),
                                           ),
                          shiny::tags$hr(),
                          # How many mismatchs to allow when mathcing new data to the rest of the dataset
                          shiny::numericInput(inputId = "new_data_mismatch", label = "Mismatch For New Data", value = 3, min = 0, step = 1),
                          # Load the file or strings into data and compare with the dataset
                          shiny::actionButton(inputId = "search_new_data", label = "Match New Data To Dataset"),
                          shiny::tags$hr(),
                          )
      )
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

  data <- NA
  am_data <- NA

  # Run allelematch and all GenotypeChecks the surrounding code when the click of the button
  observeEvent(input$groupIndividuals, {
    groupIndividuals()
    update_output_preprocess_data()
  })

  groupIndividuals <- function() {
    req(input$file1)
    req(as.numeric(input$alleleMismatchValue))

    # Reload the data incase the colmn-names have changed
    c(data_temp, am_data_temp) %<-% load_main_data(input$file1$datapath)
    data <<- data_temp
    am_data <<- am_data_temp

    # Unpack the different data returned by our wrapper of allelematch into temp variables
    c(search_data_temp, multiple_matches_temp, unclassified_temp) %<-% GenotypeCheck::create_search_data(data, am_data, as.numeric(input$alleleMismatchValue))

    # Change the session (server) data from the temp data
    search_data <<- search_data_temp
    multiple_matches <<- multiple_matches_temp
    unclassified <<- unclassified_temp
  }

  # load the main data file
  load_main_data <- function(file) {
    # Read the locus data from the ui
    locus_columns <- strsplit(input$locusColumnNames, ",")[[1]]

    # Read all of columns for the additional data from the ui
    additional_data <<- list(date = input$dateColumnName, north = input$northColumnName, east = input$eastColumnName, gender = input$genderColumnName, preset_ind = input$presetIndColumnName)
    # If the user does not specify column they get removed here to not have empty objects
    additional_data <<- additional_data[additional_data != ""]

    index_column <- input$indexColumnName

    sheet <- input$sheet

    # Convert the numbers if header is deselected (and we are handeling columnindexes instead of columnnames)
    if (!input$header) {
      locus_columns <- sapply(locus_columns, as.numeric)
      additional_data <<- sapply(additional_data, as.numeric)
      index_column <- as.numeric(index_column)
    }

    # Load the data, this will be the meta data
    data <- GenotypeCheck::import_data(file, index_column = index_column, additional_data = additional_data, locus_names = locus_columns, sheet = sheet)

    # Create allaematch dataset, ignore some meta-data as it can be read from the "data" above, the index (SEP) is the same
    am_data <- GenotypeCheck::create_allelematch_dataset(data, ignore_columns = names(additional_data))

    list(data, am_data)
  }

  update_output_preprocess_data <- function() {
    # Render the table of all sample data
    output$contents <- DT::renderDataTable(options = list(pageLength = 50, lengthMenu = c(10, 25, 50, 100, 250)), filter = "top",
                                           {
      # Show the indexes, multilocus and individual id data to the user
      search_data
    })

    # Display the amount of problematic data. TODO: Allow user to handle this data and tell the program what to do
    output$amtMultipleMatches <- renderText(
      paste0("There were: ", length(multiple_matches), " samples that matched multiple individuals.")
    )

    # Render only the ones that matched multiple so the user can choose one
    output$multipleMatchesTable <- DT::renderDataTable({
      number_indexes <- 1:length(multiple_matches)
      # If there are none, avoid an vector that look like c(1, 0)
      if (length(multiple_matches) == 0) {
        number_indexes <- c()
      }
      # Add id for the user to choose and view the details for one
      df <- data.frame(list(multipleMatchIndex = number_indexes))
      rownames(df) <- multiple_matches
      df
    })

    # Show amount of unclassified samples in text
    output$amtUnclassified <- renderText(
      paste0("There were: ", length(unclassified$index), " samples that were unclassified.")
    )

    output$currentGenderStyle <- renderText(
      paste("The datasets gender-style is: ", paste0(data$gender[!duplicated(data$gender)], sep = ", ", collapse = ""))
    )
  }

  # Observe when user types an index to view details
  observeEvent(input$multipleMatchIndex, {
    # Make sure the data is generated/button pressed, and that the user did not delete the index
    req(input$multipleMatchIndex, input$groupIndividuals)

    # If the index is parsable, continue
    if (!is.na(as.numeric(input$multipleMatchIndex)) & as.numeric(input$multipleMatchIndex) <= length(multiple_matches)) {
      # Figure out what "big" index we are handeling, SEP index
      showing_index <- multiple_matches[[as.numeric(input$multipleMatchIndex)]]

      # Create a filter c(TRUE, TRUE, FALSE, FALSE, FALSE) to select only the ones that are in the group of samples that the multimatched sample was in
      # Get the multiple instaces of the sample, here in different groups
      search_data_filter <- search_data$index == showing_index
      # Get the id of the groups that the sample is a port of
      ids <- get_id(search_data[search_data_filter,])
      # Expand the filter to include everything with those id:s aswell
      search_data_filter <- search_data_filter | get_id(search_data) %in% ids

      # Show information to user, which sample (SEP index)
      output$multiMatchDataFor <- renderText(paste0("Showing Data For ", showing_index))

      # Render all samples that are part of the process to choose
      output$multipleMatchedSingle <- DT::renderDataTable({
        DT::datatable(search_data[search_data_filter,]) %>%
          # Highlighting the entries that are the indexes we are handling
          DT::formatStyle(columns = "index", target = "row", backgroundColor = DT::styleEqual(levels = c(showing_index), values = c("yellow"), default = NULL))
      })

      # If the user have specified map coordinates, continue
      if (!is.null(additional_data$north) & !is.null(additional_data$east)) {
        # Extract the long and lat from the data
        # The data have user defined names for the columns, hence the pull with the additional_data which is the link between the user
        # defined column-names and to us known names (north, east etc)
        coords <- list(lng = data[search_data$index[search_data_filter],"east"],
                       lat = data[search_data$index[search_data_filter],"north"])
        # Create a spatialpointsdataframe with the coordinates, empty meta-data and the input GPS system
        p1 <- SpatialPointsDataFrame(coords, data = data.frame(list(temp = rep(NA, length(search_data_filter[search_data_filter == TRUE])))), proj4string = SWEREF99)
        # Convert to WGS84 to render to the map and extract the coordinates
        p2 <- spTransform(p1, WGS84) %>%
          coordinates()

        # Render to the map
        output$multiMatchMap <- leaflet::renderLeaflet({
          # Get the id:s for all points to be renderer
          ids <- get_id(search_data[search_data_filter,])
          # Change the id for the ones that have multiple ids. They have the same location and will be placed on top of each other
          ids[search_data$index[search_data_filter] %in% multiple_matches] <- "Multiple"
          # Read the dates for the relevant samples from the meta-data
          dates <- data[search_data$index[search_data_filter],"date"]
          # Create all labels with the information we want to display
          label <- paste0("Index: ", search_data$index[search_data_filter], " ID: ", ids, " Date: ", dates)

          # Render the map with leaflet and att markers
          leaflet::leaflet() %>%
            # Get the map from openstreetmap
            addProviderTiles(provider = leaflet::providers$OpenStreetMap,
                             options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
            # Add popups (take alot of space but gives all information, can be closed) - uses the label with the information created earlier
            leaflet::addPopups(lng = p2[,"lng"], lat = p2[,"lat"], popup = label, options = popupOptions(closeButton = TRUE)) %>%
            # Add markers that show the information both on click and on hover, cant disapear
            leaflet::addMarkers(lng = p2[,"lng"], lat = p2[,"lat"], label = label, popup = label)
        })
      }
    }
  })

  # Observe if user clicks the button to change the override id of the sample in question (that have been multimatched)
  shiny::observeEvent(input$multipleMatchFixConfirm, {
    # Make sure the user has written a new id
    req(input$multipleMatchFix)

    # Figure out the search_data index, (SEP index)
    showing_index <- multiple_matches[[as.numeric(input$multipleMatchIndex)]]

    # Make the change in the data structures
    c(search_data_temp, multimatch_data_temp) %<-% GenotypeCheck::handle_multimatch(search_data, multiple_matches, showing_index, input$multipleMatchFix)
    search_data <<- search_data_temp
    multiple_matches <<- multimatch_data_temp

    # Update the visual information, the big table and the count of multimatches
    update_output_preprocess_data()
    # Reset the chosen multimatch index, the conditional panel will disapear until the user chooses a new sample that have been multimatched to handle
    updateTextInput(session, "multipleMatchFix", value = "")
    updateTextInput(session, "multipleMatchIndex", value = "")
  })

  shiny::observeEvent(input$generateAllelematchProfile, {
    req(input$file1)

    # Reload the data incase teh colmnnames have changed
    c(data_temp, am_data_temp) %<-% load_main_data(input$file1$datapath)
    data <<- data_temp
    am_data <<- am_data_temp

    # Render the plot to the ui
    output$allelematchProfilePlot <- shiny::renderPlot({
      GenotypeCheck::generate_allelemtach_profile_plot(am_data)
    })
  })

  shiny::observeEvent(input$search_new_data, {
    if (input$new_data_mode == "single") {
      # Make sure the essential data is given, the rest is meta-data and it would be annoying if it were required
      req(input$new_data_index)
      req(input$new_data_locus)
      req(input$new_data_mismatch)

      # Split the locus string and name the columns accordingly in the same order that have been given in the panel to the lift
      # Order is important here
      multilocus <- strsplit(input$new_data_locus, " ")[[1]]

      # Name the locus according to the preset names, important to have the same order
      locus_names_known <- c("G10L - 1", "G10L - 2", "MU05 - 1", "MU05 - 2", "MU09 - 1", "MU09 - 2", "MU10 - 1", "MU10 - 2", "MU23 - 1", "MU23 - 2", "MU50 - 1", "MU50 - 2", "MU51 - 1", "MU51 - 2", "MU59 - 1", "MU59 - 2")
      multilocus_df_named <- data.frame(as.list(multilocus))
      colnames(multilocus_df_named) <- locus_names_known

      # Create the new data, a dataframe with one row
      new_data <- data.frame(list(index = input$new_data_index), date = input$new_data_date, north = input$new_data_north,
                             east = input$new_data_east, gender = input$new_data_gender) %>%
        cbind(multilocus_df_named)
    } else if (input$new_data_mode == "multiple") {
      # If a file is given, use the already exsiting function to load and parse it according to the specifications on the left
      c(new_data, new_am_data) %<-% load_main_data(input$new_data_file$datapath)
    }
    # Get the search_data-type of data for the new data
    c(new_search_data, new_multiple_match, new_unclassified) %<-% GenotypeCheck::match_new_data(data = data, new_data = new_data, additional_data_columns = names(additional_data), allele_mismatch = input$new_data_mismatch)

    # DEBUG: Temp
    print(new_search_data)
    print(new_multiple_match)
    print(new_unclassified)

    ####### TODO : If the user adds a file with multiple new data the ids gets weird... fix this... maybe load the previous data
    #############  and make sure the new indexes are bigger then the biggest and that if they are in the same category set the id
    #############  to be the same.
  })
}

# Create a shiny app
shinyApp(ui = ui, server = server)
