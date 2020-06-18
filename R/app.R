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
ui <- fluidPage(

  # App title
  titlePanel("Match Genotype"),

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(width = 3,

      # Input: Select a file
      fileInput("file1", "Choose Data File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xls", ".xlsx", ".ods")),

      # Input: Checkbox if file has header
      checkboxInput("header", "Header", TRUE),

      tags$hr(),
      # Select allele mismatch value
      h4("TODO::: Show amUniqueProfile To User If a button is clicked"),
      textInput(inputId = "alleleMismatchValue", label = "Allowed Allele-mismatch", value = "3"),
      plotOutput(outputId = "allelematchProfilePlot"),
      actionButton(inputId = "generateAllelematchProfile", "Generate Mismatch Plot"),

      h4("Type the column name of the specified columns."),
      h5("If header is deseleted, type the indexes of the columns."),


      # Select Index Column
      textInput(inputId = "indexColumnName", label = "Index Column", value = "SEP"),

      # All user to specify colmn-names/indexes for
      textInput(inputId = "dateColumnName", label = "Date Column", value = "Funnetdatum"),
      textInput(inputId = "northColumnName", label = "North Column", value = "Nord"),
      textInput(inputId = "eastColumnName", label = "East Column", value = "Ost"),
      textInput(inputId = "genderColumnName", label = "Gender Column", value = "Kon"),
      textInput(inputId = "presetIndColumnName", label = "PresetIndividual Column", value = "Individ"),

      # Select Locus Columns
      textInput(inputId = "locusColumnNames", label = "Locus Columns (separated by ',')", value = paste0(locus_names, collapse = "", sep = ",")),

      # Parse Data
      actionButton(inputId = "groupIndividuals", label = "GROUP INDIVIDUALS"),

      tags$hr(),

      # Display some result data to the user
      textOutput(outputId = "amtMultipleMatches"),
      textOutput(outputId = "amtUnclassified"),

      div(h4("Handle Multiple Matches")),

      textInput(inputId = "multipleMatchIndex", label = "View Details (Index of Multiple Matched Sample): ", placeholder = "tex 2"),
      DT::dataTableOutput("multipleMatchesTable"),

      # TODO:: Allow the user to handle these (similar to matching new data)
      div(h4("Handle Unclassified Samples")),

      DT::dataTableOutput("unclassifiedTable"),

    ),

    # Main panel for displaying outputs
    mainPanel(
      # Panel for handeling multiple matched data, will probably be similar to the panel for matching new data
      conditionalPanel("input.multipleMatchIndex != ''",
                       # Showing Multimatch data for SEP123123 <-- Example
                       h4(textOutput("multiMatchDataFor")),
                       h5("The sample matched the following individuals: "),
                       # Desired: map beside data, now it jumps down because of size, not that important
                       sidebarLayout(
                         sidebarPanel = sidebarPanel(width = 9,
                          # Render the ones that were similar
                           DT::dataTableOutput("multipleMatchedSingle"),
                         ),
                         mainPanel = mainPanel(
                           # render the map for the user to have all data when deciding which individual to add it to
                           leafletOutput(outputId = "multiMatchMap"),
                         ),
                       ),
                       # User choose and add to a group of samples/individual - information
                       h5("If this ID is one of the listed above the sample will be added to that group of sample/individual, if not, the sample will create a new individual IF the new ID does not already exist, make sure it is unique if that is the desired action."),
                       # Text box to type new id, either create new group or create a override id for every sample in that group
                       textInput(inputId = "multipleMatchFix", label = "Set ID/Individual to group: "),
                       actionButton(inputId = "multipleMatchFixConfirm", label = "Confirm/Save to data"),
                       tags$hr(),
                       ),

      # Output: Data file
      DT::dataTableOutput("contents")
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

  observeEvent(input$file1, {
    req(input$file1)

    load_data()
  })

  # Run allelematch and all GenotypeChecks the surrounding code when the click of the button
  observeEvent(input$groupIndividuals, {
    groupIndividuals()
    update_output_preprocess_data()
  })

  groupIndividuals <- function() {
    req(as.numeric(input$alleleMismatchValue))

    load_data()

    # Unpack the different data returned by our wrapper of allelematch into temp variables
    c(search_data_temp, multiple_matches_temp, unclassified_temp) %<-% GenotypeCheck::create_search_data(data, am_data, as.numeric(input$alleleMismatchValue))

    # Change the session (server) data from the temp data
    search_data <<- search_data_temp
    multiple_matches <<- multiple_matches_temp
    unclassified <<- unclassified_temp
  }

  load_data <- function() {
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

    # Load the data, this will be the meta data
    data <<- GenotypeCheck::import_data(input$file1$datapath, index_column = index_column, additional_data = additional_data, locus_names = locus_columns)

    # Create allaematch dataset, ignore some meta-data as it can be read from the "data" above, the index (SEP) is the same
    am_data <<- GenotypeCheck::create_allelematch_dataset(data, ignore_columns = names(additional_data))
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
        search_data[search_data_filter,]
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
    # Remove the sample from the list of samples that have multiple matches, it has been corrected by the user
    multiple_matches <<- multiple_matches[multiple_matches != showing_index]
    # Remove all duplicated of the specific sample, the order doesnt matter as override id is the important parameter for the id
    # We can remove the one with the correct id and set the override id of another without problem
    search_data <<- search_data[!(search_data$index == showing_index & duplicated(search_data$index)),]
    # Set the sample (now no duplicates, only one left) override id to be the id specified by the user
    search_data$override_id[search_data$index == showing_index] <<- input$multipleMatchFix
    # Set every sample that is in the same group to have a override id, maybe not necessary but to ensure the order generated by
    # allelelmatch doesnt change and would therefor place the user "controlled" one in a then different group
    search_data$override_id[get_id(search_data) == input$multipleMatchFix & !(search_data$index %in% multiple_matches)] <<- input$multipleMatchFix

    # Update the visual information, the big table and the count of multimatches
    update_output_preprocess_data()
    # Reset the chosen multimatch index, the conditional panel will disapear until the user chooses a new sample that have been multimatched to handle
    updateTextInput(session, "multipleMatchIndex", value = "")
  })

  shiny::observeEvent(input$generateAllelematchProfile, {
    output$allelematchProfilePlot <- shiny::renderPlot({
      plot_data <- GenotypeCheck::generate_allelemtach_profile_plot(am_data)
      print(plot_data)
    })
  })
}

# Create a shiny app
shinyApp(ui = ui, server = server)
