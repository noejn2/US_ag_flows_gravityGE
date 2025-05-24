#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Enable full stack trace for debugging
#options(shiny.fullstacktrace = TRUE)
options(shiny.minified = TRUE)


# Source module scripts & utilities
files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
files <- c(files, list.files("R/utils", pattern = "\\.R$", full.names = TRUE))
sapply(files, source)

# Load shapefile and extract state names and initials
shp_file <- shpfileSeeker()
states_list <- shp_file$Name[order(shp_file$Name)] # Full state names
statesINI_list <- shp_file$Code[order(shp_file$Name)] # State initials
lookup_table <- data.frame(
  full_name = states_list,
  short_name = statesINI_list
) # Create a lookup table for mapping

# Pre-load trade data and calculate the number of unique states
trade_flows <- readRDS("data/trade_data.rds")
trade_flows <- trade_flows[
  with(trade_flows, order(trade_flows$orig, trade_flows$dest)),
] # Order trade data by origin and destination
state_len <- length(unique(trade_flows$orig)) # Number of unique states


# Load custom CSS for the switchButton
shiny::tags$head(
  shiny::tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "SwitchButton.css"
  )
)

# Define UI
ui <- shiny::fluidPage(
  tags$head(
    tags$script(src = "www/customBitradeMatrix.js")
  ),
  shiny::navbarPage(
    theme = "switchButton.css", # Apply custom theme
    title = "US Domestic Agricultural Trade", # App title
    header = list(
      tags$head(
        tags$style(HTML(
          "
      .navbar-nav > li > a {
        font-size: 16px !important;
      }
    "
        ))
      ),
      tags$head(
        tags$style(HTML(
          "
      .navbar-brand {
        font-size: 24px !important;
      }
    "
        ))
      ),
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 2,
            shiny::div(
              shiny::selectInput(
                inputId = "state_selector", # Namespace the input ID
                label = "Select a State", # Label for the dropdown
                choices = NULL, # Choices will be dynamically updated in the server
                selected = NULL # Default selection
              ),
            )
          ),
          shiny::column(
            width = 2,
            shiny::div(
              shiny::selectInput(
                inputId = "state_delector", # Namespace the input ID
                label = "Delete a State", # Label for the dropdown
                choices = NULL, # Choices will be dynamically updated in the server
                selected = NULL # Default selection
              ),
            )
          ),
          shiny::column(
            width = 3,
            shiny::div(
              shiny::selectizeInput(
                inputId = 'focus_pool', # ID for the focus pool input
                'Focus Pool',
                choices = NULL,
                multiple = TRUE,
                size = 8
              ),
            )
          ),
          shiny::column(
            width = 2, # Column for the action button
            shiny::div(
              style = "margin-top: 25px", # Vertical alignment
              shiny::actionButton("run_btn", "Run Simulation") # Button to trigger simulation
            )
          ),
          shiny::column(
            width = 2, # Column for the action button
            shiny::div(
              style = "margin-top: 25px", # Vertical alignment
              shiny::actionButton("clear_btn", "Clear Simulation") # Button to trigger simulation
            )
          ),
          shiny::column(
            width = 1, # Column for the toggle button
            shiny::div(
              #              style = "margin-top: 8px;  margin-right: 10px;", # Vertical alignment
              switchButton(
                id = "toggle_btn", # ID for the toggle button
                label = "State initials", # Label for the toggle button
                value = TRUE, # Default value
                type = "OO" # Toggle button type
              )
            )
          )
        )
      )
    ),

    # # Tab for Trade Flow Visualization
    shiny::tabPanel(
      "Trade Flow Adjustment",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(
            style = "text-align: center; font-size: 18px; font-weight: bold; margin-bottom: 10px;",
            "All values are in millions of 2012 US dollars."
          )
        )
      ),
      tradeflowViz_ui("tradeflow") # Display summary of matrix data
    ),

    # Tab for Welfare Effects
    shiny::tabPanel(
      "Welfare Effects",
      welfareViz_ui("welfare") # Display summary of matrix data
    ),

    # Tab for Simulation Parameters
    shiny::tabPanel(
      "Simulation Parameters",
      parameters_ui("simulation_parameters") # UI for simulation parameters module
    ),

    shiny::tabPanel(
      "About",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h1("About the Tool"),
          shiny::p(
            style = "font-size: 16px;",
            "The US Domestic Agricultural Trade tool is an interactive Shiny application designed to analyze and visualize agricultural trade flows between US states.
        It allows users to explore trade data, simulate trade scenarios, and assess welfare effects using a gravity model framework. While the model is described in the published paper, the data can be thoroughly explored using the National Transportation Research Center's Freight Analysis Framework Version 5 (FAF5)."
          ),
          shiny::p(
            style = "font-size: 16px;",
            shiny::tags$a(
              href = "https://faf.ornl.gov/faf5/",
              target = "_blank",
              "Click here to access the Freight Analysis Framework Version 5 (FAF5)."
            )
          ),

          shiny::p(
            style = "font-size: 16px;",
            "Key features of this include:"
          ),
          shiny::tags$ul(
            style = "font-size: 16px;",
            shiny::tags$li(
              "Dynamic state selection and focus pool management."
            ),
            shiny::tags$li(
              "Simulation of trade flows based on user-defined parameters."
            ),
            shiny::tags$li(
              "Visualization of welfare effects and trade flow data."
            )
          ),
          shiny::p(
            style = "font-size: 16px;",
            "This tool is based on the methodology and findings presented in the published paper: "
          ),
          shiny::tags$blockquote(
            style = "font-size: 16px;",
            shiny::tags$em(
              "\"A model of the U.S. food system: What are the determinants
of the state vulnerabilities to production shocks and supply
chain disruptions?\" by Noé J Nava, William Ridley, and Sandy Dall'Erba, published in Agricultural Economics."
            )
          ),
          shiny::p(
            style = "font-size: 16px;",
            "This tool is intended for researchers, policymakers, and stakeholders interested in understanding the dynamics of domestic agricultural trade in the United States."
          ),
          shiny::p(
            style = "font-size: 16px;",
            "If you use this tool in your work, familirize with the terms and conditions in the MIT license that include citing the original paper and acknowledging the authorship of this dashboard."
          ),
          shiny::p(
            style = "font-size: 16px;",
            "For any questions or feedback, please contact the author Noé J Nava at ",
            shiny::tags$a(
              href = "mailto:noejnava2@gmail.com",
              target = "_blank",
              "noejnava2@gmail.com"
            ),
            "."
          ),
          shiny::h2("Instructions"),
          shiny::p(
            style = "font-size: 16px;",
            "To use this tool effectively, please note the following:"
          ),
          shiny::tags$ul(
            style = "font-size: 16px;",
            shiny::tags$li(
              "Navigate through the tabs to explore trade flow visualizations, welfare effects, and simulation parameters."
            ),
            shiny::tags$li(
              "Simulation parameters represent percentage changes. Ensure that you input values as percentages (e.g., 10 indicates a 10% increase and -10 indicates a 10% decrease)."
            ),
            shiny::tags$li(
              "Use the focus pool to select up to 8 states for detailed analysis."
            ),
            shiny::tags$li(
              "Click 'Run Simulation' to execute the model and view the updated results."
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the current state names (full names or initials)
  state_names <- shiny::reactiveVal(states_list)

  # Reactive toggle to track the state of the switchButton
  state_toggle <- shiny::reactiveVal(FALSE)

  # Reactive value to store the simulation results
  simulation_results <- reactiveVal(NULL) # Initialize simulation results

  # Reactive value to store the focus pool
  focus_pool <- shiny::reactiveVal(character(0)) # Initialize as an empty character vector

  # Reactive value to store the trade data
  trade_data <- shiny::reactiveVal(trade_flows) # Initialize trade data

  # Observe toggle button changes and update state names case accordingly
  shiny::observeEvent(input$toggle_btn, {
    state_toggle(!state_toggle()) # Toggle the state (default: FALSE)
    last_selection <- which(input$state_selector == state_names()) # Get the last selected state

    if (state_toggle()) {
      state_names(statesINI_list) # Use state initials
      focus_pool(statesINI_list[which(states_list %in% focus_pool())])
    } else {
      state_names(states_list) # Use full state names
      focus_pool(states_list[which(statesINI_list %in% focus_pool())])
    }

    if (!is.null(simulation_results())) {
      output <- simulation_results()
      output <- mappingOfStates(output, state_toggle(), lookup_table)
      simulation_results(output) # Update the simulation results
    }

    trade <- trade_data() # Get the current trade data
    trade <- mappingOfStates(
      trade,
      state_toggle(),
      lookup_table,
      FALSE
    ) # Update trade data
    trade_data(trade) # Store the updated trade data

    if (counter > 1) {
      shiny::updateSelectInput(
        session = session,
        inputId = "state_selector",
        choices = state_names(),
        selected = state_names()[last_selection]
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "focus_pool",
        choices = focus_pool(),
        selected = focus_pool()
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "state_delector",
        choices = focus_pool(),
        selected = ""
      )
    }
  })

  # Call the server logic for the simulation parameters module
  sim_params <- parameters_server(
    id = "simulation_parameters", # Module ID
    state_names = state_names, # Reactive state names
    state_len = state_len, # Number of unique states
    statesINI_list = statesINI_list # State initials list
  )

  # ---: State-selection vector :---
  counter <- 0
  shiny::observe({
    # Observe the first time the app is run
    if (counter < 1) {
      focus_pool(c("AZ", "CA", "CO", "NV", "NM", "UT")) # Initialize the focus pool
      shiny::updateSelectInput(
        session = session,
        inputId = "focus_pool",
        choices = focus_pool(),
        selected = focus_pool(), # Default selection
      )

      # Adding selection
      shiny::updateSelectInput(
        session = session,
        inputId = "state_selector",
        choices = state_names(),
        selected = c("UT")
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "state_delector",
        choices = focus_pool(),
        selected = character(0)
      )

      output <- modelBackEnd(
        trade_data = trade_flows, # Trade data
        bitrade = sim_params$bitrade(), # Bilateral trade matrix
        productivity = sim_params$productivity() # Productivity vector
      )

      if (state_toggle()) {
        output <- mappingOfStates(output, TRUE, lookup_table)
      }

      simulation_results(output) # Store the simulation results
    }
  })

  # Automatically add the selected state to the focus pool
  shiny::observeEvent(input$state_selector, {
    counter <<- counter + 1 # Increment the counter
    if (counter > 1) {
      selected_state <- input$state_selector # Get the selected state
      current_pool <- focus_pool() # Get the current focus pool
      if (!is.null(selected_state) & !(selected_state %in% current_pool)) {
        if (length(current_pool) < 8) {
          focus_pool(c(current_pool, selected_state)) # Add the state to the pool
        } else {
          shiny::showModal(
            shiny::modalDialog(
              title = "Focus Pool Limit Reached",
              shiny::tags$p(
                "You can only add up to 8 states to the focus pool. Please remove one before adding another."
              ),
              shiny::tags$ul(
                lapply(current_pool, shiny::tags$li) # Display the current focus pool
              ),
              easyClose = TRUE, # Allow the user to close the modal by clicking outside
              footer = shiny::modalButton("Close") # Add a close button
            )
          )
        }
      }

      shiny::updateSelectInput(
        session = session,
        inputId = "state_selector",
        choices = state_names(),
        selected = selected_state
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "focus_pool",
        choices = focus_pool(),
        selected = focus_pool()
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "state_delector",
        choices = focus_pool(),
        selected = ""
      )
    }
  })

  # Observe the state to remove and update the focus pool
  shiny::observeEvent(input$state_delector, {
    state_to_remove <- input$state_delector # Get the state to remove
    current_pool <- focus_pool() # Get the current focus pool

    # Remove the state from the pool
    updated_pool <- setdiff(current_pool, state_to_remove)
    focus_pool(updated_pool) # Update the focus pool

    shiny::updateSelectInput(
      session = session,
      inputId = "focus_pool",
      choices = focus_pool(),
      selected = focus_pool()
    )

    shiny::updateSelectInput(
      session = session,
      inputId = "state_delector",
      choices = focus_pool(),
      selected = ""
    )
  })

  # ---: Model section :---
  # Observe clear_btn click event and clear the simulation
  shiny::observeEvent(input$clear_btn, {
    sim_params$bitrade(matrix(0, nrow = state_len, ncol = state_len)) # Clear bilateral trade matrix
    sim_params$productivity(matrix(0, nrow = 1, ncol = state_len)) # Clear productivity vector
    simulation_results(NULL)
  })

  # Observe run_btnn click event and run the model in parameters_server
  shiny::observeEvent(input$run_btn, {
    output <- modelBackEnd(
      trade_data = trade_flows, # Trade data
      bitrade = sim_params$bitrade(), # Bilateral trade matrix
      productivity = sim_params$productivity() # Productivity vector
    )
    if (state_toggle()) {
      output <- mappingOfStates(output, TRUE, lookup_table)
    }
    simulation_results(output) # Store the simulation results
    # Change names as needed
  })

  # ---: Trade section :---
  shiny::observe({
    # ---: Under construction :---
    tradeflowViz_server(
      id = "tradeflow",
      state_names = state_names,
      simulation_results = simulation_results, # Simulation results
      trade_data = trade_data, # Trade data
      focus_pool = focus_pool # Focus pool
    )
  })

  # ---: Welfare section :---
  shiny::observe({
    welfareViz_server(
      id = "welfare", # Module ID
      simulation_results = simulation_results, # Simulation results
      state_names = state_names, # Reactive state names
      state_len = state_len, # Number of unique states
      shp_file = shp_file, # Shapefile data
      focus_pool = focus_pool, # Focus pool
      state_toggle = state_toggle # State toggle
    )
  })
}

# Run the Shiny app
shiny::shinyApp(ui, server)
