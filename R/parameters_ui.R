parameters_ui <- function(id) {
  ns <- shiny::NS(id) # Create a namespace for the module to avoid ID conflicts

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 2,
        shiny::radioButtons(
          inputId = ns("param_type"),
          label = "Shock Type",
          choices = c(
            "Productivity" = "productivity",
            "Trade Cost" = "bitrade"
          ),
          selected = "bitrade"
        )
      ),
      shiny::column(
        width = 2,
        shiny::div(
          style = "margin-top: 27px;", # Adjust this value as needed for alignment
          shiny::actionButton(
            ns("add_shock_btn"),
            "Add shock",
            icon = shiny::icon("bolt")
          )
        )
      ),
      shiny::column(
        width = 6,
        shiny::uiOutput(ns("param_shock_ui"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::tags$h3("Productivity Parameters (%)"), # Header for productivity parameters
        DT::DTOutput(ns("productivity_parameters")), # Output placeholder for the productivity table
        shiny::tags$h3("Bilateral Trade Parameters (%)"), # Header for bilateral trade parameters
        DT::DTOutput(ns("bitrade_parameters")) # Output placeholder for the bilateral trade table
      )
    )
  )
}
