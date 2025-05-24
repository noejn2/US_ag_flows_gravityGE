parameters_ui <- function(id) {
  ns <- shiny::NS(id) # Create a namespace for the module to avoid ID conflicts

  shiny::tagList(
    # Section for productivity parameters
    shiny::tags$h3("Productivity Parameters (%)"), # Header for productivity parameters
    DT::DTOutput(ns("productivity_parameters")), # Output placeholder for the productivity table

    # Section for bilateral trade parameters
    shiny::tags$h3("Bilateral Trade Parameters (%)"), # Header for bilateral trade parameters
    DT::DTOutput(ns("bitrade_parameters")) # Output placeholder for the bilateral trade table
  )
}
