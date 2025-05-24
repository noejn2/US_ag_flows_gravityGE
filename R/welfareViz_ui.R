welfareViz_ui <- function(id) {
    ns <- shiny::NS(id) # Namespace for module

    shiny::fluidPage(
        shiny::fluidRow(
            shiny::column(
                width = 12, # Full-width column
                style = "display: flex; justify-content: center; align-items: center; padding: 10px;", # Center alignment
                shiny::selectInput(
                    inputId = ns("variable_selector"), # Namespace the input ID
                    label = "Welfare Measure", # Label for the dropdown
                    choices = c("Expenditure", "Wage", "Price"), # Choices will be dynamically updated in the server
                    selected = "Expenditure" # Default selection
                )
            )
        ),
        shiny::fluidRow(
            # First row for two figure panels with titles
            shiny::column(
                width = 6,
                shiny::plotOutput(
                    ns("figure1"),
                    height = "500px",
                    width = "100%",
                    click = ns("figure1_click")
                ) # Full-size Figure 1
            ),
            shiny::column(
                width = 6,
                shiny::plotOutput(
                    ns("figure2"),
                    height = "500px",
                    width = "100%"
                ) # Full-size Figure 2
            )
        ),
        shiny::fluidRow(
            # Second row for the table with a title
            shiny::column(
                width = 12,
                DT::DTOutput(ns("results_table")) # Placeholder for the table
            )
        )
    )
}
