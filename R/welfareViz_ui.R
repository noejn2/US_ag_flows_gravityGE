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
                    ns("map"),
                    height = "500px",
                    width = "100%",
                    click = ns("map_click")
                ),
                shiny::div(
                    style = "margin-top: 10px;",
                    shiny::downloadButton(
                        ns("download_map_png"),
                        "Download Map (PNG)"
                    )
                )
            ),
            shiny::column(
                width = 6,
                shiny::plotOutput(
                    ns("bars"),
                    height = "500px",
                    width = "100%"
                ),
                shiny::div(
                    style = "margin-top: 10px;",
                    shiny::downloadButton(
                        ns("download_bar_png"),
                        "Download Bar Plot (PNG)"
                    )
                )
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
