tradeflowViz_ui <- function(id) {
    ns <- shiny::NS(id) # Namespace for module

    shiny::fluidPage(
        shiny::fluidRow(
            shiny::column(
                width = 3, # Full-width column
                style = "display: flex; justify-content: center; align-items: center; padding: 10px;", # Center alignment
                shiny::radioButtons(
                    inputId = ns("include_others"), # Namespace the input ID
                    label = "Include Others", # Label for the radio buttons
                    choices = c("Include" = TRUE, "Do Not Include" = FALSE), # Two options
                    selected = FALSE # Default selection is Include
                )
            ),
            shiny::column(
                width = 3, # Full-width column
                style = "display: flex; justify-content: center; align-items: center; padding: 10px;", # Center alignment
                shiny::radioButtons(
                    inputId = ns("include_self"), # Namespace the input ID
                    label = "Include self-trade", # Label for the radio buttons
                    choices = c("Include" = TRUE, "Do Not Include" = FALSE), # Two options
                    selected = FALSE # Default selection is Include
                )
            ),
            shiny::column(
                width = 3, # Full-width column
                style = "display: flex; justify-content: center; align-items: center; padding: 10px;", # Center alignment
                shiny::radioButtons(
                    inputId = ns("net_exports"), # Namespace the input ID
                    label = "Flow value", # Label for the radio buttons
                    choices = c("Total Exports" = TRUE, "Net Exports" = FALSE), # Two options
                    selected = FALSE # Default selection is Include
                )
            ),
            shiny::column(
                width = 3, # Full-width column
                style = "display: flex; justify-content: center; align-items: center; padding: 10px;", # Center alignment
                shiny::radioButtons(
                    inputId = ns("baseline"), # Namespace the input ID
                    label = "Baseline/Simulation", # Label for the radio buttons
                    choices = c("Baseline" = TRUE, "Simulation" = FALSE), # Two options
                    selected = FALSE # Default selection is Include
                )
            )
        ),
        shiny::fluidRow(
            # First row for two figure panels with titles
            shiny::column(
                width = 7,
                chorddiag::chorddiagOutput(ns("chord"), height = "600px"),
                shiny::div(
                    style = "margin-top: 10px;",
                    shiny::downloadButton(
                        ns("download_chord_png"),
                        "Download Chord Diagram (PNG)"
                    )
                )
            ),
            shiny::column(
                width = 5,
                DT::DTOutput(ns("summary_table"))
            )
        ),
        shiny::fluidRow(
            shiny::column(
                width = 12,
                DT::DTOutput(ns("left"))
            )
        )
    )
}
