parameters_server <- function(
  id,
  state_len,
  state_names,
  statesINI_list
) {
  moduleServer(id, function(input, output, session) {
    # Initialize matrix for bilateral trade
    bitrade <- reactiveVal({
      matrix(0, nrow = state_len, ncol = state_len)
    })

    # These are the dynamic selections
    output$param_shock_ui <- renderUI({
      if (input$param_type == "productivity") {
        shiny::fixedRow(
          shiny::column(
            width = 2,
            selectInput(
              inputId = session$ns("producer_shock"),
              label = "Poducer",
              choices = state_names(),
              selected = NULL
            )
          ),
          shiny::column(
            width = 4,
            shiny::sliderInput(
              inputId = session$ns("productivity_slider"),
              label = "Parameter Value (%)",
              min = -99,
              max = 200,
              value = 0,
              step = 1
            )
          )
        )
      } else {
        shiny::fixedRow(
          shiny::column(
            width = 2,
            selectInput(
              inputId = session$ns("origin_shock"),
              label = "Origin",
              choices = state_names(),
              selected = NULL
            ),
          ),
          shiny::column(
            width = 4,
            shiny::sliderInput(
              inputId = session$ns("trade_slider"),
              label = "Parameter Value (%)",
              min = -99,
              max = 200,
              value = 0,
              step = 1
            )
          ),
          shiny::column(
            width = 2,
            selectInput(
              inputId = session$ns("destination_shock"),
              label = "Destination",
              choices = state_names(),
              selected = NULL
            ),
          ),
        )
      }
    })

    # Add shock button
    shiny::observeEvent(input$add_shock_btn, {
      if (input$param_type == "productivity") {
        producer <- input$producer_shock
        value <- input$productivity_slider
        if (!is.null(producer)) {
          mat <- productivity()
          if (length(which(statesINI_list == producer)) > 0) {
            mat[which(statesINI_list == producer)] <- value
            productivity(mat)
          } else {
            mat[which(state_names() == producer)] <- value
            productivity(mat)
          }
        }
      } else {
        origin <- input$origin_shock
        destination <- input$destination_shock
        value <- input$trade_slider
        if (!is.null(origin) && !is.null(destination)) {
          browser()

          # Support both initials and full state names
          i <- which(statesINI_list == origin | state_names() == origin)
          j <- which(
            statesINI_list == destination | state_names() == destination
          )

          if (length(i) > 0 && length(j) > 0) {
            if (i == j) {
              shiny::showModal(
                shiny::modalDialog(
                  title = "Invalid Shock",
                  shiny::tags$p(
                    "Domestic trade (origin = destination) must be zero. Please select different states for origin and destination."
                  ),
                  easyClose = TRUE,
                  footer = shiny::modalButton("Close")
                )
              )
            } else {
              mat <- bitrade()
              mat[i, j] <- value
              bitrade(mat)
            }
          }
        }
      }
    })

    # Initialize productivity vector
    productivity <- reactiveVal({
      mat <- matrix(0, nrow = 1, ncol = state_len)
      mat[which(
        statesINI_list %in% c("AZ", "CA", "CO", "NV", "NM", "UT")
      )] <- -50
      mat
    })

    # Update bilateral matrix on edit
    observeEvent(input$bitrade_parameters_cell_edit, {
      info <- input$bitrade_parameters_cell_edit
      i <- info$row
      j <- info$col
      v <- as.numeric(info$value)

      if (i != j && !is.na(v)) {
        mat <- bitrade()
        mat[i, j] <- v
        bitrade(mat)
      }
    })

    observeEvent(input$productivity_parameters_cell_edit, {
      info <- input$productivity_parameters_cell_edit
      i <- info$row
      j <- info$col + 1
      v <- as.numeric(info$value)
      mat <- productivity()
      mat[i, j] <- v
      productivity(mat)
    })

    output$bitrade_parameters <- DT::renderDT({
      req(state_names())
      DT::datatable(
        bitrade(),
        editable = TRUE,
        rownames = state_names(),
        colnames = state_names(),
        options = list(
          paging = TRUE,
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        extensions = c('Buttons'),
        callback = DT::JS(
          "
table.on('draw', function () {
    var rows = table.rows().nodes();
    var headerCells = table.columns().header();
    $(rows).each(function (rowIdx, row) {
        var cells = $(row).find('td');
        var rowName = $(cells[0]).text().trim();
        $(cells).each(function (colIdx, cell) {
            var colName = $(headerCells[colIdx]).text().trim();
            if (rowName === colName) {
              $(cell).css({
                'background-color': '#696969',
                'pointer-events': 'none',
                'user-select': 'none'
              });
            }
            if (colName === '') {
              $(cell).css({
                'pointer-events': 'none',
                'user-select': 'none'
              });
            }
        });
    });
});
"
        )
      )
    })

    # Render the productivity vector
    output$productivity_parameters <- DT::renderDT({
      req(state_names())
      DT::datatable(
        productivity(),
        editable = TRUE,
        colnames = state_names(),
        rownames = NULL,
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        extensions = c('Buttons')
      )
    })

    return(
      list(
        bitrade = bitrade,
        productivity = productivity
      )
    )
  })
}
