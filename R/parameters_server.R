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
          paging = TRUE, # Ensure pagination is enabled
          pageLength = 10 #, # Set the number of rows per page
          # drawCallback = DT::JS("function(settings) { customBitradeMatrix(this.api()); }")
        ),
        callback = DT::JS(
          "
table.on('draw', function () {
    var rows = table.rows().nodes(); // Get rows
    // var nr = table.page.info().length; // Number of rows per page
    // var cp = table.page.info().page + 1; // Current page number (1-based index)
    var headerCells = table.columns().header(); // Get column headers


    $(rows).each(function (rowIdx, row) {
        var cells = $(row).find('td');  // Get all cells (excluding row names)
        var rowName = $(cells[0]).text().trim(); // Assumes row name is in first column

        $(cells).each(function (colIdx, cell) {
            var colName = $(headerCells[colIdx]).text().trim();

            // Skip the first cell in each row (the row name column) by adding 1 to colIdx
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
        rownames = NULL
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
