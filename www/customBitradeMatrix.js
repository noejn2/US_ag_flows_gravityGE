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