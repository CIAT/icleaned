
checkbox_link_multi <- function(id, ns, table_name) {
  JS(
    paste0(
      'function(settings) {
        console.log("Checkbox link function called for table: ', table_name, '");

        // Select all checkbox inputs within the current DataTable
        var checkboxes = $(settings.nTable).find("input[type=checkbox]");
        console.log("Checkboxes found in table ', table_name, ':", checkboxes.length);

        // Iterate over each checkbox element
        checkboxes.each(function(i) {
          
          var checkboxId = "', ns(paste0(id, "_", table_name)), '_" + (i + 1);
          console.log("Assigning checkbox ID for table ', table_name, ':", checkboxId);

          // Send the initial state of the checkbox to Shiny
          Shiny.setInputValue(checkboxId, this.checked, {priority: "event"});
          
          // Add an event listener to update the state on change
          $(this).on("change", function() {
            // Update Shiny input with the new state of the checkbox
            Shiny.setInputValue(checkboxId, this.checked, {priority: "event"});
          });
        });
      }'
    )
  )
}

# Add "cursor: not-allowed" to an entire disabled column
add_cursor_to_disabled_column_js <- function() {
  'function (td, cellData, rowData, row, col) {
    // Add "not-allowed" cursor style
    $(td).css("cursor", "not-allowed");
  }'
}

# Initialize column search inputs for DataTables
init_column_search_js <- function() {
  'function(settings, json) {
    var api = this.api();

    // Prevent duplicate search rows on table redraws
    // Use a flag in settings to track if we already initialized the search row
    if (settings.oInit.searchRowInitialized) {
      return;
    }
    settings.oInit.searchRowInitialized = true;

    // Create the search row element that will hold all search inputs
    var searchRow = $("<tr class=\\"search-row\\"></tr>");

    // Loop through each column to create search inputs
    api.columns().every(function(index) {
      var column = this;
      var th = $("<th></th>");

      // First column is the checkbox column - leave it empty
      if (index === 0) {
        th.appendTo(searchRow);
      } else {
        // Create a text input for filtering this column
        var input = $(\'<input type="text" placeholder="Search..." />\')
          .on("keyup change clear", function() {
            // Only trigger search if the value actually changed
            if (column.search() !== this.value) {
              column.search(this.value).draw();
            }
          })
          .on("click", function(e) {
            // Prevent clicks from bubbling up (e.g., to column sorting)
            e.stopPropagation();
          });

        th.append(input);
        th.appendTo(searchRow);
      }
    });

    // Add the completed search row to the table header
    $(api.table().header()).append(searchRow);

    // Update FixedHeader and FixedColumns to include the new search row in their clones
    setTimeout(function() {
      if (api.fixedHeader) {
        api.fixedHeader.adjust();
      }
      if (api.fixedColumns) {
        api.fixedColumns().update();
      }
      // Recalculate column widths and redraw to ensure proper alignment
      api.columns.adjust().draw(false);
    }, 30);
  }'
}
