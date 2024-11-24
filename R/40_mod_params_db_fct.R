
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
