
checkbox_link_multi <- function(id, ns, table_name) {
  JS(
    paste0(
      'function(settings) {
        console.log("Checkbox link function called for table: ', table_name, '");

        // Create DataTables API instance to access row-level methods
        var api = new $.fn.dataTable.Api(settings);

        // Iterate through all rows using DataTables API
        // This ensures we get the correct data row index regardless of sorting/filtering
        api.rows().every(function(displayIdx) {
          var row = this;
          var node = row.node();

          // Find the checkbox input in this row
          var checkbox = $(node).find("input[type=checkbox]").first();

          if (checkbox.length > 0) {
            // Get the ORIGINAL data row index (0-based)
            // This index is CONSTANT - it does not change when user sorts/filters the table
            var dataIdx = row.index();

            // Create checkbox ID based on DATA index (convert to 1-based for R)
            // This ensures the checkbox ID always matches the same data row
            var checkboxId = "', ns(paste0(id, "_", table_name)), '_" + (dataIdx + 1);
            console.log("Display row:", displayIdx, "-> Data row:", dataIdx, "-> Checkbox ID:", checkboxId);

            // Initialize checkbox state in Shiny
            Shiny.setInputValue(checkboxId, checkbox.prop("checked"), {priority: "event"});

            // Listen for checkbox changes and update Shiny
            // Remove any existing listeners first to avoid duplicates
            checkbox.off("change").on("change", function() {
              Shiny.setInputValue(checkboxId, this.checked, {priority: "event"});
            });
          }
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

#' Generate Next Unique Code
#'
#' Calculates the next sequential code for a new row by finding the maximum
#' existing numeric code in the specified column and adding 1.
#'
#' @param data A data frame containing the parameter table
#' @param code_column Character string, name of the code column
#'
#' @return Integer, the next unique code (max + 1, or 1 if table is empty)
#'
#' @examples
#' generate_next_code(crops_data, "crop_code")
generate_next_code <- function(data, code_column) {
  if (nrow(data) == 0 || all(is.na(data[[code_column]]))) {
    return(1)
  }

  # Extract numeric codes, ignoring non-numeric values
  existing_codes <- suppressWarnings(as.numeric(data[[code_column]]))
  existing_codes <- existing_codes[!is.na(existing_codes)]

  if (length(existing_codes) == 0) {
    return(1)
  }

  next_code <- max(existing_codes) + 1
  return(next_code)
}
