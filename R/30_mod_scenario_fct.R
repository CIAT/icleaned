
# Generate a Vector of Shiny Input Elements
generate_shiny_inputs <- function(FUN, len, id, value = rep(TRUE, len), ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, value = value[i], ...))
  }
  inputs
}

# Function to link the checkboxes inside renderDT to shiny inputs
# It ensures that checkbox states are linked to Shiny inputs both initially and upon change.
checkbox_link <- function(id, ns) {
  JS(
    paste0(
      'function(settings) {
         console.log("Checkbox link function called");

         // Select only checkbox inputs in the first column of the DataTable
         var checkboxes = $(settings.nTable).find("tbody tr td:first-child input[type=checkbox]");
         console.log(checkboxes);  // Log all checkboxes for debugging purposes
         
         // Iterate over each checkbox element
         checkboxes.each(function(i) {
           
           var checkboxId = "', ns(id), '" + (i+1);
           
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


js <- function(ns) {
  paste0(
    "$('[id^=intercrop_check]').on('click', function(){
  var id = this.getAttribute('id');
  var i = parseInt(/intercrop_check(\\d+)/.exec(id)[1]);
  var value = $(this).prop('checked');
  var info = {row: i, value: value};
  Shiny.setInputValue('", ns("checkbox_info"), "', info, {priority: 'event'});
  });")
}

disable_specific_rows_edit_js <- function(rows_to_disable) {
  glue('function (td, cellData, rowData, row, col) {{
                    if ([{paste(rows_to_disable, collapse = ", ")}].includes(row)) {{
                    // Add cursor style to indicate disabled state
                    $(td).css("cursor", "not-allowed");
                      
                    $(td).on("dblclick", function(e) {{
                      e.stopPropagation();
                      $(td).addClass("disabled-cell");
                      setTimeout(function() {{
                        $(td).removeClass("disabled-cell");
                      }}, 250);
                    }});
                    }}
                  }}')
}

disable_all_rows_edit_js <- function() {
  'function (td, cellData, rowData, row, col) {
    $(td).on("dblclick", function(e) {
      e.stopPropagation();
      $(td).addClass("disabled-cell");
      setTimeout(function() {
        $(td).removeClass("disabled-cell");
      }, 250);
    });
  }'
}

# Disable editing and add "not-allowed" cursor to the entire column
disable_and_add_cursor_js <- function() {
  'function (td, cellData, rowData, row, col) {
    // Disable double-click editing for all cells
    $(td).on("dblclick", function(e) {
      e.stopPropagation();
      $(td).addClass("disabled-cell");
      setTimeout(function() {
        $(td).removeClass("disabled-cell");
      }, 250);
    });
    
    // Add "not-allowed" cursor style to all cells
    $(td).css("cursor", "not-allowed");
  }'
}

# Disable editing for all column cells and conditionally add "not-allowed" cursor
disable_with_conditional_cursor_js <- function(rows_to_highlight) {
  glue('function (td, cellData, rowData, row, col) {{
        // Always disable editing
        $(td).off("dblclick").on("dblclick", function(e) {{
          e.stopPropagation();
        }});
                    
        // Check if the cell row index is in the rows_to_highlight array
       if ([{paste(rows_to_highlight, collapse = ", ")}].includes(row)) {{
         // Add "not-allowed" cursor for these specific rows
         $(td).css("cursor", "not-allowed");
       }}
}}')
}

# Define a function to click on elements with a specific prefix in their ID
click_by_prefix <- function(prefix) {
  js_code <- sprintf("
      $('[id^=\"%s\"]').first().click();
      ", prefix)
  runjs(js_code)
}

# Function to format text for selectInputs with blue color and underline
format_text_displayed <- function(column) {
  sapply(
    column, function(x) as.character(
      tags$span(
        style = "color: #153fd1; text-decoration: underline; cursor: pointer;",
        x
      )
    )
  )
}

# Helper function to get column indices by name
get_column_indices <- function(data, column_names) {
  # Ensure column_names is a vector
  if (!is.vector(column_names)) {
    column_names <- as.vector(column_names)
  }
  # Match the column names with the names in the data frame and return their indices
  which(names(data) %in% column_names)
}
