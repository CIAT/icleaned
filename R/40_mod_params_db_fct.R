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

# Initialize column search inputs in DataTables header (initComplete callback)
init_column_search_js <- function() {
  'function(settings, json) {
    var api = this.api();

    // Check if search row already exists in the DOM (prevents duplicates)
    var existingSearchRow = $(api.table().header()).find("tr.search-row");
    if (existingSearchRow.length > 0) {
      return; // Search row already exists, skip re-adding
    }

    // Create the search row element that will hold all search inputs
    var searchRow = $("<tr class=\\"search-row\\"></tr>");

    // Loop through each column to create search inputs
    api.columns().every(function(index) {
      var column = this;
      var $originalTh = $(column.header());
      var th = $("<th></th>");

      // copy sticky styles from the original header
      var position = $originalTh.css("position");
      if (position === "sticky") {
        th.css({
          "position": "sticky",
          "left": $originalTh.css("left")
        });
      }

      // Check if this column is visible
      var isVisible = column.visible();

      // First column is the checkbox column - leave it empty
      if (index === 0) {
        th.appendTo(searchRow);
      } else if (!isVisible) {
        // Skip creating search input for hidden columns (but they remain searchable)
        // This prevents empty search boxes from appearing for hidden columns like crop_code
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
    
    // Force header to match body width including scrollbar space
    setTimeout(function() {
      var wrapper = $(api.table().container());
      var scrollHead = wrapper.find(".dataTables_scrollHead");
      var scrollBody = wrapper.find(".dataTables_scrollBody");
      
      if (scrollBody.length && scrollHead.length) {
        // Make header scrollable and match body overflow behavior
        scrollHead.css({
          "overflow-y": "scroll",
          "overflow-x": "hidden"
        });
      }
      
      // Recalculate column widths to ensure proper alignment
      api.columns.adjust();
    }, 50);
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

#' Hide Column JavaScript
#'
#' JavaScript function to hide a column's cells and header.
#'
#' @return JS code for DataTables createdCell callback
hide_column_js <- function() {
  JS(
    "function(td, cellData, rowData, row, col) {
      $(td).css('display', 'none');
      // Also hide the header for this column
      var table = $(td).closest('table');
      var thIndex = $(td).index();
      table.find('thead th').eq(thIndex).css('display', 'none');
    }"
  )
}

#' Reorder FeedItem Columns
#'
#' Dynamically inserts `crop_name` column at the position of `crop_code`,
#' and moves `crop_code` to the end of the data frame.
#'
#' @param data A data frame containing the feed item data
#'
#' @return A data frame with reordered columns
reorder_feeditem_columns <- function(data) {
  col_order <- names(data)
  crop_code_idx <- which(col_order == "crop_code")
  
  if (length(crop_code_idx) > 0) {
    # Construct new order: cols_before, crop_name, cols_after, crop_code
    
    # Safely get columns before
    if (crop_code_idx > 1) {
      cols_before <- col_order[1:(crop_code_idx - 1)]
    } else {
      cols_before <- character(0)
    }
    
    # Safely get columns after
    if (crop_code_idx < length(col_order)) {
      cols_after <- col_order[(crop_code_idx + 1):length(col_order)]
    } else {
      cols_after <- character(0)
    }
    
    # Remove 'crop_name' and 'crop_code' from cols_after if they got in there
    # This acts as a sanity check against duplicates
    cols_after <- setdiff(cols_after, c("crop_name", "crop_code"))
    
    new_col_order <- c(cols_before, "crop_name", cols_after, "crop_code")
    
    # Return reordered data
    return(data[, new_col_order, drop = FALSE])
  }
  
  return(data)
}

#' Get Valid Crop Choices for Picker
#'
#' Prepares the list of unique crop choices for the input picker,
#' ensuring the current selection is valid and included.
#'
#' @param crops_table Data frame, the lookup table for crops
#' @param current_crop_code Numeric/Integer, currently selected crop code (can be NA)
#'
#' @return A list with two elements: choices (named vector) and selected (numeric)
get_valid_crop_choices <- function(crops_table, current_crop_code) {
  # Prepare unique choices for the picker
  # Ensure crop names are unique to avoid duplicates in the dropdown
  unique_crops <- crops_table[!duplicated(crops_table$crop_name), ]
  
  # Remove cases where crop_name is empty or NA
  unique_crops <- unique_crops[
    !is.na(unique_crops$crop_name) & unique_crops$crop_name != "",
  ]
  
  # Update current_crop_code if necessary to match the unique list
  # Handles cases where valid duplicates existed in source but were filtered out
  if (!is.na(current_crop_code) && !current_crop_code %in% unique_crops$crop_code) {
    
    # Find name associated with current code in full table
    current_name_matches <- crops_table$crop_name[
      crops_table$crop_code == current_crop_code
    ]
    
    if (length(current_name_matches) > 0 && !is.na(current_name_matches[1])) {
      current_name <- current_name_matches[1]
      
      # If that name exists in our filtered unique list
      if (current_name %in% unique_crops$crop_name) {
        # Return the code from the unique list that corresponds to this name
        # This ensures the picker sees it as a valid "selected" value
        # We do not change the data here, just logic to help the UI state
        # (The UI update logic elsewhere handles the actual save)
        current_crop_code <- unique_crops$crop_code[
          unique_crops$crop_name == current_name
        ]
      }
    }
  }
  
  # Create named vector
  choices <- setNames(unique_crops$crop_code, unique_crops$crop_name)
  
  return(list(choices = choices, selected = current_crop_code))
}

#' Scroll DataTable to Bottom
#'
#' Scrolls a DataTable to the bottom and highlights the last row.
#'
#' @param session Shiny session object
#' @param table_id Namespaced table ID
scroll_to_bottom <- function(session, table_id) {
  session$sendCustomMessage("scrollToBottom", list(tableId = table_id))
}
