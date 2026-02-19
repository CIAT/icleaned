# ----------- Manure/Fertilizer Tab ----------------------------------------------
# Sub-module: fertilizer management
# Sourced from 30_mod_scenario_server.R with local = TRUE
# Reactive value to store the fertilizer data
fertilizers <- reactiveVal(fertilizers_initialization)

# Add fertilizer button click
observeEvent(input$add_fertilizer, {
  req(input$json_file_name)
  if (modal_open()) return()
  modal_open(TRUE)
  showModal(modalDialog(
    title = "Add new fertilizer",
    h2("Choose a Fertilizer", class = "mb-3"),
    shinyWidgets::pickerInput(
      ns("fertilizer_code"),
      choices = setNames(
        lkp_orgfertilizer()$fertilizer_code,
        lkp_orgfertilizer()$fertilizer_desc
      )[sort(lkp_orgfertilizer()$fertilizer_desc)],
      options = list(`live-search` = TRUE)
    ),
    easyClose = TRUE,
    footer = tagList(
      actionButton(ns("ok_add_fertilizer"), "OK"),
      modalButton("Cancel")
    )
  ))
})

# OK button in modal dialog for adding fertilizer
observeEvent(input$ok_add_fertilizer, {
  req(input$fertilizer_code)
  current_fertilizers <- fertilizers()
  if (!input$fertilizer_code %in% current_fertilizers$fertilizer_code) {
    fertilizer_name <- lkp_orgfertilizer()$fertilizer_desc[
      lkp_orgfertilizer()$fertilizer_code == input$fertilizer_code
    ]
    new_row <- data.frame(
      fertilizer_code = input$fertilizer_code,
      fertilizer_desc = fertilizer_name,
      fraction = 0,
      percentage_n = ifelse(fertilizer_name %in% names(fertilizer_percentages), 
                            fertilizer_percentages[fertilizer_name], 0)
    )
    updated_fertilizers <- rbind(current_fertilizers, new_row)
    fertilizers(updated_fertilizers)
  }
  # Reset the select input
  shinyWidgets::updatePickerInput(
    session,
    "fertilizer_code",
    selected = setNames(lkp_orgfertilizer()$fertilizer_code, lkp_orgfertilizer()$fertilizer_desc)[1]
  )
  removeModal()
})

# Render the fertilizer table
output$fertilizer_table <- renderDT({
  
  # Re-initialize the fertilizer data if the columns are missing (show empty table on start)
  if (ncol(fertilizers()) == 0) {
    fertilizers(fertilizers_initialization)
  }
  
  # Identify rows to disable based on fertilizer names
  fertilizers_data <- fertilizers() %>% select(-fertilizer_code, -fraction)
  rows_to_disable <- which(
    fertilizers_data$fertilizer_desc %in% names(fertilizer_percentages)
  ) - 1
  
  # Checkboxes for selecting rows (to be deleted)
  fertilizers_data$selected_fertilizer <- generate_shiny_inputs(
    FUN = checkboxInput,
    len = nrow(fertilizers_data),
    id = ns("selected_fertilizer"),
    value = rep(FALSE, nrow(fertilizers_data)),
    width = "2px"
  )
  
  # Move the 'selected_fertilizer' column to the first position
  fertilizers_data <- fertilizers_data[
    , c("selected_fertilizer", setdiff(names(fertilizers_data), "selected_fertilizer"))
  ]
  
  datatable(
    fertilizers_data,
    colnames = c("", "Fertilizer", "% N"), # Set the first column (selected) name to an empty string
    editable = list(
      target = "cell",
      # Prevent editing of the first column (check boxes for delete rows)
      disable = list(columns = 0)
    ),
    selection = "none",
    rownames = FALSE,
    escape = FALSE,
    options = list(
      dom = "t", 
      paging = FALSE,
      columnDefs = list(
        list(
          width = "2px", # Define the width of the checkbox column
          targets = 0
        ),
        # Disable editing fertilizer's column
        list(
          targets = get_column_indices(fertilizers_data, "fertilizer_desc") - 1,
          createdCell = JS(disable_all_rows_edit_js()),
          searchable = FALSE
        ),
        # Disable editing for specific rows in the % N column
        list(
          targets = get_column_indices(fertilizers_data, "percentage_n") - 1,
          createdCell = JS(disable_specific_rows_edit_js(rows_to_disable)),
          searchable = FALSE
        )
      ),
      # Link renderDT's checkbox to the shiny input (not automatic)
      drawCallback = JS(checkbox_link(id = "selected_fertilizer", ns = ns))
    )
  )
}, server = FALSE)

# Delete fertilizer button click
observeEvent(input$delete_fertilizer, {
  req(nrow(fertilizers()) > 0)     # Ensure there are rows to process
  selected <- which(
    sapply(
      seq_len(nrow(fertilizers())),
      function(i) input[[paste0("selected_fertilizer", i)]]
    )
  )
  if (length(selected)) {
    current_fertilizers <- fertilizers()
    updated_fertilizers <- current_fertilizers[-selected, ]
    fertilizers(updated_fertilizers)
  }
})

# Update fertilizer table data with edited values
observeEvent(input$fertilizer_table_cell_edit, {
  info <- input$fertilizer_table_cell_edit
  new_data <- fertilizers()
  
  # Update the specific cell while preserving the column's data type
  new_data <- update_cell(new_data, info, offset = 2)
  
  fertilizers(new_data)
})
