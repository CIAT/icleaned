# ----------- Livestock tab ------------------------------------------------------
# Sub-module: livestock management
# Sourced from 30_mod_scenario_server.R with local = TRUE
# Initial data frame
livestock_data <- reactiveVal(livestock_data_initialization)

# Add reactive for the selected cell
selected_cell <- reactiveVal()

# Add livestock button click
observeEvent(input$add_livestock, {
  req(input$json_file_name)
  if (modal_open()) return()
  modal_open(TRUE)
  showModal(modalDialog(
    title = "Add livestock",
    h2("Choose a livestock category", class = "mb-3"),
    shinyWidgets::pickerInput(
      ns("livestock"),
      label = NULL,
      choices = setNames(
        lkp_livetype()$livetype_code,
        lkp_livetype()$livetype_desc
      )[sort(lkp_livetype()$livetype_desc)],
      options = list(`live-search` = TRUE)
    ),
    easyClose = TRUE,
    footer = tagList(
      actionButton(ns("ok_add_livestock"), "OK"),
      modalButton("Cancel")
    )
  ))
})

# Add new livestock row from modal
observeEvent(input$ok_add_livestock, {
  req(input$livestock)
  if (!(input$livestock %in% livestock_data()[, "livetype_code"])) {
    selected_livestock <- lkp_livetype()[lkp_livetype()$livetype_code == input$livestock, ]
    manureman_selection <- lkp_manureman()$manureman_desc[1]
    new_row <- data.frame(
      livetype_code = as.character(selected_livestock$livetype_code),
      livetype_desc = selected_livestock$livetype_desc,
      herd_composition = 0,
      annual_milk = 0,
      annual_wool = 0,
      annual_growth = 0,
      time_in_stable = 0,
      manure_in_stable = 0,
      manureman_stable = manureman_selection,
      time_in_non_roofed_enclosure = 0,
      manure_in_non_roofed_enclosure = 0,
      manureman_non_roofed_enclosure = manureman_selection,
      time_in_onfarm_grazing = 0,
      manure_in_field = 0,
      manureman_onfarm_grazing = manureman_selection,
      time_in_offfarm_grazing = 0,
      manureman_offfarm_grazing = manureman_selection,
      distance_to_pasture = 0,
      manure_onfarm_fraction = 0,
      manure_sales_fraction = 0,
      body_weight = selected_livestock$body_weight,
      body_weight_weaning = 0,
      body_weight_year_one = 0,
      adult_weight = 0,
      work_hour = 0,
      litter_size = selected_livestock$litter_size,
      piglets_relying_on_milk = 0,
      lactation_length = selected_livestock$lactation_length,
      proportion_growth_piglets_milk = selected_livestock$proportion_growth_piglets_milk,
      lw_gain_piglets = selected_livestock$lw_gain_piglets,
      cp_maintenance = selected_livestock$cp_maintenance,
      cp_lys_pregnancy = selected_livestock$cp_lys_pregnancy,
      cp_lactmilk = selected_livestock$cp_lactmilk,
      cp_lys_growth = selected_livestock$cp_lys_growth,
      birth_interval = selected_livestock$birth_interval,
      protein_milkcontent = selected_livestock$protein_milkcontent,
      fat_milkcontent = selected_livestock$fat_milkcontent,
      energy_milkcontent = selected_livestock$energy_milkcontent,
      energy_meatcontent = selected_livestock$energy_meatcontent,
      protein_meatcontent = selected_livestock$protein_meatcontent,
      carcass_fraction = selected_livestock$carcass_fraction,
      n_manure_content = selected_livestock$n_manure_content,
      meat_product = selected_livestock$meat_product,
      milk_product = selected_livestock$milk_product,
      ipcc_ef_category_t1 = selected_livestock$ipcc_meth_ef_t1,
      ipcc_ef_category_t2 = selected_livestock$ipcc_meth_ef_t2,
      ipcc_meth_man_category = selected_livestock$ipcc_meth_man,
      ipcc_n_exc_category = selected_livestock$ipcc_meth_exc,
      stringsAsFactors = FALSE
    )
    livestock_data(rbind(livestock_data(), new_row))
  }
  
  # Freeze and restore scroll position for livestock table
  freeze_and_unfreeze_scroll(session, ns("livestock_table"))
  
  shinyWidgets::updatePickerInput(
    session,
    "livestock",
    selected = setNames(lkp_livetype()$livetype_code, lkp_livetype()$livetype_desc)[1]
  )
  removeModal()
})

# Render the table
output$livestock_table <- renderDT({
  livestock_dt <- livestock_data()
  
  # Apply bold and underline formatting to selectInputs text
  livestock_dt[manure_cols] <- lapply(livestock_dt[manure_cols], format_text_displayed)
  
  # Remove the 'livetype_code' column
  livestock_dt <- livestock_dt %>%
    select(-livetype_code)
  
  # Checkboxes for selecting rows (to be deleted)
  livestock_dt$selected_livestock <- generate_shiny_inputs(
    FUN = checkboxInput,
    len = nrow(livestock_dt),
    id = ns("selected_livestock"),
    value = rep(FALSE, nrow(livestock_dt)),
    width = "2px"
  )
  # Move the 'selected_livestock' column to the first position
  livestock_dt <- livestock_dt[, c("selected_livestock", setdiff(names(livestock_dt), "selected_livestock"))]
  
  datatable(
    livestock_dt,
    colnames = livestock_table_colnames,
    editable = list(
      target = "cell",
      # Prevent editing of the first column (check boxes for delete rows)
      disable = list(columns = 0)
    ),
    selection = "none",
    rownames = FALSE,
    escape = FALSE,
    extensions = "FixedColumns",
    options = list(
      scrollX = TRUE,
      dom = "t",
      paging = FALSE,
      fixedColumns = list(leftColumns = 2),
      columnDefs = list(
        list(
          targets = get_column_indices(
            livestock_dt, 
            c("livetype_desc", # Disable editing for livestock type
              "manureman_stable",
              "manureman_non_roofed_enclosure",
              "manureman_onfarm_grazing",
              "manureman_offfarm_grazing",
              "ipcc_ef_category_t1",
              "ipcc_ef_category_t2",
              "ipcc_meth_man_category",
              "ipcc_n_exc_category"
            )
          ) - 1,
          createdCell = JS(disable_all_rows_edit_js()),
          searchable = FALSE
        )
      ),
      # Link renderDT's checkbox to the shiny input (not automatic)
      drawCallback = JS(checkbox_link(id = "selected_livestock", ns = ns))
    )
  ) %>% 
    formatStyle(
      columns = c(
        "herd_composition", "annual_milk", "annual_wool", "annual_growth",
        "time_in_stable", "manure_in_stable", "manureman_stable",
        "time_in_non_roofed_enclosure", "manure_in_non_roofed_enclosure",
        "manureman_non_roofed_enclosure", "time_in_onfarm_grazing",
        "manure_in_field", "manureman_onfarm_grazing", "time_in_offfarm_grazing",
        "manureman_offfarm_grazing", "distance_to_pasture", 
        "manure_onfarm_fraction", "manure_sales_fraction"
      ),
      backgroundColor = "#a9d18e"
    ) %>% 
    formatStyle(
      columns = c(
        "body_weight", "body_weight_weaning", "body_weight_year_one",
        "adult_weight", "work_hour", "litter_size", "piglets_relying_on_milk",
        "lactation_length", "proportion_growth_piglets_milk", "lw_gain_piglets",
        "cp_maintenance", "cp_lys_pregnancy", "cp_lactmilk",
         "cp_lys_growth", "birth_interval", "protein_milkcontent",
        "fat_milkcontent", "energy_milkcontent", "energy_meatcontent",
        "protein_meatcontent", "carcass_fraction", "n_manure_content",
         "meat_product", "milk_product", "ipcc_ef_category_t1",
        "ipcc_ef_category_t2", "ipcc_meth_man_category", "ipcc_n_exc_category"
      ),
      backgroundColor = "#f4b183"
    )
}, server = FALSE)

# Update the table data when edited
observeEvent(input$livestock_table_cell_edit, {
  info <- input$livestock_table_cell_edit
  new_data <- livestock_data()
  
  # Update the specific cell while preserving the column's data type
  new_data <- update_cell(new_data, info, offset = 1)
  
  # Freeze and restore scroll position for livestock table
  freeze_and_unfreeze_scroll(session, ns("livestock_table"))
  
  livestock_data(new_data)
})

# Delete selected row
observeEvent(input$delete_livestock, {
  req(nrow(livestock_data()) > 0)     # Ensure there are rows to process
  selected_row <- which(
    sapply(
      seq_len(nrow(livestock_data())),
      function(i) input[[paste0("selected_livestock", i)]]
    )
  )
  if (length(selected_row) > 0) {
    new_data <- livestock_data()
    new_data <- new_data[-selected_row, ]
    livestock_data(new_data)
  }
  
  # Freeze and restore scroll position for livestock table
  freeze_and_unfreeze_scroll(session, ns("livestock_table"))
})

# ------ LIVESTOCK TIME FRACTIONS VALIDATION ---------------------------------
observeEvent(livestock_data(), {
  # Always hide previous warnings at the start to avoid stale messages
  shinyjs::hide("alert_message_livestock_invalid_values_inputs")
  shinyjs::hide("alert_message_livestock_invalid_sum_inputs")
  
  # Validate input data: ensure data exists and contains at least one row
  time_input <- livestock_data()
  if (is.null(time_input) || nrow(time_input) == 0) return()
  
  # Identify the expected time-fraction columns
  expected_columns <- c(
    "time_in_stable",
    "time_in_non_roofed_enclosure",
    "time_in_onfarm_grazing",
    "time_in_offfarm_grazing"
  )
  
  # Keep only columns that exist in the current dataset
  valid_columns <- intersect(expected_columns, names(time_input))
  if (length(valid_columns) == 0) return()
  
  # Extract subset of time allocation data
  time_data <- time_input[valid_columns]
  
  # Map backend variable names to display labels
  display_labels <- livestock_table_colnames[
    match(valid_columns, names(livestock_data_initialization))
  ]
  
  # Initialize result containers
  invalid_value_messages <- character(0)
  livestock_invalid_values <- character(0)
  invalid_sum_messages <- character(0)
  livestock_invalid_sums <- character(0)
  
  # Check for invalid fraction values (<0 or >1)
  for (i in seq_along(valid_columns)) {
    column_name <- valid_columns[i]
    column_label <- display_labels[i]
    fraction_values <- time_data[[column_name]]
    
    invalid_rows <- which(fraction_values < 0 | fraction_values > 1)
    if (length(invalid_rows) == 0) next
    
    livestock_names <- time_input$livetype_desc[invalid_rows]
    invalid_values <- fraction_values[invalid_rows]
    
    messages <- sprintf(
      "<strong>•</strong> For <strong> %s </strong>, the value in 
       <strong>'%s'</strong> is <strong> %s </strong>. It must be ≥ 0 and ≤ 1!",
      livestock_names, column_label, invalid_values
    )
    
    invalid_value_messages <- c(invalid_value_messages, messages)
    livestock_invalid_values <- c(livestock_invalid_values, livestock_names)
  }
  
  # Check for invalid totals (sum of all four fractions ≠ 1)
  row_sums <- rowSums(time_data, na.rm = TRUE)
  
  # Use a small tolerance when checking equality to 1
  # This avoids false warnings from floating-point rounding errors
  tolerance <- 1e-6
  invalid_sum_rows <- which(abs(row_sums - 1) > tolerance)
  
  if (length(invalid_sum_rows) > 0) {
    livestock_names <- time_input$livetype_desc[invalid_sum_rows]
    total_values <- row_sums[invalid_sum_rows]
    
    # Display decimals only when needed (e.g., 0.9998 vs 1)
    formatted_totals <- ifelse(
      abs(total_values %% 1) < 1e-6,
      as.character(round(total_values, 0)),
      formatC(total_values, format = "f", digits = 4)
    )
    
    messages <- sprintf(
      "<strong>•</strong> For <strong> %s </strong>, the total across
       the four time-fraction columns is <strong> %s </strong>. It must equal 1!",
      livestock_names, formatted_totals
    )
    
    invalid_sum_messages <- c(invalid_sum_messages, messages)
    livestock_invalid_sums <- c(livestock_invalid_sums, livestock_names)
  }
  
  # Sort messages alphabetically by livestock name
  if (length(invalid_value_messages) > 0) {
    order_index <- order(tolower(livestock_invalid_values))
    invalid_value_messages <- invalid_value_messages[order_index]
  }
  
  if (length(invalid_sum_messages) > 0) {
    order_index <- order(tolower(livestock_invalid_sums))
    invalid_sum_messages <- invalid_sum_messages[order_index]
  }
  
  # Display messages if there are issues
  if (length(invalid_value_messages) > 0) {
    shinyjs::html(
      "alert_message_livestock_invalid_values_inputs",
      html = paste(invalid_value_messages, collapse = "<br>")
    )
    shinyjs::show("alert_message_livestock_invalid_values_inputs")
  }
  
  if (length(invalid_sum_messages) > 0) {
    shinyjs::html(
      "alert_message_livestock_invalid_sum_inputs",
      html = paste(invalid_sum_messages, collapse = "<br>")
    )
    shinyjs::show("alert_message_livestock_invalid_sum_inputs")
  }
})

# Show modal dialog to update manure management
observeEvent(input$livestock_table_cell_clicked, {
  info <- input$livestock_table_cell_clicked
  req(length(info) > 0)
  
  if (!is.null(info) && !is.null(info$col) && (names(livestock_data())[info$col + 1] %in% manure_cols)) {
    column_name <- names(livestock_data())[info$col]
    if (modal_open()) return()
    modal_open(TRUE)
    showModal(modalDialog(
      title = paste("Select a manure management type"),
      shinyWidgets::pickerInput(
        inputId = ns("manure_management"),
        label = NULL,
        choices = sort(
          unique(lkp_manureman()$manureman_desc)
        ),
        options = list(`live-search` = TRUE)
      ),
      footer = tagList(
        actionButton(ns("ok_update_manure_management"), "OK"),
        modalButton("Cancel")
      )
    ))
    
    selected_cell(info)
  }
})

# Update manure management in the table
observeEvent(input$ok_update_manure_management, {
  req(input$manure_management)
  selected_cell <- selected_cell()
  if (!is.null(selected_cell)) {
    new_data <- livestock_data()
    column_name <- names(new_data)[selected_cell$col + 1]
    new_data[selected_cell$row, column_name] <- input$manure_management
    livestock_data(new_data)
  }
  shinyWidgets::updatePickerInput(
    session,
    "manure_management",
    selected = lkp_manureman()$manureman_desc[1]
  )
  removeModal()
  
  # Freeze and restore scroll position for livestock table
  freeze_and_unfreeze_scroll(session, ns("livestock_table"))
})
