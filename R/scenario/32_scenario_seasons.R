# ----------- Seasons Tab --------------------------------------------------------
# Sub-module: seasons management (reactive, observers, table render)
# Sourced from 30_mod_scenario_server.R with local = TRUE
# Reactive value to store the seasons data
seasons <- reactiveVal(seasons_initialization)

# Add season button click
observeEvent(input$add_season, {
  req(input$json_file_name)
  if (modal_open()) return()
  modal_open(TRUE)
  showModal(modalDialog(
    title = "Add season",
    h2("Season name:", class = "mb-3"),
    textInput(ns("season_name"), label = NULL),
    easyClose = TRUE,
    footer = tagList(
      actionButton(ns("ok_add_season"), "OK"),
      modalButton("Cancel")
    )
  ))
})

# OK button in modal dialog for adding season
observeEvent(input$ok_add_season, {
  req(input$season_name)
  current_seasons <- seasons()
  if (!input$season_name %in% current_seasons$Season) {
    days_left <- max(365 - sum(current_seasons$Days), 0)
    new_row <- data.frame(Season = input$season_name, Days = days_left)
    updated_seasons <- rbind(current_seasons, new_row)
    seasons(updated_seasons)
  }
  updateTextInput(session, "season_name", value = "")
  removeModal()
})

# Render the table
output$season_table <- renderDT({
  
  # Re-initialize the season data if the columns are missing (show empty table on start)
  if (ncol(seasons()) == 0) {
    seasons(seasons_initialization)
  }
  
  season_dt <- seasons()
  
  # Checkboxes for selecting rows (to be deleted)
  season_dt$selected_season <- generate_shiny_inputs(
    FUN = checkboxInput,
    len = nrow(season_dt),
    id = ns("selected_season"),
    value = rep(FALSE, nrow(season_dt)),
    width = "2px"
  )
  # Move the 'selected_season' column to the first position
  season_dt <- season_dt[, c("selected_season", setdiff(names(season_dt), "selected_season"))]
  
  datatable(
    season_dt, 
    editable = list(
      target = "cell",
      # Prevent editing of the first column (check boxes for delete rows)
      disable = list(columns = 0)
    ), 
    escape = FALSE,
    selection = "none",
    rownames = FALSE,
    # Set the first column (selected) name to an empty string
    colnames = c("", colnames(season_dt)[-1]),
    options = list(
      dom = "t", 
      paging = FALSE,
      columnDefs = list(
        # 2 px width for the first column (checkboxes)
        list(width = "50px", targets = 0)  
      ),
      # Link renderDT's checkbox to the shiny input (not automatic)
      drawCallback = JS(checkbox_link(id = "selected_season", ns = ns))
    )
  )
}, server = FALSE)

# Delete season button click
observeEvent(input$delete_season, {
  req(nrow(seasons()) > 0)     # Ensure there are rows to process
  selected <- which(
    sapply(
      seq_len(nrow(seasons())),
      function(i) input[[paste0("selected_season", i)]]
    )
  )
  if (length(selected)) {
    current_seasons <- seasons()
    updated_seasons <- current_seasons[-selected, ]
    seasons(updated_seasons)
  }
})

# Update season table data with edited values
observeEvent(input$season_table_cell_edit, {
  info <- input$season_table_cell_edit
  new_data <- seasons()
  
  # Update the specific cell while preserving the column's data type
  new_data <- update_cell(new_data, info, offset = 0)
  
  seasons(new_data)
})

# Observe the season data and show/hide the error message
observeEvent(seasons(), {
  # if the sum of days is 365, hide the error message
  if (nrow(seasons()) == 0 || sum(seasons()$Days) == 365) {
    shinyjs::hide(id = "alert_message_season")
  } else {
    shinyjs::show(id = "alert_message_season")
  }
})
