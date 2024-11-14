scenario_server <- function(
    id
) { moduleServer(id, function(input, output, session) {
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  # Add reactive boolean in order to block double show modals (incompatibility between
  # BS v6 and shiny - only detectable from server side) 
  modal_open <- reactiveVal(FALSE)
  
  # Observe the JavaScript signal that modal has closed
  observeEvent(input$modal_closed, {
    modal_open(FALSE)  # Reset the flag once modal is closed
  })
  
  observe({
    # Update the Json selectInput with the list of study objects
    shinyWidgets::updatePickerInput(
      session,
      "json_file_name",
      choices = list.files(
        path = file.path(session$userData$user_folder, "study_objects"),
        full.names = FALSE
      ),
      selected = character(0)
    )
    
    # Update the shared_folder selectInput with the list of examples
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "json_shared_folder",
      choices = list.files(
        path = file.path("data", "shared_folder", "study_objects"),
        full.names = FALSE
      ),
      selected = character(0)
    )
    
    # Update the database selectInput with the list of databases
    shinyWidgets::updatePickerInput(
      session,
      "database_code",
      choices = list.files(
        path = file.path(session$userData$user_folder, "parameters_database"),
        full.names = FALSE
      ),
      selected = character(0)
    )
    
    # ------ Last modification date ------------------------------------------
    # most recent file in data/objects
    files_info <- file.info(
      list.files(
        file.path(session$userData$user_folder, "study_objects"),
        full.names = TRUE
      )
    )
    # Get the last modification date
    last_modification_date <- format(
      files_info$mtime[which.max(files_info$mtime)], "%B %d, %Y"
    )
    shinyjs::html(id = "last_update_date", html = last_modification_date)
  })
  
  # -----  * Show Scenario Editor Section after selection ----------------------
  observeEvent(input$json_file_name, {
    shinyjs::show(id = "scenario_editor_section")
  }, once = TRUE)
  
  # ------ * Initialization of params ------------------------------------------
  lapply(
    parameters_db_names,
    function(db_name) {
      assign(
        db_name,
        reactiveVal(),
        envir = parent.env(environment())
      )
    }
  )
  
  # ------ * Update the database PickerInput with the list of databases --------
  observeEvent(session$userData$databases(), {
    shinyWidgets::updatePickerInput(
      session,
      "database_code",
      choices = session$userData$databases(),
      selected = input$database_code
    )
  })
  
  # ------ * Update params accordingly with session variable  ------------------
  observe({
    req(input$database_code)
    if (input$database_code == session$userData$parameters_db_name()) {
      lapply(
        parameters_db_names,
        function(db_name) {
          get(db_name)(session$userData$parameters_db[[db_name]])
        }
      )
    }
  })
  
  # ------ * Observe update button click inside scenario input module ----------
  observeEvent(session$userData$observe_update_params_button_click(), {
    click_by_prefix("update_params_module")  # regardless of the NS
  }, ignoreInit = TRUE)
  
  # ------ * Store checked boxes in DTs ----------------------------------------
  checked_boxes <- reactiveValues(intercrop_checked = NULL)
  
  # ------ * Update the PickerInputs after Super User Cloning
  observeEvent(session$userData$super_user_cloned_folder(), {
    if (session$userData$super_user_cloned_folder() == "study_objects") {
      shinyWidgets::updatePickerInput(
        session,
        "json_file_name",
        choices = list.files(
          path = file.path(session$userData$user_folder, "study_objects"),
          full.names = FALSE
        ),
        selected = input$json_file_name
      )
      
      # Reset the cloned folder indicator
      session$userData$super_user_cloned_folder(NULL)
      
    } else if (session$userData$super_user_cloned_folder() == "parameters_database") {
      shinyWidgets::updatePickerInput(
        session,
        "database_code",
        choices = list.files(
          path = file.path(session$userData$user_folder, "parameters_database"),
          full.names = FALSE
        ),
        selected = input$database_code
      )
      
      # Reset the cloned folder indicator
      session$userData$super_user_cloned_folder(NULL)
    }
  })
  
  # ----- UX Interaction logic --------------------------------------------
  # Observe the selection from the radio buttons
  observeEvent(input$scenario_folder, {
    if (input$scenario_folder == "user") {
      # Show User Folder UI and hide the others
      shinyjs::show(id = "user_folder_ui")
      shinyjs::hide(id = "shared_examples_ui")
      shinyjs::hide(id = "shared_pool_ui")
      
    } else if (input$scenario_folder == "shared") {
      # Show Shared Examples UI and hide the others
      shinyjs::show(id = "shared_examples_ui")
      shinyjs::hide(id = "user_folder_ui")
      shinyjs::hide(id = "shared_pool_ui")
      
    } else if (input$scenario_folder == "pool") {
      # Show Shared Pool UI and hide the others
      shinyjs::show(id = "shared_pool_ui")
      shinyjs::hide(id = "user_folder_ui")
      shinyjs::hide(id = "shared_examples_ui")
    }
  })
  # on renaming a scenario
  observeEvent(input$rename_json, {
    req(input$json_file_name)
    shinyjs::show(id = "rename_scenario_section")
    shinyjs::hide(id = "select_scenario_section")
    shinyjs::hide(id = "user_folder_ui")
  })
  # on canceling the renaming of a scenario
  observeEvent(input$cancel_json_rename, {
    shinyjs::show(id = "select_scenario_section")
    shinyjs::show(id = "user_folder_ui")
    shinyjs::hide(id = "rename_scenario_section")
  })
  # on creating a new scenario
  observeEvent(input$create_new_json, {
    shinyjs::show(id = "select_scenario_section")
    shinyjs::hide(id = "upload_scenario_section")
    shinyjs::hide(id = "rename_scenario_section")
  })
  # on deleting a scenario
  observeEvent(input$delete_json, {
    req(input$json_file_name)
    shinyWidgets::ask_confirmation(
      inputId = ns("delete_scenario_json_confirmation"),
      type = "warning",
      title = "Delete Scenario?",
      btn_colors = c("grey", "#009ADB")
    )
  })
  # on confirming the deletion of a scenario
  observeEvent(input$delete_scenario_json_confirmation, {
    req(input$delete_scenario_json_confirmation)
    source_file_path <- file.path(
      session$userData$user_folder, "study_objects", input$json_file_name
    )
    file.remove(source_file_path)
    
    # Update the selectInput
    shinyWidgets::updatePickerInput(
      session = session, 
      inputId = "json_file_name",
      choices = list.files(
        path = file.path(session$userData$user_folder, "study_objects"),
        full.names = FALSE
      )
    )
    
    # Json deletion success
    showNotification(
      "The JSON has been deleted successfully!",
      duration = 3,
      type = "message"
    )
  })
  
  # on sharing Json & Database to pool
  observeEvent(input$share_to_pool, {
    req(input$json_file_name)
    shinyWidgets::ask_confirmation(
      inputId = ns("share_to_pool_confirmation"),
      type = "message",
      title = "Share the JSON and the related Parameters Database with other users?",
      btn_colors = c("grey", "#009ADB")
    )
  })
  # on confirming the sharing of a scenario
  observeEvent(input$share_to_pool_confirmation, {
    req(input$share_to_pool_confirmation)
    
    # Path for the original file
    source_file_path <- file.path(
      session$userData$user_folder, "study_objects", input$json_file_name
    )
    
    # Path for the cloned file
    share_file_path <- file.path(
      Sys.getenv("DATA_DIR"), "shared_pool", "study_objects", input$json_file_name
    )
    
    if (file.exists(source_file_path)) {
      file.copy(source_file_path, share_file_path, overwrite = TRUE)
      
      # Share the related database if it's not the default database
      if (input$database_code != "Params DB - Default") {
        # Define the source and destination paths for the database directory
        db_path <- file.path(
          session$userData$user_folder, "parameters_database", input$database_code
        )
        destination_dir <- file.path(Sys.getenv("DATA_DIR"), "shared_pool", "parameters_database")
        
        # Use versioned_copy to copy the database directory with versioning
        versioned_copy(
          source_path = db_path,
          destination_dir = destination_dir,
          base_name = input$database_code,
          versioning = FALSE  # Disable versioning to overwrite any existing folder
        )
      }
      
      # Show a success notification for JSON sharing
      showNotification(
        "The JSON has been shared successfully!",
        duration = 3,
        type = "message"
      )
    }
  })
  
  # ----------- Json Management Tab --------------------------------------------
  
  # ------ * Create JSON file
  observeEvent(input$create_new_json, {
    # Define the base name for the new JSON file
    default_name <- "New_file"
    
    # Define the source and destination paths
    source_path <- file.path("data", "primary_json.json")
    destination_dir <- file.path(session$userData$user_folder, "study_objects")
    
    # Use versioned_copy to create the JSON file with versioning
    new_file_path <- versioned_copy(
      source_path = source_path,
      destination_dir = destination_dir,
      base_name = default_name,
      extension = "json",
      versioning = TRUE
    )
    
    # Update the selectInput with the new file
    shinyWidgets::updatePickerInput(
      session = session, 
      inputId = "json_file_name",
      choices = list.files(destination_dir, full.names = FALSE),
      selected = basename(new_file_path)
    )
    
    # Show a success notification for JSON creation
    showNotification(
      "The JSON has been created successfully!",
      duration = 3,
      type = "message"
    )
    
  })
  
  # Confirm rename action
  observeEvent(input$confirm_json_rename, {
    # Define file paths
    source_file_path <- file.path(
      session$userData$user_folder, "study_objects", input$json_file_name
    )
    new_file_path <- file.path(
      session$userData$user_folder, "study_objects", paste0(input$json_new_name, ".json")
    )
    
    if (file.exists(new_file_path)) {
      # Show error message if new name already exists
      showNotification(
        "The new name is already in use. Please choose a different name.",
        duration = 5,
        type = "error"
      )
      
    } else if (file.exists(source_file_path) && input$json_new_name != "") {
      file.rename(source_file_path, new_file_path)
      
      # Update the selectInput with the new name
      shinyWidgets::updatePickerInput(
        session = session, 
        inputId = "json_file_name",
        choices = list.files(
          path = file.path(session$userData$user_folder, "study_objects"),
          full.names = FALSE
        ),
        selected = paste0(input$json_new_name, ".json")
      )
      
      # Json renaming success
      showNotification(
        "The JSON has been renamed successfully!",
        duration = 3,
        type = "message"
      )
      
      # Switch to select section after success
      shinyjs::show(id = "select_scenario_section")
      shinyjs::show(id = "user_folder_ui")
      shinyjs::hide(id = "rename_scenario_section")
    }
  })
  
  # ------ * Clone JSON file
  observeEvent(input$clone_json, {
    req(input$json_file_name)
    # Path for the original file and the destination directory
    source_file_path <- file.path(
      session$userData$user_folder, "study_objects", input$json_file_name
    )
    destination_dir <- file.path(session$userData$user_folder, "study_objects")
    
    # Set the base name for the cloned file
    base_clone_file_name <- tools::file_path_sans_ext(input$json_file_name)
    
    # Use versioned_copy to create a versioned clone of the JSON file
    clone_file_path <- versioned_copy(
      source_path = source_file_path,
      destination_dir = destination_dir,
      base_name = base_clone_file_name,
      extension = "json",
      versioning = TRUE
    )
    
    # Update the selectInput with the cloned file
    shinyWidgets::updatePickerInput(
      session = session, 
      inputId = "json_file_name",
      choices = list.files(destination_dir, full.names = FALSE),
      selected = basename(clone_file_path)
    )
    
    # Show a success notification for JSON cloning
    showNotification(
      "The JSON has been cloned successfully!",
      duration = 3,
      type = "message"
    )
    
  })
  
  # Load the parameters_db depending on the current database code --------------
  observeEvent(input$database_code, {
    req(input$database_code)
    cat(file = stderr(), "20 - Loading the parameters database...\n")
    
    selected_db <- input$database_code
    
    db_path <- file.path(
      session$userData$user_folder, "parameters_database", selected_db
    )
    db_files <- list.files(db_path, full.names = TRUE)
    
    lapply(seq_along(db_files), function(i) {
      session$userData$parameters_db[[
        tools::file_path_sans_ext(basename(db_files[i]))
      ]] <- fread(db_files[i])
    })
    
    # Update the session database name
    session$userData$parameters_db_name(selected_db)
    
    # Update the session database code
    session$userData$database_code(selected_db)
  })
  
  # ----------- Shared Folder --------------------------------------------------
  # Observe the selected shared example
  observeEvent(input$json_shared_folder, {
    # Show a modal asking if they want to clone the file
    shinyWidgets::ask_confirmation(
      inputId = ns("confirm_clone_shared_example"),
      type = "question",
      title = "Do you want to clone this shared JSON Example to your folder?",
      btn_colors = c("grey", "#009ADB")
    )
  })
  
  # Observe Cloning the shared JSON example
  observeEvent(input$confirm_clone_shared_example, {
    
    # Check if the user confirmed or cancelled the cloning
    if (isTRUE(input$confirm_clone_shared_example)) {
      
      # Path for the original file and the destination directory
      source_file_path <- file.path(
        "data", "shared_folder", "study_objects", input$json_shared_folder
      )
      destination_dir <- file.path(session$userData$user_folder, "study_objects")
      
      # Set the base name for the cloned file
      base_clone_file_name <- tools::file_path_sans_ext(input$json_shared_folder)
      
      # Use versioned_copy to create a versioned clone of the JSON file
      clone_file_path <- versioned_copy(
        source_path = source_file_path,
        destination_dir = destination_dir,
        base_name = base_clone_file_name,
        extension = "json",
        versioning = TRUE
      )
      
      # Reset the Picker Input for the shared JSON
      shinyWidgets::updatePickerInput(
        session,
        "json_shared_folder",
        selected = character(0)
      )
      
      # Update the radio button value to 'user'
      updateRadioButtons(session, inputId = "scenario_folder", selected = "user")
      
      # Update the Json PickerInput to the cloned file
      shinyWidgets::updatePickerInput(
        session = session, 
        inputId = "json_file_name",
        choices = list.files(destination_dir, full.names = FALSE),
        selected = basename(clone_file_path)
      )
      
      # Show a success notification for JSON cloning
      showNotification(
        "The JSON example has been cloned successfully!",
        duration = 3,
        type = "message"
      )
    } else {
      # Reset the picker input
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "json_shared_folder",
        selected = character(0)
      )
    }
  })
  
  # ----------- Shared Pool ----------------------------------------------------
  # Observe the Ok actionbutton
  observeEvent(input$pool_search_ok, {
    # Verify if the entered Json name is available in the shared pool folder
    if (input$pool_file_search != "" &&
        input$pool_file_search %in% list.files(
          file.path(Sys.getenv("DATA_DIR"), "shared_pool", "study_objects")
        )) {
      # Show a modal asking if they want to clone the file
      shinyWidgets::ask_confirmation(
        inputId = ns("confirm_clone_shared_pool"),
        type = "question",
        title = "Do you want to clone this shared JSON and the related Parameters Database to your folder?",
        btn_colors = c("grey", "#009ADB")
      )
    } else {
      # Show an error message if the file does not exist
      showNotification(
        "The JSON file does not exist in the shared pool folder.",
        duration = 5,
        type = "error"
      )
    }
  })
    # Observe Cloning the shared JSON from the pool
    observeEvent(input$confirm_clone_shared_pool, {
      
      # Check if the user confirmed the cloning
      if (isTRUE(input$confirm_clone_shared_pool)) {
        
        # Path for the original file in the shared pool
        source_file_path <- file.path(
          Sys.getenv("DATA_DIR"), "shared_pool", "study_objects", input$pool_file_search
        )
        
        # Set the base name for the cloned JSON file
        base_clone_file_name <- tools::file_path_sans_ext(input$pool_file_search)
        
        # Define the destination directory for the cloned JSON file
        destination_dir <- file.path(session$userData$user_folder, "study_objects")
        
        # Use versioned_copy to create a versioned clone of the JSON file
        clone_file_path <- versioned_copy(
          source_path = source_file_path,
          destination_dir = destination_dir,
          base_name = base_clone_file_name,
          extension = "json",
          versioning = TRUE
        )
        
        # Read the json file and get the related database's name
        json_data <- fromJSON(source_file_path)
        db_name <- json_data$database_code
        
        # If the database is not the default database and is available on
        # the shared_pool folder, clone the database as well
        if (db_name != "Params DB - Default" &&
            db_name %in% list.files(
              file.path(Sys.getenv("DATA_DIR"), "shared_pool", "parameters_database")
            )) {
          # Define the source and destination paths for the database directory
          db_path <- file.path(
            Sys.getenv("DATA_DIR"), "shared_pool", "parameters_database", db_name
          )
          destination_dir <- file.path(session$userData$user_folder, "parameters_database")
          
          # Use versioned_copy to clone the database directory with versioning
          versioned_copy(
            source_path = db_path,
            destination_dir = destination_dir,
            base_name = db_name,
            versioning = TRUE
          )
          
          #Update the database picker input with the cloned database
          shinyWidgets::updatePickerInput(
            session = session, 
            inputId = "database_code",
            choices = list.files(destination_dir, full.names = FALSE),
            selected = db_name
          )
        }
        
        # Reset the Text Input
        updateTextInput(session, "pool_file_search", value = "")
        
        # Update the radio button value to 'user'
        updateRadioButtons(session, inputId = "scenario_folder", selected = "user")
        
        # Update the Json PickerInput to the cloned file
        shinyWidgets::updatePickerInput(
          session = session, 
          inputId = "json_file_name",
          choices = list.files(
            file.path(session$userData$user_folder, "study_objects"), full.names = FALSE
          ),
          selected = basename(clone_file_path)
        )
        
        # Show a success notification
        showNotification(
          "The JSON and the related Parameters Database have been cloned successfully!",
          duration = 3,
          type = "message"
        )
      }
    })
  
  # ----------- Seasons Tab ----------------------------------------------------
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
  
  # ----------- Manure/Fertilizer Tab ------------------------------------------
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
        "Fertilizer",
        choices = setNames(
          lkp_orgfertilizer()$fertilizer_code,
          lkp_orgfertilizer()$fertilizer_desc
        )
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
  
  # ----------- Area tab -------------------------------------------------------
  observeEvent(input$soil_description, {
    selected_soil <- input$soil_description
    k_value <- lkp_soil()$k_value[lkp_soil()$soil_desc == selected_soil]
    updateNumericInput(session, "soil_k_value", value = k_value)
  })
  
  observeEvent(input$cropland_system, {
    selected_system <- input$cropland_system
    change_factor <- lkp_croplandsystem()$change_factor[lkp_croplandsystem()$sys_desc == selected_system]
    updateNumericInput(session, "cropland_system_ipcc", value = change_factor)
  })
  
  observeEvent(input$cropland_tillage, {
    selected_tillage <- input$cropland_tillage
    change_factor <- lkp_tillageregime()$change_factor[lkp_tillageregime()$tillage_desc == selected_tillage]
    updateNumericInput(session, "cropland_tillage_ipcc", value = change_factor)
  })
  
  observeEvent(input$cropland_orgmatter, {
    selected_input <- input$cropland_orgmatter
    change_factor <- lkp_organicmatter()$change_factor[lkp_organicmatter()$orgmatter_desc == selected_input]
    updateNumericInput(session, "cropland_orgmatter_ipcc", value = change_factor)
  })
  
  observeEvent(input$grassland_management, {
    selected_management <- input$grassland_management
    change_factor <- lkp_grasslandman()$change_factor[
      lkp_grasslandman()$management_desc == selected_management
    ]
    updateNumericInput(session, "grassland_management_ipcc", value = change_factor)
  })
  
  observeEvent(input$grassland_implevel, {
    selected_input_level <- input$grassland_implevel
    change_factor <- lkp_grassinputlevel()$change_factor[
      lkp_grassinputlevel()$grassinputlevel_desc == selected_input_level
    ]
    updateNumericInput(session, "grassland_implevel_ipcc", value = change_factor)
  })
  
  # ----------- Livestock tab --------------------------------------------------
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
        choices = setNames(lkp_livetype()$livetype_code, lkp_livetype()$livetype_desc)
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
        proportion_growth = selected_livestock$proportion_growth,
        lw_gain = selected_livestock$lw_gain,
        grazing_displacement = selected_livestock$grazing_displacement,
        cp_maintenance = selected_livestock$cp_maintenance,
        cp_grazing = selected_livestock$cp_grazing,
        cp_pregnancy = selected_livestock$cp_pregnancy,
        cp_lactation = selected_livestock$cp_lactation,
        cp_lactmilk = selected_livestock$cp_lactmilk,
        cp_growth = selected_livestock$cp_growth,
        birth_interval = selected_livestock$birth_interval,
        protein_milkcontent = selected_livestock$protein_milkcontent,
        fat_content = selected_livestock$fat_content,
        energy_milkcontent = selected_livestock$energy_milkcontent,
        energy_meatcontent = selected_livestock$energy_meatcontent,
        protein_meatcontent = selected_livestock$protein_meatcontent,
        carcass_fraction = selected_livestock$carcass_fraction,
        energy_eggcontent = selected_livestock$energy_eggcontent,
        n_content = selected_livestock$n_content,
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
          "lactation_length", "proportion_growth", "lw_gain", "grazing_displacement",
          "cp_maintenance", "cp_grazing", "cp_pregnancy", "cp_lactation",
          "cp_lactmilk", "cp_growth", "birth_interval", "protein_milkcontent",
          "fat_content", "energy_milkcontent", "energy_meatcontent",
          "protein_meatcontent", "carcass_fraction", "energy_eggcontent",
          "n_content", "meat_product", "milk_product", "ipcc_ef_category_t1",
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
          choices = unique(lkp_manureman()$manureman_desc)
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
  
  # ----------- Feed production tab --------------------------------------------
  # Initial data frame for crops
  feedtype <- reactiveVal(feedtype_initialization)
  # Add reactive for the selected cell
  selected_cell <- reactiveVal()
  
  # Initial data frame for crop inputs
  crop_inputs_data <- reactiveVal(crop_inputs_data_initialization)
  
  # Add crop button click
  observeEvent(input$add_crop, {
    req(input$json_file_name)
    if (modal_open()) return()
    modal_open(TRUE)
    showModal(modalDialog(
      title = "Add feed",
      h2("Selected a Feed", class = "mb-3"),
      shinyWidgets::pickerInput(
        inputId = ns("feed"),
        label = NULL,
        choices = setNames(lkp_feeditem()$feed_item_code, lkp_feeditem()$feed_item_name)
      ),
      br(),
      h2("Selected a Crop", class = "mb-3"),
      shinyWidgets::pickerInput(
        inputId = ns("crop"),
        label = NULL,
        choices = NULL
      ),
      easyClose = TRUE,
      footer = tagList(
        actionButton(ns("ok_add_crop"), "OK"),
        modalButton("Cancel")
      )
    ))
  })
  
  # Update second select input "crop" depending on the first input "feed"
  observeEvent(input$feed, {
    feed_type_code <- lkp_feeditem()$feed_type_code[lkp_feeditem()$feed_item_code == input$feed]
    choices <- setNames(
      lkp_feedtype()$feed_type_code[lkp_feedtype()$feed_type_code == feed_type_code],
      lkp_feedtype()$feed_type_name[lkp_feedtype()$feed_type_code == feed_type_code]
    )
    # remove NA values
    choices <- choices[!is.na(choices)]
    shinyWidgets::updatePickerInput(
      session,
      "crop",
      choices = choices
    )
  })
  
  # Add new crop row from modal
  observeEvent(input$ok_add_crop, {
    req(input$crop, input$feed)
    if (!((input$crop %in% feedtype()[, "feed_type_code"]) && (input$feed %in% feedtype()[, "feed_item_code"]))) {
      new_row <- data.frame(
        feed_type_code = input$crop,
        feed_item_code = input$feed,
        feed_item_name = lkp_feeditem()$feed_item_name[lkp_feeditem()$feed_item_code == input$feed],
        feed_type_name = lkp_feedtype()$feed_type_name[lkp_feedtype()$feed_type_code == input$crop],
        source_type = "Main", # Only column that is hard coded like the qt app
        intercrop = 0,
        intercrop_fraction = 0,
        cut_carry_fraction = 0,
        land_cover_desc = lkp_landcover()$landcover_desc[1],
        slope_desc = lkp_slope()$slope_desc[1],
        slope_length = 15,
        grassman_desc = lkp_grasslandman()$management_desc[1],
        main_product_removal = 0,
        residue_removal = 0,
        residue_burnt = 0,
        dm_content = lkp_feeditem()$dm_content[lkp_feeditem()$feed_item_code == input$feed],
        me_content = lkp_feeditem()$me_content[lkp_feeditem()$feed_item_code == input$feed],
        cp_content = lkp_feeditem()$cp_content[lkp_feeditem()$feed_item_code == input$feed],
        water_regime = "",
        cultivation_period = 0,
        ecosystem_type = "",
        organic_amendment = "",
        grassman_change_factor = lkp_grasslandman()$change_factor[1],
        landcover_c_factor = lkp_landcover()$c_factor[1],
        slope_p_factor = lkp_slope()$p_factor[1],
        dry_yield = lkp_feedtype()$dry_yield[lkp_feedtype()$feed_type_code == input$crop],
        residue_dry_yield = lkp_feedtype()$residue_dry_yield[lkp_feedtype()$feed_type_code == input$crop],
        n_content = 0,
        residue_n = lkp_feedtype()$residue_n[lkp_feedtype()$feed_type_code == input$crop],
        kc_initial = lkp_feedtype()$kc_initial[lkp_feedtype()$feed_type_code == input$crop],
        kc_midseason = lkp_feedtype()$kc_midseason[lkp_feedtype()$feed_type_code == input$crop],
        kc_late = lkp_feedtype()$kc_late[lkp_feedtype()$feed_type_code == input$crop],
        category = lkp_feedtype()$category[lkp_feedtype()$feed_type_code == input$crop],
        trees_ha = lkp_feedtype()$trees_ha[lkp_feedtype()$feed_type_code == input$crop],
        trees_dhb = lkp_feedtype()$trees_dhb[lkp_feedtype()$feed_type_code == input$crop],
        trees_growth = lkp_feedtype()$trees_growth[lkp_feedtype()$feed_type_code == input$crop],
        trees_removal = lkp_feedtype()$trees_removal[lkp_feedtype()$feed_type_code == input$crop],
        trees_ha_dbh25 = lkp_feedtype()$trees_ha_dbh25[lkp_feedtype()$feed_type_code == input$crop],
        average_dbh25 = lkp_feedtype()$average_dbh25[lkp_feedtype()$feed_type_code == input$crop],
        increase_dbh25 = lkp_feedtype()$increase_dbh25[lkp_feedtype()$feed_type_code == input$crop],
        trees_ha_dbh2550 = lkp_feedtype()$trees_ha_dbh2550[lkp_feedtype()$feed_type_code == input$crop],
        average_dbh2550 = lkp_feedtype()$average_dbh2550[lkp_feedtype()$feed_type_code == input$crop],
        increase_dbh2550 = lkp_feedtype()$increase_dbh2550[lkp_feedtype()$feed_type_code == input$crop],
        trees_ha_dbh50 = lkp_feedtype()$trees_ha_dbh50[lkp_feedtype()$feed_type_code == input$crop],
        average_dbh50 = lkp_feedtype()$average_dbh50[lkp_feedtype()$feed_type_code == input$crop],
        increase_dbh50 = lkp_feedtype()$increase_dbh50[lkp_feedtype()$feed_type_code == input$crop],
        time_horizon = lkp_feedtype()$time_horizon[lkp_feedtype()$feed_type_code == input$crop],
        diameter_breast = lkp_feedtype()$diameter_breast[lkp_feedtype()$feed_type_code == input$crop],
        # These ones are available in the json but not in the DT
        fraction_as_manure = "NULL", # We should get null in the json
        n_fertilizer = "NULL", # We should get null in the json
        main_n = lkp_feedtype()$main_n[lkp_feedtype()$feed_type_code == input$crop],
        land_cover = lkp_landcover()$landcover_code[1],
        slope = lkp_slope()$slope_code[1],
        grassman = lkp_grasslandman()$management_code[1],
        stringsAsFactors = FALSE
      )
      
      new_input_row <- data.frame(
        Feed = lkp_feeditem()$feed_item_name[lkp_feeditem()$feed_item_code == input$feed],
        Crop = lkp_feedtype()$feed_type_name[lkp_feedtype()$feed_type_code == input$crop],
        fraction_as_fertilizer = 0,
        urea = 0,
        npk = 0,
        dap = 0,
        ammonium_nitrate = 0,
        ammonium_sulfate = 0,
        n_solutions = 0,
        ammonia = 0,
        stringsAsFactors = FALSE
      )
      feedtype(rbind(feedtype(), new_row))
      crop_inputs_data(rbind(crop_inputs_data(), new_input_row))
      # updated intercrop_checked
      checked_boxes$intercrop_checked <- c(checked_boxes$intercrop_checked, FALSE)
    }
    
    # Freeze and restore scroll position for crop table and crop inputs table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
    freeze_and_unfreeze_scroll(session, ns("crop_inputs_table"))
    
    # Reset the Picker input
    shinyWidgets::updatePickerInput(
      session,
      "feed",
      selected = character(0)
    )
    
    removeModal()
  })
  
  # Render the crop table
  output$crop_table <- renderDT({
    feedtype_dt <- feedtype() # TODO: copy() when using data.table!
    
    # Identify indices of non-grass categories
    rows_not_contains_grass <- which(feedtype_dt$category != "grass") - 1 # index for js
    
    # Identify indices of non-rice crops
    rows_not_contains_rice <- which(feedtype_dt$feed_type_name != "Rice") - 1 # index for js
    
    # Identify indices of rows to disable depending on the source type
    rows_not_residue <- which(feedtype_dt$source_type != "Residue") - 1 # index for js
    
    # Reset the disabled cells to the value 0
    if (length(rows_not_contains_grass) > 0) {
      feedtype_dt[rows_not_contains_grass + 1, "grassman_change_factor"] <- lkp_grasslandman()$change_factor[1]
      feedtype_dt[rows_not_contains_grass + 1, "grassman_desc"] <- lkp_grasslandman()$management_desc[1]
    }
    
    if (length(rows_not_residue) > 0) {
      feedtype_dt[rows_not_residue + 1, "residue_removal"] <- 0
      feedtype_dt[rows_not_residue + 1, "residue_burnt"] <- 0
    }
    
    # Update feedtype with the modified feedtype_dt
    feedtype(feedtype_dt)
    
    # Checkboxes for enabling/disabling intercrop
    feedtype_dt$intercrop <- generate_shiny_inputs(
      FUN = checkboxInput,
      len = nrow(feedtype_dt),
      id  = ("intercrop_check"),
      value = checked_boxes$intercrop_checked
    )
    
    # Checkboxes for selecting rows (to be deleted)
    feedtype_dt$selected_feedtype <- generate_shiny_inputs(
      FUN = checkboxInput,
      len = nrow(feedtype_dt),
      id = ns("selected_feedtype"),
      value = rep(FALSE, nrow(feedtype_dt)),
      width = "2px"
    )
    
    # Identify indices of rows to disable based on intercrop checkbox
    if (!is.null(checked_boxes$intercrop_checked)) {
      rows_disable_intercrop <- which(!checked_boxes$intercrop_checked) - 1 # index for js
    } else {
      rows_disable_intercrop <- c()
    }
    
    # Apply bold and underline formatting to selectInputs text
    columns_to_format <- c(
      "source_type", "land_cover_desc", "slope_desc", "grassman_desc",
      "water_regime", "ecosystem_type", "organic_amendment", "category"
    )
    feedtype_dt[columns_to_format] <- lapply(feedtype_dt[columns_to_format], format_text_displayed)
    
    # Move the 'selected_feedtype' column to the first position
    feedtype_dt <- feedtype_dt[, c("selected_feedtype", setdiff(names(feedtype_dt), "selected_feedtype"))]
    
    feedtype_dt <- feedtype_dt %>%
      select(
        -feed_type_code, -feed_item_code, -fraction_as_manure, -n_fertilizer,
        -main_n, -slope, -grassman, -land_cover
      )
    
    datatable(
      feedtype_dt,
      colnames = feedtype_colnames,
      editable = list(
        target = "cell",
        # Prevent editing of the first column (check boxes for delete rows)
        disable = list(columns = 0)
      ),
      selection = "none",
      escape = FALSE,
      callback = JS(js(ns)), # send checking action to checkbox_info input
      rownames = FALSE,
      extensions = "FixedColumns",
      options = list(
        scrollX = TRUE,
        dom = "t",
        paging = FALSE,
        fixedColumns = list(leftColumns = 2),
        columnDefs = list(
          # Définit la largeur de la colonne des cases à cocher
          list(width = "2px", targets = 0),
          list(
            targets = get_column_indices(feedtype_dt, "intercrop_fraction") - 1,
            createdCell = JS(disable_specific_rows_edit_js(rows_disable_intercrop)),
            searchable = FALSE
          ),
          list(
            targets = get_column_indices(feedtype_dt, c("residue_removal", "residue_burnt")) - 1,
            createdCell = JS(disable_specific_rows_edit_js(rows_not_residue)),
            searchable = FALSE
          ),
          list(
            targets = get_column_indices(feedtype_dt, "cultivation_period") - 1,
            createdCell = JS(disable_specific_rows_edit_js(rows_not_contains_rice)),
            searchable = FALSE
          ),
          list(
            targets = get_column_indices(
              feedtype_dt, 
              c("feed_item_name",
                "feed_type_name",
                "source_type",
                "intercrop",
                "land_cover_desc",
                "slope_desc",
                "grassman_desc",
                "water_regime",
                "ecosystem_type",
                "organic_amendment",
                "category",
                "landcover_c_factor",
                "slope_p_factor",
                "grassman_change_factor"
              )
            ) - 1, # - 1 Added because rownames = FALSE
            createdCell = JS(disable_all_rows_edit_js()),
            searchable = FALSE
          )
        ),
        # Link renderDT's checkbox to the shiny input (not automatic)
        drawCallback = JS(checkbox_link(id = "selected_feedtype", ns = ns))
      )
    ) %>% 
      formatStyle(
        columns = c(
          "source_type", "intercrop", "intercrop_fraction", "cut_carry_fraction",
          "land_cover_desc", "slope_desc", "slope_length", "grassman_desc",
          "main_product_removal", "residue_removal", "residue_burnt"
        ),
        backgroundColor = "#a9d18e"
      ) %>% 
      formatStyle(
        columns = c("dm_content", "me_content", "cp_content"),
        backgroundColor = "#f4b183"
      ) %>%
      formatStyle(
        columns = c("water_regime", "cultivation_period", "ecosystem_type", 
                    "organic_amendment"),
        backgroundColor = "#d3d3d3"
      ) %>%
      formatStyle(
        columns = "grassman_change_factor",
        backgroundColor = "#b5d3e7"
      ) %>%
      formatStyle(
        columns = c("landcover_c_factor", "slope_p_factor"),
        backgroundColor = "#eedfb3"
      ) %>%
      formatStyle(
        columns = c(
          "dry_yield", "residue_dry_yield", "n_content", "residue_n", "kc_initial",
          "kc_midseason", "kc_late", "category", "trees_ha", "trees_dhb",
          "trees_growth", "trees_removal", "trees_ha_dbh25", "average_dbh25",
          "increase_dbh25", "trees_ha_dbh2550", "average_dbh2550",
          "increase_dbh2550", "trees_ha_dbh50", "average_dbh50", 
          "increase_dbh50", "time_horizon", "diameter_breast"
        ),
        backgroundColor = "#8faadc"
      )
  }, server = FALSE)
  
  # Update the crop table data when check box is checked
  observeEvent(
    input$checkbox_info, {
      info <- input$checkbox_info
      checked_boxes$intercrop_checked[info$row] <- info$value
      feedtype_dt <- feedtype()
      feedtype_dt$intercrop <- as.numeric(checked_boxes$intercrop_checked) # we need integer for json
      # Reset the disabled cell to the value 0
      feedtype_dt$intercrop_fraction[feedtype_dt$intercrop == 0] <- 0
      feedtype(feedtype_dt)
      
      # Freeze and restore scroll position for crop table
      freeze_and_unfreeze_scroll(session, ns("crop_table"))
    }
  )
  
  # Render the crop inputs table
  output$crop_inputs_table <- renderDT({
    datatable(
      crop_inputs_data(),
      colnames = crop_inputs_table_colnames,
      editable = list(
        target = "cell",
        # Prevent editing of the first column (check boxes for delete rows)
        disable = list(columns = 0)
      ),
      selection = "none",
      rownames = FALSE,
      extensions = "FixedColumns",
      escape = FALSE,
      options = list(
        scrollX = TRUE,
        dom = "t",
        paging = FALSE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(
          list(
            targets = get_column_indices(crop_inputs_data(), c("Feed", "Crop")) - 1,
            createdCell = JS(disable_all_rows_edit_js()),
            searchable = FALSE
          )
        )
      )
    ) %>% 
      formatStyle(
        columns = 3:ncol(crop_inputs_data()),
        backgroundColor = "#a9d18e"
      )
  }, server = FALSE)
  
  # Update the crop table (feedtype) data when edited
  observeEvent(input$crop_table_cell_edit, {
    info <- input$crop_table_cell_edit
    new_data <- feedtype()
    
    # Update the specific cell while preserving the column's data type
    new_data <- update_cell(new_data, info, offset = 2)
    
    # Freeze and restore scroll position for crop table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
    
    feedtype(new_data)
  })
  
  # Update the crop inputs table data when edited
  observeEvent(input$crop_inputs_table_cell_edit, {
    info <- input$crop_inputs_table_cell_edit
    new_data <- crop_inputs_data()
    
    # Update the specific cell while preserving the column's data type
    new_data <- update_cell(new_data, info, offset = 1)
    
    # Freeze and restore scroll position for crop inputs table
    freeze_and_unfreeze_scroll(session, ns("crop_inputs_table"))
    
    crop_inputs_data(new_data)
  })
  
  # Delete selected crop row
  observeEvent(input$delete_crop, {
    req(nrow(feedtype()) > 0)     # Ensure there are rows to process
    selected_row <- which(
      sapply(
        seq_len(nrow(feedtype())),
        function(i) input[[paste0("selected_feedtype", i)]]
      )
    )
    if (length(selected_row) > 0) {
      new_data <- feedtype()
      new_data <- new_data[-selected_row, ]
      feedtype(new_data)
      new_input_data <- crop_inputs_data()
      new_input_data <- new_input_data[-selected_row, ]
      crop_inputs_data(new_input_data)
      checked_boxes$intercrop_checked <- checked_boxes$intercrop_checked[-selected_row]
    }
    
    # Freeze and restore scroll position for crop table and crop inputs table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
    freeze_and_unfreeze_scroll(session, ns("crop_inputs_table"))
  })
  
  # Observes clicks on `crop_table` cells and opens a modal dialog for specific columns,
  # allowing the user to select values from dropdowns based on cell type and row content.
  observeEvent(input$crop_table_cell_clicked, {
    info <- input$crop_table_cell_clicked
    req(length(info) > 0)
    if (!is.null(info) && !is.null(info$col)) {
      if ((info$col + 2) == which(names(feedtype()) == "source_type")) {
        
        if (modal_open()) return()
        modal_open(TRUE)
        showModal(modalDialog(
          title = paste("Select a source type"),
          shinyWidgets::pickerInput(
            inputId = ns("source_type"),
            label = NULL,
            choices = c("Main", "Residue", "Purchased")
          ),
          footer = tagList(
            actionButton(ns("ok_update_source_type"), "OK"),
            modalButton("Cancel")
          )
        ))
        
        selected_cell(info)
      } else if ((info$col + 2) == which(names(feedtype()) == "land_cover_desc")) {
        
        if (modal_open()) return()
        modal_open(TRUE)
        showModal(modalDialog(
          title = paste("Select a land cover"),
          shinyWidgets::pickerInput(
            inputId = ns("land_cover"),
            label = NULL,
            choices = setNames(
              lkp_landcover()$landcover_code,
              lkp_landcover()$landcover_desc
            )
          ),
          footer = tagList(
            actionButton(ns("ok_update_land_cover"), "OK"),
            modalButton("Cancel")
          )
        ))
        
        selected_cell(info)
      } else if ((info$col + 2) == which(names(feedtype()) == "slope_desc")) {
        
        if (modal_open()) return()
        modal_open(TRUE)
        showModal(modalDialog(
          title = paste("Select a slope type"),
          shinyWidgets::pickerInput(
            inputId = ns("slope_type"),
            label = NULL,
            choices = setNames(lkp_slope()$slope_code, lkp_slope()$slope_desc)
          ),
          footer = tagList(
            actionButton(ns("ok_update_slope_type"), "OK"),
            modalButton("Cancel")
          )
        ))
        
        selected_cell(info)
      } else if ((info$col + 2) == which(names(feedtype()) == "grassman_desc")) {
        
        if (feedtype()[info$row, "category"] == "grass") {
          
          if (modal_open()) return()
          modal_open(TRUE)
          showModal(modalDialog(
            title = paste("Select a grassland management type"),
            shinyWidgets::pickerInput(
              inputId = ns("grassland_man"),
              label = NULL,
              choices = setNames(lkp_grasslandman()$management_code, lkp_grasslandman()$management_desc)
            ),
            footer = tagList(
              actionButton(ns("ok_update_grassland_man"), "OK"),
              modalButton("Cancel")
            )
          ))
          
          selected_cell(info)
        }
        
      } else if ((info$col + 2) == which(names(feedtype()) == "water_regime")) {
        
        if (feedtype()[info$row, "feed_type_name"] == "Rice") {
          
          if (modal_open()) return()
          modal_open(TRUE)
          showModal(modalDialog(
            title = paste("Select a water regime type"),
            shinyWidgets::pickerInput(
              inputId = ns("water_regime"),
              label = NULL,
              choices = water_regime_options
            ),
            footer = tagList(
              actionButton(ns("ok_update_water_regime"), "OK"),
              modalButton("Cancel")
            )
          ))
          
          selected_cell(info)
        }
        
      } else if ((info$col + 2) == which(names(feedtype()) == "ecosystem_type")) {
        
        if (feedtype()[info$row, "feed_type_name"] == "Rice") {
          
          if (modal_open()) return()
          modal_open(TRUE)
          showModal(modalDialog(
            title = paste("Select a rice ecosystem type"),
            shinyWidgets::pickerInput(
              inputId = ns("rice_ecosystem"),
              label = NULL,
              choices = rice_ecosystem_options
            ),
            footer = tagList(
              actionButton(ns("ok_update_rice_ecosystem"), "OK"),
              modalButton("Cancel")
            )
          ))
          
          selected_cell(info)
        }
        
      } else if ((info$col + 2) == which(names(feedtype()) == "organic_amendment")) {
        
        if (feedtype()[info$row, "feed_type_name"] == "Rice") {
          
          if (modal_open()) return()
          modal_open(TRUE)
          showModal(modalDialog(
            title = paste("Select a rice organic amendment"),
            shinyWidgets::pickerInput(
              inputId = ns("rice_organic_amendment"),
              label = NULL,
              choices = rice_organic_amendment_options
            ),
            footer = tagList(
              actionButton(ns("ok_update_rice_organic_amendment"), "OK"),
              modalButton("Cancel")
            )
          ))
          
          selected_cell(info)
        }
        
      } else if ((info$col + 2) == which(names(feedtype()) == "category")) {
        
        if (modal_open()) return()
        modal_open(TRUE)
        showModal(modalDialog(
          title = paste("Select a category"),
          shinyWidgets::pickerInput(
            inputId = ns("feed_category"),
            label = NULL,
            choices = unique(lkp_feedtype()$category[lkp_feedtype()$category != ""])
          ),
          footer = tagList(
            actionButton(ns("ok_update_category"), "OK"),
            modalButton("Cancel")
          )
        ))
        
        selected_cell(info)
      }
    }
  })
  
  # Update source type in the crop table
  observeEvent(input$ok_update_source_type, {
    req(input$source_type)
    selected_cell <- selected_cell()
    if (!is.null(selected_cell)) {
      new_data <- feedtype()
      new_data[selected_cell$row, "source_type"] <- input$source_type
      feedtype(new_data)
    }
    shinyWidgets::updatePickerInput(
      session,
      "source_type",
      selected = "Main"
    )
    removeModal()
    
    # Freeze and restore scroll position for crop table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
  })
  
  # Update land cover in the crop table
  observeEvent(input$ok_update_land_cover, {
    req(input$land_cover)
    selected_cell <- selected_cell()
    if (!is.null(selected_cell)) {
      new_data <- feedtype()
      new_data[selected_cell$row, "land_cover"] <- input$land_cover
      new_data[selected_cell$row, "land_cover_desc"] <- lkp_landcover()$landcover_desc[
        lkp_landcover()$landcover_code == input$land_cover
      ]
      new_data[selected_cell$row, "landcover_c_factor"] <- lkp_landcover()$c_factor[
        lkp_landcover()$landcover_code == input$land_cover
      ]
      feedtype(new_data)
    }
    shinyWidgets::updatePickerInput(
      session,
      "land_cover",
      selected = setNames(
        lkp_landcover()$landcover_code, lkp_landcover()$landcover_desc
      )[1]
    )
    removeModal()
    
    # Freeze and restore scroll position for crop table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
  })
  
  # Update slope type in the crop table
  observeEvent(input$ok_update_slope_type, {
    req(input$slope_type)
    selected_cell <- selected_cell()
    if (!is.null(selected_cell)) {
      new_data <- feedtype()
      new_data[selected_cell$row, "slope"] <- input$slope_type
      new_data[selected_cell$row, "slope_desc"] <- lkp_slope()$slope_desc[
        lkp_slope()$slope_code == input$slope_type
      ]
      new_data[selected_cell$row, "slope_p_factor"] <- lkp_slope()$p_factor[
        lkp_slope()$slope_code == input$slope_type
      ]
      feedtype(new_data)
    }
    shinyWidgets::updatePickerInput(
      session,
      "slope_type",
      selected = setNames(lkp_slope()$slope_code, lkp_slope()$slope_desc)[1]
    )
    removeModal()
    
    # Freeze and restore scroll position for crop table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
  })
  
  # Update grassland management type in the crop table
  observeEvent(input$ok_update_grassland_man, {
    req(input$grassland_man)
    selected_cell <- selected_cell()
    if (!is.null(selected_cell)) {
      new_data <- feedtype()
      new_data[selected_cell$row, "grassman"] <- input$grassland_man
      new_data[selected_cell$row, "grassman_desc"] <- lkp_grasslandman()$management_desc[
        lkp_grasslandman()$management_code == input$grassland_man
      ]
      new_data[selected_cell$row, "grassman_change_factor"] <- lkp_grasslandman()$change_factor[
        lkp_grasslandman()$management_code == input$grassland_man
      ]
      feedtype(new_data)
    }
    shinyWidgets::updatePickerInput(
      session,
      "grassland_man",
      selected = setNames(lkp_grasslandman()$management_code, lkp_grasslandman()$management_desc)[1]
    )
    removeModal()
    
    # Freeze and restore scroll position for crop table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
  })
  
  # Update water regime in the crop table
  observeEvent(input$ok_update_water_regime, {
    req(input$water_regime)
    selected_cell <- selected_cell()
    if (!is.null(selected_cell)) {
      new_data <- feedtype()
      new_data[selected_cell$row, "water_regime"] <- input$water_regime
      feedtype(new_data)
    }
    shinyWidgets::updatePickerInput(
      session,
      "water_regime",
      selected = water_regime_options[1]
    )
    removeModal()
    
    # Freeze and restore scroll position for crop table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
  })
  
  # Update rice ecosystem type in the crop table
  observeEvent(input$ok_update_rice_ecosystem, {
    req(input$rice_ecosystem)
    selected_cell <- selected_cell()
    if (!is.null(selected_cell)) {
      new_data <- feedtype()
      new_data[selected_cell$row, "ecosystem_type"] <- input$rice_ecosystem
      feedtype(new_data)
    }
    shinyWidgets::updatePickerInput(
      session,
      "rice_ecosystem",
      selected = rice_ecosystem_options[1]
    )
    removeModal()
    
    # Freeze and restore scroll position for crop table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
  })
  
  # Update rice organic amendment in the crop table
  observeEvent(input$ok_update_rice_organic_amendment, {
    req(input$rice_organic_amendment)
    selected_cell <- selected_cell()
    if (!is.null(selected_cell)) {
      new_data <- feedtype()
      new_data[selected_cell$row, "organic_amendment"] <- input$rice_organic_amendment
      feedtype(new_data)
    }
    shinyWidgets::updatePickerInput(
      session,
      "rice_organic_amendment",
      selected = rice_organic_amendment_options[1]
    )
    removeModal()
    
    # Freeze and restore scroll position for crop table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
  })
  
  # Update category in the crop table
  observeEvent(input$ok_update_category, {
    req(input$feed_category)
    selected_cell <- selected_cell()
    if (!is.null(selected_cell)) {
      new_data <- feedtype()
      new_data[selected_cell$row, "category"] <- input$feed_category
      feedtype(new_data)
    }
    shinyWidgets::updatePickerInput(
      session,
      "feed_category",
      selected = lkp_feedtype()$category[1]
    )
    removeModal()
    
    # Freeze and restore scroll position for crop table
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
  })
  
  # Observe the crop inputs data and toggle the visibility of the error message
  observeEvent(crop_inputs_data(), {
    # Hide the error message if the sum of the fraction column is valid (≤ 1)
    if (sum(crop_inputs_data()$fraction_as_fertilizer) <= 1) {
      shinyjs::hide(id = "alert_message_crop_inputs")
    } else {
      shinyjs::show(id = "alert_message_crop_inputs")
    }
  })
  
  # ----------- Livestock feeding tab ------------------------------------------
  # Reactive value to store the data frames for each season
  basket_data <- reactiveValues()
  
  # UI output for the Season/Feed Allocation tab
  output$livestock_feeding_ui <- renderUI({
    tags$div(
      if (nrow(seasons()) == 0) {
        div(class = "alert-text", "You don't have seasons")
      },
      if (nrow(livestock_data()) == 0) {
        div(class = "alert-text", "You don't have any livestock")
      },
      if (nrow(feedtype()) == 0) {
        div(class = "alert-text", "You don't have any feeds")
      }
    )
  })
  
  #UI output for the Season/Feed table
  output$livestock_feeding_table <- renderUI({
    lapply(seasons()$Season, function(season) {
      DTOutput(outputId = ns(paste0("table_", season)))
    })
  })
  
  observeEvent(list(seasons(), livestock_data(), feedtype()), {
    # Render tables only when all necessary data frames are populated
    if (nrow(livestock_data()) == 0 || nrow(feedtype()) == 0 || nrow(seasons()) == 0) {
      lapply(seasons()$Season, function(season) {
        output[[paste0("table_", season)]] <- renderDT(NULL)
      })
      return()  # Exit early to avoid running the rest of the logic
    }
    
    # Generate and Render Dynamic Feed Allocation Table for Each Season
    lapply(seasons()$Season, function(season) {
      ft <- paste(feedtype()$feed_item_name, "of", feedtype()$feed_type_name)
      lt <- livestock_data()$livetype_desc
      df <- as.data.frame(matrix(0, nrow = length(ft), ncol = length(lt)))
      colnames(df) <- lt
      rownames(df) <- ft
      
      # Identify common and unique row names
      common_rows <- intersect(rownames(df), rownames(basket_data[[season]]))
      common_cols <- intersect(colnames(df), colnames(basket_data[[season]]))
      
      unique_df_rows <- setdiff(rownames(df), rownames(basket_data[[season]]))
      unique_df_cols <- setdiff(colnames(df), colnames(basket_data[[season]]))
      
      # Check if rows or columns are empty
      if (length(unique_df_rows) == 0) {
        # Adding columns
        new_data <- df[, unique_df_cols, drop = FALSE]
        # Combine the data frames by columns
        basket_data[[season]] <- cbind(
          basket_data[[season]][common_rows, common_cols, drop = FALSE],
          new_data
        )
      } else if (length(unique_df_cols) == 0) {
        # Adding rows
        new_data <- df[unique_df_rows, , drop = FALSE]
        # Combine the data frames by rows
        basket_data[[season]] <- rbind(
          basket_data[[season]][common_rows, common_cols, drop = FALSE],
          new_data
        )
      } else {
        # Adding both rows and columns
        basket_data[[season]] <- rbind(
          basket_data[[season]][common_rows, common_cols, drop = FALSE],
          df[unique_df_rows, unique_df_cols, drop = FALSE]
        )
      }
      
      # total calculation
      basket_data[[season]] <- rbind(
        basket_data[[season]], Total = colSums(basket_data[[season]], na.rm = TRUE)
      )
      
      # Render the data table
      output[[paste0("table_", season)]] <- renderDT({
        datatable(
          basket_data[[season]],
          caption = paste("Season:", season),
          editable = list(target = "cell"),
          selection = "none",
          options = list(
            dom = "t",
            paging = FALSE,
            columnDefs = list(
              list(
                targets = 0,
                createdCell = JS(disable_all_rows_edit_js()),
                searchable = FALSE
              ),
              list(
                targets = "_all", # Apply the following behavior to all columns except the first one.
                createdCell = JS(
                  disable_specific_rows_edit_js(
                    # Disable editing the "Total" row
                    rows_to_disable = which(rownames(basket_data[[season]]) == "Total") - 1
                  )
                )
              )
            )
          )
        ) %>%
          formatStyle(
            columns = 0, # Look for 'Total' in the first column
            target = "row",
            fontWeight = styleEqual("Total", "bold") # Applying bold only to 'Total' row
          )
      }, server = FALSE)
    })
  })
  
  # Observe changes and update the "Total" row
  observe({
    lapply(seasons()$Season, function(season) {
      observeEvent(input[[paste0("table_", season, "_cell_edit")]], {
        info <- input[[paste0("table_", season, "_cell_edit")]]
        df <- basket_data[[season]]
        
        if (!is.null(df) && nrow(df) > 0 && ncol(df) > 0) {
          df[info$row, info$col] <- ifelse(info$value == "", 0, as.numeric(info$value))
          
          # Reset and recalculate the Total row
          df["Total", ] <- colSums(df[1:(nrow(df) - 1), , drop = FALSE], na.rm = TRUE)
          basket_data[[season]] <- df
        }
      })
    })
  })
  
  # ----------- Automatically Save Data as JSON --------------------------------
  observe({
    req(input$json_file_name)
    req(input$database_code)
    req(lkp_region())
    req(session$userData$study_object())
    
    cat(file = stderr(), "20 - Saving data as JSON\n")
    
    study_object <- list()
    
    # Collect seasons
    study_object$seasons <- if (nrow(seasons()) != 0) {
      lapply(seq_len(nrow(seasons())), function(i) {
        list(
          season_name = seasons()$Season[i],
          season_length = seasons()$Days[i]
        )
      })
    } else {
      list()
    }
    
    # Collect database code
    study_object$database_code <- input$database_code
    
    # Collect region input's code
    study_object$region <- input$region
    
    # Collect text inputs
    study_object <- c(
      study_object,
      setNames(lapply(text_inputs, function(x) input[[x]]), text_inputs)
    )
    
    # Collect numeric inputs
    study_object <- c(
      study_object,
      setNames(
        lapply(numeric_inputs, function(x) as.numeric(input[[x]])), numeric_inputs
      )
    )
    
    
    # Livestock
    study_object$livestock <- if (is.data.frame(livestock_data()) && nrow(livestock_data()) != 0) {
      lapply(seq_len(nrow(livestock_data())), function(i) {
        as.list(livestock_data()[i, ])
      })
    } else {
      list()
    }
    
    # Feed items
    study_object$feed_items <- if (is.data.frame(feedtype()) && nrow(feedtype()) != 0) {
      lapply(seq_len(nrow(feedtype())), function(i) {
        # Convert the current row of feedtype() to a list
        feed_row <- as.list(feedtype()[i, ])
        
        # Remove "Crop" and "Feed" columns from crop_inputs_data
        crop_data <- as.list(
          crop_inputs_data()[i, !(names(crop_inputs_data()) %in% c("Crop", "Feed"))]
        )
        
        # Merge feed_row with crop_data
        merged_data <- c(feed_row, crop_data)
        
        # Return the merged list
        return(merged_data)
      })
    } else {
      list()
    }
    
    # Fertilizer
    study_object$fertilizer <- if (
      is.data.frame(fertilizers()) && nrow(fertilizers()) != 0
    ) {
      lapply(seq_len(nrow(fertilizers())), function(i) {
        as.list(fertilizers()[i, ])
      })
    } else {
      list()
    }
    
    # Feed Basket
    study_object$feed_basket <- if (
      is.data.frame(seasons()) && nrow(seasons()) != 0 &&
      is.data.frame(feedtype()) && nrow(feedtype()) != 0 &&
      is.data.frame(livestock_data()) && nrow(livestock_data()) != 0
    ) {
      lapply(seq_len(nrow(seasons())), function(i) {
        list(
          feeds = lapply(seq_len(nrow(feedtype())), function(j) {
            list(
              feed_item_code = feedtype()$feed_item_code[j],
              feed_type_code = feedtype()$feed_type_code[j],
              livestock = lapply(seq_len(nrow(livestock_data())), function(k) {
                list(
                  livetype_code = livestock_data()$livetype_code[
                    livestock_data()$livetype_desc == colnames(basket_data[[seasons()$Season[i]]])[k]
                  ],
                  allocation = ifelse(
                    basket_data[[seasons()$Season[i]]][j, k] == "", 0,
                    as.numeric(basket_data[[seasons()$Season[i]]][j, k])
                  )
                )
              })
            )
          }),
          season_name = seasons()$Season[i]
        )
      })
    } else {
      list()
    }
    
    # Generate the filename based on the input
    file_name <- file.path(session$userData$user_folder, "study_objects", input$json_file_name)
    
    write(
      toJSON(study_object, pretty = TRUE, auto_unbox = TRUE), file = file_name
    )
    
    # Update the list of study objects
    session$userData$study_objects(
      list.files(
        file.path(session$userData$user_folder, "study_objects"),
        full.names = FALSE
      )
    )
  }, priority = -1)
  
  # ----------- Track update params clicking -----------------------------------
  observeEvent(input$update_params, {
    session$userData$observe_update_params_button_click(
      session$userData$observe_update_params_button_click() + 1
    )
  })
  
  # ----------- Load Data from JSON --------------------------------------------
  observeEvent(input$json_file_name, {
    req(input$json_file_name)
    cat(file = stderr(), "20 - Loading data from JSON\n")
    
    selected_file <- input$json_file_name
    
    # Load the JSON file
    json_file <- file.path(session$userData$user_folder, "study_objects", input$json_file_name)
    study_object <- fromJSON(json_file, flatten = TRUE)
    
    # Save object in session
    session$userData$study_object(study_object)
    
    # Adjust database depending on the selected JSON file --------
    related_database <- study_object$database_code
    
    # List available databases from the user's parameters folder
    database_dir <- file.path(session$userData$user_folder, "parameters_database")
    available_databases <- list.files(database_dir, full.names = FALSE)
    
    # Check if 'related_database' exists in the available databases
    if (!is.null(related_database) && related_database %in% available_databases) {
      
      # Use the related database if found
      selected_database <- related_database
      
    } else {
      
      # Set the default database as the selected one
      selected_database <- "Params DB - Default"
      
      if (related_database != "Params DB - Default") {
        # Show a warning message to the user
        showNotification(
          "The specified parameters database is not available. 
        'Params DB - Default' will be used instead.",
          duration = 5,
          type = "warning"
        )
      }
    }
    
    # Update the select input with the appropriate database
    shinyWidgets::updatePickerInput(
      session,
      "database_code",
      choices = list.files(database_dir, full.names = FALSE),
      selected = selected_database
    )
    
    # Reconstruct the inputs -------------------------------
    lapply(numeric_inputs, function(input_name) {
      updateNumericInput(session, input_name, value = study_object[[input_name]])
    })
    
    lapply(text_inputs, function(input_name) {
      updateTextInput(session, input_name, value = study_object[[input_name]])
    })
    
    # Reconstruct the livestock data if available ------------
    if (is.data.frame(study_object$livestock)) {
      # Define the desired column order for the feedtype
      desired_order <- colnames(livestock_data_initialization)
      
      # Reorder the columns of feed_data
      livestock_data_load <- study_object$livestock %>%
        select(all_of(desired_order))
      
      livestock_data(livestock_data_load)
    } else {
      livestock_data(livestock_data_initialization)
    }
    
    # Reconstruct the crop_inputs_data if available ------------
    if (is.data.frame(study_object$feed_items)) {
      crop_columns <- c(
        "fraction_as_fertilizer", "urea", "npk", "dap", "ammonium_nitrate", 
        "ammonium_sulfate", "n_solutions", "ammonia"
      )
      
      # Initialize empty data frames for feedtype and crop_inputs_data
      feed_data <- data.frame()
      crop_data <- data.frame()
      
      # Loop through each row in study_object$feed_items to separate the data
      invisible(lapply(seq_len(nrow(study_object$feed_items)), function(i) {
        item <- study_object$feed_items[i, ]
        
        feed_df <- item[, !(names(item) %in% crop_columns), drop = FALSE]
        feed_data <<- rbind(feed_data, feed_df)
        
        # Separate crop_inputs_data and add 'Crop' and 'Feed' columns
        crop_df <- item[, crop_columns, drop = FALSE]
        crop_df$Crop <- item$feed_type_name
        crop_df$Feed <- item$feed_item_name
        
        # Reorder columns to make 'Crop' the first and 'Feed' the second column
        crop_df <- crop_df[, c("Crop", "Feed", setdiff(names(crop_df), c("Crop", "Feed")))]
        crop_data <<- rbind(crop_data, crop_df)
      }))
      
      # Define the desired column order for the feedtype
      desired_order <- colnames(feedtype_initialization)
      
      # Reorder the columns of feed_data
      feed_data <- feed_data %>%
        select(all_of(desired_order))
      
      feedtype(feed_data)
      crop_inputs_data(crop_data)
      
      # Update the crop table's column : intercrop checkbox
      checked_boxes$intercrop_checked <- as.logical(feedtype()$intercrop)
    } else {
      feedtype(feedtype_initialization)
      crop_inputs_data(crop_inputs_data_initialization)
      checked_boxes$intercrop_checked <- NULL
    }
    
    # Reconstruct the seasons data if available ------------
    if (is.data.frame(study_object$seasons)) {
      study_object$seasons <- study_object$seasons %>%
        rename(Season = season_name, Days = season_length) %>%
        select(Season, Days)
      seasons(study_object$seasons)
    } else {
      seasons(data.frame())
    }
    
    # Reconstruct the fertilizers data if available ------------
    if (is.data.frame(study_object$fertilizer)) {
      fertilizers(study_object$fertilizer)
    } else {
      fertilizers(data.frame())
    }
    
    # Reconstruct the Livestock feeding data if available ------------
    if (is.data.frame(study_object$seasons) &&
        is.data.frame(study_object$livestock) &&
        is.data.frame(study_object$feed_items)) {
      invisible(lapply(seq_along(study_object$feed_basket$season_name), function(i) {
        season_name <- study_object$feed_basket$season_name[i]
        feeds <- study_object$feed_basket$feeds[[i]]
        
        feed_item_codes <- feeds$feed_item_code
        livetype_codes <- feeds$livestock[[1]]$livetype_code
        
        # Initialize a data frame to store the allocations for this season
        season_df <- data.frame(
          matrix(0, nrow = length(feed_item_codes), ncol = length(livetype_codes))
        )
        colnames(season_df) <- livetype_codes
        rownames(season_df) <- feed_item_codes
        
        # Loop through each feed type in the feeds data frame
        invisible(lapply(seq_len(nrow(feeds)), function(feed_index) {
          feed_item_code <- feeds$feed_item_code[feed_index]
          livestock <- feeds$livestock[[feed_index]]
          
          # Loop through each livestock entry and update the season data frame
          invisible(lapply(seq_len(nrow(livestock)), function(livestock_index) {
            livetype_code <- livestock$livetype_code[livestock_index]
            allocation <- livestock$allocation[livestock_index]
            
            # Update the data frame for this season
            season_df[as.character(feed_item_code), as.character(livetype_code)] <<- allocation
          }))
        }))
        
        # Rename the rownames & colnames
        rownames(season_df) <- paste(
          study_object$feed_items$feed_item_name,
          "of", study_object$feed_items$feed_type_name
        )
        colnames(season_df) <- study_object$livestock$livetype_desc
        
        # Add the season data frame to the basket_data list
        basket_data[[season_name]] <<- season_df
      }))
    } else {
      basket_data <- list()
    }
  })

  # Update The scenario's select inputs ----------------------------------------
  observe({
    req(lkp_region())
    req(session$userData$study_object())
    cat(file = stderr(), "20 - Updating the scenario's select inputs\n")
    
    # Reconstruct the region's input
    shinyWidgets::updatePickerInput(
      session, "region",
      choices = setNames(lkp_region()$region_code, lkp_region()$region_desc),
      selected = session$userData$study_object()$region
    )
    
    #Reconstruct the select inputs
    shinyWidgets::updatePickerInput(
      session, "climate_zone",
      choices = lkp_climate()$climate_desc,
      selected = session$userData$study_object()$climate_zone
    )
    
    shinyWidgets::updatePickerInput(
      session, "climate_zone_2", 
      choices = lkp_climate2() %>%
        filter(climate_code == "Temperate") %>%
        pull(climate2_desc),
      selected = session$userData$study_object()$climate_zone_2
    )
    
    shinyWidgets::updatePickerInput(
      session, "soil_description", 
      choices = lkp_soil()$soil_desc,
      selected = session$userData$study_object()$soil_description
    )
    
    shinyWidgets::updatePickerInput(
      session, "cropland_system", 
      choices = lkp_croplandsystem()$sys_desc,
      selected = session$userData$study_object()$cropland_system
    )
    
    shinyWidgets::updatePickerInput(
      session, "cropland_tillage", 
      choices = lkp_tillageregime()$tillage_desc,
      selected = session$userData$study_object()$cropland_tillage
    )
    
    shinyWidgets::updatePickerInput(
      session, "cropland_orgmatter", 
      choices = lkp_organicmatter()$orgmatter_desc,
      selected = session$userData$study_object()$cropland_orgmatter
    )
    
    shinyWidgets::updatePickerInput(
      session, "grassland_management", 
      choices = lkp_grasslandman()$management_desc,
      selected = session$userData$study_object()$grassland_management
    )
    
    shinyWidgets::updatePickerInput(
      session, "grassland_implevel", 
      choices = lkp_grassinputlevel()$grassinputlevel_desc,
      selected = session$userData$study_object()$grassland_implevel
    )
  })
})}
