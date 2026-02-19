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
      choices = sort(
        list.files(
          path = file.path(session$userData$user_folder, "study_objects"),
          full.names = FALSE
        )
      ),
      selected = character(0)
    )
    
    # Update the shared_folder selectInput with the list of examples
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "json_shared_folder",
      choices = sort(
        list.files(
          path = file.path("data", "shared_folder", "study_objects"),
          full.names = FALSE
        )
      ),
      selected = character(0)
    )
    
    # Update the database selectInput with the list of databases
    shinyWidgets::updatePickerInput(
      session,
      "database_code",
      choices = sort(
        list.files(
          path = file.path(session$userData$user_folder, "parameters_database"),
          full.names = FALSE
        )
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
  
  # ----- Validation Module ----------------------------------------------------
  farm_validation_server("validation", input = input, parent_session = session)
  
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
      choices = sort(session$userData$databases()),
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
        choices = sort(
          list.files(
            path = file.path(session$userData$user_folder, "study_objects"),
            full.names = FALSE
          )
        ),
        selected = input$json_file_name
      )
      
      # Reset the cloned folder indicator
      session$userData$super_user_cloned_folder(NULL)
      
    } else if (session$userData$super_user_cloned_folder() == "parameters_database") {
      shinyWidgets::updatePickerInput(
        session,
        "database_code",
        choices = sort(
          list.files(
            path = file.path(session$userData$user_folder, "parameters_database"),
            full.names = FALSE
          )
        ),
        selected = input$database_code
      )
      
      # Reset the cloned folder indicator
      session$userData$super_user_cloned_folder(NULL)
    }
  })
  
  # ----- UX Interaction logic --------------------------------------------
  # Show/hide folder UIs based on radio selection (uses shared helper)
  observeEvent(input$scenario_folder, {
    show_hide_folder_ui(input$scenario_folder, ns)
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
      choices = sort(
        list.files(
          path = file.path(session$userData$user_folder, "study_objects"),
          full.names = FALSE
        )
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
      
      # Share the related database if it's not a default (read-only) database
      if (!(input$database_code %in% primary_database_names())) {
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
      choices = sort(
        list.files(destination_dir, full.names = FALSE)
      ),
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
        choices = sort(
          list.files(
            path = file.path(session$userData$user_folder, "study_objects"),
            full.names = FALSE
          )
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
      choices = sort(
        list.files(destination_dir, full.names = FALSE)
      ),
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
        choices = sort(
          list.files(destination_dir, full.names = FALSE)
        ),
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
      
      # If the database is not a default (read-only) database and is available on
      # the shared_pool folder, clone the database as well
      if (!(db_name %in% primary_database_names()) &&
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
          choices = sort(
            list.files(destination_dir, full.names = FALSE)
          ),
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
  
  # ----------- Seasons Tab (sourced sub-module) --------------------------------
  source("R/scenario/32_scenario_seasons.R", local = TRUE)
  
  # ----------- Manure/Fertilizer Tab (sourced sub-module) ---------------------
  source("R/scenario/33_scenario_fertilizer.R", local = TRUE)
  
  # ----------- Area tab (sourced sub-module) ----------------------------------
  source("R/scenario/34_scenario_area.R", local = TRUE)
  
  # ----------- Livestock tab (sourced sub-module) -----------------------------
  source("R/scenario/35_scenario_livestock.R", local = TRUE)
  
  # ----------- Feed production tab --------------------------------------------
  # (Kept inline; seasons/fertilizer/area/livestock are sourced from R/scenario/32-35)
  
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
        choices = setNames(
          lkp_feeditem()$feed_item_code,
          lkp_feeditem()$feed_item_name
        )[sort(lkp_feeditem()$feed_item_name)],
        options = list(`live-search` = TRUE)
      ),
      br(),
      h2("Selected a Crop", class = "mb-3"),
      shinyWidgets::pickerInput(
        inputId = ns("crop"),
        label = NULL,
        choices = NULL,
        options = list(`live-search` = TRUE)
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
    crop_code <- lkp_feeditem()$crop_code[lkp_feeditem()$feed_item_code == input$feed]
    choices <- setNames(
      lkp_crops()$crop_code[lkp_crops()$crop_code == crop_code],
      lkp_crops()$crop_name[lkp_crops()$crop_code == crop_code]
    )
    # remove NA values
    choices <- choices[!is.na(choices)]
    shinyWidgets::updatePickerInput(
      session,
      "crop",
      choices = choices[sort(names(choices))]
    )
  })
  
  # Add new crop row from modal
  observeEvent(input$ok_add_crop, {
    req(input$crop, input$feed)
    if (!((input$crop %in% feedtype()[, "crop_code"]) && (input$feed %in% feedtype()[, "feed_item_code"]))) {
      new_row <- data.frame(
        crop_code = input$crop,
        feed_item_code = input$feed,
        feed_item_name = lkp_feeditem()$feed_item_name[lkp_feeditem()$feed_item_code == input$feed],
        crop_name = lkp_crops()$crop_name[lkp_crops()$crop_code == input$crop],
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
        dry_yield = lkp_crops()$dry_yield[lkp_crops()$crop_code == input$crop],
        residue_dry_yield = lkp_crops()$residue_dry_yield[lkp_crops()$crop_code == input$crop],
        residue_n = lkp_crops()$residue_n[lkp_crops()$crop_code == input$crop],
        kc_initial = lkp_crops()$kc_initial[lkp_crops()$crop_code == input$crop],
        kc_midseason = lkp_crops()$kc_midseason[lkp_crops()$crop_code == input$crop],
        kc_late = lkp_crops()$kc_late[lkp_crops()$crop_code == input$crop],
        category = lkp_crops()$category[lkp_crops()$crop_code == input$crop],
        trees_ha = lkp_crops()$trees_ha[lkp_crops()$crop_code == input$crop],
        trees_dhb = lkp_crops()$trees_dhb[lkp_crops()$crop_code == input$crop],
        trees_growth = lkp_crops()$trees_growth[lkp_crops()$crop_code == input$crop],
        trees_removal = lkp_crops()$trees_removal[lkp_crops()$crop_code == input$crop],
        trees_ha_dbh25 = lkp_crops()$trees_ha_dbh25[lkp_crops()$crop_code == input$crop],
        average_dbh25 = lkp_crops()$average_dbh25[lkp_crops()$crop_code == input$crop],
        increase_dbh25 = lkp_crops()$increase_dbh25[lkp_crops()$crop_code == input$crop],
        trees_ha_dbh2550 = lkp_crops()$trees_ha_dbh2550[lkp_crops()$crop_code == input$crop],
        average_dbh2550 = lkp_crops()$average_dbh2550[lkp_crops()$crop_code == input$crop],
        increase_dbh2550 = lkp_crops()$increase_dbh2550[lkp_crops()$crop_code == input$crop],
        trees_ha_dbh50 = lkp_crops()$trees_ha_dbh50[lkp_crops()$crop_code == input$crop],
        average_dbh50 = lkp_crops()$average_dbh50[lkp_crops()$crop_code == input$crop],
        increase_dbh50 = lkp_crops()$increase_dbh50[lkp_crops()$crop_code == input$crop],
        time_horizon = lkp_crops()$time_horizon[lkp_crops()$crop_code == input$crop],
        diameter_breast = lkp_crops()$diameter_breast[lkp_crops()$crop_code == input$crop],
        # These ones are available in the json but not in the DT
        fraction_as_manure = "NULL", # We should get null in the json
        n_fertilizer = "NULL", # We should get null in the json
        main_n = lkp_crops()$main_n[lkp_crops()$crop_code == input$crop],
        land_cover = lkp_landcover()$landcover_code[1],
        slope = lkp_slope()$slope_code[1],
        grassman = lkp_grasslandman()$management_code[1],
        stringsAsFactors = FALSE
      )
      
      new_input_row <- data.frame(
        Feed = lkp_feeditem()$feed_item_name[lkp_feeditem()$feed_item_code == input$feed],
        Crop = lkp_crops()$crop_name[lkp_crops()$crop_code == input$crop],
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
    rows_not_contains_rice <- which(feedtype_dt$crop_name != "Rice") - 1 # index for js
    
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
      id = ("intercrop_check"),
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
        -crop_code, -feed_item_code, -fraction_as_manure, -n_fertilizer,
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
              feedtype_dt, c("water_regime", "ecosystem_type", "organic_amendment")
            ) - 1,
            createdCell = JS(disable_with_conditional_cursor_js(rows_not_contains_rice)),
            searchable = FALSE
          ),
          list(
            targets = get_column_indices(
              feedtype_dt,
              c("landcover_c_factor", "slope_p_factor", "grassman_change_factor")
            ) - 1,
            createdCell = JS(disable_and_add_cursor_js()),
            searchable = FALSE
          ),
          list(
            targets = get_column_indices(
              feedtype_dt, 
              c("feed_item_name",
                "crop_name",
                "source_type",
                "intercrop",
                "land_cover_desc",
                "slope_desc",
                "grassman_desc",
                "category")
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
          "dry_yield", "residue_dry_yield", "residue_n", "kc_initial",
          "kc_midseason", "kc_late", "category", "trees_ha", "trees_dhb",
          "trees_growth", "trees_removal", "trees_ha_dbh25", "average_dbh25",
          "increase_dbh25", "trees_ha_dbh2550", "average_dbh2550",
          "increase_dbh2550", "trees_ha_dbh50", "average_dbh50", 
          "increase_dbh50", "time_horizon", "diameter_breast"
        ),
        backgroundColor = "#8faadc"
      )
  }, server = FALSE)
  
  # ------ CROP INTERCROPPING FRACTION AUTO-UPDATE -----------------------------
  observeEvent(input$checkbox_info, {
    info <- input$checkbox_info
    req(length(info) > 0)
    # Update stored checkbox states
    checked_boxes$intercrop_checked[info$row] <- info$value
    # Retrieve the current crop table data
    feedtype_dt <- feedtype()
    # Ensure required columns exist
    required_columns <- c("intercrop", "intercrop_fraction")
    if (!all(required_columns %in% names(feedtype_dt))) return()
    # Update intercropping flag (numeric for JSON export)
    feedtype_dt$intercrop <- as.numeric(checked_boxes$intercrop_checked)
    # Apply logic for automatic fraction updates when checked 
    if (isTRUE(info$value)) {
      if (is.na(feedtype_dt$intercrop_fraction[info$row]) ||
          feedtype_dt$intercrop_fraction[info$row] == 0) {
        feedtype_dt$intercrop_fraction[info$row] <- 0.01
      }
    } else {
      feedtype_dt$intercrop_fraction[info$row] <- 0
    }
    # Push updated table back into reactive
    feedtype(feedtype_dt)
    # Keep scroll position stable after re-render
    freeze_and_unfreeze_scroll(session, ns("crop_table"))
  })
  
  # ------ FEEDTYPE VALIDATION -------------------------------------------------
  observeEvent(feedtype(), {
    # Hide previous warnings before running new validation
    shinyjs::hide("alert_message_Intercropping_fraction_inputs")
    shinyjs::hide("alert_message_residue_fractions_inputs")
    # Stop if no feed table data is available
    feed_table <- feedtype()
    if (is.null(feed_table) || nrow(feed_table) == 0) return()
    
    # --- Validate intercropping fractions -------------------------------------
    intercropping_fields <- c("feed_item_name", "intercrop", "intercrop_fraction")
    if (all(intercropping_fields %in% names(feed_table))) {
      
      invalid_intercrop_msgs <- character(0)
      
      for (row_index in seq_len(nrow(feed_table))) {
        # Proceed only when intercropping checkbox is checked
        intercropping_selected <- !is.na(feed_table$intercrop[row_index]) &&
          feed_table$intercrop[row_index] == 1
        feed_label <- feed_table$feed_item_name[row_index]
        
        if (intercropping_selected) {
          intercropping_value <- feed_table$intercrop_fraction[row_index]
          # Auto-assign 0.01 when selected but value left blank
          if (is.na(intercropping_value)) {
            intercropping_value <- 0.01
            feed_table$intercrop_fraction[row_index] <- intercropping_value
          }
          # Validate that fraction is strictly between 0 and 1
          if (intercropping_value <= 0 || intercropping_value >= 1) {
            invalid_intercrop_msgs[feed_label] <- sprintf(
              "<strong>•</strong> For <strong> %s </strong>, the value in 
              <strong>'IF intercropping, fraction of field occupied by this crop'</strong> 
              column must be > 0 and < 1!",
              feed_label
            )
          }
        } else {
          # Reset fraction to 0 when intercropping is unchecked
          feed_table$intercrop_fraction[row_index] <- 0
        }
      }
      # Display intercropping validation messages
      if (length(invalid_intercrop_msgs) > 0) {
        invalid_intercrop_msgs <- invalid_intercrop_msgs[
          order(names(invalid_intercrop_msgs))
        ]
        shinyjs::html(
          id = "alert_message_Intercropping_fraction_inputs",
          html = paste(invalid_intercrop_msgs, collapse = "<br>")
        )
        shinyjs::show("alert_message_Intercropping_fraction_inputs")
      }
    }
    
    # --- Validate residue fraction columns ------------------------------------
    residue_fields <- c(
      "cut_carry_fraction",
      "main_product_removal",
      "residue_removal",
      "residue_burnt"
    )
    residue_fields <- residue_fields[residue_fields %in% names(feed_table)]
    # Stop if no residue fields exist in the dataset
    if (length(residue_fields) == 0) {
      feedtype(feed_table)
      return()
    }
    # Map technical field names to user-friendly labels for clear UI messages
    residue_field_labels <- vapply(
      residue_fields,
      function(field_name) {
        # Locate field index in initialization to map to display label
        field_index <- which(names(feedtype_initialization) == field_name)
        if (length(field_index) == 1) {
          # feedtype_colnames starts with a blank label for delete column
          label_index <- field_index - 1
          if (label_index <= length(feedtype_colnames)) {
            # Return matching label text for display
            feedtype_colnames[label_index]
          } else {
            # Fallback to backend name if label index is out of range
            field_name
          }
        } else {
          # Fallback if field name not found in initialization
          field_name
        }
      },
      character(1)
    )
    
    # Validate all residue fractions using lapply for column-wise checks
    invalid_residue_msgs <- unlist(lapply(seq_along(residue_fields), function(j) {
      
      residue_field <- residue_fields[j]
      residue_label <- residue_field_labels[j]
      # Identify rows where residue fractions are outside valid range [0,1]
      invalid_rows <- which(
        !is.na(feed_table[[residue_field]]) &
          (feed_table[[residue_field]] < 0 | feed_table[[residue_field]] > 1)
      )
      
      if (length(invalid_rows) == 0) return(NULL)
      
      feed_labels <- feed_table$feed_item_name[invalid_rows]
      # Format detailed UI messages for each invalid residue fraction
      msg_list <- sprintf(
        "<strong>•</strong> For <strong> %s </strong>, the value in 
        <strong>'%s'</strong> column must be ≥ 0 and ≤ 1!",
        feed_labels, residue_label
      )
      
      names(msg_list) <- feed_labels
      msg_list
    }))
    # Display residue validation messages
    if (length(invalid_residue_msgs) > 0) {
      invalid_residue_msgs <- invalid_residue_msgs[
        order(names(invalid_residue_msgs))
      ]
      shinyjs::html(
        id = "alert_message_residue_fractions_inputs",
        html = paste(invalid_residue_msgs, collapse = "<br>")
      )
      shinyjs::show("alert_message_residue_fractions_inputs")
    }
    
    # Save updated data so assigned defaults persist in the table
    feedtype(feed_table)
  })
  
  # Render the crop inputs table
  output$crop_inputs_table <- renderDT({
    
    # Observe fertilizers to show/hide the warning message
    if (nrow(fertilizers()) == 0 & nrow(crop_inputs_data()) != 0) {
      shinyjs::show("alert_no_fertilizers")
    } else {
      shinyjs::hide("alert_no_fertilizers")
    }
    
    # Identifies all available fertilizer columns based on the mapping
    all_fertilizer_cols <- unname(fertilizer_column_mapping)
    all_col_names <- colnames(crop_inputs_data())
    fertilizer_col_indices <- which(all_col_names %in% all_fertilizer_cols)
    
    # Identifies currently active fertilizer columns based on user selection
    active_col_names <- fertilizer_column_mapping[fertilizers()$fertilizer_desc]
    active_col_indices <- which(all_col_names %in% active_col_names)
    
    # Calculates indices of inactive fertilizer columns to disable
    blocked_col_indices <- setdiff(fertilizer_col_indices, active_col_indices)
    
    # Adjusts indices to 0-based for DataTables (JavaScript) compatibility
    blocked_js_indices <- blocked_col_indices - 1
    
    datatable(
      crop_inputs_data(),
      colnames = crop_inputs_table_colnames,
      editable = list(
        target = "cell"
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
          # Visually block Feed (0) and Crop (1) columns from double-click interaction
          list(
            targets = get_column_indices(crop_inputs_data(), c("Feed", "Crop")) - 1,
            createdCell = JS(disable_all_rows_edit_js()),
            searchable = FALSE
          ),
          # Applies visual 'not-allowed' cursor to inactive fertilizer columns
          list(
            targets = blocked_js_indices,
            createdCell = JS(disable_and_add_cursor_js()),
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
            choices = sort(
              c("Main", "Residue", "Purchased")
            ),
            options = list(`live-search` = TRUE)
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
            )[sort(lkp_landcover()$landcover_desc)],
            options = list(`live-search` = TRUE)
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
            choices = setNames(
              lkp_slope()$slope_code,
              lkp_slope()$slope_desc
            )[sort(lkp_slope()$slope_desc)],
            options = list(`live-search` = TRUE)
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
              choices = setNames(
                lkp_grasslandman()$management_code,
                lkp_grasslandman()$management_desc
              )[sort(lkp_grasslandman()$management_desc)],
              options = list(`live-search` = TRUE)
            ),
            footer = tagList(
              actionButton(ns("ok_update_grassland_man"), "OK"),
              modalButton("Cancel")
            )
          ))
          
          selected_cell(info)
        }
        
      } else if ((info$col + 2) == which(names(feedtype()) == "water_regime")) {
        
        if (feedtype()[info$row, "crop_name"] == "Rice") {
          
          if (modal_open()) return()
          modal_open(TRUE)
          showModal(modalDialog(
            title = paste("Select a water regime type"),
            shinyWidgets::pickerInput(
              inputId = ns("water_regime"),
              label = NULL,
              choices = sort(water_regime_options),
              options = list(`live-search` = TRUE)
            ),
            footer = tagList(
              actionButton(ns("ok_update_water_regime"), "OK"),
              modalButton("Cancel")
            )
          ))
          
          selected_cell(info)
        }
        
      } else if ((info$col + 2) == which(names(feedtype()) == "ecosystem_type")) {
        
        if (feedtype()[info$row, "crop_name"] == "Rice") {
          
          if (modal_open()) return()
          modal_open(TRUE)
          showModal(modalDialog(
            title = paste("Select a rice ecosystem type"),
            shinyWidgets::pickerInput(
              inputId = ns("rice_ecosystem"),
              label = NULL,
              choices = sort(rice_ecosystem_options),
              options = list(`live-search` = TRUE)
            ),
            footer = tagList(
              actionButton(ns("ok_update_rice_ecosystem"), "OK"),
              modalButton("Cancel")
            )
          ))
          
          selected_cell(info)
        }
        
      } else if ((info$col + 2) == which(names(feedtype()) == "organic_amendment")) {
        
        if (feedtype()[info$row, "crop_name"] == "Rice") {
          
          if (modal_open()) return()
          modal_open(TRUE)
          showModal(modalDialog(
            title = paste("Select a rice organic amendment"),
            shinyWidgets::pickerInput(
              inputId = ns("rice_organic_amendment"),
              label = NULL,
              choices = sort(rice_organic_amendment_options),
              options = list(`live-search` = TRUE)
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
            choices = sort(
              unique(lkp_crops()$category[lkp_crops()$category != ""])
            ),
            options = list(`live-search` = TRUE)
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
      selected = lkp_crops()$category[1]
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
  
  # Initialize livestock feeding validation
  livestock_feeding_validation_server(
    id = "validation",
    basket_data = basket_data,
    seasons = seasons,
    parent_session = session
  )
  
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
      ft <- paste(feedtype()$feed_item_name, "of", feedtype()$crop_name)
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
  # (Single observeEvent to avoid handler accumulation when seasons change;
  # previously nested observe + lapply created duplicate handlers)
  basket_cell_edits <- reactive({
    req(seasons())
    lapply(seasons()$Season, function(season) {
      input[[paste0("table_", season, "_cell_edit")]]
    })
  })
  last_processed_basket_edit <- reactiveValues()
  observeEvent(basket_cell_edits(), {
    req(seasons())
    for (season in seasons()$Season) {
      info <- input[[paste0("table_", season, "_cell_edit")]]
      if (!is.null(info) && is.list(info) && "row" %in% names(info)) {
        key <- paste(season, info$row, info$col, info$value)
        if (is.null(last_processed_basket_edit[[season]]) ||
            last_processed_basket_edit[[season]] != key) {
          last_processed_basket_edit[[season]] <- key
          df <- basket_data[[season]]
          if (!is.null(df) && nrow(df) > 0 && ncol(df) > 0) {
            df[info$row, info$col] <- ifelse(info$value == "", 0, as.numeric(info$value))
            # Reset and recalculate the Total row
            df["Total", ] <- colSums(df[1:(nrow(df) - 1), , drop = FALSE], na.rm = TRUE)
            basket_data[[season]] <- df
          }
        }
      }
    }
  }, ignoreNULL = TRUE)
  
  # ----------- Automatically Save Data as JSON (debounced to reduce write freq) -
  study_object_to_save <- reactive({
    req(input$json_file_name)
    req(input$database_code)
    req(lkp_region())
    req(session$userData$study_object())
    
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
              crop_code = feedtype()$crop_code[j],
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
    
    list(study_object = study_object, json_file_name = input$json_file_name)
  })
  
  study_object_debounced <- debounce(study_object_to_save, 1500)
  
  observe({
    data <- study_object_debounced()
    req(data)
    cat(file = stderr(), "20 - Saving data as JSON\n")
    file_name <- file.path(
      session$userData$user_folder, "study_objects", data$json_file_name
    )
    write(
      toJSON(data$study_object, pretty = TRUE, auto_unbox = TRUE), file = file_name
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
      
      # Use first available default, or first available database
      default_available <- intersect(primary_database_names(), available_databases)
      selected_database <- if (length(default_available) > 0) {
        sort(default_available)[1]
      } else if (length(available_databases) > 0) {
        sort(available_databases)[1]
      } else {
        character(0)
      }
      
      if (!is.null(related_database) && length(selected_database) > 0 && related_database != selected_database) {
        showNotification(
          paste0("The specified parameters database is not available. '",
                 selected_database, "' will be used instead."),
          duration = 5,
          type = "warning"
        )
      }
    }
    
    # Update the select input with the appropriate database
    shinyWidgets::updatePickerInput(
      session,
      "database_code",
      choices = sort(
        list.files(database_dir, full.names = FALSE)
      ),
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
        
        # Separate crop_inputs_data and add 'Feed' and 'Crop' columns
        crop_df <- item[, crop_columns, drop = FALSE]
        crop_df$Feed <- item$feed_item_name
        crop_df$Crop <- item$crop_name
        
        # Reorder columns to make 'Feed' the first and 'Crop' the second column
        crop_df <- crop_df[, c("Feed", "Crop", setdiff(names(crop_df), c("Feed", "Crop")))]
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
          "of", study_object$feed_items$crop_name
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
      choices = setNames(
        lkp_region()$region_code,
        lkp_region()$region_desc
      )[sort(lkp_region()$region_desc)],
      selected = session$userData$study_object()$region
    )
    
    #Reconstruct the select inputs
    shinyWidgets::updatePickerInput(
      session, "climate_zone_2", 
      choices = sort(
        session$userData$parameters_db[["lkp_climate"]]$climate_desc
      ),
      selected = session$userData$study_object()$climate_zone_2
    )
    
    shinyWidgets::updatePickerInput(
      session, "soil_description", 
      choices = sort(
        lkp_soil()$soil_desc
      ),
      selected = session$userData$study_object()$soil_description
    )
    
    shinyWidgets::updatePickerInput(
      session, "cropland_system", 
      choices = sort(
        lkp_croplandsystem()$sys_desc
      ),
      selected = session$userData$study_object()$cropland_system
    )
    
    shinyWidgets::updatePickerInput(
      session, "cropland_tillage", 
      choices = sort(
        lkp_tillageregime()$tillage_desc
      ),
      selected = session$userData$study_object()$cropland_tillage
    )
    
    shinyWidgets::updatePickerInput(
      session, "cropland_orgmatter", 
      choices = sort(
        lkp_organicmatter()$orgmatter_desc
      ),
      selected = session$userData$study_object()$cropland_orgmatter
    )
    
    shinyWidgets::updatePickerInput(
      session, "grassland_management", 
      choices = sort(
        lkp_grasslandman()$management_desc
      ),
      selected = session$userData$study_object()$grassland_management
    )
    
    shinyWidgets::updatePickerInput(
      session, "grassland_implevel", 
      choices = sort(
        lkp_grassinputlevel()$grassinputlevel_desc
      ),
      selected = session$userData$study_object()$grassland_implevel
    )
  })
})}
