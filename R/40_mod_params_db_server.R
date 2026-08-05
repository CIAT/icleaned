params_db_server <- function(
    id
) { moduleServer(id, function(input, output, session) {
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))

  # Reactive value to track if modal is open (prevents multiple modals)
  modal_open <- reactiveVal(FALSE)
  
  # ------ * Initialize the parameters database inputs -------------------------
  observe({
    cat(file = stderr(), "40 - Initializing the parameters database inputs...\n")
    # Initialize the database input
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "database_name",
      choices = sort(
        list.files(
          file.path(session$userData$user_folder, "parameters_database"), full.names = FALSE
        )
      ),
      selected = character(0)
    )
    
    # Update the shared folder PickerInput with the list of examples
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "database_shared_folder",
      choices = sort(
        list.files(
          # use file.path function better "data" ..
          file.path("data", "shared_folder", "parameters_database"), full.names = FALSE
        )
      ),
      selected = character(0)
    )
    
    # ------ Last modification date --------------------------------------------
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
  
  # ------ * Update the PickerInputs after Super User Cloning
  observeEvent(session$userData$super_user_cloned_folder(), {
    if (session$userData$super_user_cloned_folder() == "parameters_database") {
      shinyWidgets::updatePickerInput(
        session,
        "database_name",
        choices = sort(
          list.files(
            path = file.path(session$userData$user_folder, "parameters_database"),
            full.names = FALSE
          )
        ),
        selected = input$database_name
      )
    }
  })
  
  # ----- UX Interaction logic -------------------------------------------------
  
  # Observe the selection from the radio buttons
  observeEvent(input$parameters_folder, {
    cat(file = stderr(), "40 - Observe the selection from the radio buttons...\n")
    if (input$parameters_folder == "user") {
      # Show User Folder UI and hide the others
      shinyjs::show(id = "user_folder_ui")
      shinyjs::hide(id = "shared_examples_ui")
      shinyjs::hide(id = "shared_pool_ui")
    } else if (input$parameters_folder == "shared") {
      # Show Shared Examples UI and hide the others
      shinyjs::show(id = "shared_examples_ui")
      shinyjs::hide(id = "user_folder_ui")
      shinyjs::hide(id = "shared_pool_ui")
    } else if (input$parameters_folder == "pool") {
      # Show Shared Pool UI and hide the others
      shinyjs::show(id = "shared_pool_ui")
      shinyjs::hide(id = "user_folder_ui")
      shinyjs::hide(id = "shared_examples_ui")
    }
  })
  # on renaming a database
  observeEvent(input$rename_database, {
    
    # If the selected database is a default (read-only) database, show an error message
    if (input$database_name %in% primary_database_names()) {
      showNotification(
        "Default parameters databases cannot be renamed. Clone one to create an editable copy.",
        duration = 3,
        type = "error"
      )
      return(NULL)
    }
    
    cat("40 - Renaming a database...\n")
    shinyjs::show(id = "rename_database_section")
    shinyjs::hide(id = "select_database_section")
    shinyjs::hide(id = "user_folder_ui")
  })
  # on canceling the renaming of a database
  observeEvent(input$cancel_database_rename, {
    cat("40 - Canceling the renaming of a database...\n")
    shinyjs::show(id = "select_database_section")
    shinyjs::show(id = "user_folder_ui")
    shinyjs::hide(id = "rename_database_section")
  })
  # on creating a new database
  observeEvent(input$create_database, {
    cat("40 - Creating a new database...\n")
    shinyjs::show(id = "select_database_section")
    shinyjs::hide(id = "upload_database_section")
    shinyjs::hide(id = "rename_database_section")
  })
  # on deleting a database
  observeEvent(input$delete_database, {
    
    # If the selected database is a default (read-only) database, show an error message
    if (input$database_name %in% primary_database_names()) {
      showNotification(
        "Default parameters databases cannot be deleted. Clone one to create an editable copy.",
        duration = 3,
        type = "error"
      )
      return(NULL)
    }
    
    cat("40 - Deleting a database...\n")
    shinyWidgets::ask_confirmation(
      inputId = ns("delete_database_confirmation"),
      type = "warning",
      title = "Delete Database?",
      btn_colors = c("grey", "#009ADB")
    )
  })
  # on confirming the deletion of a database
  observeEvent(input$delete_database_confirmation, {
    
    cat("40 - Confirming the deletion of a database...\n")
    
    req(input$delete_database_confirmation)
    
    source_file_path <- file.path(
      session$userData$user_folder, "parameters_database", input$database_name
    )
    
    # Delete the selected database folder
    unlink(source_file_path, recursive = TRUE)
    
    # Update the database Input
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "database_name",
      choices = sort(
        list.files(
          file.path(session$userData$user_folder, "parameters_database"), full.names = FALSE
        )
      )
    )
    
    # Update the session with the new list of databases
    session$userData$databases(
      list.files(
        file.path(session$userData$user_folder, "parameters_database"), full.names = FALSE
      )
    )
    
    # Json deletion success
    showNotification(
      "The parameters database has been deleted successfully!",
      duration = 3,
      type = "message"
    )
  })
  
  # ----------- Database Management Tab ----------------------------------------
  
  # Create Database: clone default parameters database into user folder
  observeEvent(input$create_database, {
    cat(file = stderr(), "40 - Creating a new database...\n")
    source_path <- file.path(
      "data", "primary_database", default_parameters_database
    )
    destination_dir <- file.path(session$userData$user_folder, "parameters_database")
    clone_file_path <- versioned_copy(
      source_path = source_path,
      destination_dir = destination_dir,
      base_name = new_parameters_database_name,
      versioning = TRUE
    )
    session$userData$databases(
      list.files(
        file.path(session$userData$user_folder, "parameters_database"), full.names = FALSE
      )
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "database_name",
      choices = sort(
        list.files(
          file.path(session$userData$user_folder, "parameters_database"), full.names = FALSE
        )
      ),
      selected = basename(clone_file_path)
    )
    showNotification(
      ui = "The parameters database has been created successfully!",
      duration = 3,
      type = "message"
    )
  })
  
  # Confirm rename action
  observeEvent(input$confirm_database_rename, {
    source_file_path <- file.path(
      session$userData$user_folder, "parameters_database", input$database_name
    )
    new_file_path <- file.path(
      session$userData$user_folder, "parameters_database", input$database_new_name
    )
    
    if (file.exists(new_file_path)) {
      # Show error message if new name already exists
      showNotification(
        "The new name is already in use. Please choose a different name.",
        duration = 5,
        type = "error"
      )
      
    } else if (file.exists(source_file_path) && input$database_new_name != "") {
      file.rename(source_file_path, new_file_path)
      
      # Update the selectInput with the new name
      shinyWidgets::updatePickerInput(
        session,
        "database_name",
        choices = sort(
          list.files(
            path = file.path(session$userData$user_folder, "parameters_database"),
            full.names = FALSE
          )
        ),
        selected = input$database_new_name
      )
      
      # Update the session with the new list of databases
      session$userData$databases(
        list.files(
          path = file.path(session$userData$user_folder, "parameters_database"),
          full.names = FALSE
        )
      )
      
      # Json renaming success
      showNotification(
        "The parameters database has been renamed successfully!",
        duration = 3,
        type = "message"
      )
      
      # Switch to select section after success
      shinyjs::show(id = "select_database_section")
      shinyjs::show(id = "user_folder_ui")
      shinyjs::hide(id = "rename_database_section")
    }
  })
  
  # Clone Database
  observeEvent(input$clone_database, {
    cat(file = stderr(), "40 - Cloning the parameters database...\n")
    # Define the source and destination paths
    source_path <- file.path(
      session$userData$user_folder, "parameters_database", input$database_name
    )
    destination_dir <- file.path(session$userData$user_folder, "parameters_database")
    
    # Generate a base clone directory name
    base_clone_file_name <- input$database_name
    
    # Use versioned_copy to clone the directory with versioning
    clone_file_path <- versioned_copy(
      source_path = source_path,
      destination_dir = destination_dir,
      base_name = base_clone_file_name,
      versioning = TRUE
    )
    
    # Update the selectInput with the cloned database
    shinyWidgets::updatePickerInput(
      session,
      "database_name",
      choices = sort(
        list.files(destination_dir, full.names = FALSE)
      ),
      selected = basename(clone_file_path)
    )
    
    # Update the session with the new list of databases
    session$userData$databases(list.files(destination_dir, full.names = FALSE))
    
    # Show a success notification for the cloning process
    showNotification(
      "The parameters database has been cloned successfully!",
      duration = 3,
      type = "message"
    )
  })
  
  # on sharing Params Database to pool
  observeEvent(input$share_to_pool, {
    shinyWidgets::ask_confirmation(
      inputId = ns("share_to_pool_confirmation"),
      type = "message",
      title = "Share the Parameters Database with other users?",
      btn_colors = c("grey", "#009ADB")
    )
  })
  # on confirming the sharing of a parameters database
  observeEvent(input$share_to_pool_confirmation, {
    req(input$share_to_pool_confirmation)
    
    # Share the related database if it's not a default (read-only) database
    if (!(input$database_name %in% primary_database_names())) {
      db_path <- file.path(
        session$userData$user_folder, "parameters_database", input$database_name
      )
      
      # Create file name
      file_name <- file.path(
        Sys.getenv("DATA_DIR"), "shared_pool", "parameters_database", input$database_name
      )
      
      # Create the destination directory if it doesn't exist
      if (!dir.exists(file_name)) {
        dir.create(file_name, recursive = TRUE)
      }
      
      # Copy the contents of the source directory to the destination directory, overwriting existing files
      file.copy(list.files(db_path, full.names = TRUE), file_name, recursive = TRUE, overwrite = TRUE)
      
      # Database sharing success
      showNotification(
        "The Params Database has been shared successfully!",
        duration = 3,
        type = "message"
      )
    }
  })
  
  # ----------- Shared Folder --------------------------------------------------
  # Observe the selected shared example
  observeEvent(input$database_shared_folder, {
    # Show a modal asking if they want to clone the file
    shinyWidgets::ask_confirmation(
      inputId = ns("confirm_clone_shared_example"),
      type = "question",
      title = "Do you want to clone this shared Parameters Database Example to your folder?",
      btn_colors = c("grey", "#009ADB")
    )
  })
  
  # Observe Cloning the shared Parameters Database example
  observeEvent(input$confirm_clone_shared_example, {
    
    # Check if the user confirmed or cancelled the cloning
    if (isTRUE(input$confirm_clone_shared_example)) {
      cat(file = stderr(), "40 - Cloning the shared Parameters Database example...\n")
      # Define the source and destination paths
      source_file_path <- file.path(
        "data", "shared_folder", "parameters_database", input$database_shared_folder
      )
      destination_dir <- file.path(session$userData$user_folder, "parameters_database")
      # Set the base name for the clone directory
      base_clone_file_name <- input$database_shared_folder
      
      # Use versioned_copy to clone the directory with versioning
      clone_file_path <- versioned_copy(
        source_path = source_file_path,
        destination_dir = destination_dir,
        base_name = base_clone_file_name,
        versioning = TRUE
      )
      
      # Reset the Picker Input for the shared database
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "database_shared_folder",
        selected = character(0)
      )
      
      # Update the radio button value to 'user'
      updateRadioButtons(session, inputId = "parameters_folder", selected = "user")
      
      # Update the PickerInput with the cloned database
      shinyWidgets::updatePickerInput(
        session,
        "database_name",
        choices = sort(
          list.files(destination_dir, full.names = FALSE)
        ),
        selected = basename(clone_file_path)
      )
      
      # Update the session's list of databases
      session$userData$databases(list.files(destination_dir, full.names = FALSE))
      
      # Show a success notification for the cloning process
      showNotification(
        "The Parameters Database has been cloned successfully!",
        duration = 3,
        type = "message"
      )
    } else {
      # Reset the picker input
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "database_shared_folder",
        selected = character(0)
      )
    }
  })
  
  # ----------- Shared Pool ----------------------------------------------------
  # Observe the Ok actionbutton
  observeEvent(input$pool_search_ok, {
    # Verify if the entered Parameters Database is available in the shared pool folder
    if (input$pool_file_search != "" &&
        input$pool_file_search %in% list.files(
          file.path(Sys.getenv("DATA_DIR"), "shared_pool", "parameters_database")
        )) {
      # Show a modal asking if they want to clone the file
      shinyWidgets::ask_confirmation(
        inputId = ns("confirm_clone_shared_pool"),
        type = "question",
        title = "Do you want to clone this shared Parameters Database to your folder?",
        btn_colors = c("grey", "#009ADB")
      )
    } else {
      # Show an error message if the file does not exist
      showNotification(
        "The Parameters Database does not exist in the shared pool folder.",
        duration = 5,
        type = "error"
      )
    }
  })
  
  # Observe Cloning the shared JSON from the pool
  observeEvent(input$confirm_clone_shared_pool, {
    
    # Check if the user confirmed or cancelled the cloning
    if (isTRUE(input$confirm_clone_shared_pool)) {
      cat(file = stderr(), "40 - Cloning the shared Parameters Database from the pool...\n")
      # Define the source and destination paths
      source_file_path <- file.path(
        Sys.getenv("DATA_DIR"), "shared_pool",
        "parameters_database", input$pool_file_search
      )
      destination_dir <- file.path(session$userData$user_folder, "parameters_database")
      
      # Set the base name for the clone directory
      base_clone_file_name <- input$pool_file_search
      
      # Use versioned_copy to clone the directory with versioning
      clone_file_path <- versioned_copy(
        source_path = source_file_path,
        destination_dir = destination_dir,
        base_name = base_clone_file_name,
        versioning = TRUE
      )
      
      # Reset the Text Input
      updateTextInput(session, "pool_file_search", value = "")
      
      # Update the radio button value to 'user'
      updateRadioButtons(session, inputId = "parameters_folder", selected = "user")
      
      # Update the PickerInput with the cloned database
      shinyWidgets::updatePickerInput(
        session,
        "database_name",
        choices = sort(
          list.files(destination_dir, full.names = FALSE)
        ),
        selected = basename(clone_file_path)
      )
      
      # Update the session's list of databases
      session$userData$databases(list.files(destination_dir, full.names = FALSE))
      
      # Show a success notification for the cloning process
      showNotification(
        "The Parameters Database has been cloned successfully!",
        duration = 3,
        type = "message"
      )
    }
  })
  
  # ------ * Load the selected database ----------------------------------------
  observeEvent(input$database_name, {
    req(input$database_name)
    selected_db <- input$database_name
    
    # Load from canonical primary_database for defaults, else from user's folder
    db_path <- if (selected_db %in% primary_database_names()) {
      file.path("data", "primary_database", selected_db)
    } else {
      file.path(session$userData$user_folder, "parameters_database", selected_db)
    }
    db_files <- list.files(db_path, full.names = TRUE)
    
    lapply(seq_along(db_files), function(i) {
      session$userData$parameters_db[[
        tools::file_path_sans_ext(basename(db_files[i]))
      ]] <- fread(db_files[i])
    })
    
    # Update the session with the selected database
    session$userData$parameters_db_name(selected_db)
    
  })
  
  # ------ * Observe database code change from scenario ------------------------
  observeEvent(session$userData$database_code(), {
    req(session$userData$database_code())
    
    # Update the selectInput with the used database
    shinyWidgets::updatePickerInput(
      session,
      "database_name",
      choices = sort(
        list.files(
          file.path(session$userData$user_folder, "parameters_database"),
          full.names = FALSE
        )
      ),
      selected = session$userData$database_code()
    )
  })
  
  # ------ * Render DT for each tab --------------------------------------------
  lapply(parameters_db_names, function(name) {
    output[[paste0("table_", name)]] <- renderDT({
      
      req(input$database_name)
      data_table <- session$userData$parameters_db[[name]]
      
      # For feeditem table, add crop_name from crops table
      if (name == "lkp_feeditem") {
        crops_table <- session$userData$parameters_db[["lkp_crops"]]
        if (!is.null(crops_table)) {
          # Ensure data_table is a data.frame
          data_table <- as.data.frame(data_table)
          
          # Add crop_name by matching crop_code
          data_table$crop_name <- crops_table$crop_name[
            match(data_table$crop_code, crops_table$crop_code)
          ]
          
          # Reorder columns: Insert crop_name at crop_code's position, move crop_code to end
          data_table <- reorder_feeditem_columns(data_table)
        }
      }
      
      # Checkboxes for selecting rows (to be deleted)
      data_table$selected_row <- generate_shiny_inputs(
        FUN = checkboxInput,
        len = nrow(data_table),
        id = ns(paste0("selected_row_", name)),
        value = rep(FALSE, nrow(data_table)),
        width = "2px"
      )
      
      # Move the checkbox column to the first position
      data_table <- as.data.frame(data_table)[
        , c("selected_row", setdiff(names(data_table), "selected_row")), drop = FALSE
      ]
      
      # Define the common column definitions
      column_defs <- list(
        # Set the width of the checkbox column to be small
        list(width = "2px", targets = 0)
      )
      
      # Add specific column definitions for the "lkp_feeditem" table
      if (name == "lkp_feeditem") {
        # Find crop_code column index using the NEW column order
        # The data_table has already been reordered above, so names(data_table) reflects the new order
        # Note: We subtract 1 because DataTables uses 0-based indexing for targets
        col_names <- names(data_table)
        crop_code_col_idx <- which(col_names == "crop_code") - 1
        crop_name_col_idx <- which(col_names == "crop_name") - 1
        
        column_defs <- append(column_defs, list(
          # Hide crop_code column (keep in data but don't display)
          # Using visible=FALSE, width=0px, and CSS to hide both header and cells
          list(
            targets = crop_code_col_idx,
            visible = FALSE,
            width = "0px",
            createdCell = hide_column_js()
          ),
          # Make crop_name clickable with pointer cursor
          list(
            targets = crop_name_col_idx,
            className = "crop-name-cell dt-body-left"
          )
        ))
      }
      
      # Add specific column definition for the "lkp_livetype" table
      if (name == "lkp_livetype") {
        column_defs <- append(column_defs, list(
          list(
            targets = which(
              names(data_table) %in% c(
                "livetype_code", "livetype_desc", "ipcc_meth_ef_t1",
                "ipcc_meth_ef_t2", "ipcc_meth_man", "ipcc_meth_exc")
            ) - 1,
            createdCell = JS(disable_and_add_cursor_js())
          )
        ))
      }
      
      # Disable buttons specifically for lkp_livetype
      if (name == "lkp_livetype") {
        shinyjs::disable(id = paste0("add_rows_", name))
        shinyjs::disable(id = paste0("delete_rows_", name))
        shinyjs::disable(id = paste0("clone_rows_", name))
      } else {
        # reset the buttons
        shinyjs::enable(id = paste0("add_rows_", name))
        shinyjs::enable(id = paste0("delete_rows_", name))
        shinyjs::enable(id = paste0("clone_rows_", name))
      }
      
      # Check if the database is a default (read-only) one
      if (input$database_name %in% primary_database_names()) {
        editablity <- FALSE
        
        # Append disabled cursor to existing column_defs
        # This preserves table-specific column definitions like crop_code hiding
        column_defs <- append(column_defs, list(
          list(
            targets = "_all", # Apply to all cells
            createdCell = JS(add_cursor_to_disabled_column_js())
          )
        ))
        
        # Disable all modification buttons
        shinyjs::disable(id = paste0("add_rows_", name))
        shinyjs::disable(id = paste0("delete_rows_", name))
        shinyjs::disable(id = paste0("clone_rows_", name))
      } else {
        # For feeditem table, disable crop_name editing (click only)
        if (name == "lkp_feeditem") {
          editablity <- list(
            target = "cell",
            # Prevent editing of checkbox and crop_name columns
            disable = list(
              columns = c(0, which(names(data_table) == "crop_name") - 1)
            )
          )
        } else {
          editablity <- list(
            target = "cell",
            # Prevent editing of the first column (check boxes for delete rows)
            disable = list(columns = 0)
          )
        }
      }
      
      # Determine number of fixed columns based on table type
      fixed_columns_count <- if (name %in% c("lkp_crops", "lkp_livetype")) {
        3  # Freeze checkbox + code + name columns
      } else {
        1  # Only freeze checkbox column
      }
      
      # Render datatable with all rows and frozen columns
      datatable(
        data = data_table,
        editable = editablity,
        rownames = FALSE,
        escape = FALSE,
        extensions = c("FixedColumns"),
        selection = "none",
        colnames = c("", colnames(data_table)[-1]),  # Hide first column name
        options = list(
          scrollX = TRUE,
          scrollY = "480px",
          processing = FALSE,
          paging = FALSE,
          searching = TRUE,
          info = FALSE,
          columnDefs = column_defs,
          drawCallback = JS(
            checkbox_link_multi(
              id = "selected_row",
              ns = ns,
              table_name = name
            )
          ),
          fixedColumns = list(leftColumns = fixed_columns_count),
          initComplete = JS(init_column_search_js())
        )
      )
      
    }, server = FALSE)
  })
  
  # ------ Check for duplicates on table load ----------------------------------
  # Detect and display alert message list of duplicate codes in parameter 
  # database tables
  lapply(parameters_db_names, function(name) {
    observe({
      # Only check duplicates for lkp_feeditem and lkp_crops
      if (!name %in% c("lkp_feeditem", "lkp_crops")) {
        return()
      }

      # Ensure database is selected and data table is available
      req(input$database_name)
      req(session$userData$parameters_db[[name]])
        
      # Extract table and identify the code column by name
      data_table <- session$userData$parameters_db[[name]]
      # Determine which column to check based on table name
      code_column <- if (name == "lkp_feeditem") {
        "feed_item_code"
      } else if (name == "lkp_crops") {
        "crop_code"
      }

      # Verify the column exists in the data table
      if (!code_column %in% names(data_table)) {
        return()
      }
    
      # Count occurrences of each code to find duplicates
      code_counts <- table(data_table[[code_column]])
      duplicate_codes <- names(code_counts[code_counts > 1])
      
      # Handle duplicate codes if any exist
      if (length(duplicate_codes) > 0) {
          
        # Sort duplicate codes intelligently:
        # - Numeric codes: sort numerically (e.g., 2, 10, 100)
        # - Text codes: sort alphabetically (e.g., A, B, C)
        numeric_codes <- suppressWarnings(as.numeric(duplicate_codes))
        if (all(!is.na(numeric_codes))) {
          duplicate_codes <- as.character(sort(numeric_codes))
        } else {
          duplicate_codes <- sort(duplicate_codes)
        }
          
        # Format each duplicate code with bold HTML tags for emphasis
        bold_codes <- sapply(
          duplicate_codes, 
          function(code) sprintf("<strong> %s </strong>", code)
        )
          
        # Build grammatically correct list of codes
        if (length(bold_codes) == 1) {
          codes_text <- bold_codes[1]
        } else if (length(bold_codes) == 2) {
          codes_text <- paste(bold_codes, collapse = " and ")
        } else {
          codes_text <- paste(
            paste(bold_codes[-length(bold_codes)], collapse = ", "), 
            "and", 
            bold_codes[length(bold_codes)]
          )
        }
          
        # Construct error message with column name and duplicate codes
        error_message <- sprintf(
          paste0(
            "In <strong>'%s'</strong> column, the following codes are duplicated : %s. ",
            "Please fix these duplications to have a unique code for each row."
          ),
          code_column, 
          codes_text
        )
          
        # Display the alert message to the user
        shinyjs::html(
          id = paste0("alert_duplicate_code_", name), 
          html = error_message
        )
        shinyjs::show(id = paste0("alert_duplicate_code_", name))
          
      } else {
        # No duplicates found - hide the alert message
        shinyjs::hide(id = paste0("alert_duplicate_code_", name))
      }
    })
  })
  # ------ * Observe edits on rendered DT  -------------------------------------
  lapply(parameters_db_names, function(name) {
    observeEvent(input[[paste0("table_", name, "_cell_edit")]], {
      req(input$database_name)
      req(!(input$database_name %in% primary_database_names()))
      # Get the info of the edited cell
      info <- input[[paste0("table_", name, "_cell_edit")]]
      new_data <- session$userData$parameters_db[[name]]
      
      # Update the specific cell while preserving the column's data type
      new_data <- update_cell(new_data, info, offset = 0)
      
      # Reassign to trigger reactivity
      session$userData$parameters_db[[name]] <- new_data
      
      # Freeze and restore scroll position after editing
      freeze_and_unfreeze_scroll(session, ns(paste0("table_", name)))
      
      # Write updated table back to CSV
      fwrite(
        session$userData$parameters_db[[name]],
        file.path(session$userData$user_folder, "parameters_database",
                  input$database_name, paste0(name, ".csv"))
      )
    })
  })
  
  # ------ * Handle crop_name click for feeditem table ------------------------
  # Show modal dialog to select a crop when clicking crop_name cell
  observeEvent(input$table_lkp_feeditem_cell_clicked, {
    info <- input$table_lkp_feeditem_cell_clicked
    req(length(info) > 0)
    # Don't allow editing default (read-only) DBs
    req(!(input$database_name %in% primary_database_names()))
    
    # Get data tables
    feeditem_data <- session$userData$parameters_db[["lkp_feeditem"]]
    crops_table <- session$userData$parameters_db[["lkp_crops"]]
    
    if (is.null(crops_table) || is.null(feeditem_data)) {
      return()
    }
    
    # Reconstruct the display table structure to get correct column names
    temp_data <- as.data.frame(feeditem_data)
    temp_data$crop_name <- crops_table$crop_name[
      match(temp_data$crop_code, crops_table$crop_code)
    ]
    
    # Reorder same as display
    temp_data <- reorder_feeditem_columns(temp_data)
    
    # Add checkbox column (first column in display)
    temp_data <- data.frame(
      selected_row = rep(FALSE, nrow(temp_data)),
      temp_data,
      stringsAsFactors = FALSE
    )
    
    # Check if clicked column is crop_name
    clicked_col_name <- names(temp_data)[info$col + 1]
    
    if (!is.null(info$col) && clicked_col_name == "crop_name") {
      if (modal_open()) {
        return()
      }
      modal_open(TRUE)
      
      # Use row index directly as the unique identifier
      # DataTables sends 1-based indices for this table config
      clicked_row_idx <- info$row
      
      # Verify row index is valid
      if (clicked_row_idx < 1 || clicked_row_idx > nrow(feeditem_data)) {
        return()
      }
      
      current_crop_code <- feeditem_data$crop_code[clicked_row_idx]
      
      # Get valid choices using helper function
      crop_data <- get_valid_crop_choices(crops_table, current_crop_code)
      
      showModal(modalDialog(
        title = "Select a Crop Name",
        shinyWidgets::pickerInput(
          inputId = ns("crop_selector"),
          label = NULL,
          choices = crop_data$choices,
          selected = crop_data$selected,
          options = list(`live-search` = TRUE)
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("ok_update_crop"), "OK"),
          actionButton(ns("cancel_crop_selector"), "Cancel")
        )
      ))
      
      # Store the row index (unique identifier) instead of code
      session$userData$feeditem_clicked_row_idx <- clicked_row_idx
    }
  })
  
  # ------ * Update crop_code when crop selection confirmed --------------------
  observeEvent(input$ok_update_crop, {
    req(input$crop_selector)
    req(!(input$database_name %in% primary_database_names()))
    clicked_row_idx <- session$userData$feeditem_clicked_row_idx
    
    if (!is.null(clicked_row_idx)) {
      new_data <- session$userData$parameters_db[["lkp_feeditem"]]
      
      if (clicked_row_idx < 1 || clicked_row_idx > nrow(new_data)) {
        modal_open(FALSE)
        removeModal()
        return()
      }
      
      # Update crop_code for the specific row index
      new_data$crop_code[clicked_row_idx] <- as.integer(input$crop_selector)
      
      # Update in session - reassign entire parameters_db to trigger reactivity
      temp_params_db <- session$userData$parameters_db
      temp_params_db[["lkp_feeditem"]] <- new_data
      session$userData$parameters_db <- temp_params_db
      
      # Write updated table back to CSV
      fwrite(
        new_data,
        file.path(
          session$userData$user_folder, "parameters_database",
          input$database_name, "lkp_feeditem.csv"
        )
      )
      
      # Freeze and restore scroll position after the table re-renders
      freeze_and_unfreeze_scroll(session, ns("table_lkp_feeditem"))
    }
    
    # Clear stored identifier
    session$userData$feeditem_clicked_code <- NULL
    modal_open(FALSE)
    removeModal()
  })
  
  # Handle cancel button click
  observeEvent(input$cancel_crop_selector, {
    modal_open(FALSE)
    session$userData$feeditem_clicked_code <- NULL
    removeModal()
  })
  
  # ------ DELETE ROW BUTTON ---------------------------------------------------
  # Delete selected parameter records and update memory + disk immediately.
  lapply(parameters_db_names, function(table_name) {
    observeEvent(input[[paste0("delete_rows_", table_name)]], {
      req(input$database_name)
      req(!(input$database_name %in% primary_database_names()))
      
      parameter_table <- session$userData$parameters_db[[table_name]]
      if (nrow(parameter_table) == 0) return()
      
      # Detect which checkboxes are selected
      selected_indices <- which(vapply(
        seq_len(nrow(parameter_table)),
        function(i) isTRUE(input[[paste0("selected_row_", table_name, "_", i)]]),
        logical(1)
      ))
      if (length(selected_indices) == 0) return()
      
      # Remove selected rows
      parameter_table <- parameter_table[-selected_indices, , drop = FALSE]
      
      # Update memory
      session$userData$parameters_db[[table_name]] <- parameter_table
      
      # Persist to disk
      fwrite(
        parameter_table,
        file.path(
          session$userData$user_folder,
          "parameters_database",
          input$database_name,
          paste0(table_name, ".csv")
        )
      )
      
      # Maintain visual continuity after deletion
      freeze_and_unfreeze_scroll(session, ns(paste0("table_", table_name)))
    })
  })
  
  # ------ ADD ROW BUTTON ------------------------------------------------------
  # Add a single empty record to the table.
  lapply(parameters_db_names, function(table_name) {
    observeEvent(input[[paste0("add_rows_", table_name)]], {
      req(input$database_name)
      req(!(input$database_name %in% primary_database_names()))
      
      # Retrieve current table from memory
      parameter_table <- session$userData$parameters_db[[table_name]]
      
      # Append one empty record with NA placeholders
      new_record <- as.list(rep(NA, ncol(parameter_table)))
      names(new_record) <- names(parameter_table)

      # Auto-generate unique code for first column
      code_column <- names(parameter_table)[1]
      new_record[[code_column]] <- generate_next_code(parameter_table, code_column)

      # Append to table
      parameter_table <- rbind(parameter_table, new_record)
      
      # Update in-memory data and persist to disk
      session$userData$parameters_db[[table_name]] <- parameter_table
      fwrite(
        parameter_table,
        file.path(
          session$userData$user_folder,
          "parameters_database",
          input$database_name,
          paste0(table_name, ".csv")
        )
      )
      
      # Keep table scroll position to prevent UI jump
      freeze_and_unfreeze_scroll(session, ns(paste0("table_", table_name)))
      
      # Auto-jump to the bottom to show newly added row
      session$onFlushed(function() {
        scroll_to_bottom(session, ns(paste0("table_", table_name)))
      }, once = TRUE)
    })
  })
  
  # ------ CLONE ROW BUTTON ----------------------------------------------------
  # Duplicates a selected parameter record and inserts it immediately after.
  # Only one record can be cloned at a time.
  lapply(parameters_db_names, function(table_name) {
    observeEvent(input[[paste0("clone_rows_", table_name)]], {
      req(input$database_name)
      req(!(input$database_name %in% primary_database_names()))
      
      parameter_table <- session$userData$parameters_db[[table_name]]
      if (nrow(parameter_table) == 0) return()
      
      # Find which record is selected
      selected_row <- which(vapply(
        seq_len(nrow(parameter_table)),
        function(i) isTRUE(input[[paste0("selected_row_", table_name, "_", i)]]),
        logical(1)
      ))
      req(length(selected_row) == 1)
      
      # Clone the selected record and ensure unique ID
      cloned_record <- parameter_table[selected_row, , drop = FALSE]
      id_column <- names(parameter_table)[1]
      
      if (!is.na(cloned_record[[id_column]]) && cloned_record[[id_column]] != "") {
        cloned_record[[id_column]] <- paste0(cloned_record[[id_column]], "_clone")
      } else {
        cloned_record[[id_column]] <- "new_clone"
      }
      
      # Insert cloned record immediately after original
      if (selected_row < nrow(parameter_table)) {
        parameter_table <- rbind(
          parameter_table[1:selected_row, ],
          cloned_record,
          parameter_table[(selected_row + 1):nrow(parameter_table), ]
        )
      } else {
        parameter_table <- rbind(parameter_table, cloned_record)
      }
      
      # Update memory and persist to disk
      session$userData$parameters_db[[table_name]] <- parameter_table
      fwrite(
        parameter_table,
        file.path(
          session$userData$user_folder,
          "parameters_database",
          input$database_name,
          paste0(table_name, ".csv")
        )
      )
      
      # Keep scroll position for a seamless editing experience
      freeze_and_unfreeze_scroll(session, ns(paste0("table_", table_name)))
    })
  })
})
}
