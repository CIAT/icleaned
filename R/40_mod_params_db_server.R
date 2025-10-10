params_db_server <- function(
    id
) { moduleServer(id, function(input, output, session) {
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  # ------ Pagination setup per table -------------------------------------------
  rv <- reactiveValues()
  rows_per_page <- 10
  for (nm in parameters_db_names) {
    rv[[paste0(nm, "_page")]] <- 1
  }
  
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
    
    # If the selected database is the Default database, show an error message
    if (input$database_name == "Params DB - Default") {
      showNotification(
        "The Default database cannot be renamed.",
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
    
    # If the selected database is the Default database, show an error message
    if (input$database_name == "Params DB - Default") {
      showNotification(
        "The Default database cannot be deleted.",
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
  
  # Create Database
  observeEvent(input$create_database, {
    cat(file = stderr(), "40 - Creating a new database...\n")
    # Define source and destination paths
    source_path <- file.path("data", "primary_database")
    destination_dir <- file.path(session$userData$user_folder, "parameters_database")
    
    # Copy the directory with versioning
    clone_file_path <- versioned_copy(
      source_path = source_path,
      destination_dir = destination_dir,
      base_name = "Params DB - Default",
      versioning = TRUE
    )
    
    # Update the session with the new list of databases
    session$userData$databases(
      list.files(
        file.path(session$userData$user_folder, "parameters_database"), full.names = FALSE
      )
    )
    
    # Update the selectInput with the new database
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
    
    # Database creation success
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
    
    # Share the related database if it's not the default database
    if (input$database_name != "Params DB - Default") {
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
    
    db_path <- file.path(session$userData$user_folder, "parameters_database", selected_db)
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
      
      # Add specific column definition for the "lkp_livetype" table
      if (name == "lkp_livetype") {
        column_defs <- append(column_defs, list(
          list(
            targets = which(
              names(data_table) %in% c(
                "livetype_desc", "ipcc_meth_ef_t1", "ipcc_meth_ef_t2",
                "ipcc_meth_man", "ipcc_meth_exc")
            ) - 1,
            createdCell = JS(disable_and_add_cursor_js()),
            searchable = FALSE
          )
        ))
      }
      
      # reset the buttons
      shinyjs::enable(id = paste0("add_rows_", name))
      shinyjs::enable(id = paste0("delete_rows_", name))
      shinyjs::enable(id = paste0("clone_rows_", name))
      
      # Check if the database is the default one
      if (input$database_name == "Params DB - Default") {
        editablity <- FALSE
        
        column_defs <- list(
          list(
            targets = "_all", # Apply to all cells
            createdCell = JS(add_cursor_to_disabled_column_js()),
            searchable = FALSE
          )
        )
        
        # Disable all modification buttons
        shinyjs::disable(id = paste0("add_rows_", name))
        shinyjs::disable(id = paste0("delete_rows_", name))
        shinyjs::disable(id = paste0("clone_rows_", name))
      } else {
        editablity <- list(
          target = "cell",
          disable = list(columns = 0)
        )
      }
      
      page_var <- paste0(name, "_page")
      current_page <- rv[[page_var]]
      
      start_row <- (current_page - 1) * rows_per_page + 1
      end_row <- min(start_row + rows_per_page - 1, nrow(data_table))
      paged_data <- data_table[start_row:end_row, , drop = FALSE]
      
      datatable(
        data = paged_data,
        editable = editablity,
        rownames = FALSE,
        escape = FALSE,
        extensions = "FixedColumns",
        selection = "none",
        # Set the first column (selected) name to an empty string
        colnames = c("", colnames(data_table)[-1]),
        options = list(
          scrollX = TRUE,
          processing = FALSE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          columnDefs = column_defs,
          drawCallback = JS(checkbox_link_multi(id = "selected_row", ns = ns, table_name = name)),
          fixedColumns = list(leftColumns = 1)
        )
      )
    }, server = FALSE)
  })
  
  
  # ------ Pagination observers per table ---------------------------------------
  lapply(parameters_db_names, function(name) {
    
    observeEvent(input[[paste0("next_page_", name)]], {
      total_rows <- nrow(session$userData$parameters_db[[name]])
      total_pages <- ceiling(total_rows / rows_per_page)
      if (rv[[paste0(name, "_page")]] < total_pages) {
        rv[[paste0(name, "_page")]] <- rv[[paste0(name, "_page")]] + 1
      }
    })
    
    observeEvent(input[[paste0("prev_page_", name)]], {
      if (rv[[paste0(name, "_page")]] > 1) {
        rv[[paste0(name, "_page")]] <- rv[[paste0(name, "_page")]] - 1
      }
    })
    
    output[[paste0("entries_info_", name)]] <- renderText({
      total_rows <- nrow(session$userData$parameters_db[[name]])
      if (total_rows == 0) return("No entries")
      start_row <- (rv[[paste0(name, "_page")]] - 1) * rows_per_page + 1
      end_row <- min(start_row + rows_per_page - 1, total_rows)
      paste0("Showing ", start_row, " - ", end_row, " of ", total_rows, " total entries")
    })
    
  })
  
  # ------ * Observe edits on rendered DT  -------------------------------------
  lapply(parameters_db_names, function(name) {
    observeEvent(input[[paste0("table_", name, "_cell_edit")]], {
      req(input$database_name)
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
  
  lapply(parameters_db_names, function(name) {
    observeEvent(input[[paste0("delete_rows_", name)]], {
      req(input$database_name)
      
      isolate({
        df <- session$userData$parameters_db[[name]]
        n <- nrow(df)
        if (n == 0) return(NULL)
        
        # ---- Determine current page -------------------------------------
        current_page <- rv[[paste0(name, "_page")]]
        start_row <- (current_page - 1) * rows_per_page + 1
        end_row <- min(start_row + rows_per_page - 1, n)
        
        # ---- Identify selected checkboxes on visible page ---------------
        visible_selected <- which(
          vapply(
            seq_len(end_row - start_row + 1),
            function(i) {
              val <- input[[paste0("selected_row_", name, "_", i)]]
              if (is.null(val)) FALSE else isTRUE(val)
            },
            logical(1)
          )
        )
        
        # ---- Map to global row indices ----------------------------------
        rows_to_delete <- start_row + visible_selected - 1
        
        # ---- Delete selected rows --------------------------------------
        if (length(rows_to_delete) > 0) {
          df <- df[-rows_to_delete, , drop = FALSE]
          session$userData$parameters_db[[name]] <- df
          
          # ---- Adjust current page if now too high ----------------------
          total_pages <- max(1, ceiling(nrow(df) / rows_per_page))
          if (rv[[paste0(name, "_page")]] > total_pages) {
            rv[[paste0(name, "_page")]] <- total_pages
          }
          
          # ---- Save updated table --------------------------------------
          data.table::fwrite(
            df,
            file.path(
              session$userData$user_folder,
              "parameters_database",
              input$database_name,
              paste0(name, ".csv")
            )
          )
          
          # ---- Keep scroll position -------------------------------------
          freeze_and_unfreeze_scroll(session, ns(paste0("table_", name)))
        }
      })
    })
  })
  
  
  lapply(parameters_db_names, function(name) {
    observeEvent(input[[paste0("add_rows_", name)]], {
      req(input$database_name)
      
      # ---- Add a new empty row ---------------------------------------
      session$userData$parameters_db[[name]] <- rbind(
        session$userData$parameters_db[[name]],
        as.list(rep(NA, ncol(session$userData$parameters_db[[name]])))
      )
      
      # ---- Save updated table ----------------------------------------
      fwrite(
        session$userData$parameters_db[[name]],
        file.path(
          session$userData$user_folder,
          "parameters_database",
          input$database_name,
          paste0(name, ".csv")
        )
      )
      
      # ---- Jump to last page -----------------------------------------
      total_rows <- nrow(session$userData$parameters_db[[name]])
      total_pages <- ceiling(total_rows / rows_per_page)
      rv[[paste0(name, "_page")]] <- total_pages
      
      # ---- Keep scroll position (refresh DT) -------------------------
      freeze_and_unfreeze_scroll(session, ns(paste0("table_", name)))
    })
  })
  
  # ------ * Observe Clone Row button ------------------------------------------
  lapply(parameters_db_names, function(name) {
    observeEvent(input[[paste0("clone_rows_", name)]], {
      req(input$database_name)
      
      isolate({
        # ---- 1. Retrieve data and setup ----------------------------------
        df <- session$userData$parameters_db[[name]]
        n <- nrow(df)
        if (n == 0) return(NULL)
        
        # ---- Determine current page -------------------------------------
        current_page <- rv[[paste0(name, "_page")]]
        start_row <- (current_page - 1) * rows_per_page + 1
        end_row <- min(start_row + rows_per_page - 1, n)
        
        # ---- Identify selected checkbox on visible page -----------------
        visible_selected <- which(
          vapply(
            seq_len(end_row - start_row + 1),
            function(i) isTRUE(input[[paste0("selected_row_", name, "_", i)]]),
            logical(1)
          )
        )
        req(length(visible_selected) == 1)
        row_index <- start_row + visible_selected - 1  # global index
        
        # ---- 2. Clone the selected row -----------------------------------
        cloned_row <- df[row_index, , drop = FALSE]
        
        # Update ID column
        id_col <- names(df)[1]
        if (!is.na(cloned_row[[id_col]]) && cloned_row[[id_col]] != "") {
          cloned_row[[id_col]] <- paste0(cloned_row[[id_col]], "_clone")
        } else {
          cloned_row[[id_col]] <- "new_clone"
        }
        
        # Insert cloned row right after original
        if (row_index < n) {
          df <- rbind(
            df[1:row_index, ],
            cloned_row,
            df[(row_index + 1):n, ]
          )
        } else {
          df <- rbind(df, cloned_row)
        }
        
        # ---- 3. Update memory + disk -------------------------------------
        session$userData$parameters_db[[name]] <- df
        data.table::fwrite(
          df,
          file.path(
            session$userData$user_folder,
            "parameters_database",
            input$database_name,
            paste0(name, ".csv")
          )
        )
        
        # ---- 5. Maintain scroll continuity -------------------------------
        freeze_and_unfreeze_scroll(session, ns(paste0("table_", name)))
      })
    })
  })
  
})}
