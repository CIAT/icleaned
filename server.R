server <- function(input, output, session) {
  
  # ------ LOGOUT --------------------------------------------------------------
  observeEvent(input$logout, {
    cat("Logging out...\n")
    auth0::logout() # Logs the user out of the Auth0 session
  })
  
  # ------ * Loading spinner ---------------------------------------------------
  observe({
    shinyjs::hide(id = "spin_initial_dash")
    shinyjs::hide(id = "loading_text_initial_dash")
  })
  
  # ------ DATA_DIR VALIDATION -------------------------------------------------
  # Validate the DATA_DIR environment variable and its required subdirectories
  validation <- validate_data_dir(env_var = "DATA_DIR")
  
  # If validation fails, render an error page and stop the application
  if (!validation$valid) {
    
    # Log the validation failure and the error_message for easier debugging
    cat(file = stderr(), "Validation failed:", "\n", validation$error_message, "\n")
    
    # Replace the entire body with a pre-defined HTML error page
    shinyjs::html(
      selector = "body",
      html = as.character(
        htmlTemplate(here::here("www", "html", "data_dir_not_found.html"))
      )
    )
    
    # Return to stop further execution of the server code
    return()
  }
  
  # ------ INITIALIZATION ------------------------------------------------------
  observe({
    cat(file = stderr(), "Initializing the app...\n")
    # Define the user_id
    user_id <- session$userData$auth0_info$name
    
    # Set user_id and role based on whether AUTH0 is enabled or not.
    # If AUTH0 is disabled (i.e., dev mode), assign "dev" as user_id and "Super User" role.
    # Otherwise, retrieve the user's role from their authentication token.
    if (is.null(user_id) && Sys.getenv("AUTH0_ENABLED") == "false") {
      user_id <- "dev"
      user_role <- "Super User"  # Treat dev as a Super User
    } else {
      # Retrieve the user's role (can return an empty list, handled naturally by %in%)
      user_role <- get_user_roles(session$userData$auth0_credentials$id_token)
    }
    
    # Check if the user is a Super User
    if ("Super User" %in% user_role) {
      # Show the Super User Section if the user has the 'Super User' role
      shinyjs::show(id = "super_user_section")
      
      # Allow Only Super Users to browse the Users data folder
      shinyFiles::shinyFileChoose(
        input = input, id = "browse_users_data", roots = roots, session = session
      )
      
      # Allow only Super Users to download the onboarding folder
      output$download_onboarding <- downloadHandler(
        filename = function() {
          "onboarding.zip"
        },
        content = function(file) {
          cat(file = stderr(), "Downloading Onboarding folder\n")
          # Zip the files in the onboarding folder
          zip::zip(
            zipfile = file,
            files = list.files(
              file.path(Sys.getenv("DATA_DIR"), "Onboarding"), full.names = TRUE
            ),
            mode = "cherry-pick"
          )
        },
        contentType = "application/zip"
      )
    }
    
    # Define the user's folder path
    user_folder <- file.path(Sys.getenv("DATA_DIR"), "Users", user_id)
    
    # Check if the user's folder exists, if not, copy the default folder for the new user
    if (!dir.exists(user_folder)) {
      default_folder <- "data/default_data"
      
      # Copy the default folder to the new user folder
      fs::dir_copy(default_folder, user_folder)
      
      # Get the onboarding questions
      questions_data <- yaml::read_yaml("onboarding_questions.yaml")
      
      # Trigger onboarding modal
      showModal(
        tags$div(
          class = "onboarding-modal", # Allow custom CSS styling specific to this modal instance
          modalDialog(
            title = "Onboarding Questions",
            easyClose = FALSE,
            lapply(questions_data$questions, function(q) {
              tagList(
                h2(q$question, class = "mb-3 mt-3"),
                textAreaInput(
                  inputId = q$id,
                  label = NULL,
                  height = "75px"
                )
              )
            }),
            footer = tagList(
              actionButton("ok_onboarding", "Submit")
            )
          )
        )
      )
    } else {
      # ask user consent for data interaction collection
      if (!is.null(session$userData$auth0_info$name)) {
        shinyWidgets::ask_confirmation(
          inputId = "confirm_share_navigation_data",
          type = "question",
          title = paste(
            "To help us improve this tool, may we collect your navigation data?",
            "Your data will support ongoing enhancements."
          ),
          btn_colors = c("grey", "#009ADB"),
          btn_labels = c("No", "Yes"),
          width = "55%"
        )
      }
    }
    
    # run scenario counter
    session$userData$run_scenario_counter <- reactiveVal(0)
    
    # Store user folder in session$userData
    session$userData$user_folder <- user_folder
    
    # Initialize the parameters_db
    session$userData$parameters_db <- reactiveValues()
    
    # Helps updating the params module with the database used in scenario
    session$userData$database_code <- reactiveVal(NULL)
    
    # Helps to update the scenario database input on db addition/deletion
    session$userData$databases <- reactiveVal(NULL)
    
    # Helps to update the scenario database input after loading a scenario:
    # keep the selected selectinput's information
    session$userData$study_object <- reactiveVal(NULL)
    
    # Helps to manage the reactivity from update params to scenario inputs
    # only used database should update the scenario inputs
    session$userData$parameters_db_name <- reactiveVal("Params DB - Default")
    
    # Helps to trigger the update_params_modal from other modules
    session$userData$observe_update_params_button_click <- reactiveVal(0)
    
    # Load the study objects
    session$userData$study_objects <- reactiveVal(
      list.files(file.path(user_folder, "study_objects"), full.names = FALSE)
    )
    
    # Load the list of scenarios
    session$userData$scenarios_list <- reactiveVal(
      list.files(file.path(user_folder, "scenarios"), full.names = FALSE)
    )
    
    # Update the PickerInputs after Super User Cloning
    session$userData$super_user_cloned_folder <- reactiveVal(NULL)
    
    # ------ Most recent Json file  ------------------------------------------
    # most recent file in data/objects
    files_info <- file.info(
      list.files(file.path(user_folder, "study_objects"), full.names = TRUE)
    )
    # Get the most recent file name
    most_recent_file <- basename(
      rownames(files_info)[which.max(files_info$mtime)]
    )
    shinyjs::html(id = "dataset_last_update", html = most_recent_file)
  })
  
  # ------ ONBOARDING Questions ------------------------------------------------
  observeEvent(input$ok_onboarding, {
    
    cat(file = stderr(), "Submitting onboarding responses...\n")
    
    # Load onboarding questions from the YAML configuration file
    onboarding_questions <- yaml::read_yaml("onboarding_questions.yaml")
    
    # Capture new user responses from the form inputs
    user_responses <- sapply(onboarding_questions$questions, function(question) {
      input[[question$id]]
    })
    
    # Retrieve the current user's email from the session data
    user_email <- session$userData$auth0_info$name
    
    # Extract the current question texts (to be used as column names)
    question_texts <- sapply(onboarding_questions$questions, function(question) {
      question$question
    })
    
    # Create a new data frame with the user's email and their responses
    response_data <- data.frame(
      Email = user_email,
      t(user_responses),  # Transpose the responses to align as columns
      stringsAsFactors = FALSE
    )
    
    # Set the column names: "Email" followed by the actual question texts
    colnames(response_data) <- c("Email", question_texts)
    
    # Define the file path for storing the onboarding responses
    onboarding_file_path <- file.path(
      Sys.getenv("DATA_DIR"), "Onboarding", "onboarding.csv"
    )
    
    # Check if the onboarding CSV file already exists
    if (file.exists(onboarding_file_path)) {
      
      # Read the existing data, preserving special characters in the column names
      existing_data <- read.csv(
        onboarding_file_path, stringsAsFactors = FALSE, check.names = FALSE
      )
      
      # Extract the existing question column names (excluding the "Email" column)
      existing_question_texts <- colnames(existing_data)[-1]  # Remove "Email"
      
      # If the question texts have changed, rename the old file and start fresh
      if (!identical(existing_question_texts, question_texts)) {
        timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
        file.rename(
          onboarding_file_path,
          file.path(
            Sys.getenv("DATA_DIR"),
            "Onboarding",
            paste0("onboarding_", timestamp, ".csv")
          )
        )
        
        # Initialize a new data set with the user's responses
        updated_data <- response_data
      } else {
        # If the question texts match, append the new responses to the existing data
        updated_data <- rbind(existing_data, response_data)
      }
    } else {
      # If the CSV file doesn't exist, create a new one with the user's responses
      updated_data <- response_data
    }
    
    # Save the updated data to the onboarding CSV file
    write.csv(updated_data, onboarding_file_path, row.names = FALSE, quote = TRUE)
    
    # Close the modal and show a success alert to the user
    removeModal()
    
    shinyWidgets::show_alert(
      title = "Success",
      text = "Thank you for answering the questions!",
      type = "success",
      btn_labels = "Get Started",
      btn_class = "#009adb"
    )
    # ask user consent for data interaction collection
    if (!is.null(session$userData$auth0_info$name)) {
      shinyWidgets::ask_confirmation(
        inputId = "confirm_share_navigation_data",
        type = "question",
        title = paste(
          "To help us improve this tool, may we collect your navigation data?",
          "Your data will support ongoing enhancements."
        ),
        btn_colors = c("grey", "#009ADB"),
        btn_labels = c("No", "Yes"),
        width = "55%"
      )
    }
  })
  
  observeEvent(input$confirm_share_navigation_data, {
    cat(file = stderr(), "Sharing navigation data...\n")
    if (isTRUE(input$confirm_share_navigation_data)) {
      activate_statcounter(
        sc_project = sc_project,
        sc_security = sc_security,
        user_id = session$userData$auth0_info$name,
        url_hostname = input$client_ip_js,
        file_path = file.path(Sys.getenv("DATA_DIR"), "Users", "SC_linkage.csv")
      )
    }
  })
  
  # ------ * Super user browser button -----------------------------------------
  # Set the root to the Shiny app directory
  # Limits access to the Users data folder
  roots <- c(Users = file.path(Sys.getenv("DATA_DIR"), "Users"))
  
  # Trigger modal to confirm to clone the file/folder into the admin's folder
  observeEvent(input$browse_users_data, {
    cat(file = stderr(), "Cloning data confirmation modal\n")
    # Parse file paths selected by the user
    files <- shinyFiles::parseFilePaths(roots, input$browse_users_data)
    # Only trigger the confirmation modal if a valid file/folder is selected
    req(files$datapath)  # Ensure a valid selection has been made
    shinyWidgets::ask_confirmation(
      inputId = "confirm_clone_super_user",
      type = "question",
      title = "Do you want to clone the data to your folder?",
      btn_colors = c("grey", "#009ADB")
    )
  })
  
  # Observe Cloning the data
  observeEvent(input$confirm_clone_super_user, {
    cat(file = stderr(), "Cloning the data...\n")
    
    # Check if the user confirmed or cancelled the cloning
    if (isTRUE(input$confirm_clone_super_user)) {
      
      # Get the selected file or directory path
      files <- shinyFiles::parseFilePaths(roots, input$browse_users_data)
      selected_path <- files$datapath
      
      # Check if the user is attempting to clone from their own folder
      if (grepl(session$userData$user_folder, selected_path)) {
        showNotification(
          "You cannot clone data from your own folder!",
          duration = 3,
          type = "error"
        )
        return(NULL)
      }
      
      # Check if the file is in one of the special folders 
      # (parameters_database, scenarios, comparisons)
      if (grepl("parameters_database|scenarios|comparisons", selected_path)) {
        # Get the folder where the file resides
        folder_to_clone <- dirname(selected_path)
        folder_name <- basename(folder_to_clone)
        
        # Identify which parent folder the selected file belongs to
        parent_folder <- if (grepl("parameters_database", selected_path)) {
          "parameters_database"
        } else if (grepl("scenarios", selected_path)) {
          "scenarios"
        } else if (grepl("comparisons", selected_path)) {
          "comparisons"
        }
        
        # Define the destination directory for the clone
        destination_dir <- file.path(session$userData$user_folder, parent_folder)
        
        # Use versioned_copy to clone the folder with versioning
        clone_file_path <- versioned_copy(
          source_path = folder_to_clone,
          destination_dir = destination_dir,
          base_name = folder_name,
          versioning = TRUE
        )
        
        # Save the cloned parent folder name in the session data
        session$userData$super_user_cloned_folder(parent_folder)
        
        # Show a success notification for the cloning process
        showNotification(
          "The Folder has been cloned successfully!",
          duration = 3,
          type = "message"
        )
        
      } else if (grepl("study_objects", selected_path)) {
        
        # Get the file name and generate a base name without extension
        file_name <- basename(selected_path)
        base_clone_file_name <- tools::file_path_sans_ext(file_name)
        
        # Define the destination directory for the cloned file
        destination_dir <- file.path(session$userData$user_folder, "study_objects")
        
        # Use versioned_copy to clone the file with versioning
        clone_file_path <- versioned_copy(
          source_path = selected_path,
          destination_dir = destination_dir,
          base_name = base_clone_file_name,
          extension = "json",
          versioning = TRUE
        )
        
        # Save the cloned parent folder name in the session data
        session$userData$super_user_cloned_folder("study_objects")
        
        # Show a success notification for the cloning process
        showNotification(
          "The Json File has been cloned successfully!",
          duration = 3,
          type = "message"
        )
      }
    }
  })
  
  # ------ Initialize helpers popup --------------------------------------------
  shinyhelper::observe_helpers(withMathJax = TRUE)
  
  # ------ * UI MENU -----------------------------------------------------------
  observeEvent(input$tabcontent, {
    shinyjs::removeClass(
      selector = "#menu a",
      class = "active"
    )
    shinyjs::addClass(
      id = glue::glue("link_{input$tabcontent}"),
      class = "active"
    )
  })
  
  # ------ MODULES -------------------------------------------------------------
  
  # ------ * Board page --------------------------------------------------------
  board_simulation_server("board")
  board_comparison_server("board")
  
  # ------ * Scenario Input Page -----------------------------------------------
  scenario_server("scenario")
  
  # ------ * Params Input Page -------------------------------------------------
  params_db_server("params_db")
  
}

if (Sys.getenv("AUTH0_ENABLED") != "false") {
  auth0::auth0_server(server, info = auth0_info)
} else {
  server
}
