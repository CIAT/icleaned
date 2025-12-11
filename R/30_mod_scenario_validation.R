# ------ VALIDATION MODULE ---------------------------------------------------
#'
#' Real-time data validation engine providing immediate feedback on user inputs
#' across multiple application tabs. Implements rule-based validation logic with
#' dynamic error messaging to ensure data integrity and guide users toward
#' correct input values.
#'
#' VALIDATION SYSTEMS:
#'
#' 1. FARM TAB: farm_validation_server()
#'    Field-level validation for Area, Waste, and Manure inputs
#'    Rules: non-negative, positive, percentage, between 0-1
#'
#' 2. LIVESTOCK FEEDING TAB: livestock_feeding_validation_server()
#'    Table-level validation for allocation percentages
#'    Rule: Each livestock column must sum to exactly 100% per season

# ------ SECTION 1: FARM TAB VALIDATION --------------------------------------

# VALIDATION RULES & ERROR MESSAGES ------------------------------------------

# Lookup table for validation rule functions
# Each function takes a numeric value and returns TRUE if valid, FALSE otherwise
validation_conditions <- list(
  # Validates value is zero or greater
  non_negative = function(x) x >= 0,
  # Validates value is strictly greater than zero
  positive = function(x) x > 0,
  # Validates value is a percentage (0-100 inclusive)
  percentage = function(x) x >= 0 && x <= 100,
  # Validates value is a proportion (0-1 inclusive)
  between_0_1 = function(x) x >= 0 && x <= 1
)

# Error message templates corresponding to each validation rule
# These are displayed to users when validation fails
error_message_templates <- list(
  non_negative = "It must be ≥ 0!",
  positive = "It must be > 0!",
  percentage = "It must be ≥ 0 and ≤ 100!",
  between_0_1 = "It must be ≥ 0 and ≤ 1!"
)

# Configuration for area input fields
# Each field specifies its input ID, display label, and validation rule
area_fields_config <- list(
  list(
    id = "soil_n",
    label = "Soil N (g/kg)",
    validator = "non_negative"
  ),
  list(
    id = "soil_c",
    label = "Soil C (g/kg)",
    validator = "non_negative"
  ),
  list(
    id = "soil_clay",
    label = "Soil clay (%)",
    validator = "non_negative"
  ),
  list(
    id = "soil_bulk",
    label = "Bulk density (g/cm³)",
    validator = "non_negative"
  ),
  list(
    id = "soil_depth",
    label = "Soil depth (m)",
    validator = "non_negative"
  ),
  list(
    id = "et",
    label = "ET₀ (mm/year)",
    validator = "non_negative"
  ),
  list(
    id = "grassland_toarable",
    label = "Conversion Grassland to arable land (ha)",
    validator = "non_negative"
  ),
  list(
    id = "arable_tograssland",
    label = "Conversion Arable land to grassland (ha)",
    validator = "non_negative"
  )
)

# Configuration for waste input fields (percentages)
# Each field specifies its input ID, display label, and validation rule
waste_fields_config <- list(
  list(
    id = "waste_production_milk",
    label = "Production (milk)",
    validator = "percentage"
  ),
  list(
    id = "waste_distribution_milk",
    label = "Distribution (milk)",
    validator = "percentage"
  ),
  list(
    id = "waste_processing_milk",
    label = "Processing (milk)",
    validator = "percentage"
  ),
  list(
    id = "waste_consume_milk",
    label = "Consumption (milk)",
    validator = "percentage"
  ),
  list(
    id = "waste_production_meat",
    label = "Production (meat)",
    validator = "percentage"
  ),
  list(
    id = "waste_distribution_meat",
    label = "Distribution (meat)",
    validator = "percentage"
  ),
  list(
    id = "waste_processing_meat",
    label = "Processing (meat)",
    validator = "percentage"
  ),
  list(
    id = "waste_consume_meat",
    label = "Consumption (meat)",
    validator = "percentage"
  )
)

# Configuration for manure/fertilizer input fields
# Each field specifies its input ID, display label, and validation rule
manure_fields_config <- list(
  list(
    id = "purchased_manure",
    label = "Annual purchase of manure (kg N)",
    validator = "non_negative"
  ),
  list(
    id = "purchased_compost",
    label = "Annual purchase of compost (kg N)",
    validator = "non_negative"
  ),
  list(
    id = "purchased_organic_n",
    label = "Annual purchase of other organic N additions (kg N)",
    validator = "non_negative"
  ),
  list(
    id = "purchased_bedding",
    label = "Annual purchase of bedding materials (kg N)",
    validator = "non_negative"
  )
)

# ------ VALIDATION REGISTRY -------------------------------------------------

# Central registry organizing validation configurations by input group
# Each entry contains field configurations and the associated alert element ID
validation_registry <- list(
  area_inputs = list(
    fields = area_fields_config,
    alert_id = "alert_message_area_inputs"
  ),
  waste_inputs = list(
    fields = waste_fields_config,
    alert_id = "alert_message_waste_inputs"
  ),
  manure_inputs = list(
    fields = manure_fields_config,
    alert_id = "alert_message_manure_inputs"
  )
)

# ------ CORE VALIDATION LOGIC -----------------------------------------------

#' Execute validation logic and synchronize UI state
#'
#' Validates all provided fields against their validation rules and displays
#' error messages in the UI alert element when validation fails.
#'
#' @param fields List of field objects, each containing:
#'   - id: Input field identifier
#'   - label: Human-readable field name
#'   - value: Current field value
#'   - validate_fn: Validation function
#'   - error_msg: Error message template
#' @param alert_id Character string specifying the UI element ID for displaying
#'   validation messages
#'
#' @return Invisible logical: TRUE if validation errors found, FALSE otherwise

execute_validation <- function(fields, alert_id) {
  # Initially hide the alert message element
  shinyjs::hide(id = alert_id, asis = TRUE)

  # Initialize empty list to collect validation errors with field information
  validation_errors <- list()

  # Iterate through each field configuration with index
  for (i in seq_along(fields)) {
    field <- fields[[i]]

    # Only validate fields with non-null and non-NA values
    # Empty or NA values are considered acceptable and skip validation
    if (!is.null(field$value) && !is.na(field$value)) {
      # Apply the validation function to the field's current value
      is_valid <- field$validate_fn(field$value)

      # If validation fails, construct and collect error with field index
      if (!is_valid) {
        formatted_error_message <- sprintf(
          "<strong>•</strong> The value in <strong>'%s'</strong> is <strong>%s</strong>. %s",
          field$label,
          field$value,
          field$error_msg
        )

        # Store error with field index for sorting
        validation_errors[[length(validation_errors) + 1]] <- list(
          index = i,
          message = formatted_error_message
        )
      }
    }
  }

  # If any validation errors were found, sort and display them
  if (length(validation_errors) > 0) {
    # Sort errors by field index to maintain config order
    sorted_errors <- validation_errors[order(sapply(validation_errors, function(e) e$index))]

    # Extract just the messages
    validation_error_messages <- sapply(sorted_errors, function(e) e$message)

    # Combine all error messages with line breaks for display
    combined_error_html <- paste(validation_error_messages, collapse = "<br>")

    # Update the alert element with the combined error messages
    shinyjs::html(id = alert_id, html = combined_error_html, asis = TRUE)

    # Make the alert element visible
    shinyjs::show(id = alert_id, asis = TRUE)

    return(invisible(TRUE))
  }

  # No validation errors found
  return(invisible(FALSE))
}

# ------ FARM TAB VALIDATION SERVER MODULE -----------------------------------

#' Farm Tab Validation Server Module
#'
#' TARGET TAB: Farm
#'
#' Validates Area, Waste, and Manure input fields. Displays errors in
#' tab-specific alert elements.
#'
#' @param id Module namespace ID
#' @param input Shiny input object from parent scope
#' @param parent_session Shiny session object from parent scope
#'
#' @return Shiny module server function

farm_validation_server <- function(id, input, parent_session) {
  moduleServer(id, function(input_module, output, session) {
    #' Prepare field configurations with current input values
    #'
    #' Enriches field configuration with current input values and resolves
    #' validator function references to actual functions.
    #'
    #' @param field_config_list List of field configuration objects
    #'
    #' @return List of enriched field objects ready for validation
    prepare_fields <- function(field_config_list) {
      lapply(field_config_list, function(field_config) {
        # Create a copy of the field configuration
        enriched_field <- field_config

        # Add the current value from the input object
        enriched_field$value <- input[[field_config$id]]

        # Resolve validator name to actual validation function and error message
        if (!is.null(field_config$validator)) {
          enriched_field$validate_fn <- validation_conditions[[field_config$validator]]
          enriched_field$error_msg <- error_message_templates[[field_config$validator]]
        }

        return(enriched_field)
      })
    }

    # Retrieve validation configuration for area inputs
    area_validation_config <- validation_registry$area_inputs

    # Create a reactive trigger that fires when any area input field changes
    # This reactive returns a list of all field values, causing it to
    # re-execute whenever any field in the list changes
    area_inputs_trigger <- reactive({
      lapply(area_validation_config$fields, function(field_config) {
        input[[field_config$id]]
      })
    })

    # Observe the reactive trigger and execute validation when it fires
    observeEvent(area_inputs_trigger(), {
      # Prepare fields with current values and validation functions
      prepared_fields <- prepare_fields(
        area_validation_config$fields
      )

      # Execute validation and update UI
      execute_validation(
        fields = prepared_fields,
        alert_id = parent_session$ns(area_validation_config$alert_id)
      )
    })

    # Retrieve validation configuration for waste inputs
    waste_validation_config <- validation_registry$waste_inputs

    # Create a reactive trigger that fires when any waste input field changes
    # This reactive returns a list of all field values, causing it to
    # re-execute whenever any field in the list changes
    waste_inputs_trigger <- reactive({
      lapply(waste_validation_config$fields, function(field_config) {
        input[[field_config$id]]
      })
    })

    # Observe the reactive trigger and execute validation when it fires
    observeEvent(waste_inputs_trigger(), {
      # Prepare fields with current values and validation functions
      prepared_fields <- prepare_fields(
        waste_validation_config$fields
      )

      # Execute validation and update UI
      execute_validation(
        fields = prepared_fields,
        alert_id = parent_session$ns(waste_validation_config$alert_id)
      )
    })

    # Retrieve validation configuration for manure inputs
    manure_validation_config <- validation_registry$manure_inputs

    # Create a reactive trigger that fires when any manure input field changes
    # This reactive returns a list of all field values, causing it to
    # re-execute whenever any field in the list changes
    manure_inputs_trigger <- reactive({
      lapply(manure_validation_config$fields, function(field_config) {
        input[[field_config$id]]
      })
    })

    # Observe the reactive trigger and execute validation when it fires
    observeEvent(manure_inputs_trigger(), {
      # Prepare fields with current values and validation functions
      prepared_fields <- prepare_fields(
        manure_validation_config$fields
      )

      # Execute validation and update UI
      execute_validation(
        fields = prepared_fields,
        alert_id = parent_session$ns(manure_validation_config$alert_id)
      )
    })
  })
}

# ------ SECTION 2: LIVESTOCK FEEDING TAB VALIDATION -------------------------

# ------ LIVESTOCK FEEDING TABLE VALIDATION ----------------------------------

#' Validate Livestock Feeding Allocation Percentages
#'
#' Validates that allocation percentages for each livestock type sum to 100%
#' per season. This is a table-based validation where each column (livestock)
#' must sum to exactly 100 across all feed rows.
#'
#' @param season_data Data frame containing allocation percentages for one season
#'   Rows represent feeds, columns represent livestock types
#' @param season_name Character string specifying the season name for error messages
#'
#' @return List containing:
#'   - has_errors: Logical, TRUE if any column sum is not 100
#'   - error_messages: Character vector of formatted error messages

validate_season_allocations <- function(season_data, season_name) {
  # Initialize result structure
  validation_result <- list(
    has_errors = FALSE,
    error_messages = character(0)
  )

  # Return early if data is NULL or empty
  if (is.null(season_data) || nrow(season_data) == 0 || ncol(season_data) == 0) {
    return(validation_result)
  }

  # Exclude the "Total" row from validation (it's auto-calculated)
  # We only validate the actual allocation rows
  data_without_total <- season_data[rownames(season_data) != "Total", , drop = FALSE]

  # Return early if no data rows to validate
  if (nrow(data_without_total) == 0) {
    return(validation_result)
  }

  # Calculate column sums for each livestock type
  column_sums <- colSums(data_without_total, na.rm = TRUE)

  # Identify columns where sum is not 100 (with small tolerance for floating point)
  # Using tolerance of 0.01 to handle floating point arithmetic issues
  tolerance <- 0.01
  invalid_columns <- which(abs(column_sums - 100) > tolerance)

  # If any invalid columns found, generate error messages
  if (length(invalid_columns) > 0) {
    validation_result$has_errors <- TRUE

    # Generate formatted error message for each invalid column
    validation_result$error_messages <- sapply(invalid_columns, function(col_index) {
      livestock_name <- names(column_sums)[col_index]
      actual_sum <- round(column_sums[col_index], 2)

      sprintf(
        paste0(
          "<strong>•</strong> For season <strong>'%s'</strong>, ",
          "the total of feed allocations in <strong>'%s'</strong> ",
          "is <strong>%s%%</strong>. It must equal <strong>100%%</strong>."
        ),
        season_name,
        livestock_name,
        actual_sum
      )
    })
  }

  return(validation_result)
}

#' Livestock Feeding Tab Validation Server Module
#'
#' TARGET TAB: Livestock Feeding
#'
#' Validates allocation tables: each livestock column must sum to 100% per season.
#' Uses single observer pattern to prevent observer accumulation.
#'
#' @param id Module namespace ID
#' @param basket_data ReactiveValues containing allocation data per season
#' @param seasons Reactive data frame with season information
#' @param parent_session Shiny session object from parent scope
#'
#' @return Shiny module server function


livestock_feeding_validation_server <- function(
  id,
  basket_data,
  seasons,
  parent_session
) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to track validation errors for each season
    # Using reactiveValues() allows direct assignment
    season_errors <- reactiveValues()

    # Single observer that validates all seasons when any data changes
    observeEvent(c(seasons(), reactiveValuesToList(basket_data)), {
      # Get current seasons
      current_seasons <- if (!is.null(seasons()) && nrow(seasons()) > 0) {
        seasons()$Season
      } else {
        character(0)
      }

      # Clean up errors for deleted seasons
      all_error_keys <- names(reactiveValuesToList(season_errors))
      deleted_keys <- setdiff(all_error_keys, current_seasons)
      for (key in deleted_keys) {
        season_errors[[key]] <- NULL
      }

      # Validate each existing season
      for (season in current_seasons) {
        season_data <- basket_data[[season]]
        
        # Skip if no valid data
        if (is.null(season_data) || !is.data.frame(season_data) || nrow(season_data) == 0) {
          next
        }

        # Validate and store errors
        validation_result <- validate_season_allocations(
          season_data = season_data,
          season_name = season
        )
        season_errors[[season]] <- if (validation_result$has_errors) {
          validation_result$error_messages
        } else {
          NULL
        }
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Observer to update the global alert based on all validation errors
    observe({
      global_alert_id <- parent_session$ns("alert_livestock_feeding_global")
      
      # Get all error messages
      all_errors <- reactiveValuesToList(season_errors)
      non_null_errors <- all_errors[!sapply(all_errors, is.null)]
      messages <- unlist(non_null_errors)

      if (length(messages) > 0) {
        # Show alert with combined errors
        combined_html <- paste(messages, collapse = "<br>")
        shinyjs::html(id = global_alert_id, html = combined_html, asis = TRUE)
        shinyjs::show(id = global_alert_id, asis = TRUE)
      } else {
        # Hide alert when no errors
        shinyjs::hide(id = global_alert_id, asis = TRUE)
      }
    })
  })
}
