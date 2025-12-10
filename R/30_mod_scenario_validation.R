# ------ VALIDATION MODULE -----------------------------------------------------
#'
#' Real-time validation for scenario input fields. Validates user inputs against
#' predefined rules and displays formatted error messages.
#'
#' Usage: Call \code{validation_server()} in the main server function.

# ------ VALIDATION RULES & ERROR MESSAGES -----------------------------------------------------

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

# ------ VALIDATION REGISTRY -----------------------------------------------------

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
  )
)

# ------ CORE VALIDATION LOGIC -----------------------------------------------------

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
#'
#' @details
#'   The function performs the following steps:
#'   1. Hides the alert element initially
#'   2. Validates each field with a non-null, non-NA value
#'   3. Collects error messages for invalid fields
#'   4. Displays combined error messages in the alert element if any errors found
#'   5. Returns TRUE if errors found, FALSE if all fields valid
execute_validation <- function(fields, alert_id) {
  # Initially hide the alert message element
  shinyjs::hide(id = alert_id, asis = TRUE)

  # Initialize empty vector to collect validation error messages
  validation_error_messages <- character(0)

  # Iterate through each field configuration
  for (field in fields) {
    # Only validate fields with non-null and non-NA values
    # Empty or NA values are considered acceptable and skip validation
    if (!is.null(field$value) && !is.na(field$value)) {
      # Apply the validation function to the field's current value
      is_valid <- field$validate_fn(field$value)

      # If validation fails, construct and collect error message
      if (!is_valid) {
        formatted_error_message <- sprintf(
          "<strong>•</strong> The value in <strong>'%s'</strong> is <strong>%s</strong>. %s",
          field$label,
          field$value,
          field$error_msg
        )
        validation_error_messages <- c(validation_error_messages, formatted_error_message)
      }
    }
  }

  # If any validation errors were found, display them
  if (length(validation_error_messages) > 0) {
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

# ------ SERVER MODULE -----------------------------------------------------

#' Validation Server Module
#'
#' Shiny server module that provides reactive validation for input fields.
#' Automatically validates inputs when they change and displays error messages.
#'
#' @param id Character string specifying the module namespace ID
#' @param input Shiny input object from the parent scope
#' @param parent_session Shiny session object from the parent scope, used for
#'   proper namespacing of alert elements
#'
#' @return A Shiny module server function
#'
#' @details
#'   This module monitors specified input fields and triggers validation
#'   whenever any field value changes. It uses the validation registry to
#'   determine which fields to monitor and which rules to apply.
#'
#' @examples
#' # In server.R:
#' validation_server("validation", input, session)
validation_server <- function(id, input, parent_session) {
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
  })
}
