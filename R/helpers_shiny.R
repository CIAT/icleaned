# add documenatation to this function:
#' Go to a specific tab
#' 
#' This function generates a JavaScript snippet to switch to a specific tab in a Shiny app.
#' 
#' @param tab The tab to switch to
#' @param target The target tab to switch to
#' 
#' @return A JavaScript snippet to switch to the target tab
#' 
#' @examples
#' go_to("tabcontent", "tab")
go_to <- function(tab, target) {
    glue::glue(
        "$('#tabcontent').data('shiny-input-binding').setValue($('#tabcontent'), '{target}');",
        if (target == "scenario") {
            # Hide id "maj_helper_btn"
            "$('#maj_helper_btn').hide();"
        } else if (target == "params_db") {
            # Hide id "maj_helper_btn"
            "$('#maj_helper_btn').hide();"
        } else if (target == "board") {
            # Show id "maj_helper_btn"
            "$('#maj_helper_btn').show();"
        }
    )
}

#' Include No Data placeholder
#' 
#' This placeholder is used in place of charts whene there is nothing to display
#' 
#' @text Text to display
include_no_data_placeholder <- function(text) {
    shinyjs::hidden(div(
        class = "bg-light no-data",
        style = "height: 320px",
        div(
            class = paste(
                "bg-white ms-5 mt-5 border-start border-left",
                "border-bottom border-dark h-100",
                "d-flex align-items-center justify-content-center"
            ),
            div(
                class = "no-data text-center fs-5 text-primary",
                style = "opacity: 0.5; font-family: Merriweather",
                icon("circle-exclamation"),
                p(
                    class = "color blue",
                    text
                )
            )
        )
    ))
}

#' Generate a dynamic legend with hidden items
#'
#' This function generates a legend component for a Shiny UI
#'
#' @param class The CSS class for the div container, typically "legend".
#' @param id The id attribute for the div container
#' @param ns A namespace function
#' @param n_items The maximum number of items in the legend.
#' @param id_item_prefix A prefix for the id of each legend item
#'
#' @return A Shiny div containing the legend with hidden items.
#' 
#' @examples
#' # For a seasons legend with max_seasons = 10:
#' generate_legend(class = "legend", id = "legend_1", ns = ns, n_items = 10, id_item_prefix = "Seasons")
#'
#' # For a feeds legend with max_feed_items = 8:
#' generate_legend(class = "legend", id = "legend_2", ns = ns, n_items = 8, id_item_prefix = "Feeds")
generate_legend <- function(class, id, ns, n_items, id_item_prefix) {
  div(
    class = class,
    id = ns(id),
    fluidRow(
      style = "width: 100%;",
      column(1),
      column(
        5,
        lapply(seq(1, n_items - 1, by = 2), function(i) {
          shinyjs::hidden(
            p(
              class = paste0("legend-text LegendText LegendItemtxt_", i), 
              id = ns(paste0(id_item_prefix, "_LegendItemtxt_", i))
            )
          )
        })
      ),
      column(
        5,
        lapply(seq(2, n_items, by = 2), function(i) {
          shinyjs::hidden(
            p(
              class = paste0("legend-text LegendText LegendItemtxt_", i), 
              id = ns(paste0(id_item_prefix, "_LegendItemtxt_", i))
            )
          )
        })
      ),
      column(1)
    )
  )
}

#' Extract User Roles from ID Token
#'
#' This function extracts user roles from a JSON Web Token (JWT). It takes an ID token,
#' decodes its payload, and returns the roles associated with the user.
#'
#' @param id_token A character string representing the ID token (JWT). The token should consist 
#' of three parts (header, payload, and signature) separated by periods.
#'
#' @return A character vector containing the user roles extracted from the token's payload.
#' If no roles are present, it will return `NULL`.
get_user_roles <- function(id_token) {
  # Split the token into its three parts (header, payload, signature)
  payload <- strsplit(id_token, ".", fixed = TRUE)[[1]][2]
  
  # Decode the Base64-encoded payload and convert to a character string
  payload_json <- rawToChar(base64enc::base64decode(payload))
  
  # Extract and return the user roles from the JSON payload
  fromJSON(payload_json)[["r_roles"]]
}

#' Validate Data Directory and Required Subdirectories
#'
#' This function checks whether a specified environment variable (default "DATA_DIR")
#' contains a valid path and ensures that the required subdirectories ("shared_pool",
#' "Users", and "Onboarding") exist within that directory.
#'
#' @param env_var The name of the environment variable that contains the data directory path.
#'                Defaults to "DATA_DIR".
#' @return A list with:
#'         - `valid`: TRUE if the directory and subdirectories are valid, FALSE otherwise.
#'         - `error_message`: A character string explaining the reason if validation fails.
validate_data_dir <- function(env_var = "DATA_DIR") {
  
  # Retrieve the directory path from the specified environment variable
  data_dir <- Sys.getenv(env_var)
  
  # If the environment variable is not set, return an invalid status with an error_message
  if (is.null(data_dir) || data_dir == "") {
    return(
      list(
        valid = FALSE, error_message = paste(env_var, "environment variable is not set")
      )
    )
  }
  
  # Define the required subdirectories within the data directory
  required_dirs <- c("shared_pool", "Users", "Onboarding")
  
  # Identify any missing subdirectories by checking if they exist
  missing_dirs <- required_dirs[!dir.exists(file.path(data_dir, required_dirs))]
  
  # If one or more directories are missing, return an invalid status with details
  if (length(missing_dirs) > 0) {
    return(
      list(
        valid = FALSE,
        error_message = paste(
          "Missing directories:",
          paste(missing_dirs, collapse = ", ")
        )
      )
    )
  }
  
  # If all checks pass, return a valid status
  return(list(valid = TRUE))
}

#' Update a cell in a data frame while preserving the column's data type.
#'
#' This function is designed to handle cell edits in the interactive data table (DT::datatable)
#' and ensures that numeric columns remain numeric when users clear a value in a cell.
#' If the column is numeric and the user clears the value (leaving it as an empty string), 
#' it replaces the empty string with `0`. For non-numeric columns, the cleared value remains as-is.
#'
#' @param data A data frame that holds the table data being edited.
#' @param info A list containing information about the edited cell.
#' @param offset A numeric offset to adjust the base column index shift.
#'
#' @return The updated data frame with the edited value applied, while preserving the original data types.
#' 
update_cell <- function(data, info, offset) {
  
  # Calculate the target column index by adding the offset
  target_col <- info$col + offset
  
  # Extract the target row and the new value from the 'info' list
  target_row <- info$row
  new_value <- info$value
  
  # Check if the target column is numeric
  if (is.numeric(data[[target_col]])) {
    # If numeric, replace empty strings with 0
    data[target_row, target_col] <- ifelse(new_value == "", 0, new_value)
  } else {
    # If non-numeric, store the value as-is (preserving the data type)
    data[target_row, target_col] <- new_value
  }
  
  # Return the updated data frame
  return(data)
}

#' Activate StatCounter Data Collection
#'
#' This function inserts the StatCounter tracking script into the UI of a Shiny app.
#' It dynamically creates the required script tags with the provided project ID and security token.
#'
#' @param sc_project A numeric project ID for the StatCounter account.
#' @param sc_security A character string representing the security token for the StatCounter account.
#' @param user_id A unique identifier for the current user.
#' @param url_hostname The hostname of the current URL.
#' @param file_path The path to the file where user data will be stored.
#' @import shiny
activate_statcounter <- function(sc_project, sc_security, user_id, url_hostname, file_path) {
  # Retrieve the user ID from the session
  user_data <- data.frame(
    user_id = user_id,
    url_hostname = url_hostname
  )
  # Append it to the existing file, if doesn't exist, then create it
  write.table(
    user_data,
    file = file_path,
    append = TRUE,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(file_path)
  )
  insertUI(
    selector = "head",
    where = "beforeEnd",
    ui = tagList(
      tags$script(src = "https://www.statcounter.com/counter/counter.js", async = NA),
      # Inject variables directly into a script tag
      tags$script(HTML(paste0("
        var sc_project = ", sc_project, ";
        var sc_invisible = 1;
        var sc_security = '", sc_security, "';
        ")))
    )
  )
}

# Temporarily freeze and unfreeze table scrolling after table editing
freeze_and_unfreeze_scroll <- function(session, table_id) {
  session$sendCustomMessage("freezeScroll", list(tableId = table_id))
  session$onFlushed(function() {
    session$sendCustomMessage("unfreezeScroll", list(tableId = table_id))
  }, once = TRUE)
}

#' Copy File or Directory with Optional Versioning
#'
#' This function copies a file or directory to a specified destination directory.
#' If versioning is enabled, it appends a counter suffix to the file or directory
#' name if a copy with the same name already exists, ensuring unique file names.
#'
#' @param source_path Character. The full path of the file or directory to copy.
#' @param destination_dir Character. The directory where the new copy will be created.
#' @param base_name Character. The base name for the new file or directory.
#' @param extension Character, optional. The file extension (e.g., `"json"`).
#'        Ignored if copying a directory.
#' @param versioning Logical. If `TRUE`, appends a counter to the file or directory name
#'        if a copy with the same name exists. Default is `TRUE`.
#' 
#' @return Character. The full path to the newly created file or directory copy.
#'
versioned_copy <- function(
    source_path, destination_dir, base_name, extension = NULL, versioning = TRUE) {
  
  # Initialize the version counter if versioning is enabled
  version_counter <- if (versioning) 0 else NULL
  destination_path <- ""
  
  # Construct the initial destination path and check for existence with optional versioning
  while (TRUE) {
    # Construct the name with an optional counter suffix
    suffix <- if (
      !is.null(version_counter) && version_counter > 0
    ) paste0(" (", version_counter, ")") else ""
    final_name <- paste0(
      base_name, suffix, if (!is.null(extension)) paste0(".", extension) else ""
    )
    destination_path <- file.path(destination_dir, final_name)
    
    # If destination path doesn't exist, proceed with copying
    if (!file.exists(destination_path) && !dir.exists(destination_path)) {
      if (dir.exists(source_path)) {
        dir.create(destination_path)  # Create destination directory
        file.copy(
          list.files(source_path, full.names = TRUE), destination_path, recursive = TRUE
        )
      } else if (file.exists(source_path)) {
        file.copy(source_path, destination_path)
      } else {
        stop("Source path does not exist.")
      }
      break  # Exit loop after copying
    }
    
    # Increment counter if versioning is enabled and try the next version
    if (!is.null(version_counter)) version_counter <- version_counter + 1 else break
  }
  
  # Return the path of the new copy
  return(destination_path)
}
