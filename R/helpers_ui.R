# UI helpers - shared patterns across modules -------------------------------

#' Show/hide folder choice UIs based on radio button selection.
#' Used when user selects "User Folder", "Shared Examples", or "Shared Pool".
#' Keeps the same pattern in scenario and params_db modules for consistency.
#'
#' @param folder_value Character: "user", "shared", or "pool" from the radio input
#' @param ns Namespace function (e.g. session$ns from a Shiny module)
#' @noRd
show_hide_folder_ui <- function(folder_value, ns) {
  if (folder_value == "user") {
    # Show User Folder UI and hide the others
    shinyjs::show(id = ns("user_folder_ui"))
    shinyjs::hide(id = ns("shared_examples_ui"))
    shinyjs::hide(id = ns("shared_pool_ui"))
  } else if (folder_value == "shared") {
    # Show Shared Examples UI and hide the others
    shinyjs::show(id = ns("shared_examples_ui"))
    shinyjs::hide(id = ns("user_folder_ui"))
    shinyjs::hide(id = ns("shared_pool_ui"))
  } else if (folder_value == "pool") {
    # Show Shared Pool UI and hide the others
    shinyjs::show(id = ns("shared_pool_ui"))
    shinyjs::hide(id = ns("user_folder_ui"))
    shinyjs::hide(id = ns("shared_examples_ui"))
  }
}
