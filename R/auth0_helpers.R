bypass_auth0 <- function(ui, info) {
  function(req) {
    print(req$QUERY_STRING)
    query <- shiny::parseQueryString(req$QUERY_STRING)
    if (
      Sys.getenv("AUTH0_ENABLED") != "false" &&
      (is.null(query$bypass) || query$bypass != "true")
    ) {
      auth0::auth0_ui(ui, info)(req)
    } else {
      ui
    }
  }
}