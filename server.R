server <- function(input, output, session) {
  
}

if (Sys.getenv("AUTH0_ENABLED") != "false") {
  auth0::auth0_server(server, info = auth0_info)
} else {
  server
}
