ui <- fluidPage(
  title = "iCLEANED",
  # ------ BOOTSTRAP -----------------------------------------------------------
  theme = bslib::bs_theme(
    version = 5,
    primary = "#005275",
    light = "#f5f5f5",
    danger = "#E35230",
    success = "#889636",
    `font-size-base` = "1rem",
    `btn-border-radius` = "0px",
    `body-color` = "#444"
  ),
  shinyjs::useShinyjs(),
  # ------ Favicon -------------------------------------------------------------
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "img/logo.png")
  ),
  # ------ Scripts & styles ----------------------------------------------------
  includeCSS("www/css/app.min.css"), # App style
  includeScript("www/js/hack.js"), # Hack for bootstrap select dropdown
  includeScript("www/js/bg-modal.js"), # Change backdrop modal
  # Add JS code to freeze the horizontal scroll on table edit
  includeScript("www/js/horizontal-scroll.js"),
  includeScript("www/js/auth-url-cleanup.js"), # Clean the URL after login
  #   # ------ Header ----------------------------------------------------------
  tags$header(
    tags$nav(
      class = "navbar m-0",
      fixedPage(
        div(
          class = "navbar-brand d-flex align-items-center",
          onclick = go_to(target = "scenario"),
          img(
            src = "img/logo.png",
            width = 1073 / 13
          ),
          div(
            h1(
              "iCLEANED"
            ),
            p("Transforming Livestock for a Sustainable Future")
          )
        ),
        tags$ul(
          id = "menu",
          class = "navbar-nav ms-auto",
          tags$li(
            class = "nav-item",
            a(
              class = "nav-link active",
              id = "link_scenario",
              "Scenario - Enterprise Description",
              onclick = go_to(target = "scenario")
            )
          ),
          tags$li(
            class = "nav-item",
            a(
              class = "nav-link",
              id = "link_board",
              "Dashboard",
              onclick = go_to(target = "board")
            )
          ),
          tags$li(
            class = "nav-item modal-item",
            span(
              class = "nav-link help-navlink",
              shinyhelper::helper(
                span(
                  "?",
                  class = "help",
                  span("Help")
                ),
                size = "l",
                type = "inline",
                content = htmlTemplate(here::here("www/html/helper.html"))
              ),
            )
          ),
          tags$li(
            class = "nav-item modal-item",
            auth0::logoutButton(
              id = "logout",
              label = span(
                tags$svg(
                  xmlns = "http://www.w3.org/2000/svg",
                  viewBox = "0 0 23.71 19.58",
                  tags$g(
                    `stroke-width` = "3",
                    tags$path(
                      d = "M8.29 9.8h12M16.37 3.8l5.36 6.25-5.36 6.25"
                    )
                  ),
                  tags$path(
                    `stroke-width` = "2",
                    d = "M7.11.8 1 5.33v8.71l6.11 4.75"
                  )
                ),
                class = "help",
                span(
                  "Logout"
                )  
              )
            )
          )
        )
      )
    )
  ),
  # ------ Initial loading ---------------------------------------------------
  div(id = "spin_initial_dash", class = "cover-spin"),
  div(
    id = "loading_text_initial_dash",
    class = "loading-text",
    "Loading Data..."
  ),
  # ------ Body --------------------------------------------------------------
  div(
    class = "bg-primary",
    column(
      width = 12,
      fixedPage(
        div(
          class =
            "d-flex justify-content-between align-items-center py-1 topbar",
          p(
            span("Last scenario created:"),
            strong(span(id = "dataset_last_update"))
          ),
          div(
            class = "helper-btn me-0",
            id = "maj_helper_btn",
            tags$a(
              href = paste0(
                "https://docs.google.com/document/d/1MmQ1r_IK6mEi4pbBVXJ0pZqKRsvdGTv4/",
                "edit?usp=sharing&ouid=102650625222330231063&rtpof=true&sd=true"
              ),
              target = "_blank",  # Opens in a new tab
              class = "btn btn-blue",  # Use Bootstrap classes if needed
              tags$span(
                "GUIDE", 
                tags$i(class = "fa-solid fa-share-from-square ms-2")
              )
            )
          )
        )
      )
    )
  ),
  # ------ Tab Panels with different modules -----------------------------------
  tabsetPanel(
    id = "tabcontent", 
    type = "hidden",
    tabPanelBody(
      value = "scenario",
      scenario_ui("scenario")
    ),
    tabPanelBody(
      value = "params_db",
      params_db_ui("params_db")
    ),
    tabPanelBody(
      value = "board",
      fixedPage(
        board_simulation_ui("board"),
        board_comparison_ui("board")
      )
    )
  ),
  # ------ Footer --------------------------------------------------------------
  div(
    class = "footer bg-primary",
    tags$footer(
      fixedPage(
        div(
          class = "d-flex justify-content-between align-items-center",
          p("Copyright ©", class = "text-white"),
          shinyjs::hidden( # Hidden section for super user functionality
            div(
              id = "super_user_section",
              shinyFiles::shinyFilesButton(
                "browse_users_data",
                "Browse Files",
                "Users Data", # Visible title
                multiple = FALSE
              ),
              span(
                "?",
                class = "help",
                style = "display: inline-block; margin: 10px;",
                span(
                  strong("Cloning Files from Other Users:"), br(),
                  strong(" - Clone ", em("Study_objects"), ":"), br(),
                  "Select the desired study object file from any user's 
                  folder.", br(), "Confirm the cloning process to copy it into
                  your own ", em("Study_objects"), " folder.", br(),
                  strong(" - Clone ", em("Parameters_database, Scenarios, or 
                  Comparisons"), ":"), br(),
                  "Navigate to the specific folder and select any file available
                  in it.", br(), "The entire parent folder is cloned into your 
                  corresponding folder.", br(), br(), em("Note: Individual files
                  within these 3 last folders can not be cloned separately.")
                )
              ),
              downloadButton("download_onboarding", "Download Onboarding Folder"),
              span(
                "?",
                class = "help",
                style = "display: inline-block; margin: 10px;",
                span(
                  strong("Download ", em("Onboarding"), " Folder:"), br(),
                  "Click this button to download the ", em("Onboarding"), " folder,
                  which contains CSV files with user responses to the onboarding 
                  questionnaire.", br(), "The main file, ",
                  strong(em("onboarding.csv")), ", stores the most recent responses.",
                  br(), "Older versions of the onboarding CSV (due to question
                  changes) are saved with a timestamp to indicate when they
                  were replaced."
                )
              )
            )
          )
        )
      )
    )
  )
)

# ------ if Auth0 is enabled, add the Auth0 UI ---------------------------------
if (Sys.getenv("AUTH0_ENABLED") != "false") {
  auth0::auth0_ui(ui, info = auth0_info)
} else {
  ui
}
