params_db_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    div(
      class = "container",
      # ------ Last Update -----------------------------------------------------
      p(
        class = "pt-3 pb-5 text-end text-primary text-small",
        span("Last update: "),
        span(strong(id = ns("last_update_date")))
      ),
    ),
    fixedPage(
      class = "bg-light px-4 pt-5 pb-5  mt-5 mb-5",
      style = "margin-top: 20px;",
      h2("Choose a parameters database option:", class = "mb-3"),
      fluidRow(
        column(
          width = 6,
          div(
            class = "d-flex align-items-center px-1",
            actionButton(
              inputId = ns("create_database"),
              label = HTML("&nbsp;&nbsp;&nbsp; New Parameters Database"),
              class = "btn btn-primary filters-btn",
              icon("add")
            )
          )
        ),
        column(
          width = 6,
        )
      ),
      div(
        id = ns("select_database_section"),
        fluidRow(
          class = "mb-3 mt-5",
          column(
            width = 4,
            h2("Select Parameters Database:")
          ),
          column(3, 
                 div(
                   style = "display: flex; align-items: center;",
                   span(
                     "?",
                     class = "help",
                     style = "display: inline-block; margin-right: 10px;",
                     span(
                       "When choosing a parameters database, you can: ", br(),
                       "1. Browse your own folder to select files.", br(),
                       "2. Clone shared examples provided by the admins", br(),
                       "3. Search in the shared pool for files by name, which
                       you can clone to your folder if found.", br(),
                       "Note: If you want to share a file with others, make 
                       sure it has a very unique name. 
                       Otherwise, someone could overwrite your file and it 
                       may be lost."
                     )
                   ),
                   p("Choose Parameters Folder", 
                     class = "baseSenerioP",
                     style = "display: inline-block;"),
                 )
          ),
          column(5, 
                 tags$div(
                   class = "baseSenerioP",
                   radioButtons(
                     inputId = ns("parameters_folder"),
                     label = NULL,
                     choices =  c("User Folder" = "user", "Shared Examples" = "shared", "Shared Pool" = "pool"),
                     inline = TRUE
                   )
                 )
          )
        )
      ),
      # First Option UI (User Folder)
      div(
        id = ns("user_folder_ui"), # Add an ID to manage this section
        fluidRow(
          column(
            width = 4,
            div(
              class = "d-flex align-items-center px-1",
              shinyWidgets::pickerInput(
                inputId = ns("database_name"),
                label = NULL,
                choices = NULL
              )
            )
          ),
          column(
            width = 2,
            div(
              class = "d-flex align-items-center px-1",
              actionButton(
                inputId = ns("rename_database"),
                label = "Rename",
                class = "btn btn-primary filters-btn",
                icon = icon("edit")
              )
            )
          ),
          column(
            width = 2,
            div(
              class = "d-flex align-items-center px-1",
              actionButton(
                inputId = ns("share_to_pool"),
                label = "Share",
                class = "btn btn-primary filters-btn",
                icon = icon("share")
              )
            )
          ),
          column(
            width = 2,
            div(
              class = "d-flex align-items-center px-1",
              actionButton(
                inputId = ns("clone_database"),
                label = "Clone",
                class = "btn btn-primary filters-btn",
                icon = icon("copy")
              )
            )
          ),
          column(
            width = 2,
            div(
              class = "d-flex align-items-center px-1",
              actionButton(
                inputId = ns("delete_database"),
                label = "Delete",
                class = "btn btn-primary filters-btn",
                icon = icon("trash")
              )
            )
          )
        )
      ),
      # Second Option UI (Shared Examples)
      shinyjs::hidden(
        div(
          id = ns("shared_examples_ui"), # Add an ID for the second UI
          fluidRow(
            column(
              width = 4,
              div(
                class = "d-flex align-items-center px-1",
                shinyWidgets::pickerInput(
                  inputId = ns("database_shared_folder"),
                  label = NULL,
                  choices = NULL
                )
              )
            )
          )
        )
      ),
      
      # Third Option UI (Shared Pool)
      shinyjs::hidden(
        div(
          id = ns("shared_pool_ui"), # Add an ID for the third UI
          fluidRow(
            column(
              width = 4,
              div(
                class = "d-flex align-items-center px-1",
                textInput(
                  inputId = ns("pool_file_search"), 
                  label = NULL,
                  placeholder = "Enter exact parameters database name"
                )
              )
            ),
            column(
              width = 2,
              div(
                class = "d-flex align-items-center px-1",
                actionButton(inputId = ns("pool_search_ok"), label = "OK", class = "btn btn-primary filters-btn")
              )
            )
          )
        )
      ),
      shinyjs::hidden(
        div(
          id = ns("rename_database_section"),
          h2("Rename Parameters Database:", class = "mb-3 mt-5"),
          fluidRow(
            column(
              width = 8,
              div(
                class = "d-flex align-items-center px-1",
                textInput(
                  inputId = ns("database_new_name"),
                  label = NULL,
                  placeholder = "New Name...",
                )
              )
            ),
            column(
              width = 2,
              div(
                class = "d-flex align-items-center px-1",
                actionButton(
                  inputId = ns("cancel_database_rename"),
                  label = "Cancel",
                  class = "btn btn-primary filters-btn",
                  icon("x")
                )
              )
            ),
            column(
              width = 2,
              div(
                class = "d-flex align-items-center px-1",
                actionButton(
                  inputId = ns("confirm_database_rename"),
                  label = "Confirm",
                  class = "btn btn-primary filters-btn",
                  icon("check")
                )
              )
            )
          )
        )
      )
    ),
    div(
      class = "container mt-2 mb-2",
      style = "text-align: left",
      a(
        class = "editInformationBtn",
        icon("chevron-left", class = "editInfoIcon"),
        "Scenario - Enterprise Description",
        onclick = go_to(target = "scenario")
      ) 
    ),
    fixedPage(
      style = "margin-top: 50px;",
      fluidRow(
        class = "bg-light",
        style = "margin-top: 50px;",
        div(
          class = "custom-tab-width",
          # Use lapply to loop over each tab name and generate the tabPanels
          do.call(
            tabsetPanel,
            c(
              id = "tabs",
              lapply(parameters_db_names, function(tab_name) {
                tabPanel(
                  sub("^lkp_", "", tab_name),
                  div(
                    class = "p-5 bg-light",
                    fluidRow(
                      class = "mb-4",
                      column(
                        width = 3,
                        actionButton(
                          inputId = ns(paste0("add_rows_", tab_name)),
                          label = "New Row",
                          class = "btn btn-primary filters-btn",
                          icon = icon("plus")
                        )
                      ),
                      column(
                        width = 3,
                        div(
                          br(),
                          class = "d-flex align-items-center px-1",
                          actionButton(
                            inputId = ns(paste0("delete_rows_", tab_name)),
                            label = "Delete",
                            class = "btn btn-primary filters-btn",
                            icon = icon("trash")
                          )
                        )
                      )
                    ),
                    div(DTOutput(ns(paste0("table_", tab_name))), class = "with_checkbox"),
                    tags$div(
                      "The data is immediately saved to the corresponding CSV
                      file, no confirmation is required!", 
                      class = "mb-5 mt-5 text-center", 
                      style = "font-size: 20px; line-height: 20px;
                      font-weight: 500; text-align: left; color: #005275;
                      font-family: 'serif , Merriweather';"
                    )
                  )
                )
              })
            )
          )
        )
      )
    )
  )
}
