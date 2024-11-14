scenario_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # JavaScript to detect modal close and apply namespace
    tags$script(HTML(sprintf("
      $(document).on('hidden.bs.modal', function () {
        Shiny.setInputValue('%s', Math.random());
      });", ns("modal_closed")))),
    div(
      class = "container",
      # ------ Dernière mise à jour ------------------------------------------
      p(
        class = "pt-3 pb-5 text-end text-primary text-small",
        span("Last update: "),
        span(strong(id = ns("last_update_date")))
      ),
    ),
    fixedPage(
      class = "bg-light px-4 pt-5 pb-5 mt-5 mb-5",
      style = "margin-top: 20px;",
      h2("Choose a file option:", class = "mb-3"),
      fluidRow(
        column(
          width = 6,
          div(
            class = "d-flex align-items-center px-1",
            actionButton(
              inputId = ns("create_new_json"),
              label = HTML("&nbsp;&nbsp;&nbsp; New Scenario"),
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
        id = ns("select_scenario_section"),
        fluidRow(
          class = "mb-3 mt-5",
          column(3,
                 h2("Select Scenario:")
          ),
          column(3, 
                 div(
                   style = "display: flex; align-items: center;",
                   span(
                     "?",
                     class = "help",
                     style = "display: inline-block; margin-right: 10px;",
                     span(
                       "When choosing a scenario, you can: ", br(),
                       "1. Browse your own folder to select files.", br(),
                       "2. Clone shared examples provided by the admins", br(),
                       "3. Search in the shared pool for files by name, which
                       you can clone to your folder if found.", br(),
                       "Note: If you want to share a file with others, make sure
                       it has a very unique name. 
                       Otherwise, someone could overwrite your file and it may be lost."
                     )
                   ),
                   p("Choose Scenario Folder", 
                     class = "baseSenerioP",
                     style = "display: inline-block;"),
                 )
          ),
          column(5, 
                 tags$div(
                   class = "baseSenerioP",
                   radioButtons(
                     inputId = ns("scenario_folder"),
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
                inputId = ns("json_file_name"),
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
                inputId = ns("rename_json"),
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
                inputId = ns("clone_json"),
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
                inputId = ns("delete_json"),
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
                  inputId = ns("json_shared_folder"),
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
                  placeholder = "Enter exact file name"
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
          id = ns("rename_scenario_section"),
          h2("Rename Scenario:", class = "mb-3 mt-5"),
          fluidRow(
            column(
              width = 8,
              div(
                class = "d-flex align-items-center px-1",
                textInput(
                  inputId = ns("json_new_name"),
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
                  inputId = ns("cancel_json_rename"),
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
                  inputId = ns("confirm_json_rename"),
                  label = "Confirm",
                  class = "btn btn-primary filters-btn",
                  icon("check")
                )
              )
            )
          )
        )
      ),
      fluidRow(
        h2("Select Parameter Database:", class = "mb-3 mt-5"),
        shinyWidgets::pickerInput(
          inputId = ns("database_code"),
          label = NULL,
          choices = NULL
        )
      )
    ),
    div(
      class = "container mt-2 mb-2",
      style = "text-align: right",
      a(
        class = "editInformationBtn",
        icon("edit"),
        "Edit Parameters",
        icon("chevron-right", class = "editInfoIcon"),
        onclick = go_to(target = "params_db")
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("scenario_editor_section"),
        fixedPage(
          style = "margin-top: 40px;",
          fluidRow(
            class = "bg-light",
            style = "margin-top: 40px;",
            tabsetPanel(
              id = "tabs",
              tabPanel(
                "Farm",
                class = "container",
                tags$div(
                  id = "sectionCarousel",
                  class = "carousel slide",
                  tags$div(
                    class = "carousel-inner",
                    tags$div(
                      class = "carousel-item active",
                      div(class = "p-5 bg-light",
                          h2("General Information", class = "tabsH2"),
                          h2("Study information", class = "mb-4"),
                          h2("Region", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            shinyWidgets::pickerInput(
                              inputId = ns("region"),
                              label = NULL,
                              choices = NULL
                            )
                          ),
                          h2("Climate:", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            shinyWidgets::pickerInput(
                              inputId = ns("climate_zone"),
                              label = NULL,
                              choices = NULL
                            ),
                          ),
                          h2("Sub-climate:", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            shinyWidgets::pickerInput(
                              inputId = ns("climate_zone_2"),
                              label = NULL,
                              choices = NULL
                            ),
                          ),
                          h2("Farm name:", class = "mb-3"),
                          tags$div(
                            class = "mb-3",
                            textInput(
                              inputId = ns("farm_name"),
                              label = NULL
                            )
                          )
                      )
                    ),
                    tags$div(
                      class = "carousel-item",
                      div(class = "p-5 bg-light",
                          h2("Seasons", class = "tabsH2"),
                          fluidRow(
                            class = "mb-4",
                            column(
                              width = 2,
                              div(
                                class = "d-flex align-items-center px-1",
                                actionButton(
                                  inputId = ns("add_season"),
                                  label = "Add",
                                  class = "btn btn-primary filters-btn",
                                  icon = icon("plus")
                                )
                              )
                            ),
                            column(
                              width = 2,
                              div(
                                class = "d-flex align-items-center px-1",
                                actionButton(
                                  inputId = ns("delete_season"),
                                  label = "Delete",
                                  class = "btn btn-primary filters-btn",
                                  icon = icon("trash")
                                )
                              )
                            )
                          ),
                          shinyjs::hidden(
                            div(
                              id = ns("alert_message_season"),
                              class = "alert alert-danger",
                              "The total number of days in all seasons should be precisely 365!"
                            )
                          ),
                          div(DTOutput(ns("season_table")), class = "with_checkbox")
                          
                      )
                    ),
                    tags$div(
                      class = "carousel-item",
                      div(class = "p-5 bg-light",
                          h2("Manure/Fertilizer bought", class = "tabsH2"),
                          h2("Manure", class = "mb-4"),
                          h2("Annual purchase of manure (kg N):", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            textInput(
                              inputId = ns("purchased_manure"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Annual purchase of compost (kg N):", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            textInput(
                              inputId = ns("purchased_compost"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Annual purchase of other organic N additions (kg N):", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            textInput(
                              inputId = ns("purchased_organic_n"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Annual purchase of bedding materials (kg N):", class = "mb-3"),
                          tags$div(
                            class = "mb-5",
                            textInput(
                              inputId = ns("purchased_bedding"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Fertilizer", class = "mb-4"),
                          fluidRow(
                            column(
                              width = 2,
                              div(
                                class = "d-flex align-items-center px-1",
                                actionButton(
                                  inputId = ns("add_fertilizer"),
                                  label = "Add",
                                  class = "btn btn-primary filters-btn",
                                  icon = icon("plus")
                                )
                              )
                            ),
                            column(
                              width = 2,
                              div(
                                class = "d-flex align-items-center px-1",
                                actionButton(
                                  inputId = ns("delete_fertilizer"),
                                  label = "Remove",
                                  class = "btn btn-primary filters-btn mb-4",
                                  icon = icon("trash")
                                )
                              )
                            )
                          ),
                          div(DTOutput(ns("fertilizer_table")), class = "with_checkbox")
                      )
                    ),
                    tags$div(
                      class = "carousel-item",
                      div(class = "p-5 bg-light",
                          h2("Waste of milk and meat", class = "tabsH2"),
                          fluidRow(
                            column(
                              width = 6,
                              h2("% waste along the milk value chain", class = "mb-4"),
                            ),
                            column(
                              width = 6,
                              h2("% waste along the meat value chain", class = "mb-4"),
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Production", class = "mb-3")
                            ),
                            column(
                              width = 6,
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              tags$div(
                                class = "mb-4",
                                numericInput(
                                  ns("waste_production_milk"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            ),
                            column(
                              width = 6,
                              tags$div(
                                class = "mb-4",
                                numericInput(
                                  ns("waste_production_meat"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Distribution", class = "mb-3")
                            ),
                            column(
                              width = 6,
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              tags$div(
                                class = "mb-4",
                                numericInput(
                                  ns("waste_distribution_milk"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            ),
                            column(
                              width = 6,
                              tags$div(
                                class = "mb-4",
                                numericInput(
                                  ns("waste_distribution_meat"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Processing", class = "mb-3")
                            ),
                            column(
                              width = 6,
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              tags$div(
                                class = "mb-4",
                                numericInput(
                                  ns("waste_processing_milk"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            ),
                            column(
                              width = 6,
                              tags$div(
                                class = "mb-4",
                                numericInput(
                                  ns("waste_processing_meat"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Consumption", class = "mb-3")
                            ),
                            column(
                              width = 6,
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              tags$div(
                                class = "mb-3",
                                numericInput(
                                  ns("waste_consume_milk"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            ),
                            column(
                              width = 6,
                              tags$div(
                                class = "mb-3",
                                numericInput(
                                  ns("waste_consume_meat"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            )
                          )
                      )
                    ),
                    tags$div(
                      class = "carousel-item",
                      div(class = "p-5 bg-light",
                          h2("Area", class = "tabsH2"),
                          h2("Annual precipitation (mm/yr)", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            numericInput(
                              ns("annual_prec"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Rainy season (no of months/year)", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            numericInput(
                              ns("rain_length"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Soil type", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            shinyWidgets::pickerInput(
                              inputId = ns("soil_description"),
                              label = NULL,
                              choices = NULL
                            )
                          ),
                          h2("Estimated K Value", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            shinyjs::disabled(
                              numericInput(
                                ns("soil_k_value"),
                                label = NULL,
                                value = 0
                              )
                            )
                          ),
                          h2("Soil N (g/kg)", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            numericInput(
                              ns("soil_n"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Soil C (g/kg)", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            numericInput(
                              ns("soil_c"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Soil clay (%)", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            numericInput(
                              ns("soil_clay"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Bulk density (g/cm3)", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            numericInput(
                              ns("soil_bulk"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("Soil depth (m)", class = "mb-3"),
                          tags$div(
                            class = "mb-4",
                            numericInput(
                              ns("soil_depth"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          h2("ETO (mm/year)", class = "mb-3"),
                          tags$div(
                            class = "mb-5",
                            numericInput(
                              ns("et"),
                              label = NULL,
                              value = 0
                            )
                          ),
                          hr(),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Cropland system", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyWidgets::pickerInput(
                                  inputId = ns("cropland_system"),
                                  label = NULL,
                                  choices = NULL
                                )
                              )
                            ),
                            column(
                              width = 6,
                              h2("IPCC Default", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyjs::disabled(
                                  numericInput(
                                    ns("cropland_system_ipcc"),
                                    label = NULL,
                                    value = 0
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Cropland tillage regime", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyWidgets::pickerInput(
                                  inputId = ns("cropland_tillage"),
                                  label = NULL,
                                  choices = NULL
                                )
                              )
                            ),
                            column(
                              width = 6,
                              h2("IPCC Default", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyjs::disabled(
                                  numericInput(
                                    ns("cropland_tillage_ipcc"),
                                    label = NULL,
                                    value = 0
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Cropland input of organic matter", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyWidgets::pickerInput(
                                  inputId = ns("cropland_orgmatter"),
                                  label = NULL,
                                  choices = NULL
                                )
                              )
                            ),
                            column(
                              width = 6,
                              h2("IPCC Default", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyjs::disabled(
                                  numericInput(
                                    ns("cropland_orgmatter_ipcc"),
                                    label = NULL,
                                    value = 0
                                  )
                                )
                              )
                            )
                          ),
                          hr(),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Grassland management", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyWidgets::pickerInput(
                                  inputId = ns("grassland_management"),
                                  label = NULL,
                                  choices = NULL
                                )
                              )
                            ),
                            column(
                              width = 6,
                              h2("IPCC Default", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyjs::disabled(
                                  numericInput(
                                    ns("grassland_management_ipcc"),
                                    label = NULL,
                                    value = 0
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Grassland Input level (applied only to improve condition)", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyWidgets::pickerInput(
                                  inputId = ns("grassland_implevel"),
                                  label = NULL,
                                  choices = NULL
                                )
                              )
                            ),
                            column(
                              width = 6,
                              h2("IPCC Default", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                shinyjs::disabled(
                                  numericInput(
                                    ns("grassland_implevel_ipcc"),
                                    label = NULL,
                                    value = 0
                                  )
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Conversion Grassland to arable land (ha)", class = "mb-3"),
                              tags$div(
                                class = "mb-4",
                                numericInput(
                                  inputId = ns("grassland_toarable"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            ),
                            column(
                              width = 6,
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              h2("Conversion Arable land to grassland (ha)", class = "mb-3"),
                              tags$div(
                                class = "mb-3",
                                numericInput(
                                  inputId = ns("arable_tograssland"),
                                  label = NULL,
                                  value = 0
                                )
                              )
                            ),
                            column(
                              width = 6,
                            )
                          )
                      )
                    )
                  ),
                  # Carousel controls with custom symbols
                  tags$button(
                    id = "carousel_prv_bttn",
                    class = "carousel-control-prev custom-arrow",
                    type = "button",
                    `data-bs-target` = "#sectionCarousel",
                    `data-bs-slide` = "prev",
                    icon("chevron-left"),
                    span(class = "visually-hidden", "Previous")
                  ),
                  tags$button(
                    id = "carousel_nxt_bttn",
                    class = "carousel-control-next custom-arrow",
                    type = "button",
                    `data-bs-target` = "#sectionCarousel",
                    `data-bs-slide` = "next",
                    icon("chevron-right"),
                    span(class = "visually-hidden", "Next")
                  )
                )
              ),
              tabPanel(
                "Livestock",
                div(class = "p-5 bg-light",
                    fluidRow(
                      class = "tabsH2",
                      column(
                        width = 2,
                        div(
                          br(),
                          class = "d-flex align-items-center px-1",
                          actionButton(
                            inputId = ns("add_livestock"),
                            label = "Add",
                            class = "btn btn-primary filters-btn",
                            icon = icon("plus")
                          )
                        )
                      ),
                      column(
                        width = 2,
                        div(
                          br(),
                          class = "d-flex align-items-center px-1",
                          actionButton(
                            inputId = ns("delete_livestock"),
                            label = "Remove",
                            class = "btn btn-primary filters-btn",
                            icon = icon("trash")
                          )
                        )
                      ),
                      column(
                        width = 3,
                      ),
                      column(
                        width = 5,
                        style = "text-align: right;",
                        div(
                          style = "padding-top: 15px;",
                          span(
                            style = "display: inline-block; width: 20px; 
                            height: 20px; background-color: #a9d18e; border: 1px
                            solid black; margin-right: 5px;"
                          ),
                          span(" Enterprise/scenario data"),
                          span(
                            style = "display: inline-block; width: 20px;
                            height: 20px; background-color: #f4b183; border: 1px
                            solid black; margin-left: 20px; margin-right: 5px;"
                          ),
                          span(" parameters")
                        )
                      )
                    ),
                    div(DTOutput(ns("livestock_table")), class = "with_checkbox")
                )
              ),
              tabPanel(
                "Feed Production",
                div(class = "p-5 bg-light",
                    fluidRow(
                      column(
                        width = 2,
                        div(
                          class = "d-flex align-items-center px-1 mb-5",
                          actionButton(
                            inputId = ns("add_crop"),
                            label = "Add",
                            class = "btn btn-primary filters-btn",
                            icon = icon("plus")
                          )
                        )
                      ),
                      column(
                        width = 2,
                        div(
                          class = "d-flex align-items-center px-1 mb-5",
                          actionButton(
                            inputId = ns("delete_crop"),
                            label = "Remove",
                            class = "btn btn-primary filters-btn",
                            icon = icon("trash")
                          )
                        )
                      )
                    ),
                    fluidRow(
                      class = "tabsH2--no-border",
                      column(
                        width = 3,
                        span(
                          style = "display: inline-block; width: 20px;
                          height: 20px; background-color: #a9d18e; 
                          border: 1px solid black; margin-right: 5px;"
                        ),
                        span(" Enterprise/scenario data")
                      ),
                      column(
                        width = 3,
                        span(
                          style = "display: inline-block; width: 20px;
                          height: 20px; background-color: #f4b183;
                          border: 1px solid black; margin-left: 20px;
                          margin-right: 5px;"
                        ),
                        span(" Feed parameters")
                      ),
                      column(
                        width = 3,
                        span(
                          style = "display: inline-block; width: 20px;
                          height: 20px; background-color: #d3d3d3;
                          border: 1px solid black; margin-left: 20px;
                          margin-right: 5px;"
                        ),
                        span(" Rice cultivation practices")
                      ),
                      column(
                        width = 3,
                        span(
                          style = "display: inline-block; width: 20px;
                          height: 20px; background-color: #b5d3e7;
                          border: 1px solid black; margin-left: 20px;
                          margin-right: 5px;"
                        ),
                        span(" Stock change parameters")
                      )
                    ),
                    fluidRow(
                      class = "tabsH2",
                      column(
                        width = 3,
                        span(
                          style = "display: inline-block; width: 20px;
                          height: 20px; background-color: #eedfb3;
                          border: 1px solid black; margin-right: 5px;"
                        ),
                        span(" Soil erosion parameters")
                      ),
                      column(
                        width = 3,
                        span(
                          style = "display: inline-block; width: 20px;
                          height: 20px; background-color: #8faadc;
                          border: 1px solid black; margin-left: 20px;
                          margin-right: 5px;"
                        ),
                        span(" Crop parameters")
                      )
                    ),
                    h2("Crop areas and residue management", class = "mb-3"),
                    div(DTOutput(ns("crop_table")), class = "with_checkbox"),
                    h2("Crop inputs", class = "mb-3 mt-5"),
                    shinyjs::hidden(
                      div(
                        id = ns("alert_message_crop_inputs"),
                        class = "alert alert-danger",
                        "The total of 'Fraction collected manure used as fertilizer'
                    across all rows should not exceed 1!"
                      )
                    ),
                    div(DTOutput(ns("crop_inputs_table")), class = "without_checkbox"),
                    br()
                )
              ),
              tabPanel(
                "Livestock Feeding",
                div(class = "p-5 bg-light",
                    uiOutput(ns("livestock_feeding_ui")),
                    h2("Allocation in percentage of a feed to livestock by season", class = "mb-5"),
                    uiOutput(ns("livestock_feeding_table"))
                )
              )
            )
          )
        )
      )
    )
  )
}