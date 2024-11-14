board_simulation_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # ------ Last Update -------------------------------------------------------
    p(
      class = "pt-3 pb-5 text-end text-primary text-small",
      span("Last update: "),
      span(strong(id = ns("last_update_date")))
    ),
    
    # ------ Board description & Run Scenario Section --------------------------
    fluidRow(
      class = "pb-5 mb-5",
      column(
        width = 5,
        div(
          class = "card h-100",
          div(
            class = "align-items-center",
            div(
              style = "display: flex; align-items: center;",
              span(
                "?",
                class = "help",
                style = "display: inline-block; margin-right: 10px;",
                span("To run a simulation: select a JSON file andclick the 
                'Run Scenario' button below.")
              ),
              h2(
                style = "display: inline-block;",
                "Run Scenario", 
              ),
            ),
            br(),
            shinyWidgets::pickerInput(
              inputId = ns("json_file_name"),
              label = NULL,
              choices = NULL,
              multiple = TRUE,
              options = list(
                `selected-text-format` = "count",
                `count-selected-text` = "{0}/{1} Scenarios"
              )
            ),
            br(),
            actionButton(
              inputId = ns("run_scenario"),
              label = "Run Scenario(s)",
              class = "btn-primary w-100",
              icon = icon("cogs"),
              style = "width: 200px !important; padding: 12px 0px !important"
            ),
            div(
              id = ns("spin_initial_run"),
              class = "cover-spin coverSpinScenario"
            )
          )
        )
      ),
      column(
        width = 7,
        div(
          class = "card h-100 d-flex flex-column justify-content-center",
          p("In this tab of ", strong("iCLEANED", class = "text-primary"),
            "you can find all the saved scenarios via the selector on the 
            left."),
          p("Select one or more scenarios to simulate. The results, are saved in
          your user data file and you can then view them in the section below."),
          p("Further down, you can select scenarios to ",
            strong("compare", class = "text-primary"), " - ",
            "choose a base scenario if needed, and run comparisons. The comparison 
            results could be visualized in the lower section."),
        )
      )
    ),
    # ------ Simulation results Graphics (4) -----------------------------------
    # Header with the scenario selector, download button, and delete button
    fluidRow(
      style = "padding-bottom: 3rem !important;",
      class = "bg-light px-4 pt-5 pb-3",
      column(
        width = 7,
        div(
          class = "d-flex filters-select align-items-center",
          div(
            span(
              "?",
              class = "help",
              span("Please select a scenario to view its simulation results. 
                   You can also download the results using the download button 
                   or delete them if needed.")
            ),
          ),
          shinyWidgets::pickerInput(
            inputId = ns("scenario_name"), 
            label = NULL,
            choices = NULL,
            multiple = FALSE
          )
        )
      ),
      column(
        width = 2,
        div(
          class = "d-flex align-items-center px-1",
          actionButton(
            inputId = ns("delete_scenario"),
            label = "Delete",
            class = "btn btn-primary filters-btn",
            icon = icon("trash")
          )
        )
      ),
      column(
        width = 3,
        div(
          class = "d-flex align-items-center px-1",
          downloadButton(
            outputId = ns("download_results"),
            label = "Download Results",
            class = "btn btn-primary filters-btn"
          )
        )
      ),
    ),
    # GHG Emission and N Balance
    fluidRow(
      # warning message
      shinyjs::hidden(
        div(
          id = ns("warning_scenario_text"),
          class = "alert-text"
        )
      ),
      class = "bg-light px-4 pb-3",
      # GHG Emission
      column(
        width = 6,
        class = "pe-4",
        h2(
          span(
            "?",
            class = "help",
            span(graphs_desc$description_long_abs[
              graphs_desc$indicator == "ghg_emission"
            ])
          ),
          graphs_desc$title[graphs_desc$indicator == "ghg_emission"],
          class = "text-center mb-2"
        ),
        p(
          graphs_desc$description_short_abs[
            graphs_desc$indicator == "ghg_emission"
          ],
          a("here", href = "ReadMe.xlsx", download = NA, class = "text-primary"),
          "."
        ),
        div(
          class = "mt-4",
          id = ns("ghg_emission"),
          shinyjs::hidden(
            span(
              class = "axis-title y-axis-title",
              graphs_desc$unit[graphs_desc$indicator == "ghg_emission"]
            )
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput(
              outputId = ns("plot_ghg_emission"),
              height = "480px"
            ),
            color.background = "#F5F5F5"
          ),
          include_no_data_placeholder(text = "No data available")
        )
      ),
      # N Balance
      column(
        width = 6,
        class = "ps-4",
        h2(
          span(
            "?",
            class = "help",
            span(graphs_desc$description_long_abs[
              graphs_desc$indicator == "n_balance"
            ])
          ),
          graphs_desc$title[graphs_desc$indicator == "n_balance"],
          class = "text-center mb-2",
        ),
        p(graphs_desc$description_short_abs[
          graphs_desc$indicator == "n_balance"
        ]),
        div(
          class = "mt-4",
          id = ns("n_balance"),
          shinyjs::hidden(
            span(
              class = "axis-title y-axis-title",
              graphs_desc$unit[graphs_desc$indicator == "n_balance"]
            )
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput(
              outputId = ns("plot_n_balance"),
              height = "480px"
            ),
            color.background = "#F5F5F5"
          ),
          include_no_data_placeholder(text = "No data available"),
          shinyjs::hidden(span(class = "axis-title x-axis-title", "Feed Item"))
        )
      )
    ),
    # Land Requirement and Water Feed
    fluidRow(
      class = "bg-light px-4 pb-4",
      # Land Requirement
      column(
        width = 6,
        class = "pe-4",
        h2(
          span(
            "?",
            class = "help",
            span(graphs_desc$description_long_abs[
              graphs_desc$indicator == "land_req"
            ])
          ),
          graphs_desc$title[graphs_desc$indicator == "land_req"],
          class = "text-center mb-2"
        ),
        p(graphs_desc$description_short_abs[
          graphs_desc$indicator == "land_req"
        ]),
        div(
          class = "mt-4",
          id = ns("land_req"),
          shinyjs::hidden(
            span(
              class = "axis-title y-axis-title",
              graphs_desc$unit[graphs_desc$indicator == "land_req"]
            )
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput(
              outputId = ns("plot_land_req"),
              height = "480px"
            ),
            color.background = "#F5F5F5"
          ),
          include_no_data_placeholder(text = "No data available"),
          shinyjs::hidden(span(class = "axis-title x-axis-title", "Feed Item"))
        ),
        generate_legend(
          class = "legend", 
          id = "legend_1", 
          ns = ns, 
          n_items = max_seasons, 
          id_item_prefix = "Seasons"
        )
      ),
      # Water Feed
      column(
        width = 6,
        class = "ps-4",
        h2(
          span(
            "?",
            class = "help",
            span(graphs_desc$description_long_abs[
              graphs_desc$indicator == "water_feed"
            ])
          ),
          graphs_desc$title[graphs_desc$indicator == "water_feed"],
          class = "text-center mb-2",
        ),
        p(graphs_desc$description_short_abs[
          graphs_desc$indicator == "water_feed"
        ]),
        div(
          class = "mt-4",
          id = ns("water_feed"),
          shinyjs::hidden(
            span(
              class = "axis-title y-axis-title",
              graphs_desc$unit[graphs_desc$indicator == "water_feed"]
            )
          ),
          tags$div(
            id = ns("div_plot_water_feed"),
            class = "plot_water_feed",
            shinycssloaders::withSpinner(
              ggiraph::girafeOutput(
                outputId = ns("plot_water_feed"),
                height = "380px"
              ),
              color.background = "#F5F5F5"
            )
          ),
          include_no_data_placeholder(text = "No data available")
        ),
        generate_legend(
          class = "legend", 
          id = "legend_2", 
          ns = ns, 
          n_items = max_feed_items, 
          id_item_prefix = "Feeds"
        )
      )
    )
  )
}
