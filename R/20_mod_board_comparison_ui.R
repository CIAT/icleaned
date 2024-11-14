board_comparison_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # ------ Run Comparison Section --------------------------------------------
    fluidRow(
      class = "pb-12 mb-12",
      style = "margin-top: 50px;",
      column(
        width = 12,
        div(
          class = "card",
          div(
            class = "align-items-center",
            div(
              div(
                div(
                  style = "display: flex; align-items: center;",
                  span(
                    "?",
                    class = "help",
                    style = "display: inline-block; margin-right: 10px;",
                    span(
                      "To run a comparison: first, select the scenarios you want 
                       to compare and click the button below to start the 
                       comparison."
                    )
                  ),
                  h2(
                    "Compare Scenarios", 
                    style = "display: inline-block;"
                  ),
                ),
                br(),
              ),
              br(),
              fluidRow(
                column(
                  width = 8,
                  shinyWidgets::pickerInput(
                    inputId = ns("scenario_results_comp"),
                    label = NULL,
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE
                  )
                ),
                column(
                  width = 4, 
                  actionButton(
                    inputId = ns("comp_scenario_results"),
                    label = "Run Comparison",
                    icon = icon("code-compare"),
                    class = "btn-primary w-100",
                  )
                )
              )
            )
          )
        )
      )
    ),
    # ------ Select Comparison, Delete, and Download ---------------------------
    fluidRow(
      class = "bg-light px-4 pt-5 pb-4",
      style = "margin-top: 50px;",
      column(
        width = 7,
        div(
          class = "d-flex filters-select align-items-center",
          div(
            span(
              "?",
              class = "help",
              span("Please select a comparison to visualize the results. You can
                   download or visualize the results below.")
            ),
          ),
          shinyWidgets::pickerInput(
            inputId = ns("comp_name"), 
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
            inputId = ns("delete_comp"),
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
            outputId = ns("download_results_comp"),
            label = "Download Results",
            class = "btn btn-primary filters-btn"
          )
        )
      )
    ),
    # ------ Pick Comparison Category ------------------------------------------
    fluidRow(
      class = "bg-light px-4",
      column(
        width = 5,
        div(
          class = "d-flex filters-select align-items-center",
          style = "margin-left: 30px;",
          shinyWidgets::pickerInput(
            inputId = ns("comp_category"),
            label = NULL,
            choices = unique(graphs_desc$category)[-1], # remove scenario graphs
            selected = unique(graphs_desc$category)[1],
            multiple = FALSE
          )
        )
      )
    ),
    # ------ Choose Base Scenario ----------------------------------------------
    fluidRow(
      class = "bg-light px-4 pt-4",
      column(
        3, 
        style = "padding-top: 10px;",
        div(
          style = "display: flex; align-items: center;",
          span(
            "?",
            class = "help",
            style = "display: inline-block; margin-right: 10px;",
            span("You can also choose a base scenario to compare 
                                the selected scenarios with.")
          ),
          p( 
            class = "baseSenerioP",
            style = "display: inline-block;",
            "Choose Base Scenario"
          ),
        )),
      column(
        9, 
        style = "padding-top: 10px;",
        tags$div(
          class = "baseSenerioP",
          radioButtons(
            inputId = ns("base_scenario"),
            label = NULL,
            choices =  character(0),
            selected =  character(0),
            inline = TRUE
          )
        )
      )
    ),
    # ------ Comparison Graphs with lapply loop --------------------------------
    fluidRow(
      class = "bg-light pt-4 px-4 pb-5",
      tagList(
        lapply(
          graphs_desc$indicator[5:48], # exclude first 4 plots (in 1st sections)
          function(indicator) {
            shinyjs::hidden(
              column(
                width = 6,
                class = "pe-4",
                id = ns(paste0(indicator, "_column")),
                h2(
                  style = "margin-top: 35px;",
                  span(
                    "?",
                    class = "help",
                    span(
                      id = ns(paste0("description_long_", indicator)),
                    )
                  ),
                  graphs_desc$title[graphs_desc$indicator == indicator],
                  class = "text-center mb-2"
                ),
                p(
                  id = ns(paste0("description_short_", indicator))
                ),
                div(
                  class = "mt-4",
                  id = ns(indicator),
                  shinyjs::hidden(
                    span(
                      class = "axis-title y-axis-title",
                      graphs_desc$unit[graphs_desc$indicator == indicator]
                    )
                  ),
                  shinycssloaders::withSpinner(
                    ggiraph::girafeOutput(
                      outputId = ns(paste0("plot_", indicator)),
                      height = "480px"
                    ),
                    color.background = "#F5F5F5"
                  ),
                  include_no_data_placeholder(text = "No data available")
                )
              )
            )
          }
        )
      )
    )
  )
}
