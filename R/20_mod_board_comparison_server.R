board_comparison_server <- function(
    id
) { moduleServer(id, function(input, output, session) {
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  # ------ update scenarios lists on new scenario Runs (in Scenario Module) ----
  observeEvent(session$userData$run_scenario_counter(), {
    cat(file = stderr(), "20 - Update scenarios lists on new scenario Runs\n")
    shinyWidgets::updatePickerInput(
      inputId = "scenario_results_comp",
      choices = list.files(
        file.path(session$userData$user_folder, "scenarios"), full.names = FALSE
      ),
      selected = list.files(
        file.path(session$userData$user_folder, "scenarios"), full.names = FALSE
      )[1]
    )
  })
  
  # ------ Initiate the list of scenarios to compare ---------------------------
  observe({
    cat(file = stderr(), "20 - Initiate the list of scenarios to compare\n")
    # Update the list of scenarios to compare
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "scenario_results_comp",
      choices = list.files(
        file.path(
          session$userData$user_folder, "scenarios"
        ), full.names = FALSE
      ),
      selected = head(
        list.files(
          file.path(
            session$userData$user_folder, "scenarios"
          ), full.names = FALSE
        ),
        2
      )
    )
    # Update the list of comparisons to display
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "comp_name",
      choices = list.files(
        file.path(
          session$userData$user_folder, "comparisons"
        ), full.names = FALSE
      )
    )
  })
  
  # ------ * Update the PickerInputs after Super User Cloning
  observeEvent(session$userData$super_user_cloned_folder(), {
    if (session$userData$super_user_cloned_folder() == "scenarios") {
      shinyWidgets::updatePickerInput(
        session,
        "scenario_results_comp",
        choices = list.files(
          path = file.path(session$userData$user_folder, "scenarios"),
          full.names = FALSE
        ),
        selected = input$scenario_results_comp
      )
      
      # Reset the cloned folder indicator
      session$userData$super_user_cloned_folder(NULL)
      
    } else if (session$userData$super_user_cloned_folder() == "comparisons") {
      shinyWidgets::updatePickerInput(
        session,
        "comp_name",
        choices = list.files(
          path = file.path(session$userData$user_folder, "comparisons"),
          full.names = FALSE
        ),
        selected = input$comp_name
      )
      
      # Reset the cloned folder indicator
      session$userData$super_user_cloned_folder(NULL)
    }
  })
  
  # ------ Update Radio Buttons according to selected results ------------------
  observeEvent(input$comp_name, {
    cat(file = stderr(), "20 - Update Radio Buttons on selected results\n")
    # Read the scenario names from the result rds file
    scenario_names <- names(
      readRDS(
        file.path(
          session$userData$user_folder,
          "comparisons",
          input$comp_name,
          "results_list.rds"
        )
      )
    )
    updateRadioButtons(
      inputId = "base_scenario",
      choices = scenario_names,
      selected = scenario_names[1]
    )
  }) 
  
  # ------ Compare Scenario -------------------------------------------------------
  observeEvent(input$comp_scenario_results, {
    
    cat(file = stderr(), "20 - Compare Scenario\n")
    
    # ------- Check if at least two scenarios are selected
    if (length(input$scenario_results_comp) < 2) {
      showNotification(
        "You need to select at least two Scenarios to compare!",
        type = "error"
      )
      return(NULL)
    }
    
    withProgress(message = "Comparing Scenarios...", value = 0.5, {
      
      # Get scenarios paths
      scenarios_paths <- file.path(
        session$userData$user_folder, "scenarios", input$scenario_results_comp,
        paste0(input$scenario_results_comp, ".json")
      )
      
      # Create results folder
      result_path <- file.path(
        session$userData$user_folder, "comparisons",
        paste(input$scenario_results_comp, collapse = " - ")
      )
      dir.create(result_path)
      
      # Create the comparison file path
      comparison_file_path <- file.path(result_path, "comparison_output.json")
      
      # Run Compare Scenario
      comparison_args <- c(
        comparison_file_path,
        scenarios_paths
      )
      do.call("calculate_differences", as.list(comparison_args))
      
      # Save data for all the scenarios as base scenario (none included)
      
      # Initialize the results_list
      results_list <- list()
      
      # For "None" - No Base Scenario
      results_list[["None"]] <- clean_plotting(
        comparison_file_path,
        paste0(result_path, "/")
      )
      
      # For the rest of the scenarios, each as base scenario
      scenario_results <- lapply(
        input$scenario_results_comp,
        function(scenario) {
          compare_scenario(
            scenario,
            comparison_file_path,
            paste0(result_path, "/")
          )
        }
      )
      
      # Combine both "None" and the scenario results
      names(scenario_results) <- input$scenario_results_comp
      results_list <- c(results_list, scenario_results)
      
      # Save simulation results
      saveRDS(
        results_list,
        file.path(result_path, "results_list.rds")
      )
      
      # update choices for comp_name input
      shinyWidgets::updatePickerInput(
        inputId = "comp_name",
        choices = list.files(
          file.path(
            session$userData$user_folder, "comparisons"
          ), full.names = FALSE
        ),
        selected = paste(input$scenario_results_comp, collapse = " - ")
      )
    })
  })
  
  # ------ Download Comparison Results -----------------------------------------
  output$download_results_comp <- downloadHandler(
    filename = function() {
      paste0(paste(input$comp_name, collapse = " - "), ".zip")
    },
    content = function(file) {
      
      cat(file = stderr(), "20 - Download Comparison Results\n")
      
      withProgress(message = "Downloading Comparison...", value = 0.5, {
        
        # Get the path to the results folder
        result_path <- file.path(
          session$userData$user_folder,
          "comparisons",
          paste(input$comp_name, collapse = " - ")
        )
        results_list <- readRDS(
          file.path(
            session$userData$user_folder, "comparisons",
            input$comp_name, "results_list.rds"
          )
        )[[input$base_scenario]]
        
        relative <- if (input$base_scenario == "None") {
          FALSE
        } else {
          TRUE
        }
        
        # save plot in the results folder
        lapply(
          graphs_desc$indicator[5:48],
          function(indicator) {
            datos <- results_list[[indicator]]$datos
            tt <- results_list[[indicator]]$tt
            y_title <- results_list[[indicator]]$y_title
            title <- results_list[[indicator]]$title
            unit <- graphs_desc[graphs_desc$indicator == indicator, ]$unit
            plot_comparison(
              datos, tt, y_title, title, unit, png = TRUE, relative = relative
            )
            ggsave(
              file.path(result_path, paste0(indicator, ".png")),
              width = 400, height = 250, units = "mm"
            )
          }
        )
        
        # Zip the files in the results folder
        zip::zip(
          zipfile = file,
          files = list.files(result_path, full.names = TRUE),
          mode = "cherry-pick"
        )
      })
    },
    contentType = "application/zip"
  )
  
  # ------ delete comparison ---------------------------------------------------
  observeEvent(input$delete_comp, {
    cat(file = stderr(), "20 - Delete Comparison\n")
    shinyWidgets::ask_confirmation(
      inputId = ns("delete_comp_confirmation"),
      type = "warning",
      title = "Delete comparison?",
      btn_colors = c("grey", "#009ADB")
    )
  })
  observeEvent(input$delete_comp_confirmation, {
    
    cat(file = stderr(), "20 - Delete Comparison Confirmation\n")
    req(input$delete_comp_confirmation) # Ensure code runs only on confirmation
    source_file_path <- file.path(
      session$userData$user_folder, "comparisons", input$comp_name
    )
  
    # Delete the selected comparison folder
    unlink(source_file_path, recursive = TRUE)
    
    # Update the selectInput choices after deletion
    shinyWidgets::updatePickerInput(
      session, "comp_name",
      choices = list.files(file.path(
        session$userData$user_folder, "comparisons"
      ), full.names = FALSE),
      selected = NULL
    )
    showNotification(
      tags$span(tags$b(input$comp_name), " deleted successfully!"),
      type = "message"
    )
  })
  
  
  # ------ Show comparison graph columns according to category selected --------
  observeEvent(list(input$comp_category, input$base_scenario), {
    
    cat(file = stderr(), "20 - Show comparison graph columns on category\n")
    
    req(input$comp_category)
    req(input$base_scenario)
    
    indicators <- graphs_desc$indicator[
      graphs_desc$category == input$comp_category
    ]
    
    # Hide all columns except the selected category/comparison
    # Indicators are defined in the Google Sheet "iCLEANED - Graphs Information"
    # Sourced in global.R
    lapply(
      graphs_desc$indicator,
      function(indicator) {
        if (indicator %in% indicators) {
          shinyjs::show(id = paste0(indicator, "_column"))
        } else {
          shinyjs::hide(id = paste0(indicator, "_column"))
        }
        # Update description short and long according to input$base_scenario
        shinyjs::html(
          id = paste0("description_short_", indicator),
          html = if (input$base_scenario == "None") {
            graphs_desc$description_short_abs[
              graphs_desc$indicator == indicator
            ]
          } else {
            graphs_desc$description_short_relative[
              graphs_desc$indicator == indicator
            ]
          }
        )
        shinyjs::html(
          id = paste0("description_long_", indicator),
          html = if (input$base_scenario == "None") {
            graphs_desc$description_long_abs[graphs_desc$indicator == indicator]
          } else {
            graphs_desc$description_long_relative[
              graphs_desc$indicator == indicator
            ]
          }
        )
      }
    )
  }, ignoreInit = FALSE)
  
  # ------ Hide or show placeholder according to data availabilty --------------
  lapply(
    c(graphs_desc$indicator[5:48]),
    function(indicator) {
      observeEvent(list(input$comp_name, input$comp_scenario_results), {
        cat(
          file = stderr(), paste(
            "20 - Hide/show placeholder for:", indicator, "\n"
          )
        )
        # Check if the inputs are available
        if (is.null(input$comp_name)) {
          shinyjs::hide(selector = jns(paste(indicator, ".axis-title")))
          shinyjs::hide(
            selector = jns(paste(indicator, ".shiny-spinner-output-container"))
          )
          shinyjs::show(
            selector = jns(paste(indicator, ".no-data"))
          )
          shinyjs::disable(id = "download_results_comp")
          shinyjs::disable(id = "delete_comp")
        } else {
          shinyjs::show(selector = jns(paste(indicator, ".axis-title")))
          shinyjs::show(
            selector = jns(paste(indicator, ".shiny-spinner-output-container"))
          )
          shinyjs::hide(selector = jns(paste(indicator, ".no-data")))
          shinyjs::enable(id = "download_results_comp")
          shinyjs::enable(id = "delete_comp")
        }
      })
    }
  )
  
  
  # ------ PLOT comparisons graphs ---------------------------------------------
  lapply(
    graphs_desc$indicator[5:48], #1st 4 plots removed, belong to the 1st section
    function(indicator) {
      output[[paste0("plot_", indicator)]] <- renderGirafe({
        
        cat(file = stderr(), "20 - Plot comparison - ", indicator, "\n")
        
        # Check if the inputs are available
        req(input$comp_name)
        req(input$comp_category)
        req(input$base_scenario)
        
        # Read comparison results
        results_list <- readRDS(
          file.path(
            session$userData$user_folder, "comparisons",
            input$comp_name, "results_list.rds"
          )
        )[[input$base_scenario]]
        
        # Get the data for each indicator
        datos <- results_list[[indicator]]$datos
        tt <- results_list[[indicator]]$tt
        y_title <- results_list[[indicator]]$y_title
        title <- results_list[[indicator]]$title
        unit <- graphs_desc[graphs_desc$indicator == indicator, ]$unit
        
        # Check if the plot is relative or not
        relative <- if (input$base_scenario == "None") {
          FALSE
        } else {
          TRUE
        }
        
        plot_comparison(datos, tt, y_title, title, unit, png = FALSE, relative)
      })
    }
  )
})}
