board_simulation_server <- function(
    id
) { moduleServer(id, function(input, output, session) {
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  observe({
    cat(file = stderr(), "10 - Initialization\n")
    # ------ Initiate the Select Input -----------------------------------------
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "json_file_name",
      choices = list.files(
        path = file.path(session$userData$user_folder, "study_objects"), 
        full.names = FALSE
      ),
      selected = list.files(
        path = file.path(session$userData$user_folder, "study_objects"), 
        full.names = FALSE
      )[1]
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "scenario_name",
      choices = list.files(
        path = file.path(session$userData$user_folder, "scenarios"), 
        full.names = FALSE
      )
    )
    # ------ Update Last modification date -------------------------------------
    # most recent file in data/objects
    files_info <- file.info(
      list.files(
        path = file.path(session$userData$user_folder, "study_objects"), 
        full.names = TRUE
      )
    )
    # Get the last modification date and assign it to the html element
    last_modification_date <- format(
      files_info$mtime[which.max(files_info$mtime)], "%B %d, %Y"
    )
    shinyjs::html(id = "last_update_date", html = last_modification_date)
    
    # ------ Hid Run Scenario spinner ------------------------------------------
    shinyjs::hide(id = "spin_initial_run")
  })
  
  # ------ * Update the PickerInputs after Super User Cloning
  observeEvent(session$userData$super_user_cloned_folder(), {
    if (session$userData$super_user_cloned_folder() == "study_objects") {
      shinyWidgets::updatePickerInput(
        session,
        "json_file_name",
        choices = list.files(
          path = file.path(session$userData$user_folder, "study_objects"),
          full.names = FALSE
        ),
        selected = input$json_file_name
      )
      
    } else if (session$userData$super_user_cloned_folder() == "scenarios") {
      shinyWidgets::updatePickerInput(
        session,
        "scenario_name",
        choices = list.files(
          path = file.path(session$userData$user_folder, "scenarios"),
          full.names = FALSE
        ),
        selected = input$scenario_name
      )
    }
  })
  
  # Update json_file_name choices after creating new scenario ------------------
  observeEvent(session$userData$study_objects(), {
    cat(file = stderr(), "10 - Update json_file_name choices\n")
    shinyWidgets::updatePickerInput(
      inputId = "json_file_name",
      choices = list.files(
        path = file.path(session$userData$user_folder, "study_objects"),
        full.names = FALSE
      ),
      selected = list.files(
        path = file.path(session$userData$user_folder, "study_objects"),
        full.names = FALSE
      )[1]
    )
  })
  
  # ------ Hide or show placeholder according to data availability -------------
  # This lapply function generates 4 observeEvent() for the 4 scenario plots
  lapply(
    c("ghg_emission", "n_balance", "land_req", "water_feed"),
    function(type) {
      observeEvent(list(input$scenario_name, input$run_scenario), {
        cat(
          file = stderr(), paste("10 - Hide/show placeholder for:", type, "\n")
        )
        if (is.null(input$scenario_name)) {
          shinyjs::hide(selector = jns(paste(type, ".axis-title")))
          shinyjs::hide(
            selector = jns(paste(type, ".shiny-spinner-output-container"))
          )
          shinyjs::show(selector = jns(paste(type, ".no-data")))
          shinyjs::hide(id = "legend_1")
          shinyjs::hide(id = "legend_2")
          shinyjs::disable(id = "download_results")
          shinyjs::disable(id = "delete_scenario")
          shinyjs::hide(id = "div_plot_water_feed")
        } else {
          shinyjs::show(selector = jns(paste(type, ".axis-title")))
          shinyjs::show(
            selector = jns(paste(type, ".shiny-spinner-output-container"))
          )
          shinyjs::hide(selector = jns(paste(type, ".no-data")))
          shinyjs::show(id = "legend_1")
          shinyjs::show(id = "legend_2")
          shinyjs::enable(id = "download_results")
          shinyjs::enable(id = "delete_scenario")
          shinyjs::show(id = "div_plot_water_feed")
        }
      })
    }
  )
  
  # ------ Display/Hide warning_text -------------------------------------------
  observeEvent(list(input$run_scenario, input$scenario_name), {
    cat(file = stderr(), "10 - Display warning text\n")
    shinyjs::hide(selector = jns("warning_scenario_text"))
    # according to the scenario data
    if (!is.null(input$scenario_name)) {
      warning_msg <- readRDS(
        file.path(
          session$userData$user_folder, "scenarios",
          input$scenario_name, "scenario_data.rds"
        )
      )$warning_msg
      if (!is.null(warning_msg$warning_text)) {
        shinyjs::show(id = "warning_scenario_text")
        shinyjs::html(
          id = "warning_scenario_text", html = warning_msg$warning_text
        )
      }
    }
  }, ignoreInit = FALSE)
  
  # ------ Run Scenario --------------------------------------------------------
  observeEvent(input$run_scenario, {
    cat(file = stderr(), "10 - Run Scenario\n")
    # Check if at least one scenario is selected
    if (is.null(input$json_file_name)) {
      showNotification(
        "You need to select at least one Scenario to simulate!",
        type = "error"
      )
      return(NULL)
    }
    n_scenarios <- length(input$json_file_name)
    shinyjs::show(id = "spin_initial_run")
    
    # Progress bar
    withProgress(message = "Scenario Simulation:", value = 0, {
      
      lapply(input$json_file_name, function(json_file_name) {
        
        incProgress(1 / n_scenarios, detail = json_file_name)
        
        # Read Study Object ----------------------------------------------------
        para <- fromJSON(
          file.path(
            session$userData$user_folder,
            "study_objects",
            json_file_name
          ),
          flatten = TRUE
        )
        
        # TODO: correct this in the R package instead! (See Backlog) - quick fix
        environment(n_balance) <- environment()
        environment(ghg_emission) <- environment()
        environment(combineOutputs) <- environment() # nolint: object_name_linter
        
        # Intermediate calculation ---------------------------------------------
        feed_basket_quality <- feed_quality(para = para)
        # energy required
        energy_required <- energy_requirement(
          para = para,
          feed_basket_quality = feed_basket_quality,
          energy_parameters = energy_parameters
        )
        # land required
        land_required <- land_requirement(
          feed_basket_quality = feed_basket_quality,
          energy_required = energy_required,
          para = para
        )
        # soil erosion
        soil_erosion <- soil_health(
          para = para,
          land_required = land_required
        )
        # water required
        water_required <- water_requirement(
          para = para,
          land_required = land_required
        )
        # nitrogen balance
        nitrogen_balance <- n_balance(
          para = para,
          land_required = land_required,
          soil_erosion = soil_erosion
        )
        # livestock productivity
        livestock_productivity <- land_productivity(
          para = para,
          energy_required = energy_required
        )
        # biomass
        biomass <- biomass_calculation(
          para = para,
          land_required = land_required
        )
        # soil carbon
        soil_carbon <- soil_organic_carbon(
          para = para,
          stock_change_para = stock_change_para,
          land_required = land_required,
          biomass = biomass
        )
        # GHG emissions
        ghg_emissions <- ghg_emission(
          para = para,
          energy_required = energy_required,
          ghg_ipcc_data = ghg_ipcc_data,
          land_required = land_required,
          nitrogen_balance = nitrogen_balance
        )

        # Add dry matter intake checks -----------------------------------------
        # Convert to daily intake
        dmi_per_day <- energy_required$annual_results$dmi_tot / 365 
        # 4% of body weight
        recommended_intake_level <- 
          energy_required$annual_results$body_weight * 0.04
        # Check if intake is greater than 4%
        warning_check_index <- which(dmi_per_day > recommended_intake_level)
        warning_check_names <- 
          energy_required$annual_results$livestock_category_name[
            warning_check_index
          ]
        # Warning message
        if (length(warning_check_index) > 0) {
          warning_text <- paste0(
            "The following livestock types have a daily dry matter intake 
            greater than 4% of their body weight: ",
            paste(
              warning_check_names,
              collapse = ", "
            )
          )
        }
        
        # Calculate and save results -------------------------------------------
        result_path <- file.path(
          session$userData$user_folder, "scenarios",
          sub("\\.json$", "", json_file_name)
        ) 
        dir.create(result_path)
        cleanedOut <- combineOutputs( # nolint: object_name_linter
          para = para, feed_basket_quality = feed_basket_quality,
          energy_required = energy_required, land_required = land_required,
          soil_erosion = soil_erosion, water_required = water_required,
          nitrogen_balance = nitrogen_balance,
          livestock_productivity = livestock_productivity,
          biomass = biomass, soil_carbon = soil_carbon,
          ghg_emission = ghg_emissions,
          filePath = file.path(
            result_path, paste0(sub("\\.json$", "", json_file_name))
          ), primary_excel = primary_excel
        )
        
        # Save the cleaned output
        write(
          cleanedOut$json_output,
          file.path(
            result_path, paste0(sub("\\.json$", "", json_file_name), ".json")
          )
        )
        
        # The warning messages
        warning_msg <- if (length(warning_check_index) > 0) {
          list(
            warning_check_index = warning_check_index,
            warning_text = warning_text
          )
        } else {
          list(
            warning_check_index = NULL,
            warning_text = NULL
          )
        }
        
        # Save Scenario Simulation results
        saveRDS(
          list(
            warning_msg = warning_msg,
            on_farm_table = cleanedOut$on_farm_table,
            nitrogen_balance = cleanedOut$nitrogen_balance,
            land_required = cleanedOut$land_required,
            water_use_per_feed_item = cleanedOut$water_use_per_feed_item
          ), 
          file.path(result_path, "scenario_data.rds")
        )
      })
      shinyjs::hide(id = "spin_initial_run")
    })
    
    # Update the list of scenarios
    session$userData$scenarios_list(
      list.files(
        file.path(
          session$userData$user_folder, "scenarios"
        ), full.names = FALSE
      )
    )
    
    # update choices for scenario_name and scenario_results_comp input
    shinyWidgets::updatePickerInput(
      inputId = "scenario_name",
      choices = list.files(
        file.path(session$userData$user_folder, "scenarios"), full.names = FALSE
      ),
      selected = sub("\\.json$", "", input$json_file_name[1])
    )
    
    # update choices for scenario_results_comp input (comparison module)
    # User Data is used to establish the connection between the two modules
    session$userData$run_scenario_counter(
      session$userData$run_scenario_counter() + 1
    )
  })
  
  # ------ Download Scenario Simulation Results --------------------------------
  output$download_results <- downloadHandler(
    filename = function() {
      paste0(input$scenario_name, ".zip")
    },
    content = function(file) {
      cat(file = stderr(), "10 - Download Scenario Simulation Results\n")
      withProgress(message = "Downloading Scenario results...", value = 0.5, {
        # Get the path to the results folder
        result_path <- file.path(
          session$userData$user_folder, "scenarios", input$scenario_name
        )
        scenario_data <- readRDS(
          file.path(
            session$userData$user_folder,
            "scenarios",
            input$scenario_name,
            "scenario_data.rds"
          )
        )
        
        # Save PNGs in the results folder
        # ghg emission
        plot_ghg_emissions(
          data_table = scenario_data$on_farm_table,
          unit = graphs_desc[graphs_desc$indicator == "ghg_emission", ]$unit,
          png = TRUE
        )
        ggsave(
          file.path(result_path, "ghg_emission.png"),
          width = 400, height = 250, units = "mm"
        )
        
        # n balance
        plot_n_balance(
          data_table = scenario_data$nitrogen_balance,
          unit = graphs_desc[graphs_desc$indicator == "n_balance", ]$unit,
          png = TRUE
        )
        ggsave(
          file.path(result_path, "n_balance.png"),
          width = 400, height = 250, units = "mm"
        )
        
        # land required
        plot_land_req(
          data_table = scenario_data$land_required,
          unit = graphs_desc[graphs_desc$indicator == "land_req", ]$unit,
          png = TRUE
        )
        ggsave(
          file.path(result_path, "land_req.png"),
          width = 400, height = 250, units = "mm"
        )
        
        # water use per feed
        plot_water_feed(
          data_table = scenario_data$water_use_per_feed_item,
          unit = graphs_desc[graphs_desc$indicator == "water_feed", ]$unit,
          png = TRUE
        )
        ggsave(
          file.path(result_path, "water_feed.png"),
          width = 400, height = 250, units = "mm"
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
  
  # ------ Delete Scenario -----------------------------------------------------
  observeEvent(input$delete_scenario, {
    cat(file = stderr(), "10 - Delete Scenario\n")
    shinyWidgets::ask_confirmation(
      inputId = ns("delete_scenario_confirmation"),
      type = "warning",
      title = "Delete Scenario Results?",
      btn_colors = c("grey", "#009ADB")
    )
  })
  observeEvent(input$delete_scenario_confirmation, {
    cat(file = stderr(), "10 - Delete confirmation\n")
    req(input$delete_scenario_confirmation)
    source_file_path <- file.path(
      session$userData$user_folder, "scenarios", input$scenario_name
    )
    # Delete the selected scenario folder
    unlink(source_file_path, recursive = TRUE)
    # Update of scenario_results_comp and scenario_name select input
    shinyWidgets::updatePickerInput(
      session = session,
      "scenario_name",
      choices = list.files(
        file.path(session$userData$user_folder, "scenarios"), full.names = FALSE
      ),
      selected = list.files(
        file.path(session$userData$user_folder, "scenarios"), full.names = FALSE
      )[1]
    )
    shinyWidgets::updatePickerInput(
      inputId = "scenario_results_comp",
      choices = list.files(
        file.path(session$userData$user_folder, "scenarios"), full.names = FALSE
      ),
      selected = list.files(
        file.path(session$userData$user_folder, "scenarios"), full.names = FALSE
      )[1]
    )
    showNotification(
      tags$span(tags$b(input$scenario_name), "deleted successfully!"),
      type = "message"
    )
  })
  
  # ------ PLOT GHG emission ---------------------------------------------------
  output$plot_ghg_emission <- renderGirafe({
    
    req(input$scenario_name)
    req(session$userData$run_scenario_counter())
    cat(file = stderr(), "10 - Plot GHG emission\n")
    
    # Read the data
    on_farm_table <- readRDS(
      file.path(
        session$userData$user_folder, "scenarios",
        input$scenario_name,
        "scenario_data.rds"
      )
    )[["on_farm_table"]]
    
    unit <- graphs_desc[graphs_desc$indicator == "ghg_emission", ]$unit
    plot_ghg_emissions(on_farm_table, unit)
  })
  
  # ------ PLOT N BALANCE ------------------------------------------------------
  output$plot_n_balance <- renderGirafe({
    
    req(input$scenario_name)
    req(session$userData$run_scenario_counter())
    cat(file = stderr(), "10 - Plot N Balance\n")
    
    # Read the data
    nitrogen_balance <- readRDS(
      file.path(
        session$userData$user_folder,
        "scenarios",
        input$scenario_name,
        "scenario_data.rds"
      )
    )[["nitrogen_balance"]]
    
    unit <- graphs_desc[graphs_desc$indicator == "n_balance", ]$unit
    plot_n_balance(nitrogen_balance, unit)
  })
  
  # ------ PLOT LAND REQUIRED --------------------------------------------------
  output$plot_land_req <- renderGirafe({
    
    req(session$userData$run_scenario_counter())
    req(input$scenario_name)
    cat(file = stderr(), "10 - Plot Land Required\n")
    
    # hide all the seasons legend items
    lapply(1:max_seasons, function(i) {
      shinyjs::hide(id = paste0("Seasons_LegendItemtxt_", i))
    })
    
    # Read the data
    land_required <- readRDS(
      file.path(
        session$userData$user_folder,
        "scenarios", input$scenario_name,
        "scenario_data.rds"
      )
    )[["land_required"]]
    
    # unique seasons
    seasons <- unique(land_required$season_name)
    # display the legend according to the seasons
    lapply(seq_along(seasons), function(i) {
      # Show the hidden column
      shinyjs::show(id = paste0("Seasons_LegendItemtxt_", i))
      # Update the p tag content
      shinyjs::html(id = paste0("Seasons_LegendItemtxt_", i), html = seasons[i]) 
    })
    
    unit <- graphs_desc[graphs_desc$indicator == "land_req", ]$unit
    plot_land_req(land_required, unit)
  })
  
  # ----- PLOT WATER USE PER FEED ----------------------------------------------
  output$plot_water_feed <- renderGirafe({
    
    req(session$userData$run_scenario_counter())
    req(input$scenario_name)
    cat(file = stderr(), "10 - Plot Water Use per Feed\n")
    
    # hide all the feeds legend items
    lapply(1:max_feed_items, function(i) {
      shinyjs::hide(id = paste0("Feeds_LegendItemtxt_", i))
    })
    
    # Read the data
    water_use_per_feed_item <- readRDS(
      file.path(
        session$userData$user_folder, "scenarios",
        input$scenario_name, "scenario_data.rds"
      )
    )[["water_use_per_feed_item"]]
    
    # unique feeds
    feeds <- unique(water_use_per_feed_item$feed)
    
    # display the legend according to the feeds
    lapply(seq_along(feeds), function(i) {
      # Show the hidden column
      shinyjs::show(id = paste0("Feeds_LegendItemtxt_", i))
      # Update the p tag content
      shinyjs::html(id = paste0("Feeds_LegendItemtxt_", i), html = feeds[i]) 
    })
    
    unit <- graphs_desc[graphs_desc$indicator == "water_feed", ]$unit
    plot_water_feed(water_use_per_feed_item, unit)
  })
})}
