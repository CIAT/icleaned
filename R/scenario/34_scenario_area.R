# ----------- Area tab ---------------------------------------------------------
# Sub-module: area/soil/cropland/grassland observers
# Sourced from 30_mod_scenario_server.R with local = TRUE
observeEvent(input$soil_description, {
  selected_soil <- input$soil_description
  k_value <- lkp_soil()$k_value[lkp_soil()$soil_desc == selected_soil]
  updateNumericInput(session, "soil_k_value", value = k_value)
})

observeEvent(input$cropland_system, {
  selected_system <- input$cropland_system
  change_factor <- lkp_croplandsystem()$change_factor[lkp_croplandsystem()$sys_desc == selected_system]
  updateNumericInput(session, "cropland_system_ipcc", value = change_factor)
})

observeEvent(input$cropland_tillage, {
  selected_tillage <- input$cropland_tillage
  change_factor <- lkp_tillageregime()$change_factor[lkp_tillageregime()$tillage_desc == selected_tillage]
  updateNumericInput(session, "cropland_tillage_ipcc", value = change_factor)
})

observeEvent(input$cropland_orgmatter, {
  selected_input <- input$cropland_orgmatter
  change_factor <- lkp_organicmatter()$change_factor[lkp_organicmatter()$orgmatter_desc == selected_input]
  updateNumericInput(session, "cropland_orgmatter_ipcc", value = change_factor)
})

observeEvent(input$grassland_management, {
  selected_management <- input$grassland_management
  change_factor <- lkp_grasslandman()$change_factor[
    lkp_grasslandman()$management_desc == selected_management
  ]
  updateNumericInput(session, "grassland_management_ipcc", value = change_factor)
})

observeEvent(input$grassland_implevel, {
  selected_input_level <- input$grassland_implevel
  change_factor <- lkp_grassinputlevel()$change_factor[
    lkp_grassinputlevel()$grassinputlevel_desc == selected_input_level
  ]
  updateNumericInput(session, "grassland_implevel_ipcc", value = change_factor)
})
