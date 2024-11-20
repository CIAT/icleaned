# Updated column names for crop and livestock tables ---------------------------
# for better presentation in Shiny data table
feedtype_colnames <- c(
  "", # For the delete_row column
  "Feed",
  "Crop",
  "Source type",
  "Intercropping (yes / no)",
  "IF intercropping, fraction of field occupied by this crop",
  "Cut-and-carry (fraction)",
  "Land cover",
  "Slope",
  "Length of slope (m)",
  "Management grassland",
  "Main product removal (fraction)",
  "Crop residue removal from field (fraction)",
  "Crop residue burnt (fraction)",
  "DM content (%)",
  "ME content (MJ/kg DM)",
  "CP content (% DM)",
  "water regime",
  "Cultivation period",
  "rice ecosystem type",
  "rice organic amendment",
  "Grassland change factor",
  "Land cover C Factor",
  "Slope P Factor",
  "Main product dry yield (t DM/ha)",
  "Residue dry yield (t DM/ha)",
  "Main product N content (kg N/kg DM)",
  "Reside N content (kg N/kg DM)",
  "Kc: Initial",
  "Kc: MidSeason",
  "Kc: Late",
  "Category",
  "Trees/ha",
  "Trees DBH",
  "Trees annual growth (kg)",
  "Trees annual removal (kg)",
  "Number of trees per hectare with a diameter at breast height of less than 25cm",
  "Average diameter at breast height of trees with a DBH of less than 25cm",
  "Diameter at breast height increase in trees with a DBH of less than 25cm (cm/year)",
  "Number of trees per hectare with a diameter at breast height of 25-50cm",
  "Average diameter at breast height of trees with a DBH of 25-50cm",
  "Diameter at breast height increase in trees with a DBH of 25-50cm (cm/year)",
  "Number of trees per hectare with a diameter at breast height more than 50cm",
  "Average diameter at breast height of trees with a DBH more than 50cm",
  "Diameter at breast height increase in trees with a DBH of more than 50cm (cm/year)",
  "Time it takes for tree to mature (years)",
  "Tree breast diameter (cm)"
)

livestock_table_colnames <- c(
  "", # For the delete_row column
  "Category",
  "Number",
  "Average annual milk production (kg/animal)",
  "Average annual wool production (kg/animal)",
  "Average annual live weight gain (kg/animal)",
  "Time spent in stable (fraction of day)",
  "Collection of manure in stable (fraction)",
  "Manure management in stable",
  "Time spent in non-roofed enclosure  (fraction of day)",
  "Collection of manure in non-roofed enclosure (fraction)",
  "Manure management in non-roofed enclosure",
  "Time spent grazing  pasture/fields on-farm (fraction of day)",
  "Collection of manure in fields/pasture (fraction)",
  "Manure management during on-farm grazing",
  "Time spent grazing off-farm (fraction of day)",
  "Manure management during off-farm grazing",
  "Distance stable / enclosure to pasture (km)",
  "Collection of manure during off-farm grazing (fraction)",
  "Fraction of collected manure being sold",
  "Average Body weight (kg)",
  "The live bodyweight at weaning (kgs)",
  "The live bodyweight of sheep and goat at 1-year old or at slaughter 
  (live-weight) if slaughtered prior to 1 year of age (kgs)",
  "The mature body weight of an adult animal (kgs)",
  "Work animals (hours/day/animal)",
  "Litter size  (pigs)",
  "Number of piglets relying on milk",
  "Lactation length (small ruminants/pigs, days)",
  "Proportion growth piglets covered by milk (%)",
  "LW gain piglets (kg/day)",
  "Grazing displacement (km/day)",
  "Crude Protein/Lysine requirement maintenance (kg/day)",
  "Crude Protein requirement grazing (kg/km)",
  "Crude Protein/ Lysine requirement pregnancy (kg)",
  "Crude Protein/ Lysine requirement lactation (kg/ lactation)",
  "Crude Protein requirement  (kg/kg milk)",
  "Crude Protein/ Lysine requirement growth (kg/kg LWG)",
  "Parturition interval (years)",
  "Protein content milk (%)",
  "Fat content milk (%)",
  "Energy content milk",
  "Energy content meat",
  "Protein content meat (%)",
  "Carcass fraction",
  "Energy content eggs",
  "Average N content manure",
  "Meat product",
  "Milk product",
  "Methane emissions enteric fermentation - Tier 1",
  "Methane emissions enteric fermentation - Tier 2",
  "Methane emissions manure - Tier 1",
  "Default N-excretion rates Tier 1"
)

crop_inputs_table_colnames <- c(
  "Feed",
  "Crop",
  "Fraction collected manure used as fertilizer",            
  "Urea (kg/ha)",
  "NPK (kg/ha)",
  "DAP (kg/ha)",
  "Ammonium nitrate (kg/ha)",
  "Ammonium sulfate (kg/ha)",
  "N solutions (kg/ha)",
  "Ammonia (kg/ha)"
)

# Initialize data frames -------------------------------------------------------

seasons_initialization <- data.frame(
  Season = character(),
  Days = numeric(),
  stringsAsFactors = FALSE
)

fertilizers_initialization <- data.frame(
  fertilizer_code = character(),
  fertilizer_desc = character(),
  fraction = numeric(),
  percentage_n = numeric(),
  stringsAsFactors = FALSE
)

livestock_data_initialization <- data.frame(
  livetype_code = character(),
  livetype_desc = character(),
  herd_composition = numeric(),
  annual_milk = numeric(),
  annual_wool = numeric(),
  annual_growth = numeric(),
  time_in_stable = numeric(),
  manure_in_stable = numeric(),
  manureman_stable = character(),
  time_in_non_roofed_enclosure = numeric(),
  manure_in_non_roofed_enclosure = numeric(),
  manureman_non_roofed_enclosure = character(),
  time_in_onfarm_grazing = numeric(),
  manure_in_field = numeric(),
  manureman_onfarm_grazing = character(),
  time_in_offfarm_grazing = numeric(),
  manureman_offfarm_grazing = character(),
  distance_to_pasture = numeric(),
  manure_onfarm_fraction = numeric(),
  manure_sales_fraction = numeric(),
  body_weight = numeric(),
  body_weight_weaning = numeric(),
  body_weight_year_one = numeric(),
  adult_weight = numeric(),
  work_hour = numeric(),
  litter_size = numeric(),
  piglets_relying_on_milk = numeric(),
  lactation_length = numeric(),
  proportion_growth = numeric(),
  lw_gain = numeric(),
  grazing_displacement = numeric(),
  cp_maintenance = numeric(),
  cp_grazing = numeric(),
  cp_pregnancy = numeric(),
  cp_lactation = numeric(),
  cp_lactmilk = numeric(),
  cp_growth = numeric(),
  birth_interval = numeric(),
  protein_milkcontent = numeric(),
  fat_content = numeric(),
  energy_milkcontent = numeric(),
  energy_meatcontent = numeric(),
  protein_meatcontent = numeric(),
  carcass_fraction = numeric(),
  energy_eggcontent = numeric(),
  n_content = numeric(),
  meat_product = character(),
  milk_product = character(),
  ipcc_ef_category_t1 = character(),
  ipcc_ef_category_t2 = character(),
  ipcc_meth_man_category = character(),
  ipcc_n_exc_category = character(),
  stringsAsFactors = FALSE
)

feedtype_initialization <- data.frame(
  feed_type_code = numeric(),
  feed_item_code = numeric(),
  feed_type_name = character(),
  feed_item_name = character(),
  source_type = character(),
  intercrop = numeric(),
  intercrop_fraction = numeric(),
  cut_carry_fraction = numeric(),
  land_cover_desc = character(),
  slope_desc = character(),
  slope_length = numeric(),
  grassman_desc = character(),
  main_product_removal = numeric(),
  residue_removal = numeric(),
  residue_burnt = numeric(),
  dm_content = numeric(),
  me_content = numeric(),
  cp_content = numeric(),    
  water_regime = character(),     
  cultivation_period = numeric(),
  ecosystem_type = character(),
  organic_amendment = character(),
  grassman_change_factor = numeric(),
  landcover_c_factor = numeric(),
  slope_p_factor = numeric(),
  dry_yield = numeric(),
  residue_dry_yield = numeric(),
  n_content = numeric(),
  residue_n = numeric(),
  kc_initial = numeric(),
  kc_midseason = numeric(),
  kc_late = numeric(),
  category = character(),
  trees_ha = numeric(),
  trees_dhb = numeric(),
  trees_growth = numeric(),
  trees_removal = numeric(),
  trees_ha_dbh25 = numeric(),
  average_dbh25 = numeric(),
  increase_dbh25 = numeric(),
  trees_ha_dbh2550 = numeric(),
  average_dbh2550 = numeric(),
  increase_dbh2550 = numeric(),
  trees_ha_dbh50 = numeric(),
  average_dbh50 = numeric(),
  increase_dbh50 = numeric(),
  time_horizon = numeric(),
  diameter_breast = numeric(),
  
  # These ones are available in the json but not in the DT
  fraction_as_manure = character(),
  n_fertilizer = character(),
  main_n = numeric(),
  land_cover = character(),
  slope = character(),
  grassman = character(),
  
  stringsAsFactors = FALSE
)

crop_inputs_data_initialization <- data.frame(
  Crop = character(),
  Feed = character(),
  fraction_as_fertilizer = numeric(),
  urea = numeric(),
  npk = numeric(),
  dap = numeric(),
  ammonium_nitrate = numeric(),
  ammonium_sulfate = numeric(),
  n_solutions = numeric(),
  ammonia = numeric(),
  stringsAsFactors = FALSE
)

# Define the list of inputs for saving & loading -------------------------------

numeric_inputs <- c(
  "purchased_manure", "purchased_compost", "purchased_organic_n", 
  "purchased_bedding", "waste_production_milk", "waste_production_meat", 
  "waste_distribution_milk", "waste_distribution_meat", "waste_processing_milk", 
  "waste_processing_meat", "waste_consume_milk", "waste_consume_meat", 
  "annual_prec", "rain_length", "soil_k_value", "soil_n", "soil_c", 
  "soil_clay", "soil_bulk", "soil_depth", "et", "cropland_system_ipcc", 
  "cropland_tillage_ipcc", "cropland_orgmatter_ipcc", "grassland_management_ipcc", 
  "grassland_implevel_ipcc", "grassland_toarable", "arable_tograssland"
)

text_inputs <- c(
  "farm_name", "climate_zone", "climate_zone_2", "soil_description", 
  "cropland_system", "cropland_tillage", "cropland_orgmatter", 
  "grassland_management", "grassland_implevel"
)

manure_cols <- c(
  "manureman_stable",
  "manureman_non_roofed_enclosure",
  "manureman_onfarm_grazing",
  "manureman_offfarm_grazing"
)

# Named vector for hard coded percentage_n values ------------------------------
fertilizer_percentages <- c(
  "Urea" = 46,
  "DAP" = 18,
  "Ammonium nitrate" = 34.5,
  "Ammonium sulfate" = 21,
  "N solutions" = 32,
  "Ammonia" = 82
)

# Water regime SelectInput options ---------------------------------------------
water_regime_options <- c(
  "Non-flooded pre-season <180 days( often in double cropping of rice)",
  "Non-flooded pre-season >180 days (single rice crop following a dry fallow period)",
  "Flooded pre-season (>30)"
)

# Ecosystem type SelectInput options -------------------------------------------
rice_ecosystem_options <- c(
  "Irrigated - continuously flooded",
  "Irrigated - intermittently flooded-single aeration",
  "Irrigated - Intermittently flooded-multiple aeration",
  "Rainfed - regular rainfed",
  "Rainfed - drought prone",
  "Rainfed - deep water",
  "Upland",
  "None"
)

# Organic amendment SelectInput options ----------------------------------------
rice_organic_amendment_options <- c(
  "Straw incorporated in soil shortly (<30 days) before cultivation",
  "Straw incorporated in soil long (>30 days) before cultivation",
  "Compost",
  "Farm yard manure",
  "Green manure",
  "None"
)
