# Settings.R
## This file contains all user-adjustable input parameters for CBW-CSNAPNI.

#Print Troubleshooting Tags
print_tags = 0
if(print_tags == 1){
  print("Config/Settings.R")
}

#Loading all the packages used in the toolbox ####
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)
packagesused <- c('data.table','dplyr','EnvStats','fitdistrplus','knitr','ggplot2','patchwork','plotly','readxl','rnassqs','sf','stringr','tidyverse','scales')
pacman::p_load(char = packagesused)

#Regenerate input files? 0=no, 1=yes ####
get_new_data=1
print(paste("Input files regenerated (0=no, 1=yes): ", get_new_data),quote=FALSE)

# Monte Carlo Uncertainty Analysis Settings ####
run_monte_carlo <- FALSE  # Set to TRUE to enable Monte Carlo uncertainty analysis
n_iterations <- 10      # Number of Monte Carlo iterations (recommended: 1000-10000)
mc_seed <- 123           # Random seed for reproducibility

if(run_monte_carlo) {
  cat("Monte Carlo Uncertainty Analysis: ENABLED\n")
  cat("Number of iterations:", n_iterations, "\n")
} else {
  cat("Monte Carlo Uncertainty Analysis: DISABLED\n")
}

#Key names of variables
anim_names <- c("[1]fattened_cattle", "[2]milk_cows", "[3]hogs_for_breeding", "[4]hogs_for_slaughter", "[5]layers",
  "[6]breeding_turkeys", "[7]pullets", "[8]broilers", "[9]slaughter_turkeys", "[10]beef_breeding_herd",
  "[11]beef_calves", "[12]dairy_calves", "[13]beef_heifers", "[14]dairy_heifers",
  "[15]beef_stockers", "[16]dairy_stockers", "[17]sheep", "[18]horses", "[19]goats")

cropnames <- c("[1] corn_grain", "[2] corn_silage", "[3] wheat_areas", "[4] oats_areas", "[5] barley_areas",
           "[6] sorghum_grain", "[7] sorghum_silage", "[8] potatoes", "[9] rye",
           "[10] alfalfa_hay", "[11] other_hay", "[12] soybeans", "[13] cropland_pasture",
           "[14] noncropland_pasture", "[15] rice", "[16] peanuts", "[17] grass", "[18] winter_rye",
           "[19] CGF", "[20] CGM", "[21] DGS")

##input file creation settings
##allocation method
alloc_methods = c("output masses", "energy content", "market value", "input mass (i.e., corn starch content)", "no allocation to ethanol","no allocation to feed coproducts")
alloc_method = 4
print(paste("Ethanol and coproduct allocation method: ", alloc_methods[alloc_method]),quote=FALSE)

#protein assumptions
protassump=1
if(protassump==1){
  print(paste("Using Costello's protein assumptions for meat."),quote=FALSE)
}else if(protassump==2){
  print(paste("Using DeVries' protein assumptions for meat."),quote=FALSE)
}

#animal P intake
#animdatadyn = t(array(scan("InputFiles/animdatadyn_min.txt"), c(23,19))) #minimum P intakes
#animdatadyn = t(array(scan("InputFiles/animdatadyn_max.txt"), c(23,19))) #maximum P intakes (Used in the past)
#animdatadyn = t(array(scan("InputFiles/animdatadyn_Pdigest.txt"), c(23,19))) #digestibility-reduced P intakes and P excreted for swine and poultry
animdatadyn = t(array(scan("InputFiles_CBW/animdatadyn_max_updated.txt"), c(23,19))) #maximum P intakes (new version updated)
rownames(animdatadyn) <- anim_names
colnames(animdatadyn) <- c("Cycles per Year","Cycles per year^-1","(cycles -1)/cycles","(cycles per year^-1) * ((cycles-1)/cycles)","Days on the Farm","days on farm/365","(days on farm/365) * 0.5","Animal N Intake (kg-N/animal/yr)","Animal P Intake (kg-P/animal/yr)","N in Animal Excretion (kg-N/animal/yr)","P in Animal Excretion (kg-P/animal/yr)","Ammonia Emission (kg-N/animal/yr)","% Loss in Human Consumption","Product Weight (kg/animal)","Edible Proportion","N in Edible Proportion","kcal/kg edible ","g protein/kg edible ","grain feed only? 1 = yes, 0 = no","LOW N in Animal Excretion (kg-N/animal/yr)","HIGH N in Animal Excretion (kg-N/animal/yr)","LOW Ammonia Emission (kg-N/animal/yr)","HIGH Ammonia Emission (kg-N/animal/yr)")

#data years to load
run_yrs = c("97","02","07","12","17","22") #last two digits of data years to import
year_labels = c(1997,2002,2007,2012,2017,2022)
nyrs = length(run_yrs)
print(paste("Running for the following year(s): "), quote=FALSE)
print(paste(run_yrs), quote=FALSE)

ws_name = "Miss_Atch"
ws = c(22,25) ##Missippi River and Atchafalaya Watersheds
#ws = c(52:60) ##Chesapeake Bay Watersheds (Patuxent,Potomac,Rappahannock,
#York,James,Chester,Choptank,Tangier/Pokomoke,Chesapeake Bay Mainstream)
ws <- 919:933 ## Lancaster LRS


# Fertilizer assumptions
fertassump=4
if(fertassump==1){
  print(paste("Using national CSNAPNI fertilizer values."),quote=FALSE)
}else if(fertassump==2){
  print(paste("Using NASS Survey fertilizer data."),quote=FALSE)
}else if(fertassump==3){
  print(paste("Using PSU Agronomy Guide approach."),quote=FALSE)
}else if(fertassump==4){
  print(paste("Using Inorganic Fertilizer+Manure approach."),quote=FALSE)
}

nass_survey_fert <- 0 # (Y = 1, N = 0) Use NASS fertilizer data? If 0, use CSNAPNI fertilizer data
Nfert_lbsperacre2

# Manure management scenarios
rec_manure = 2 # 1 is cs-nani style, 2 is new style where it not only considers recoverable manure nutrient calculation according to Kellog 2014 

# Grass scenario ####
# Conditions for this scenario 
grass_scenario <-  2 # (Y = 1, N = 0)
grass_fert_scenario <- 0 # (Y = 1, N = 0)

#[Options] "Corn","Sorghum","Soybeans","Peanuts","Barley","Wheat","Rye","Oats","Alfalfa","Other.Hay.Non.Alfalfa","Potatoes","Grass.Pasture"  
#crot1 <- c("Corn","Corn","Soybeans","Other.Hay.Non.Alfalfa","Other.Hay.Non.Alfalfa")
#[Options] "corn for grain","corn for silage","wheat","oats","barley","sorghum for grain","sorghum for silage","potatoes","rye",
#"alfalfa hay","other hay","soybeans","cropland pasture","noncropland pasture","rice","peanuts","grass","CGF","CGM","DGS"    
#crot2 <- c("corn for grain","corn for grain","soybeans","other hay","other hay")

#Crotation <- c(crot1[1],crot2[1],crot1[2],crot2[2],crot1[3],crot2[3],crot1[4],crot2[4],crot1[5],crot2[5]) # Don't change this one (updated automatically)
# Constants and Variables
land_use_grass <- 0.10 # Reference: Zhou et al.(2014)
grass_yield_no_fert <- 5 # Reference: Woodbury et al.(2018) Unit: Mg/ha (DM)
# grass_yield_fert <- 19.7 # Reference: Kering et al.(2012) Unit: Mg/ha

# Winter crop scenario ####
wr_scenario <- 1 # (Y = 1, N = 0)
wr_use <- 2 # (Cover crop = 1, Double Crop = 2)
wr_adoption_corn <- 0.3 # Percentage of corn land adopting cover crop
wr_adoption_soybean <- 0.3 # Percentage of corn land adopting cover crop
wr_yield_cc <- 5 # ton DM/ha (eqv 2.37 ton DM/acre) Reference
wr_yield_dc <- 5 # ton DM/ha (eqv 3.88 ton DM/acre) # Reference ???
wr_biogas <- 0 # (Y = 1, N = 0)
# ============================================================================
# Function to set scenario parameters
set_scenario <- function(scenario_type) {
  
  # Initialize all parameters
  params <- list()
  
  if (scenario_type == "baseline") {
    # Baseline scenario - all practices inactive
    params$grass_scenario <- 0
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0
    params$grass_yield_no_fert <- 0
    params$wr_scenario <- 0
    params$wr_use <- 0
    params$wr_adoption_corn <- 0
    params$wr_adoption_soybean <- 0
    params$wr_yield_cc <- 0
    params$wr_yield_dc <- 0
    params$wr_biogas <- 0
    
    # BMP factors
    params$grass_bmp_factor <- 1  # No grass
    params$wr_bmp_factor <- 1 # No winter rye
    
  } else if (scenario_type == "grass_pct") {
    # Grass scenario - 10% implementation
    params$grass_scenario <- 1  # 10% implementation
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0.10
    params$grass_yield_no_fert <- 5
    params$wr_scenario <- 0
    params$wr_use <- 0
    params$wr_adoption_corn <- 0
    params$wr_adoption_soybean <- 0
    params$wr_yield_cc <- 0
    params$wr_yield_dc <- 0
    params$wr_biogas <- 0
    
    # BMP factors
    params$grass_bmp_factor <- 0.87  # 20% N load reduction from grass (profitability)
    params$wr_bmp_factor <- 1 # No winter rye
    
  } else if (scenario_type == "grass_profitability") {
    # Grass scenario - profitability-based
    params$grass_scenario <- 2  # profitability-based
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0.10
    params$grass_yield_no_fert <- 5
    params$wr_scenario <- 0
    params$wr_use <- 0
    params$wr_adoption_corn <- 0
    params$wr_adoption_soybean <- 0
    params$wr_yield_cc <- 0
    params$wr_yield_dc <- 0
    params$wr_biogas <- 0
    
    # BMP factors
    params$grass_bmp_factor <- 0.87  # 20% N load reduction from grass (profitability)
    params$wr_bmp_factor <- 1 # No winter rye
    
  } else if (scenario_type == "cover_crop") {
    # Cover crop scenario
    params$grass_scenario <- 0
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0
    params$grass_yield_no_fert <- 0
    params$wr_scenario <- 1
    params$wr_use <- 1  # Cover crop
    params$wr_adoption_corn <- 0.3
    params$wr_adoption_soybean <- 0.3
    params$wr_yield_cc <- 5
    params$wr_yield_dc <- 0
    params$wr_biogas <- 0
    
  } else if (scenario_type == "double_crop") {
    # Double crop scenario
    params$grass_scenario <- 0
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0
    params$grass_yield_no_fert <- 0
    params$wr_scenario <- 1
    params$wr_use <- 2  # Double crop
    params$wr_adoption_corn <- 0.3
    params$wr_adoption_soybean <- 0.3
    params$wr_yield_cc <- 0
    params$wr_yield_dc <- 5
    params$wr_biogas <- 1
    
    # BMP factors
    params$grass_bmp_factor <- 0.87  # 20% N load reduction from grass (profitability)
    params$wr_bmp_factor <- 0.90 # 10% N load reduction from winter rye (double crop)
    
  } else if (scenario_type == "grass_cover_combo") {
    # Combined grass and cover crop scenario
    params$grass_scenario <- 1
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0.10
    params$grass_yield_no_fert <- 5
    params$wr_scenario <- 1
    params$wr_use <- 1  # Cover crop
    params$wr_adoption_corn <- 0.3
    params$wr_adoption_soybean <- 0.3
    params$wr_yield_cc <- 5
    params$wr_yield_dc <- 0
    params$wr_biogas <- 0
    
    # BMP factors
    params$grass_bmp_factor <- 0.87  # 20% N load reduction from grass (profitability)
    params$wr_bmp_factor <- 0.88 # 10% N load reduction from winter rye (cover crop)
    
  } else if (scenario_type == "grass_double_combo") {
    # Combined grass and double crop scenario
    params$grass_scenario <- 1
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0.10
    params$grass_yield_no_fert <- 5
    params$wr_scenario <- 1
    params$wr_use <- 2  # Double crop
    params$wr_adoption_corn <- 0.3
    params$wr_adoption_soybean <- 0.3
    params$wr_yield_cc <- 0
    params$wr_yield_dc <- 5
    params$wr_biogas <- 0
    
    # BMP factors
    params$grass_bmp_factor <- 0.87  # 20% N load reduction from grass (profitability)
    params$wr_bmp_factor <- 0.90 # 10% N load reduction from winter rye (double crop)
    
  } else if (scenario_type == "biogas_grass_pct_double_combo") {
    # Combined grass and double crop scenario
    params$grass_scenario <- 1
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0.10
    params$grass_yield_no_fert <- 5
    params$wr_scenario <- 1
    params$wr_use <- 2  # Double crop
    params$wr_adoption_corn <- 0.3
    params$wr_adoption_soybean <- 0.3
    params$wr_yield_cc <- 0
    params$wr_yield_dc <- 5
    params$wr_biogas <- 1
    
    # BMP factors
    params$grass_bmp_factor <- 0.87  # 20% N load reduction from grass (profitability)
    params$wr_bmp_factor <- 0.90 # 10% N load reduction from winter rye (double crop)
    
  } else if (scenario_type == "biogas_grass_profit_double_combo") {
    # Combined grass and double crop scenario
    params$grass_scenario <- 2
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0
    params$grass_yield_no_fert <- 5
    params$wr_scenario <- 1
    params$wr_use <- 2  # Double crop
    params$wr_adoption_corn <- 0.3
    params$wr_adoption_soybean <- 0.3
    params$wr_yield_cc <- 0
    params$wr_yield_dc <- 5
    params$wr_biogas <- 1
    
    # BMP factors
    params$grass_bmp_factor <- 0.87  # 20% N load reduction from grass (profitability)
    params$wr_bmp_factor <- 0.90 # 10% N load reduction from winter rye (double crop)
    
  } else if (scenario_type == "biogas_grass_profit_cover_combo") {
    # Combined grass and double crop scenario
    params$grass_scenario <- 2
    params$grass_fert_scenario <- 0
    params$land_use_grass <- 0
    params$grass_yield_no_fert <- 5
    params$wr_scenario <- 1
    params$wr_use <- 2  # Double crop
    params$wr_adoption_corn <- 0.3
    params$wr_adoption_soybean <- 0.3
    params$wr_yield_cc <- 0
    params$wr_yield_dc <- 5
    params$wr_biogas <- 1
    
    # BMP factors
    params$grass_bmp_factor <- 0.87  # 20% N load reduction from grass (profitability)
    params$wr_bmp_factor <- 0.90 # 10% N load reduction from winter rye (double crop)
    
  } else {
    stop("Invalid scenario type. Choose from: 'baseline', 'grass_pct', 'grass_profitability', 'cover_crop', 'double_crop', 'grass_cover_combo', 'grass_double_combo','biogas_grass_pct_double_combo','biogas_grass_profit_double_combo','biogas_grass_profit_cover_combo'")
  }
  
  return(params)
}

# Apply the scenario settings
scenario_params <- set_scenario(CURRENT_SCENARIO)

# Assign parameters to variables
grass_scenario <- scenario_params$grass_scenario
grass_fert_scenario <- scenario_params$grass_fert_scenario
land_use_grass <- scenario_params$land_use_grass
grass_yield_no_fert <- scenario_params$grass_yield_no_fert
wr_scenario <- scenario_params$wr_scenario
wr_use <- scenario_params$wr_use
wr_adoption_corn <- scenario_params$wr_adoption_corn
wr_adoption_soybean <- scenario_params$wr_adoption_soybean
wr_yield_cc <- scenario_params$wr_yield_cc
wr_yield_dc <- scenario_params$wr_yield_dc
wr_biogas <- scenario_params$wr_biogas
grass_bmp_factor <- scenario_params$grass_bmp_factor
wr_bmp_factor <- scenario_params$wr_bmp_factor

# Print current scenario settings
cat("Current Scenario:", CURRENT_SCENARIO, "\n")
cat("Grass scenario:", grass_scenario, "\n")
cat("Winter rye scenario:", wr_scenario, "\n")
cat("Winter rye use:", wr_use, "\n")
cat("Biogas production enabled:", wr_biogas, "\n")


#Functions created to facilitate conversions ####
# Unit Conversion Functions for Agricultural Engineering
# Functions to convert between different units commonly used in agricultural studies

# ============================================================================
# BUSHELS TO TONS AND TONS TO BUSHELS (by crop type)
# ============================================================================

# Convert bushels to metric tons
bushels_to_tonnes <- function(bushels, crop) {
  # Conversion factors: bushels per metric ton
  conversion_factors <- list(
    "corn" = 39.368,
    "sorghum" = 39.368,
    "barley" = 45.9296,
    "wheat" = 36.7437,
    "soybeans" = 36.7437,
    "oats" = 64.8420,
    "rye" = 39.3680
  )
  
  # Check if crop is valid
  if (!(tolower(crop) %in% names(conversion_factors))) {
    stop("Invalid crop type. Available crops: ", paste(names(conversion_factors), collapse = ", "))
  }
  
  # Convert bushels to tons
  tons <- bushels / conversion_factors[[tolower(crop)]]
  return(tons)
}

# Convert metric tons to bushels
tonnes_to_bushels <- function(tons, crop) {
  # Conversion factors: bushels per metric ton
  conversion_factors <- list(
    "corn" = 39.368,
    "sorghum" = 39.368,
    "barley" = 45.9296,
    "wheat" = 36.7437,
    "soybeans" = 36.7437,
    "oats" = 64.8420,
    "rye" = 39.3680
  )
  
  # Check if crop is valid
  if (!(tolower(crop) %in% names(conversion_factors))) {
    stop("Invalid crop type. Available crops: ", paste(names(conversion_factors), collapse = ", "))
  }
  
  # Convert tons to bushels
  bushels <- tons * conversion_factors[[tolower(crop)]]
  return(bushels)
}

# ============================================================================
# WEIGHT CONVERSIONS (kg to lbs and lbs to kg)
# ============================================================================

# Convert kilograms to pounds
kg_to_lbs <- function(kg) {
  lbs <- kg * 2.20462
  return(lbs)
}

# Convert pounds to kilograms
lbs_to_kg <- function(lbs) {
  kg <- lbs / 2.20462
  return(kg)
}

# ============================================================================
# AREA CONVERSIONS (acres to km2 and km2 to acres)
# ============================================================================

# Convert acres to square kilometers
acres_to_km2 <- function(acres) {
  km2 <- acres * 0.00405
  return(km2)
}

# Convert square kilometers to acres
km2_to_acres <- function(km2) {
  acres <- km2 / 0.00405
  return(acres)
}

# ============================================================================
# VOLUME CONVERSIONS (gallons to liters and liters to gallons)
# ============================================================================

# Convert gallons to liters
gallons_to_liters <- function(gallons) {
  liters <- gallons * 3.78541
  return(liters)
}

# Convert liters to gallons
liters_to_gallons <- function(liters) {
  gallons <- liters / 3.78541
  return(gallons)
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

# Example usage of the functions:
# 
# # Convert 1000 bushels of corn to tons
# corn_tons <- bushels_to_tons(1000, "corn")
# print(paste("1000 bushels of corn =", round(corn_tons, 2), "metric tons"))
# 
# # Convert back to bushels
# corn_bushels <- tons_to_bushels(corn_tons, "corn")
# print(paste(round(corn_tons, 2), "metric tons of corn =", round(corn_bushels, 2), "bushels"))
# 
# # Convert 100 kg to lbs
# weight_lbs <- kg_to_lbs(100)
# print(paste("100 kg =", round(weight_lbs, 2), "lbs"))
# 
# # Convert 1000 acres to km2
# area_km2 <- acres_to_km2(1000)
# print(paste("1000 acres =", round(area_km2, 2), "km2"))
# 
# # Convert 100 gallons to liters
# volume_liters <- gallons_to_liters(100)
# print(paste("100 gallons =", round(volume_liters, 2), "liters"))

# ============================================================================
# HELPER FUNCTION: Get conversion factor for a specific crop
# ============================================================================

get_bushel_per_tonne_factor <- function(crop) {
  conversion_factors <- list(
    "corn" = 39.368,
    "sorghum" = 39.368,
    "barley" = 45.9296,
    "wheat" = 36.7437,
    "soybeans" = 36.7437,
    "oats" = 64.8420,
    "rye" = 39.3680
  )
  
  if (!(tolower(crop) %in% names(conversion_factors))) {
    stop("Invalid crop type. Available crops: ", paste(names(conversion_factors), collapse = ", "))
  }
  
  return(conversion_factors[[tolower(crop)]])
}
# ============================================================================
# NAME GENERATION FUNCTION
# ============================================================================

create_cbw_dimnames <- function(..., style = "bracket") {
  
  # Define all available entity types and their properties
  entity_library <- list(
    "lrs" = list(
      count = 1925,
      labels = CBW_lrs_shp$LndRvrSeg,
      description = "Land River Segments"
    ),
    "counties" = list(
      count = 197, 
      labels = FIPS,
      description = "Counties"
    ),
    "crops" = list(
      count = 21,
      labels = crop_labels,
      description = "Crops"
    ),
    "animals" = list(
      count = 19,
      labels = animalnames, 
      description = "Animals"
    ),
    "year" = list(
      count = 6,
      labels = year_labels,
      description = "Years"
    ),
    "meat" = list(
      count = 9,
      labels = c("beef","dairy","swine","sheep","horse","layers","broilers","turkey","goats"),
      description = "Meat Products"
    ),
    "states" = list(
      count = 50,
      labels = paste0("State_", 1:50),
      description = "States"
    )
  )
  
  # Get the entity types requested
  requested_entities <- list(...)
  
  # Validate inputs
  for(entity in requested_entities) {
    if(!entity %in% names(entity_library)) {
      stop(paste("Unknown entity type:", entity, 
                 "\nAvailable types:", paste(names(entity_library), collapse = ", ")))
    }
  }
  
  # Generate dimension names based on style
  dimension_names <- list()
  
  for(i in seq_along(requested_entities)) {
    entity_type <- requested_entities[[i]]
    entity_info <- entity_library[[entity_type]]
    
    if(style == "bracket") {
      # Style: [1,] LRS_001 or [,1] Corn
      if(i == 1) {
        # Rows
        dim_names <- paste0("[", seq(1, entity_info$count), ",] ", entity_info$labels)
      } else if(i == 2) {
        # Columns  
        dim_names <- paste0("[,", seq(1, entity_info$count), "] ", entity_info$labels)
      } else {
        # Higher dimensions
        dim_names <- paste0("[", paste(rep(",", i-1), collapse=""), 
                            seq(1, entity_info$count), "] ", entity_info$labels)
      }
    } else if(style == "simple") {
      # Style: just the labels
      dim_names <- entity_info$labels
    } else if(style == "indexed") {
      # Style: EntityType_001_LabelName
      dim_names <- paste0(toupper(entity_type), "_", 
                          sprintf("%03d", seq(1, entity_info$count)), "_", 
                          entity_info$labels)
    } else if(style == "descriptive") {
      # Style: [Dim 1: Counties] FIPS_Code
      dim_names <- paste0("[Dim ", i, ": ", entity_info$description, "] ", entity_info$labels)
    }
    
    dimension_names[[entity_type]] <- dim_names
  }
  
  
  return(dimension_names)
}

