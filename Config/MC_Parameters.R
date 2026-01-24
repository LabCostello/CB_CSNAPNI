# MC_Parameters.R
# Monte Carlo Uncertainty Parameter Definitions
# This file defines the uncertainty distributions for all variables in the uncertainty analysis

if(print_tags == 1){
  print("Config/MC_Parameters.R")
}

# ============================================================================
# PARAMETER UNCERTAINTY DEFINITIONS
# ============================================================================
# For each uncertain variable, define:
# - distribution type: "normal", "uniform", "triangular", "lognormal"
# - parameters: min, max, mode, mean, sd, etc.
# ============================================================================

# Initialize list to store all MC parameter definitions
mc_params <- list()

# CROP USE PARAMETERS ####
# cornuse - Corn grain usage allocation
mc_params$cornuse <- list(
  enabled = TRUE,
  type = "uniform",  # or "normal", "triangular"
  min_multiplier = 0.90,  # Allow 10% variation below baseline
  max_multiplier = 1.10,  # Allow 10% variation above baseline
  description = "Corn grain usage allocation uncertainty"
)

# EXPORT PARAMETERS ####
# exports - Export quantities
mc_params$exports <- list(
  enabled = FALSE,
  type = "normal",
  mean_multiplier = 1.0,  # Centered on baseline value
  cv = 0.15,  # Coefficient of variation (sd/mean) = 15%
  description = "Export quantities uncertainty"
)

# CROP DATA PARAMETERS ####
# cropdata_params - Crop production/yield parameters
# CROP N TO ANIMALS ####
# cropNtoanim - Crop nitrogen allocated to animal feed
mc_params$cropNtoanim <- list(
  enabled = TRUE,
  type = "uniform",
  min_multiplier = 0.85,
  max_multiplier = 1.15,
  description = "Crop N to animals allocation uncertainty"
)

# MEAT DATA ####
# meatdata - Meat production data
mc_params$meatdata <- list(
  enabled = TRUE,
  type = "normal",
  mean_multiplier = 1.0,
  cv = 0.12,
  description = "Meat production data uncertainty"
)

# FERTILIZER PARAMETERS ####
# Nfert - Nitrogen fertilizer application rates
mc_params$Nfert <- list(
  enabled = TRUE,
  type = "normal",
  mean_multiplier = 1.0,
  cv = 0.20,  # 20% coefficient of variation for fertilizer
  description = "Nitrogen fertilizer rate uncertainty"
)

# ANIMAL DATA DYNAMIC ####
# animdatadyn - Animal intake and excretion parameters
# Column-specific uncertainty control (19 rows × 23 columns)
# 
# Column names (from Settings.R):
# 1: Cycles per Year
# 2: Cycles per year^-1
# 3: (cycles -1)/cycles
# 4: (cycles per year^-1) * ((cycles-1)/cycles)
# 5: Days on the Farm
# 6: days on farm/365
# 7: (days on farm/365) * 0.5
# 8: Animal N Intake (kg-N/animal/yr)
# 9: Animal P Intake (kg-P/animal/yr)
# 10: N in Animal Excretion (kg-N/animal/yr)
# 11: P in Animal Excretion (kg-P/animal/yr)
# 12: Ammonia Emission (kg-N/animal/yr)
# 13: % Loss in Human Consumption
# 14: Product Weight (kg/animal)
# 15: Edible Proportion
# 16: N in Edible Proportion
# 17: kcal/kg edible
# 18: g protein/kg edible
# 19: grain feed only? 1 = yes, 0 = no
# 20: LOW N in Animal Excretion (kg-N/animal/yr)
# 21: HIGH N in Animal Excretion (kg-N/animal/yr)
# 22: LOW Ammonia Emission (kg-N/animal/yr)
# 23: HIGH Ammonia Emission (kg-N/animal/yr)

mc_params$animdatadyn <- list(
  enabled = TRUE,
  type = "column_specific",  # Special type for column-wise control
  description = "Animal intake/excretion parameter uncertainty (column-specific)",
  
  # Define uncertainty for each column
  # Set enabled = FALSE for columns that should NOT vary
  columns = list(
    
    # Columns 1-7: Derived/fixed values - NO UNCERTAINTY
    "1" = list(enabled = FALSE, description = "Cycles per Year - FIXED"),
    "2" = list(enabled = FALSE, description = "Cycles per year^-1 - FIXED"),
    "3" = list(enabled = FALSE, description = "(cycles -1)/cycles - FIXED"),
    "4" = list(enabled = FALSE, description = "(cycles per year^-1) * ((cycles-1)/cycles) - FIXED"),
    "5" = list(enabled = FALSE, description = "Days on the Farm - FIXED"),
    "6" = list(enabled = FALSE, description = "days on farm/365 - FIXED"),
    "7" = list(enabled = FALSE, description = "(days on farm/365) * 0.5 - FIXED"),
    
    # Column 8: Animal N Intake - HIGH UNCERTAINTY
    "8" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.15,  # 15% CV - feed intake varies
      description = "Animal N Intake (kg-N/animal/yr)"
    ),
    
    # Column 9: Animal P Intake - HIGH UNCERTAINTY
    "9" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.15,  # 15% CV
      description = "Animal P Intake (kg-P/animal/yr)"
    ),
    
    # Column 10: N in Animal Excretion - HIGH UNCERTAINTY
    "10" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.20,  # 20% CV - excretion has high variability
      description = "N in Animal Excretion (kg-N/animal/yr)"
    ),
    
    # Column 11: P in Animal Excretion - HIGH UNCERTAINTY
    "11" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.20,  # 20% CV
      description = "P in Animal Excretion (kg-P/animal/yr)"
    ),
    
    # Column 12: Ammonia Emission - VERY HIGH UNCERTAINTY
    "12" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.30,  # 30% CV - NH3 emissions are highly variable
      description = "Ammonia Emission (kg-N/animal/yr)"
    ),
    
    # Column 13: % Loss in Human Consumption - MODERATE UNCERTAINTY
    "13" = list(
      enabled = TRUE,
      type = "uniform",
      min_multiplier = 0.85,
      max_multiplier = 1.15,
      description = "% Loss in Human Consumption"
    ),
    
    # Column 14: Product Weight - LOW UNCERTAINTY
    "14" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.05,  # 5% CV - carcass weights relatively consistent
      description = "Product Weight (kg/animal)"
    ),
    
    # Column 15: Edible Proportion - FIXED (biological constant)
    "15" = list(
      enabled = FALSE,
      description = "Edible Proportion - FIXED"
    ),
    
    # Column 16: N in Edible Proportion - LOW UNCERTAINTY
    "16" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.05,  # 5% CV - meat composition relatively fixed
      description = "N in Edible Proportion"
    ),
    
    # Column 17: kcal/kg edible - FIXED (nutritional constant)
    "17" = list(
      enabled = FALSE,
      description = "kcal/kg edible - FIXED"
    ),
    
    # Column 18: g protein/kg edible - FIXED (nutritional constant)
    "18" = list(
      enabled = FALSE,
      description = "g protein/kg edible - FIXED"
    ),
    
    # Column 19: grain feed only - FIXED (binary flag)
    "19" = list(
      enabled = FALSE,
      description = "grain feed only? - FIXED"
    ),
    
    # Column 20: LOW N in Animal Excretion - HIGH UNCERTAINTY
    "20" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.25,  # 25% CV - low end estimate
      description = "LOW N in Animal Excretion (kg-N/animal/yr)"
    ),
    
    # Column 21: HIGH N in Animal Excretion - HIGH UNCERTAINTY  
    "21" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.25,  # 25% CV - high end estimate
      description = "HIGH N in Animal Excretion (kg-N/animal/yr)"
    ),
    
    # Column 22: LOW Ammonia Emission - VERY HIGH UNCERTAINTY
    "22" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.35,  # 35% CV
      description = "LOW Ammonia Emission (kg-N/animal/yr)"
    ),
    
    # Column 23: HIGH Ammonia Emission - VERY HIGH UNCERTAINTY
    "23" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.35,  # 35% CV
      description = "HIGH Ammonia Emission (kg-N/animal/yr)"
    )
  )
)

# CROP DATA ####
# cropdata - Crop attribute data (18 columns × crop types rows)
# Column-specific uncertainty control
# 
# Columns represent crop attributes:
# 1: prop_dm - Proportion dry matter
# 2: N/dm - Nitrogen per dry matter
# 3: P/dm - Phosphorus per dry matter
# 4: prop_human - Proportion to human consumption
# 5: prop_anim - Proportion to animal feed
# 6: prop_human_loss - Loss in human consumption
# 7: prop_anim_loss - Loss in animal consumption
# 8: kcal/kg crop - Energy content
# 9: g prot/kg crop - Protein content
# 10: grain? - Binary grain indicator
# 11: waste_1 - Waste fraction 1
# 12: waste_2 - Waste fraction 2
# 13: waste_3 - Waste fraction 3
# 14: N2O CO2eq/kg crop, 100yrGWP - Emission factor
# 15: CH4 CO2eq/kg crop, 100yrGWP - Emission factor
# 16: N2O CO2eq/kg crop, 20yrGWP - Emission factor
# 17: CH4 CO2eq/kg crop, 20yrGWP - Emission factor
# 18: m2 planted/kg - Land use intensity

mc_params$cropdata <- list(
  enabled = TRUE,
  type = "column_specific",
  description = "Crop attribute data uncertainty (column-specific)",
  
  columns = list(
    
    # Column 1: Proportion dry matter - LOW uncertainty
    "1" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.05,  # 5% CV - physical property, relatively stable
      description = "Proportion dry matter"
    ),
    
    # Column 2: N/dm - LOW to MODERATE uncertainty
    "2" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.08,  # 8% CV - varies with variety, soil, fertilization
      description = "Nitrogen per dry matter (N/dm)"
    ),
    
    # Column 3: P/dm - LOW to MODERATE uncertainty
    "3" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.08,  # 8% CV - similar to N
      description = "Phosphorus per dry matter (P/dm)"
    ),
    
    # Column 4: prop_human - MODERATE uncertainty
    "4" = list(
      enabled = TRUE,
      type = "uniform",
      min_multiplier = 0.90,
      max_multiplier = 1.10,
      description = "Proportion to human consumption"
    ),
    
    # Column 5: prop_anim - MODERATE uncertainty
    "5" = list(
      enabled = TRUE,
      type = "uniform",
      min_multiplier = 0.90,
      max_multiplier = 1.10,
      description = "Proportion to animal feed"
    ),
    
    # Column 6: prop_human_loss - MODERATE to HIGH uncertainty
    "6" = list(
      enabled = TRUE,
      type = "uniform",
      min_multiplier = 0.80,
      max_multiplier = 1.20,
      description = "Proportion lost in human consumption"
    ),
    
    # Column 7: prop_anim_loss - MODERATE uncertainty
    "7" = list(
      enabled = TRUE,
      type = "uniform",
      min_multiplier = 0.85,
      max_multiplier = 1.15,
      description = "Proportion lost in animal consumption"
    ),
    
    # Column 8: kcal/kg crop - FIXED (nutritional constant)
    "8" = list(
      enabled = FALSE,
      description = "Energy content (kcal/kg crop) - FIXED"
    ),
    
    # Column 9: g prot/kg crop - FIXED (nutritional constant)
    "9" = list(
      enabled = FALSE,
      description = "Protein content (g prot/kg crop) - FIXED"
    ),
    
    # Column 10: grain? - FIXED (binary indicator)
    "10" = list(
      enabled = FALSE,
      description = "Grain indicator (binary) - FIXED"
    ),
    
    # Column 11: waste_1 - MODERATE uncertainty
    "11" = list(
      enabled = TRUE,
      type = "uniform",
      min_multiplier = 0.85,
      max_multiplier = 1.15,
      description = "Waste fraction 1"
    ),
    
    # Column 12: waste_2 - MODERATE uncertainty
    "12" = list(
      enabled = TRUE,
      type = "uniform",
      min_multiplier = 0.85,
      max_multiplier = 1.15,
      description = "Waste fraction 2"
    ),
    
    # Column 13: waste_3 - MODERATE uncertainty
    "13" = list(
      enabled = TRUE,
      type = "uniform",
      min_multiplier = 0.85,
      max_multiplier = 1.15,
      description = "Waste fraction 3"
    ),
    
    # Column 14: N2O emission factor (100yr GWP) - HIGH uncertainty
    "14" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.30,  # 30% CV - emission factors highly variable
      description = "N2O CO2eq/kg crop, 100yrGWP"
    ),
    
    # Column 15: CH4 emission factor (100yr GWP) - HIGH uncertainty
    "15" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.30,  # 30% CV
      description = "CH4 CO2eq/kg crop, 100yrGWP"
    ),
    
    # Column 16: N2O emission factor (20yr GWP) - HIGH uncertainty
    "16" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.30,  # 30% CV
      description = "N2O CO2eq/kg crop, 20yrGWP"
    ),
    
    # Column 17: CH4 emission factor (20yr GWP) - HIGH uncertainty
    "17" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.30,  # 30% CV
      description = "CH4 CO2eq/kg crop, 20yrGWP"
    ),
    
    # Column 18: Land use intensity - MODERATE uncertainty
    "18" = list(
      enabled = TRUE,
      type = "normal",
      mean_multiplier = 1.0,
      cv = 0.12,  # 12% CV - varies with yield
      description = "m2 planted/kg - Land use intensity"
    )
  )
)
# HUMAN PROTEIN REQUIREMENTS ####
# prot_per_hmn_per_day - Protein requirement per human per day
mc_params$prot_per_hmn_per_day <- list(
  enabled = TRUE,
  type = "normal",
  mean_multiplier = 1.0,
  cv = 0.10,
  description = "Human protein requirement uncertainty"
)

# N PER PROTEIN UNIT ####
# N_per_prot_unit - Nitrogen per protein unit with fixation correction
mc_params$N_per_prot_unitfixNC <- list(
  enabled = TRUE,
  type = "triangular",
  min_multiplier = 0.95,
  mode_multiplier = 1.0,
  max_multiplier = 1.05,
  description = "N per protein unit uncertainty"
)

# ============================================================================
# SAMPLING FUNCTIONS
# ============================================================================

# Function to sample from specified distribution
sample_parameter <- function(baseline_value, param_def, iteration = NULL) {
  
  if(!param_def$enabled) {
    return(baseline_value)
  }
  
  # Handle zero or near-zero baseline values
  # For these, we'll just return the baseline (no sampling)
  if(abs(baseline_value) < 1e-10) {
    return(baseline_value)
  }
  
  if(param_def$type == "normal") {
    mean_val <- baseline_value * param_def$mean_multiplier
    sd_val <- mean_val * param_def$cv
    sampled <- rnorm(1, mean = mean_val, sd = sd_val)
    # Ensure positive values
    sampled <- pmax(sampled, 0)
    
  } else if(param_def$type == "uniform") {
    min_val <- baseline_value * param_def$min_multiplier
    max_val <- baseline_value * param_def$max_multiplier
    
    # Ensure min < max (handle negative baseline values)
    if(baseline_value < 0) {
      min_val <- baseline_value * param_def$max_multiplier
      max_val <- baseline_value * param_def$min_multiplier
    }
    
    sampled <- runif(1, min = min_val, max = max_val)
    
  } else if(param_def$type == "triangular") {
    require(EnvStats)
    
    min_val <- baseline_value * param_def$min_multiplier
    mode_val <- baseline_value * param_def$mode_multiplier
    max_val <- baseline_value * param_def$max_multiplier
    
    # Handle negative baseline values
    if(baseline_value < 0) {
      temp_min <- baseline_value * param_def$max_multiplier
      temp_max <- baseline_value * param_def$min_multiplier
      min_val <- temp_min
      max_val <- temp_max
      mode_val <- baseline_value * param_def$mode_multiplier
    }
    
    # Ensure strict ordering: min < mode < max
    # Add small epsilon to prevent equality
    epsilon <- abs(baseline_value) * 1e-6
    if(epsilon < 1e-10) epsilon <- 1e-10
    
    # Adjust if needed
    if(min_val >= mode_val) {
      mode_val <- min_val + epsilon
    }
    if(mode_val >= max_val) {
      max_val <- mode_val + epsilon
    }
    
    sampled <- rtri(1, min = min_val, max = max_val, mode = mode_val)
    
  } else if(param_def$type == "lognormal") {
    # Only use lognormal for positive values
    if(baseline_value <= 0) {
      return(baseline_value)  # Can't use lognormal for zero/negative
    }
    meanlog <- log(baseline_value * param_def$mean_multiplier)
    sdlog <- param_def$cv
    sampled <- rlnorm(1, meanlog = meanlog, sdlog = sdlog)
    
  } else {
    stop(paste("Unknown distribution type:", param_def$type))
  }
  
  return(sampled)
}

# Function to sample entire vector/matrix
sample_parameter_array <- function(baseline_array, param_def, iteration = NULL) {
  if(!param_def$enabled) {
    return(baseline_array)
  }
  
  # Check if this is a column-specific definition (for animdatadyn)
  if(!is.null(param_def$type) && param_def$type == "column_specific") {
    return(sample_parameter_by_column(baseline_array, param_def, iteration))
  }
  
  # Apply sampling to each element (standard approach)
  sampled_array <- baseline_array
  
  if(is.matrix(baseline_array) || is.array(baseline_array)) {
    for(i in 1:length(baseline_array)) {
      sampled_array[i] <- sample_parameter(baseline_array[i], param_def, iteration)
    }
  } else {
    # Vector
    for(i in seq_along(baseline_array)) {
      sampled_array[i] <- sample_parameter(baseline_array[i], param_def, iteration)
    }
  }
  
  return(sampled_array)
}

# Function to sample matrix by column with column-specific parameters
sample_parameter_by_column <- function(baseline_matrix, param_def, iteration = NULL) {
  
  if(!is.matrix(baseline_matrix)) {
    stop("sample_parameter_by_column requires a matrix input")
  }
  
  if(is.null(param_def$columns)) {
    stop("column_specific type requires 'columns' definition")
  }
  
  sampled_matrix <- baseline_matrix
  n_cols <- ncol(baseline_matrix)
  
  # Process each column
  for(col_idx in 1:n_cols) {
    
    col_key <- as.character(col_idx)
    
    # Check if this column has uncertainty defined
    if(col_key %in% names(param_def$columns)) {
      
      col_param <- param_def$columns[[col_key]]
      
      if(col_param$enabled) {
        # Sample this column
        for(row_idx in 1:nrow(baseline_matrix)) {
          baseline_val <- baseline_matrix[row_idx, col_idx]
          sampled_matrix[row_idx, col_idx] <- sample_parameter(baseline_val, col_param, iteration)
        }
      }
      # else: column disabled, keep baseline values
      
    } else {
      # Column not defined - keep baseline values
      # This allows flexibility to not define all columns
      next
    }
  }
  
  return(sampled_matrix)
}

# Print summary of uncertainty parameters
if(exists("run_monte_carlo") && run_monte_carlo) {
  cat("\n=== Monte Carlo Parameter Summary ===\n")
  for(param_name in names(mc_params)) {
    param <- mc_params[[param_name]]
    if(param$enabled) {
      cat(sprintf("%-25s: %s (Type: %s)\n", 
                  param_name, 
                  param$description, 
                  param$type))
    }
  }
  cat("=====================================\n\n")
}