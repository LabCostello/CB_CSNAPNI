# MC_Wrapper_v2.R
# Monte Carlo Simulation with Dynamic Variable Storage
# This version uses variable_list from write_outputs_CBW.R to determine what to store

if(print_tags == 1){
  print("ModelSubs/MC_Wrapper_v2.R")
}

# ============================================================================
# MONTE CARLO SIMULATION SETUP
# ============================================================================

if(run_monte_carlo) {
  
  cat("\n")
  cat("=", rep("=", 78), "=\n", sep="")
  cat("  MONTE CARLO UNCERTAINTY ANALYSIS - STARTING\n")
  cat("=", rep("=", 78), "=\n", sep="")
  cat(sprintf("  Number of iterations: %d\n", n_iterations))
  cat(sprintf("  Random seed: %d\n", mc_seed))
  cat("=", rep("=", 78), "=\n", sep="")
  cat("\n")
  
  # Set seed for reproducibility
  set.seed(mc_seed)
  
  # Load MC parameter definitions
  source("Config/MC_Parameters.R")
  
  # ============================================================================
  # DISCOVER OUTPUT VARIABLES FROM variable_list
  # ============================================================================
  
  cat("Discovering output variables from write_outputs_CBW.R...\n")
  
  # Run one iteration to establish baseline and get variable_list
  source("ModelSubs/LoadData_CBW.R")
  source("ModelSubs/manure_CBWv2.R")
  source("ModelSubs/NEEAshedsdyn.R")
  source("ModelSubs/hmn_reqs.R")
  source("ModelSubs/CropProd.R")
  source("ModelSubs/meat_alloc.R")
  source("ModelSubs/food_totals.R")
  source("ModelSubs/Cprodfertfix.R")
  source("ModelSubs/Mprodimpacts.R")
  source("ModelSubs/pernutrition.R")
  source("ModelSubs/biogas.R")
  source("ModelSubs/NPinputs_aggregate.R")
  source("ModelSubs/TN.R")
  source("ModelSubs/OutputN.R")
  
  # Get variable_list from write_outputs_CBW.R
  # We'll source it but not actually write files (set a flag)
  write_mc_outputs <- FALSE  # Flag to prevent file writing during MC
  source("ModelSubs/write_outputs_CBW.R")
  
  if(!exists("variable_list")) {
    stop("variable_list not found! Make sure write_outputs_CBW.R creates this variable.")
  }
  
  # ============================================================================
  # VALIDATE AND CLEAN variable_list
  # ============================================================================
  
  # Convert to character and remove invalid entries
  variable_list_original <- variable_list
  variable_list <- as.character(variable_list)
  variable_list <- variable_list[!is.na(variable_list)]
  variable_list <- variable_list[nchar(variable_list) > 0]
  variable_list <- unique(variable_list)  # Remove duplicates
  
  cat(sprintf("Found %d variables in variable_list (after cleaning)\n", length(variable_list)))
  cat("Variables to track:\n")
  for(i in 1:min(10, length(variable_list))) {
    cat(sprintf("  - %s\n", variable_list[i]))
  }
  if(length(variable_list) > 10) {
    cat(sprintf("  ... and %d more\n", length(variable_list) - 10))
  }
  cat("\n")
  
  # ============================================================================
  # SAVE BASELINE VALUES
  # ============================================================================
  
  cat("Saving baseline parameter values...\n")
  
  baseline_values <- list(
    cornuse = if(exists("cornuse")) cornuse else NULL,
    exports = if(exists("exports")) exports else NULL,
    cropdata = if(exists("cropdata")) cropdata else NULL,
    cropNtoanim = if(exists("cropNtoanim")) cropNtoanim else NULL,
    meatdata = if(exists("meatdata")) meatdata else NULL,
    Nfert = if(exists("Nfert")) Nfert else NULL,
    animdatadyn = animdatadyn,
    prot_per_hmn_per_day = if(exists("prot_per_hmn_per_day")) prot_per_hmn_per_day else NULL,
    N_per_prot_unitfixNC = if(exists("N_per_prot_unitfixNC")) N_per_prot_unitfixNC else NULL
  )
  
  cat("Baseline values saved.\n\n")
  
  # ============================================================================
  # INITIALIZE STORAGE FOR ALL VARIABLES IN variable_list
  # ============================================================================
  
  cat("Initializing storage for Monte Carlo results...\n")
  
  mc_results <- list()
  
  # For each variable in variable_list, create storage
  for(var_name in variable_list) {
    tryCatch({
      if(exists(var_name, inherits = TRUE)) {
        var_obj <- get(var_name, inherits = TRUE)
        
        # Determine storage structure based on object type
        if(is.matrix(var_obj) || is.array(var_obj)) {
          # For matrices/arrays, store as list of iterations
          # Each iteration gets the full matrix/array
          mc_results[[var_name]] <- vector("list", n_iterations)
        } else if(is.vector(var_obj) && length(var_obj) > 1) {
          # For vectors, create matrix: rows=iterations, cols=vector length
          mc_results[[var_name]] <- matrix(NA, nrow = n_iterations, ncol = length(var_obj))
        } else {
          # For scalars, create vector of length n_iterations
          mc_results[[var_name]] <- numeric(n_iterations)
        }
      } else {
        cat(sprintf("  Warning: Variable '%s' not found, skipping\n", var_name))
      }
    }, error = function(e) {
      cat(sprintf("  Error initializing '%s': %s\n", var_name, e$message))
    })
  }
  
  # Add iteration counter
  mc_results$iteration <- 1:n_iterations
  
  cat(sprintf("Initialized storage for %d variables\n\n", length(mc_results) - 1))
  
  # ============================================================================
  # PRE-FILTER VALID VARIABLES FOR FASTER ITERATION
  # ============================================================================
  
  # Only store variables that were successfully initialized
  valid_vars <- intersect(names(mc_results), variable_list)
  valid_vars <- valid_vars[valid_vars != "iteration"]  # Exclude iteration counter
  
  cat(sprintf("Will store %d variables per iteration\n", length(valid_vars)))
  cat("\n")
  
  # ============================================================================
  # MONTE CARLO ITERATION LOOP
  # ============================================================================
  
  cat("Starting Monte Carlo iterations...\n")
  progress_interval <- max(1, floor(n_iterations / 20))  # Update every 5%
  
  ptm_mc <- proc.time()
  
  for(iter in 1:n_iterations) {
    
    # Progress reporting
    if(iter %% progress_interval == 0) {
      cat(sprintf("  Iteration %d of %d (%.1f%%)...\n", 
                  iter, n_iterations, 100*iter/n_iterations))
    }
    
    # ------------------------------------------------------------------------
    # SAMPLE UNCERTAIN PARAMETERS
    # ------------------------------------------------------------------------
    
    # Sample each uncertain parameter based on its defined distribution
    
    if(!is.null(baseline_values$cornuse)) {
      cornuse <- sample_parameter_array(baseline_values$cornuse, mc_params$cornuse, iter)
    }
    
    if(!is.null(baseline_values$exports)) {
      exports <- sample_parameter_array(baseline_values$exports, mc_params$exports, iter)
    }
    
    if(!is.null(baseline_values$cropdata)) {
      cropdata <- sample_parameter_array(baseline_values$cropdata, mc_params$cropdata, iter)
    }
    
    if(!is.null(baseline_values$cropNtoanim)) {
      cropNtoanim <- sample_parameter_array(baseline_values$cropNtoanim, mc_params$cropNtoanim, iter)
    }
    
    if(!is.null(baseline_values$meatdata)) {
      meatdata <- sample_parameter_array(baseline_values$meatdata, mc_params$meatdata, iter)
    }
    
    if(!is.null(baseline_values$Nfert)) {
      Nfert <- sample_parameter_array(baseline_values$Nfert, mc_params$Nfert, iter)
    }
    
    animdatadyn <- sample_parameter_array(baseline_values$animdatadyn, mc_params$animdatadyn, iter)
    
    if(!is.null(baseline_values$prot_per_hmn_per_day)) {
      prot_per_hmn_per_day <- sample_parameter_array(baseline_values$prot_per_hmn_per_day, 
                                                     mc_params$prot_per_hmn_per_day, iter)
    }
    
    if(!is.null(baseline_values$N_per_prot_unitfixNC)) {
      N_per_prot_unitfixNC <- sample_parameter_array(baseline_values$N_per_prot_unitfixNC, 
                                                     mc_params$N_per_prot_unitfixNC, iter)
    }
    
    # ------------------------------------------------------------------------
    # RUN MODEL WITH SAMPLED PARAMETERS
    # ------------------------------------------------------------------------
    
    # Run all model components
    source("ModelSubs/manure_CBWv2.R")
    source("ModelSubs/NEEAshedsdyn.R")
    source("ModelSubs/hmn_reqs.R")
    source("ModelSubs/CropProd.R")
    source("ModelSubs/meat_alloc.R")
    source("ModelSubs/food_totals.R")
    source("ModelSubs/Cprodfertfix.R")
    source("ModelSubs/Mprodimpacts.R")
    source("ModelSubs/pernutrition.R")
    source("ModelSubs/biogas.R")
    source("ModelSubs/NPinputs_aggregate.R")
    source("ModelSubs/TN.R")
    source("ModelSubs/OutputN.R")
    
    # ------------------------------------------------------------------------
    # STORE RESULTS FROM THIS ITERATION (USING PRE-FILTERED valid_vars)
    # ------------------------------------------------------------------------
    
    for(var_name in valid_vars) {
      tryCatch({
        var_obj <- get(var_name, inherits = TRUE)
        
        # Store based on structure
        if(is.matrix(var_obj) || is.array(var_obj)) {
          mc_results[[var_name]][[iter]] <- var_obj
        } else if(is.vector(var_obj) && length(var_obj) > 1) {
          mc_results[[var_name]][iter, ] <- var_obj
        } else {
          mc_results[[var_name]][iter] <- var_obj
        }
      }, error = function(e) {
        # Silently skip errors during storage
      })
    }
    
    # Clean up memory
    if(iter %% 100 == 0) {
      gc(verbose = FALSE)
    }
    
  }  # End of MC iteration loop
  
  runtime_mc <- proc.time() - ptm_mc
  
  cat("\n")
  cat("=", rep("=", 78), "=\n", sep="")
  cat("  MONTE CARLO SIMULATION COMPLETED\n")
  cat("=", rep("=", 78), "=\n", sep="")
  cat(sprintf("  Total time: %.1f seconds (%.1f minutes)\n", 
              runtime_mc[3], runtime_mc[3]/60))
  cat(sprintf("  Average time per iteration: %.2f seconds\n", 
              runtime_mc[3]/n_iterations))
  cat("=", rep("=", 78), "=\n", sep="")
  cat("\n")
  
  # ============================================================================
  # POST-PROCESS AND ANALYZE RESULTS
  # ============================================================================
  
  cat("Analyzing Monte Carlo results...\n")
  
  # Calculate summary statistics for each output variable
  mc_summary <- list()
  
  # Function to calculate summary stats
  calc_summary_stats <- function(data, name) {
    
    if(is.list(data) && !is.data.frame(data)) {
      # List of matrices/arrays - calculate element-wise statistics
      # This is complex, so we'll store the raw list for now
      return(list(
        name = name,
        type = "list_of_arrays",
        note = "Complex structure - see raw results"
      ))
    } else if(is.matrix(data)) {
      # Matrix: rows=iterations, cols=variable dimensions
      return(list(
        name = name,
        type = "matrix",
        mean = colMeans(data, na.rm = TRUE),
        median = apply(data, 2, median, na.rm = TRUE),
        sd = apply(data, 2, sd, na.rm = TRUE),
        q025 = apply(data, 2, quantile, 0.025, na.rm = TRUE),
        q975 = apply(data, 2, quantile, 0.975, na.rm = TRUE),
        cv = apply(data, 2, sd, na.rm = TRUE) / colMeans(data, na.rm = TRUE)
      ))
    } else {
      # Vector
      return(list(
        name = name,
        type = "vector",
        mean = mean(data, na.rm = TRUE),
        median = median(data, na.rm = TRUE),
        sd = sd(data, na.rm = TRUE),
        q025 = quantile(data, 0.025, na.rm = TRUE),
        q975 = quantile(data, 0.975, na.rm = TRUE),
        cv = sd(data, na.rm = TRUE) / mean(data, na.rm = TRUE)
      ))
    }
  }
  
  # Calculate summaries for all stored variables
  cat("Calculating summary statistics for all variables...\n")
  for(var_name in names(mc_results)) {
    if(var_name != "iteration") {
      tryCatch({
        mc_summary[[var_name]] <- calc_summary_stats(mc_results[[var_name]], var_name)
      }, error = function(e) {
        cat(sprintf("  Warning: Could not calculate stats for '%s'\n", var_name))
      })
    }
  }
  
  cat(sprintf("Summary statistics calculated for %d variables\n", length(mc_summary)))
  
  # ============================================================================
  # SAVE RESULTS
  # ============================================================================
  
  cat("\nSaving Monte Carlo results...\n")
  
  # Create output directory if it doesn't exist
  if(!dir.exists("OutputFiles_CBW/MonteCarlo")) {
    dir.create("OutputFiles_CBW/MonteCarlo", recursive = TRUE)
  }
  
  # Save complete results object
  saveRDS(mc_results, 
          file = "OutputFiles_CBW/MonteCarlo/mc_results_complete.rds")
  cat("  Saved: mc_results_complete.rds\n")
  
  # Save summary statistics
  saveRDS(mc_summary, 
          file = "OutputFiles_CBW/MonteCarlo/mc_summary_all_vars.rds")
  cat("  Saved: mc_summary_all_vars.rds\n")
  
  # Save a summary report as text
  sink("OutputFiles_CBW/MonteCarlo/mc_summary_report.txt")
  cat("=" ,rep("=", 78), "=\n", sep="")
  cat("  MONTE CARLO UNCERTAINTY ANALYSIS - SUMMARY REPORT\n")
  cat("=" ,rep("=", 78), "=\n\n", sep="")
  cat(sprintf("Number of iterations: %d\n", n_iterations))
  cat(sprintf("Random seed: %d\n", mc_seed))
  cat(sprintf("Runtime: %.1f minutes\n", runtime_mc[3]/60))
  cat(sprintf("Variables tracked: %d\n\n", length(mc_summary)))
  
  cat("Summary Statistics (first 20 variables):\n")
  cat(rep("-", 78), "\n", sep="")
  
  count <- 0
  for(var_name in names(mc_summary)) {
    count <- count + 1
    if(count > 20) break
    
    summ <- mc_summary[[var_name]]
    cat(sprintf("\n%s (%s):\n", var_name, summ$type))
    
    if(summ$type == "vector") {
      cat(sprintf("  Mean:   %.4e\n", summ$mean))
      cat(sprintf("  Median: %.4e\n", summ$median))
      cat(sprintf("  SD:     %.4e\n", summ$sd))
      cat(sprintf("  95%% CI: [%.4e, %.4e]\n", summ$q025, summ$q975))
      cat(sprintf("  CV:     %.1f%%\n", summ$cv * 100))
    } else if(summ$type == "matrix") {
      cat(sprintf("  Dimensions: %d columns\n", length(summ$mean)))
      cat(sprintf("  Mean CV across columns: %.1f%%\n", mean(summ$cv, na.rm=TRUE) * 100))
    } else {
      cat(sprintf("  %s\n", summ$note))
    }
  }
  
  sink()
  cat("  Saved: mc_summary_report.txt\n")
  
  # ============================================================================
  # GENERATE UNCERTAINTY PLOTS
  # ============================================================================
  
  cat("\nGenerating uncertainty visualization plots...\n")
  
  tryCatch({
    source("ModelSubs/MC_Plots.R")
  }, error = function(e) {
    cat("Warning: Plot generation failed, continuing without plots...\n")
    cat(sprintf("Error: %s\n", e$message))
  })
  
  cat("\nMonte Carlo uncertainty analysis complete!\n")
  cat("Results saved to: OutputFiles_CBW/MonteCarlo/\n\n")
  
} else {
  
  # If Monte Carlo is disabled, run model normally (single run)
  cat("\nMonte Carlo disabled - running single baseline simulation\n")
  
}
