#CSNAPNIv2.R
ptm <- proc.time()
#specify the working directory as the location of this file

this.dir <- getwd()
setwd(this.dir)

print("Welcome to...")
cat(readLines("ascii_art.txt",warn=FALSE), sep = "\n")

#Choose from: 'baseline', 'grass_pct', 'grass_profitability', 'cover_crop', 'double_crop',
#             'grass_cover_combo', 'grass_double_combo','biogas_grass_pct_double_combo','biogas_grass_profit_double_combo','biogas_grass_profit_cover_combo'

# Set the current scenario
CURRENT_SCENARIO <- "baseline"  # Change this to switch scenarios

#settings
source("Config/Settings.R")

# CSNAPNI for Chesapeake Bay
print("CSNAPNI for CB is running")

#pull data from raw data files
source("ModelSubs/CreateInputs_CBW.R")

if(run_monte_carlo) {
  # Monte Carlo mode: Run model multiple times with parameter uncertainty
  source("ModelSubs/MC_Wrapper.R")
  
} else {

  ##Load data
  source("ModelSubs/LoadData_CBW.R")
    
  ##Calculate recoverable manure for NANI/NAPI Toolbox Watersheds
  source("ModelSubs/manure_CBWv2.R")
  
  ##Sort crop, animal, and manure production, NANI and NAPI data, human populations, and areas into NEEA watersheds
  source("ModelSubs/NEEAshedsdyn.R")
  
  ## DETERMINE HUMAN N and P REQUIREMENTS
  source("ModelSubs/hmn_reqs.R")
  
  ## CALCULATE CROP PRODUCTION SUBROUTINE
  source("ModelSubs/CropProd.R")
  
  ## CALCULATE MEAT PRODUCTION
  source("ModelSubs/meat_alloc.R") #creates kgmeat
  
  ## ANIMAL AND CROPS AS FOOD
  source("ModelSubs/food_totals.R")
  
  ## CROP, ETHANOL, and CROPRODUCT FERTILIZER AND FIXATION
  source("ModelSubs/Cprodfertfix.R")
  
  ## MEAT FERT, FIX, NH3, N2O, CH4, LU, and MANURE
  source("ModelSubs/Mprodimpacts.R")
  
  ## IMPACTS PER NUTRITIONAL UNIT
  source("ModelSubs/pernutrition.R")
  
  ## BIOGAS PRODUCTION
  source("ModelSubs/biogas.R")
  
  ## calculates domestic meat availability given trade, also calculates N lost due to food waste SUBROUTINE
  #Temporarily commented-out - *this code has not been converted to R and updated for additional years yet*
  #meattrade
  
  ## BUILD FINAL NANI/NAPI MATRICES
  source("ModelSubs/NPinputs_aggregate.R")
  
  ## TOTAL NITROGEN CALCULATION
  source("ModelSubs/TN.R")
  
  ## TMDL
  #source("ModelSubs/TMDL.R")
  
  ## OUTPUTS
  source("ModelSubs/OutputN.R")
  
  ## WRITE OUTPUT DATA TO TEXT FILES
  source("ModelSubs/write_outputs_CBW.R")
}

## UNCERTAINTY CALCULATION
#source("ModelSubs/mcs_uncertainty.R")

## GRAPHS
source("ModelSubs/GraphsModel.R")

# Stop the clock
runtime = proc.time() - ptm
runtime_msg = paste("input files created in", runtime, "seconds", sep = " ")
print(runtime_msg[3])

