#CSNAPNIv1.R

#specify the working directory as the location of this file

this.dir <- getwd()
setwd(this.dir)

#settings
source("Config/Settings.R")

# CSNAPNI for Chesapeake Bay
if(filter_region == 1){
  print("CSNAPNI for CB is running")
   #pull data from raw data files
    if (get_new_data == 1){
    source("ModelSubs/CreateInputs_CBW.R")
  }
  source("ModelSubs/LoadData_CBW.R")
  
  ##Calculate recoverable manure for NANI/NAPI Toolbox Watersheds
  source("ModelSubs/manure_CBW.R")

# CSNAPNI for whole US    
} else if(filter_region == 0){
  #pull data from raw data files
  print("CSNAPNI for whole US is running")
  if (get_new_data == 1){
    source("ModelSubs/CreateInputs.R")
  }
  source("ModelSubs/LoadData.R")
  
  ##Calculate recoverable manure for NANI/NAPI Toolbox Watersheds
  source("ModelSubs/manure.R")
}
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

## calculates domestic meat availability given trade, also calculates N lost due to food waste SUBROUTINE
#Temporarily commented-out - *this code has not been converted to R and updated for additional years yet*
#meattrade

## BUILD FINAL NANI/NAPI MATRICES
source("ModelSubs/NPinputs_aggregate.R")

## WRITE OUTPUT DATA TO TEXT FILES
if(filter_region == 1){
  source("ModelSubs/write_outputs_CBW.R")
  }else if(filter_region == 0){
  source("ModelSubs/write_outputs.R")}

