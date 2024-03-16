# Settings.R
## This file contains all user-adjustable input parameters for CBW-CSNAPNI.

#Print Troubleshooting Tags
print_tags = 0
if(print_tags == 1){
  print("Config/Settings.R")
}

#Loading all the packages used in the toolbox ####
packagesused <- c('data.table','dplyr','EnvStats','fitdistrplus','knitr','ggplot2','patchwork','plotly','readxl','rnassqs','sf','stringr','tidyverse')
if (length(packagesused[!(packagesused %in% installed.packages()[,"Package"])]) != 0 ){
  library(pacman)
  p_install(data.table,dplyr,EnvStats,fitdistrplus,knitr,ggplot2,plotly,readxl,rnassqs,stringr,tidyverse)
}

#Regenerate input files? 0=no, 1=yes ####
get_new_data=1
print(paste("Input files regenerated (0=no, 1=yes): ", get_new_data),quote=FALSE)

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

#data years to load
run_yrs = c("97","02","07","12","17") #last two digits of data years to import
nyrs = length(run_yrs)
print(paste("Running for the following year(s): "), quote=FALSE)
print(paste(run_yrs), quote=FALSE)

ws_name = "Miss_Atch"
ws = c(22,25) ##Missippi River and Atchafalaya Watersheds
#ws = c(52:60) ##Chesapeake Bay Watersheds (Patuxent,Potomac,Rappahannock,
#York,James,Chester,Choptank,Tangier/Pokomoke,Chesapeake Bay Mainstream)
ws <- 919:933 ## Lancaster LRS


# Fertilizer assumptions
fertassump=3
if(fertassump==1){
  print(paste("Using national CSNAPNI fertilizer values."),quote=FALSE)
}else if(fertassump==2){
  print(paste("Using NASS Survey fertilizer data."),quote=FALSE)
}else if(fertassump==3){
  print(paste("Using PSU Agronomy Guide approach."),quote=FALSE)
}

# Grass scenario ####
# Conditions for this scenario 
grass_scenario <-  0 # (Y = 1, N = 0)
grass_fert_scenario <- 0 # (Y = 1, N = 0)

#[Options] "Corn","Sorghum","Soybeans","Peanuts","Barley","Wheat","Rye","Oats","Alfalfa","Other.Hay.Non.Alfalfa","Potatoes","Grass.Pasture"  
crot1 <- c("Corn","Corn","Soybeans","Other.Hay.Non.Alfalfa","Other.Hay.Non.Alfalfa")
#[Options] "corn for grain","corn for silage","wheat","oats","barley","sorghum for grain","sorghum for silage","potatoes","rye",
#"alfalfa hay","other hay","soybeans","cropland pasture","noncropland pasture","rice","peanuts","grass","CGF","CGM","DGS"    
crot2 <- c("corn for grain","corn for grain","soybeans","other hay","other hay")

Crotation <- c(crot1[1],crot2[1],crot1[2],crot2[2],crot1[3],crot2[3],crot1[4],crot2[4],crot1[5],crot2[5]) # Don't change this one (updated automatically)
# Constants and Variables
land_use_grass <- 0.10 # Reference: Zhou et al.(2014)
grass_yield_no_fert <- 9.9 # Reference: Woodbury et al.(2018) Unit: Mg/ha (DM)
# grass_yield_fert <- 19.7 # Reference: Kering et al.(2012) Unit: Mg/ha

# Winter crop scenario ####

