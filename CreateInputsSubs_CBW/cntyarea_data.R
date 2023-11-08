#This script pulls county areas (3111 counties from the continental US) from the NANI Accounting Tool V3.1.0
if(print_tags == 1){
  print("CreateInputsSubs_CBW/cntyarea_data.R")
}

data <- data.frame(FIPS=CBW_lrs_shp$FIPS, Area = area)
data <- aggregate(data$Area, by=list(data$FIPS),FUN = sum)

#Get count
areakm2_cnty = as.numeric(data[1:n_cnty,2]) # 197 counties within CBW
