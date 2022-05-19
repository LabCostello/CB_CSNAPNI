#This script pulls county areas (3111 counties from the continental US) from the NANI Accounting Tool V3.1.0
if(print_tags == 1){
  print("CreateInputsSubs_CBW/cntyarea_data.R")
}

read_file = "InputFiles_CBW/CB_counties.xlsx"
read_sheet = "CB_counties"  #several sheets contain the area (km2) data, so I just picked one
data = as.matrix(read_excel(read_file, sheet = read_sheet))
#Get county areas
areakm2_cnty = as.numeric(data[1:203,9]) #clipped area 203 counties for CBW
