#This script pulls county areas (3111 counties from the continental US) from the NANI Accounting Tool V3.1.0
if(print_tags == 1){
  print("CreateInputsSubs_CBW/cntyarea_data.R")
}

read_file = "InputFiles_CBW/cnty_da_cdl.xlsx"
read_sheet = "CB_Counties"  #several sheets contain the area (km2) data, so I just picked one
data = as.matrix(read_excel(read_file, sheet = read_sheet))
#Get count
areakm2_cnty = as.numeric(data[1:n_cnty,9]) #clipped area 202 counties for CBW
