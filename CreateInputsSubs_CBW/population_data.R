#population_data.r
#This script pulls interpolated county populations (3111 counties from the continental US) for 1987-2012 from the NANI Accounting Tool V3.1.0

if(print_tags == 1){
  print("CreateInputsSubs_CBW/population_data.R")
}

read_file = 'RawData/NANI_Accounting_Tool_V3.1.0_People_Areas.xlsx'
import_yrs = c(1997,2002,2007,2012,2017)  #data years to import
read_sheet = 'People'
data = as.matrix(read_excel(read_file, sheet = read_sheet))

# Processing variable data to get only CBW
data[,32]= gsub("W","",as.character((data[,32]))) #taking out the W from the FIPS
list = as.vector(unlist(CB_counties[,1])) #list of FIPS
data = data[data[,32] %in% list,] #filtering

#need columns 33 (2012 & 2017), 34 (2007), 35 (2002), 36 (1997)
populationdenscty=data[,33:36]
populationcty=array(0,c(n_cnty,length(import_yrs)))
populationws=array(0,c(n_ws_tbx,length(import_yrs)))
populationdensws=array(0,c(n_ws_tbx,length(import_yrs)))
for(i in 1:length(import_yrs)){
  #if the year is closest to 2012, use 2012 population
  if(abs(import_yrs[i]-2012)<abs(import_yrs[i]-2007) && abs(import_yrs[i]-2012)<abs(import_yrs[i]-2002) && 
     abs(import_yrs[i]-2012)<abs(import_yrs[i]-1997) && abs(import_yrs[i]-2012)<abs(import_yrs[i]-1992) && 
     abs(import_yrs[i]-2012)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[,1])*areakm2_cnty
  }
  #if the year is closest to 2007, use 2007 population
  if(abs(import_yrs[i]-2007)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-2007)<abs(import_yrs[i]-2002) && 
     abs(import_yrs[i]-2007)<abs(import_yrs[i]-1997) && abs(import_yrs[i]-2007)<abs(import_yrs[i]-1992) && 
     abs(import_yrs[i]-2007)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[,2])*areakm2_cnty
  }
  #if the year is closest to 2002, use 2002 population
  if(abs(import_yrs[i]-2002)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-2002)<abs(import_yrs[i]-2007) && 
     abs(import_yrs[i]-2002)<abs(import_yrs[i]-1997) && abs(import_yrs[i]-2002)<abs(import_yrs[i]-1992) && 
     abs(import_yrs[i]-2007)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[,3])*areakm2_cnty
  }
  #if the year is closest to 1997, use 1997 population
  if(abs(import_yrs[i]-1997)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-1997)<abs(import_yrs[i]-2007) && 
     abs(import_yrs[i]-1997)<abs(import_yrs[i]-2002) && abs(import_yrs[i]-1997)<abs(import_yrs[i]-1992) && 
     abs(import_yrs[i]-1997)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[,4])*areakm2_cnty
  }
  #if the year is closest to 1992, use 1992 population
  if(abs(import_yrs[i]-1992)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-1992)<abs(import_yrs[i]-2007) && 
     abs(import_yrs[i]-1992)<abs(import_yrs[i]-2002) && abs(import_yrs[i]-1992)<abs(import_yrs[i]-1997) && 
     abs(import_yrs[i]-1992)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[,5])*areakm2_cnty
  }
  #if the year is closest to 1987, use 1987 population
  if(abs(import_yrs[i]-1987)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-1987)<abs(import_yrs[i]-2007) && 
     abs(import_yrs[i]-1987)<abs(import_yrs[i]-2002) && abs(import_yrs[i]-1987)<abs(import_yrs[i]-1997) && 
     abs(import_yrs[i]-1987)<abs(import_yrs[i]-1992)){
    populationcty[,i]=as.numeric(populationdenscty[,6])*areakm2_cnty
  }
  #if the year is 2017, use the 2012 pop + 5%
  if((import_yrs[i]-2017)==0){
    populationcty[,i]=as.numeric(populationdenscty[,1])*areakm2_cnty*1.05 #increase
  }
  populationws[,i]=populationcty[,i]%*%cnty_ws
  populationdensws[,i]=populationws[,i]/area
}

# write text files
write_name = "InputFiles_CBW/population.txt"
write.table(populationdensws, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = "InputFileKeys/population_key.txt"
population_key = array(" ", c(length(populationdensws)+1,length(import_yrs)+1))
population_key[1,]=c(" ",import_yrs) #column headings
population_key[,1]=c("ws_num", 1:length(populationdensws)) #row headings
write.table(population_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

# write text files
write_name = "InputFiles_CBW/population_cnty.txt"
write.table(populationcty, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = "InputFileKeys/populationcnty_key.txt"
populationcty_key = array(" ", c(length(populationcty)+1,length(import_yrs)+1))
populationcty_key[1,]=c(" ",import_yrs) #column headings
populationcty_key[,1]=c("cty_index", 1:length(populationcty)) #row headings
write.table(populationcty_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

