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
list = FIPS #list of FIPS
data = data[data[,32] %in% list,] #filtering

# Population in 2017
#### Population fix for 2017 ####
census_pop <- read.csv("RawData/new_pop.csv")

new_pop <- merge(data.frame(FIPS=FIPS, Area = areakm2_cnty), census_pop, by = "FIPS")
new_pop$density17 <- new_pop$POPESTIMATE2017/new_pop$Area 
new_pop$density12 <- new_pop$POPESTIMATE2012/new_pop$Area 
new_pop$realdens12 <- as.numeric(populationdenscty[,1])


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
  #if the year is 2017, use the new_pop that has density calculated based on 2017 population estimations
  if((import_yrs[i]-2012)==0){
    populationcty[,i]=as.numeric(new_pop$POPESTIMATE2012/new_pop$Area)*areakm2_cnty
  }
  if((import_yrs[i]-2017)==0){
    populationcty[,i]=as.numeric(new_pop$POPESTIMATE2017/new_pop$Area)*areakm2_cnty
  }
  populationws[,i]=populationcty[,i]%*%cnty_ws
  populationdensws[,i]=populationws[,i]/area
}

dummy <- merge(cbind(FIPS,populationcty),percent_developed_in_cbw,by="FIPS")
dummy <- dummy[,2:6]*c(dummy[,9],dummy[,9],dummy[,9],dummy[,10],dummy[,11])
colnames(dummy) <- c("V1","V2","V3","V4","V5")
populationws <- as.matrix(dummy)
for (i in 1:length(import_yrs)) {populationdensws[,i] <- populationws[,i]/area}

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