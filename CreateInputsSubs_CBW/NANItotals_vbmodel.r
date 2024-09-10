#NANItotals_vbmodel.r
#This script imports Excel data from the NANI model
#For 
#Atm_N_Dep in kg/km2/yr (column 1)
#Fert_N_App in kgN/km2/yr (columns 2, 3, 4)
#Ag_N_Fix in kgN/km2/yr (columns 5, 6, 7, 8, 9, 10, 11)
#Food_Feed_N in kgN/km2/yr (column 12)
#Non_Food_Crops in kgN/km2/yr (columns 13, 14, 15)

if(print_tags == 1){
  print("CreateInputsSubs_CBW/NANItotals_vbmodel.R")
}

read_file = 'RawData/NANIdata_extract.xlsx'
import_yrs3 = c("1997","2002","2007","2012","2017") #data years to import
import_yrs = c("97","02","07","12","17","22")  #years for file names
read_sheet1 = 'Atm_N_Dep'
read_sheet2 = 'Fert_N_App'
read_sheet3 = 'Ag_N_Fix'
read_sheet4 = 'Food_Feed_N'
read_sheet5 = 'Non_Food_Crops'

data1 = as.matrix(read_excel(read_file, sheet = read_sheet1))
data2 = as.matrix(read_excel(read_file, sheet = read_sheet2))
data3 = as.matrix(read_excel(read_file, sheet = read_sheet3))
data4 = as.matrix(read_excel(read_file, sheet = read_sheet4))
data5 = as.matrix(read_excel(read_file, sheet = read_sheet5))

data_range1 = data1[,2:5] #exclude some cells
data_range2 = data2[,2:19] #exclude some cells
data_range3 = data3[,2:43] #exclude some cells
data_range4 = data4[,2:7] #exclude some cells
data_range5 = data5[,2:19] #exclude some cells

# Processing variable data to get only CBW
data_range1_ = data_range1[4:3114,]
data_range2_ = data_range2[4:3114,]
data_range3_ = data_range3[4:3114,]
data_range4_ = data_range4[4:3114,]
data_range5_ = data_range5[4:3114,]

data_range1_ = data_range1_[list_row_cbw,]
data_range2_ = data_range2_[list_row_cbw,]
data_range3_ = data_range3_[list_row_cbw,]
data_range4_ = data_range4_[list_row_cbw,]
data_range5_ = data_range5_[list_row_cbw,]
# Adding the the years over the columns
data_range1 = rbind(data_range1[3,],data_range1_) #updated considering only cbw counties
data_range2 = rbind(data_range2[3,],data_range2_) #updated considering only cbw counties
data_range3 = rbind(data_range3[3,],data_range3_) #updated considering only cbw counties
data_range4 = rbind(data_range4[3,],data_range4_) #updated considering only cbw counties
data_range5 = rbind(data_range5[3,],data_range5_) #updated considering only cbw counties
# Preparing dataframe to convert county data to ws data

dummy1 <- data.frame(FIPS=CBW_lrs_shp$FIPS, Area = area)
dummy2 <- aggregate(dummy1$Area, by=list(dummy1$FIPS),FUN = sum)
colnames(dummy2) <- c("FIPS","Areacty")

dummy <- merge(dummy1,dummy2,by="FIPS")
dummy <- data.frame("FIPS" = dummy$FIPS, "Area_ratio" = dummy$Area/dummy$Areacty)

ncolumns=15
NANIdatactydens=array(0,c(n_cnty,ncolumns,length(import_yrs3)+1))
NANIdatacty=array(0,c(n_cnty,ncolumns,length(import_yrs3)+1))
NANIdataws=array(0,c(n_ws_tbx,ncolumns,length(import_yrs3)+1))
NANIdatadensws=array(0,c(n_ws_tbx,ncolumns,length(import_yrs3)+1))

#Atm_N_Dep in kg/km2/yr (column 1)
ndepcols=1
index02=4
index06=3
index07=2
index08=1
for(n in 1:length(import_yrs3)){
  for(i in 1:ndepcols){
    if(as.double(import_yrs3[n])<2004){ #use the 2002 CMAQ
      NANIdatactydens[,ndepcols,n]=data_range1[2:198,index02]
    }
    if(as.double(import_yrs3[n])>2004 && as.double(import_yrs3[n])<2007){ #use the 2006 CMAQ
      NANIdatactydens[,ndepcols,n]=data_range1[2:198,index06]
    }
    if(as.double(import_yrs3[n])==2007){ #use the 2007 CMAQ
      NANIdatactydens[,ndepcols,n]=data_range1[2:198,index07]
    }
    if(as.double(import_yrs3[n])>2007){ #use the 2008 CMAQ
      NANIdatactydens[,ndepcols,n]=data_range1[2:198,index08]
    }
    NANIdatacty[,ndepcols,n]=as.numeric(NANIdatactydens[,ndepcols,n])*areakm2_cnty
  }
}

# Changing NANI Atm deposition for 2017
atmdep2017 <- read.csv("RawData/CMAQ_data/CMAQv5.0.2_2017/atmdep2017.csv")
atmdep2017cty <- aggregate(atmdep2017$AtmDep2017, by=list(atmdep2017$FIPS),FUN = sum)
NANIdatacty[,1,5] <- atmdep2017cty$x
NANIdatactydens[,1,5] <- NANIdatacty[,1,5]/areakm2_cnty

# Changing NANI Atm deposition for 2022
atmdep2022 <- read.csv("RawData/CMAQ_data/CMAQv5.0.2_2022/atmdep2019.csv")
atmdep2022cty <- aggregate(atmdep2022$AtmDep2019, by=list(atmdep2022$FIPS),FUN = sum)
NANIdatacty[,1,6] <- atmdep2022cty$x
NANIdatactydens[,1,6] <- NANIdatacty[,1,5]/areakm2_cnty

#
#Fert_N_App in kgN/km2/yr (columns 2, 3, 4)
nfertcols=3
yr_cols=array(0,c(length(import_yrs3),nfertcols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range2[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range2[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nfertcols){
    NANIdatactydens[,ndepcols+i,n]=data_range2[2:length(data_range2[,1]),yr_cols[n,i]]
    NANIdatacty[,ndepcols+i,n]=as.numeric(NANIdatactydens[,ndepcols+i,n])*areakm2_cnty
  }
}

#Ag_N_Fix in kgN/km2/yr (columns 5, 6, 7, 8, 9, 10, 11)
nfixcols=7
yr_cols=array(0,c(length(import_yrs3),nfixcols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range3[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range3[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nfixcols){
    NANIdatactydens[,ndepcols+nfertcols+i,n]=data_range3[2:length(data_range3[,1]),yr_cols[n,i]]
    NANIdatacty[,ndepcols+nfertcols+i,n]=as.numeric(NANIdatactydens[,ndepcols+nfertcols+i,n])*areakm2_cnty
  }
}

#Food_Feed_N in kgN/km2/yr (column 12)
nffcols=1
yr_cols=array(0,c(length(import_yrs3),nffcols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range4[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range4[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nffcols){
    NANIdatactydens[,ndepcols+nfertcols+nfixcols+i,n]=data_range4[2:length(data_range4[,1]),yr_cols[n,i]]
    NANIdatacty[,ndepcols+nfertcols+nfixcols+i,n]=as.numeric(NANIdatactydens[,ndepcols+nfertcols+nfixcols+i,n])*areakm2_cnty
  }
}

#Non_Food_Crops in kgN/km2/yr (columns 13, 14, 15)
nnfccols=3
yr_cols=array(0,c(length(import_yrs3),nnfccols))

for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range5[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range5[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nnfccols){
    NANIdatactydens[,ndepcols+nfertcols+nfixcols+nffcols+i,n]=data_range5[2:length(data_range5[,1]),yr_cols[n,i]]
    NANIdatacty[,ndepcols+nfertcols+nfixcols+nffcols+i,n]=as.numeric(NANIdatactydens[,ndepcols+nfertcols+nfixcols+nffcols+i,n])*areakm2_cnty
  }}

NANIdatacty[,2:15,6] <- NANIdatacty[,2:15,5]

for(n in 1:length(year_labels)){
  #watershed crop production
  dummy_lrs <- 0
  dummy_cty <- cbind("FIPS"=FIPS, NANIdatacty[,,n])
  dummy_lrs <- merge(dummy,dummy_cty,by="FIPS")
  
  dataws <-data.frame("Atm_N_Dep"=dummy_lrs$Area_ratio*dummy_lrs$V2,
                      "Fert1"=dummy_lrs$Area_ratio*dummy_lrs$V3,
                      "Fert2"=dummy_lrs$Area_ratio*dummy_lrs$V4,
                      "Fert3"=dummy_lrs$Area_ratio*dummy_lrs$V5,
                      "Agfix1"=dummy_lrs$Area_ratio*dummy_lrs$V6,
                      "Agfix2"=dummy_lrs$Area_ratio*dummy_lrs$V7,
                      "Agfix3"=dummy_lrs$Area_ratio*dummy_lrs$V8,
                      "Agfix4"=dummy_lrs$Area_ratio*dummy_lrs$V9,
                      "Agfix5"=dummy_lrs$Area_ratio*dummy_lrs$V10,
                      "Agfix6"=dummy_lrs$Area_ratio*dummy_lrs$V11,
                      "Agfix7"=dummy_lrs$Area_ratio*dummy_lrs$V12,
                      "FF1"=dummy_lrs$Area_ratio*dummy_lrs$V13,
                      "NFC1"=dummy_lrs$Area_ratio*dummy_lrs$V14,
                      "NFC2"=dummy_lrs$Area_ratio*dummy_lrs$V15,
                      "NFC3"=dummy_lrs$Area_ratio*dummy_lrs$V16)
    
  NANIdataws[,,n] <- as.matrix(dataws)
  #NANIdataws[,,n] = t(cnty_ws)%*%NANIdatacty[,,n]
  for(c in 1:length(NANIdataws[1,,n])){ #for each column, divide by watershed area to get density
    NANIdatadensws[,c,n] = NANIdataws[,c,n]/area
  }
  #write text files
  write_name =paste("InputFiles_CBW/NANIdata",import_yrs[n],".txt",sep = "")
  write.table(NANIdatadensws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}
#write key
write_name = paste("InputFileKeys/NANIdata_key.txt")
NANIdata_key = array(" ", c(n_ws_tbx+1,length(NANIdatadensws[1,,1])+1))
NANIdata_key[1,] = c(" ", data1[2,2],data2[2,2],data2[2,8],data2[2,14],
                     paste(data3[2,2], "fix"),paste(data3[2,8], "fix"),paste(data3[2,14], "fix"),paste(data3[2,20], "fix"),
                     paste(data3[2,26], "fix"),paste(data3[2,32], "fix"),paste(data3[2,38], "fix"),
                     data4[2,2],paste(data5[2,2], "nonfoodcropfert"),paste(data5[2,8], "fert"),paste(data5[2,14], "fert")) #column headings
NANIdata_key[,1]=c("ws_num", 1:n_ws_tbx) #row headings
write.table(NANIdata_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

