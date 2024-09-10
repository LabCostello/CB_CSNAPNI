#NAPItotals_vbmodel.r
#This script imports Excel data from the NAPI model
#For 
#Fert_P_App in kgP/km2/yr (columns 1, 2, 3)
#Food_Feed_P in kgP/km2/yr (column 4)
#Non_Food_Crops in kgP/km2/yr (columns 5, 6, 7)

if(print_tags == 1){
  print("CreateInputsSubs_CBW/NAPItotals_vbmodel.R")
}

read_file = 'RawData/NAPIdata_extract.xlsx'
import_yrs3 = c("1997","2002","2007","2012","2017") #data years to import
import_yrs = c("97","02","07","12","17","22")  #years for file names
read_sheet1 = 'Fert_P_App'
read_sheet2 = 'Food_Feed_P'
read_sheet3 = 'Non_Food_Crops'

data1 = as.matrix(read_excel(read_file, sheet = read_sheet1))
data2 = as.matrix(read_excel(read_file, sheet = read_sheet2))
data3 = as.matrix(read_excel(read_file, sheet = read_sheet3))

data_range1 = data1[3:3114,2:19] #exclude some cells
data_range2 = data2[3:3114,2:7] #exclude some cells
data_range3 = data3[3:3114,2:19] #exclude some cells

# Processing variable data to get only CBW
data_range1_ = data_range1[2:3112,]
data_range2_ = data_range2[2:3112,]
data_range3_ = data_range3[2:3112,]

data_range1_ = data_range1_[list_row_cbw,]
data_range2_ = data_range2_[list_row_cbw,]
data_range3_ = data_range3_[list_row_cbw,]

data_range1 = rbind(data_range1[1,],data_range1_) #updated considering only cbw counties
data_range2 = rbind(data_range2[1,],data_range2_) #updated considering only cbw counties
data_range3 = rbind(data_range3[1,],data_range3_) #updated considering only cbw counties

dummy1 <- data.frame(FIPS=CBW_lrs_shp$FIPS, Area = area)
dummy2 <- aggregate(dummy1$Area, by=list(dummy1$FIPS),FUN = sum)
colnames(dummy2) <- c("FIPS","Areacty")

dummy <- merge(dummy1,dummy2,by="FIPS")
dummy <- data.frame("FIPS" = dummy$FIPS, "Area_ratio" = dummy$Area/dummy$Areacty)

ncolumns=7
NAPIdatactydens=array(0,c(n_cnty,ncolumns,length(import_yrs)))
NAPIdatacty=array(0,c(n_cnty,ncolumns,length(import_yrs)))
NAPIdataws=array(0,c(n_ws_tbx,ncolumns,length(import_yrs)))
NAPIdatadensws=array(0,c(n_ws_tbx,ncolumns,length(import_yrs)))

#Fert_P_App in kgP/km2/yr (columns 1, 2, 3)
nfertcols=3
yr_cols=array(0,c(length(import_yrs3),nfertcols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range1[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range1[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nfertcols){
    NAPIdatactydens[,i,n]=data_range1[2:length(data_range2[,1]),yr_cols[n,i]]
    NAPIdatacty[,i,n]=as.numeric(NAPIdatactydens[,i,n])*areakm2_cnty
  }
}

#Food_Feed_P in kgP/km2/yr (column 4)
nffcols=1
yr_cols=array(0,c(length(import_yrs3),nffcols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range2[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range2[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nffcols){
    NAPIdatactydens[,nfertcols+i,n]=data_range2[2:length(data_range2[,1]),yr_cols[n,i]]
    NAPIdatacty[,nfertcols+i,n]=as.numeric(NAPIdatactydens[,nfertcols+i,n])*areakm2_cnty
  }
}

#Non_Food_Crops in kgN/km2/yr (columns 5, 6, 7)
nnfccols=3
yr_cols=array(0,c(length(import_yrs3),nnfccols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range3[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range3[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nnfccols){
    NAPIdatactydens[,nfertcols+nffcols+i,n]=data_range3[2:length(data_range3[,1]),yr_cols[n,i]]
    NAPIdatacty[,nfertcols+nffcols+i,n]=as.numeric(NAPIdatactydens[,nfertcols+nffcols+i,n])*areakm2_cnty
  }}

NAPIdatacty[,,6] <- NAPIdatacty[,,5]
  
for(n in 1:length(year_labels)){  
  #watershed crop production
  dummy_lrs <- 0
  dummy_cty <- cbind("FIPS"=FIPS, NAPIdatacty[,,n])
  dummy_lrs <- merge(dummy,dummy_cty,by="FIPS")
  
  dataws <-data.frame("Fert1"=dummy_lrs$Area_ratio*dummy_lrs$V2,
                      "Fert2"=dummy_lrs$Area_ratio*dummy_lrs$V3,
                      "Fert3"=dummy_lrs$Area_ratio*dummy_lrs$V4,
                      "FF"=dummy_lrs$Area_ratio*dummy_lrs$V5,
                      "Nonag1"=dummy_lrs$Area_ratio*dummy_lrs$V6,
                      "Nonag2"=dummy_lrs$Area_ratio*dummy_lrs$V7,
                      "Nonag3"=dummy_lrs$Area_ratio*dummy_lrs$V8)
  
  NAPIdataws[,,n] <- as.matrix(dataws)
  #NAPIdataws[,,n] = t(cnty_ws)%*%NAPIdatacty[,,n]
  for(c in 1:length(NAPIdataws[1,,n])){ #for each column, divide by watershed area to get density
    NAPIdatadensws[,c,n] = NAPIdataws[,c,n]/area
  }
  #write text files
  write_name =paste("InputFiles_CBW/NAPIdata",import_yrs[n],".txt",sep = "")
  write.table(NAPIdatadensws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}



#write key
write_name = paste("InputFileKeys/NAPIdata_key.txt")
NAPIdata_key = array(" ", c(n_ws_tbx+1,length(NAPIdatadensws[1,,1])+1))
NAPIdata_key[1,] = c(" ", data1[2,2],data1[2,8],data1[2,14],
                     data2[2,2],paste(data3[2,2], "nonfoodcropfert"),paste(data3[2,8], "fert"),paste(data3[2,14], "fert")) #column headings
NAPIdata_key[,1]=c("ws_num", 1:n_ws_tbx) #row headings
write.table(NAPIdata_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)