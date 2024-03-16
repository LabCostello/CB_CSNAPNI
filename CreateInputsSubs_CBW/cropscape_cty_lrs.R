# Functions ####
read_and_preprocess_cdl <- function(file_path, code_class_cdl, lrs_shp) {
  lrs_cdl <- read_excel(file_path)
  
  # Drop column value_0
  lrs_cdl <- lrs_cdl[,-3]
  
  # Change column names
  old_colnames <- colnames(lrs_cdl[,-c(1,2)])
  index_new_name <- match(old_colnames, code_class_cdl$CODE)
  column_names <- c("OBJECTID", "LNDRVRSEG", unlist(code_class_cdl[index_new_name,2]))
  colnames(lrs_cdl) <- column_names
  
  # Add FIPS and Region columns
  lrs_cdl
  lrs_cdl <- cbind(lrs_cdl, "FIPS" = lrs_shp$FIPS, "Region" = lrs_shp$Region)
  
  # Filter data for Chesapeake Bay Watershed
  cbw_lrs_cdl <- lrs_cdl[lrs_cdl$Region == "Chesapeake Bay Watershed",]
  cbw_lrs_cdl <- cbw_lrs_cdl[order(cbw_lrs_cdl$FIPS),]
  
  return(list(lrs_cdl,cbw_lrs_cdl))
}
#Variables
code_class_cdl <- read_excel("RawData/code_class_cdl.xlsx")
table2007 <- "RawData/Table_LRS_CDL_2008.xlsx"
table2012 <- "RawData/Table_LRS_CDL_2012.xlsx"
table2017 <- "RawData/Table_LRS_CDL_2017.xlsx"
developedland <- c("Developed/Open Space","Developed/Low Intensity","Developed/Med Intensity","Developed/High Intensity")

### < 2007 ################################################################################################################################################
# It will be used for the years <2007 (it consists on Cropscape CDL in the year 2008. The reason to use 2008 is because for the previous years the data were not available)
lrs_cdl_2007 <- as.data.frame(read_and_preprocess_cdl(table2007,code_class_cdl,lrs_shp)[1],check.names=FALSE)
cbw_lrs_cdl_2007 <- as.data.frame(read_and_preprocess_cdl(table2007,code_class_cdl,lrs_shp)[2],check.names=FALSE)
cbw_lrs_cdl_2007 <- cbw_lrs_cdl_2007 %>% arrange(FIPS,LNDRVRSEG)

### Population Allocation ##
# This section corresponds to allocation of population to the land river segments. The variable percent_developed_in_cbw will be used in the population_data.R
# The variable has to be restricted to cbw only because the populationcty is already calculated for the counties inside of cbw, which means just the 1925 LRSs
cdl_developed_aggregated_2007 <- data.frame("OBJECTID"=cbw_lrs_cdl_2007$OBJECTID,
                                            "LNDRVRSEG"=cbw_lrs_cdl_2007$LNDRVRSEG,
                                            "FIPS"=cbw_lrs_cdl_2007$FIPS,
                                            "REGION"=cbw_lrs_cdl_2007$Region,
                                            "DEVELOPED2007"=rowSums(cbw_lrs_cdl_2007[,developedland]))

cdl_developed_aggregated_2007_fips <- cdl_developed_aggregated_2007 %>% group_by(FIPS) %>% summarize(across(DEVELOPED2007,sum))

dummy <- merge(cdl_developed_aggregated_2007,cdl_developed_aggregated_2007_fips,by="FIPS")

dummy <-  cbind(dummy,"percentage" = dummy$DEVELOPED2007.x/dummy$DEVELOPED2007.y)

percent_developed_in_cbw <- dummy[,-c(4,5,6)]
percent_developed_in_cbw2007 <- percent_developed_in_cbw

### Animal Allocation ###
# This section corresponds to allocation of animals to the land river segments. That should be done using the cropland as a proxy (one of the Booth approach).
allnames <- names(lrs_cdl_2007)
noncropnames <- c("Open Water","Developed/Open Space","Developed/Low Intensity","Developed/Med Intensity","Developed/High Intensity","Barren",
                  "Deciduous Forest","Evergreen Forest","Mixed Forest","Shrubland","Woody Wetlands","Herbaceous Wetlands")
cropnames2007 <- allnames[!allnames %in% c(noncropnames,"OBJECTID","LNDRVRSEG","FIPS","Region")]

cdl_cropland_aggregated <- data.frame("OBJECTID"=lrs_cdl_2007$OBJECTID,
                                      "LNDRVRSEG"=lrs_cdl_2007$LNDRVRSEG,
                                      "FIPS"=lrs_cdl_2007$FIPS,
                                      "REGION"=lrs_cdl_2007$Region,
                                      "CROPLAND"=rowSums(lrs_cdl_2007[,cropnames2007]),
                                      "NONCROP"=rowSums(lrs_cdl_2007[,noncropnames]),
                                      "CORN"=lrs_cdl_2007[,"Corn"])

cdl_cropland_aggregated_fips <- cdl_cropland_aggregated %>% group_by(FIPS) %>% summarize(across(CROPLAND,sum))
cdl_corn_aggregated_fips <- cdl_cropland_aggregated %>% group_by(FIPS) %>% summarize(across(CORN,sum))

dummy <- merge(cdl_cropland_aggregated,cdl_cropland_aggregated_fips,by="FIPS")

dummy <-  cbind(dummy,"percentage2007" = dummy$CROPLAND.x/dummy$CROPLAND.y)
percent_crop_lrs <- dummy[,-c(5,6,7)]
percent_crop_in_cbw <- dummy[dummy$REGION == "Chesapeake Bay Watershed",]
percent_crop_in_cbw <- percent_crop_in_cbw[,-c(4,5,6,7,8)]
percent_crop_in_cbw2007 <- percent_crop_in_cbw

dummy_corn <- merge(cdl_cropland_aggregated,cdl_corn_aggregated_fips,by="FIPS")

dummy_corn <-  cbind(dummy_corn,"percentage.corn2007" = dummy_corn$CORN.x/dummy_corn$CORN.y)
percent_corn_lrs <- dummy_corn[,-c(5,6,7,8)]
percent_corn_in_cbw <- percent_corn_lrs[percent_corn_lrs$REGION == "Chesapeake Bay Watershed",]
percent_corn_in_cbw <- percent_corn_in_cbw[,-c(4)]
percent_corn_in_cbw[is.na(percent_corn_in_cbw)] <- 0
percent_corn_in_cbw2007 <- percent_corn_in_cbw

### Crop Allocation ##
# This section corresponds to allocation of crop to the land river segments. 
Corn_pixels <- rowSums(lrs_cdl_2007[,grep("Corn",names(lrs_cdl_2007))])
Soybean_pixels <- rowSums(lrs_cdl_2007[,grep("Soybean",names(lrs_cdl_2007))])
Sorghum_pixels <- rowSums(lrs_cdl_2007[,grep("Sorghum",names(lrs_cdl_2007))])
Wheat_pixels <- rowSums(lrs_cdl_2007[,grep("Wheat",names(lrs_cdl_2007))])
Oats_pixels <- rowSums(lrs_cdl_2007[,grep("Oats",names(lrs_cdl_2007))])
Barley_pixels <- rowSums(lrs_cdl_2007[,grep("Barley",names(lrs_cdl_2007))])

lrs_cdl_crop_pxl <- data.frame("OBJECTID"=lrs_cdl_2007$OBJECTID,
                               "LNDRVRSEG"=lrs_cdl_2007$LNDRVRSEG,
                               "FIPS"=lrs_cdl_2007$FIPS,
                               "REGION"=lrs_cdl_2007$Region,
                               "Corn" = Corn_pixels,
                               "Sorghum" = Sorghum_pixels,
                               "Soybeans" = Soybean_pixels,
                               "Peanuts" = lrs_cdl_2007[,8],
                               "Barley" = Barley_pixels,
                               "Wheat" = Wheat_pixels,
                               "Rye" = lrs_cdl_2007[,15],
                               "Oats" = Oats_pixels,
                               "Alfalfa" = lrs_cdl_2007[,21],
                               "Other Hay/Non Alfalfa" = lrs_cdl_2007[,22],
                               "Potatoes" = lrs_cdl_2007[,26],
                               "Grass/Pasture" = lrs_cdl_2007[,58]) 

lrs_cdl_crop_pxl_groupby_cty <- lrs_cdl_crop_pxl %>% group_by(lrs_cdl_crop_pxl$FIPS) %>% summarize(across(colnames(lrs_cdl_crop_pxl[,5:16]),sum))
colnames(lrs_cdl_crop_pxl_groupby_cty)[1] <- "FIPS"

lrs_cdl_crop_pxl_merged <- merge(lrs_cdl_crop_pxl, lrs_cdl_crop_pxl_groupby_cty, by="FIPS")

lrs_cdl_crop_pxl_merged[,29:40] <- lrs_cdl_crop_pxl_merged[,5:16]/lrs_cdl_crop_pxl_merged[,17:28]

lrs_cdl_percent <-  data.frame("FIPS"=lrs_cdl_crop_pxl_merged$FIPS,
                               "LNDRVRSEG"=lrs_cdl_crop_pxl_merged$LNDRVRSEG,
                               "OBJECTID"=lrs_cdl_crop_pxl_merged$OBJECTID,
                               "REGION"=lrs_cdl_crop_pxl_merged$REGION,
                               "Corn" = lrs_cdl_crop_pxl_merged[,29],
                               "Sorghum" = lrs_cdl_crop_pxl_merged[,30],
                               "Soybeans" = lrs_cdl_crop_pxl_merged[,31],
                               "Peanuts" = lrs_cdl_crop_pxl_merged[,32],
                               "Barley" = lrs_cdl_crop_pxl_merged[,33],
                               "Wheat" = lrs_cdl_crop_pxl_merged[,34],
                               "Rye" = lrs_cdl_crop_pxl_merged[,35],
                               "Oats" = lrs_cdl_crop_pxl_merged[,36],
                               "Alfalfa" = lrs_cdl_crop_pxl_merged[,37],
                               "Other Hay/Non Alfalfa" = lrs_cdl_crop_pxl_merged[,38],
                               "Potatoes" = lrs_cdl_crop_pxl_merged[,39],
                               "Grass/Pasture" = lrs_cdl_crop_pxl_merged[,40])
lrs_cdl_percent[is.na(lrs_cdl_percent)] <- 0

lrs_cdl_percent <- lrs_cdl_percent %>% arrange(FIPS,LNDRVRSEG)
lrs_cdl_percent2007 <- lrs_cdl_percent
### 2012 ################################################################################################################################################

lrs_cdl_2012 <- as.data.frame(read_and_preprocess_cdl(table2012,code_class_cdl,lrs_shp)[1],check.names=FALSE)
cbw_lrs_cdl_2012 <- as.data.frame(read_and_preprocess_cdl(table2012,code_class_cdl,lrs_shp)[2],check.names=FALSE)
cbw_lrs_cdl_2012 <- cbw_lrs_cdl_2012 %>% arrange(FIPS,LNDRVRSEG)

### Population Allocation ##
# This section corresponds to allocation of population to the land river segments. The variable percent_developed_in_cbw will be used in the population_data.R
# The variable has to be restricted to cbw only because the populationcty is already calculated for the counties inside of cbw, which means just the 1925 LRSs
cdl_developed_aggregated_2012 <- data.frame("OBJECTID"=cbw_lrs_cdl_2012$OBJECTID,
                                            "LNDRVRSEG"=cbw_lrs_cdl_2012$LNDRVRSEG,
                                            "FIPS"=cbw_lrs_cdl_2012$FIPS,
                                            "REGION"=cbw_lrs_cdl_2012$Region,
                                            "DEVELOPED2012"=rowSums(cbw_lrs_cdl_2012[,developedland]))

cdl_developed_aggregated_2012_fips <- cdl_developed_aggregated_2012 %>% group_by(FIPS) %>% summarize(across(DEVELOPED2012,sum))

dummy <- merge(cdl_developed_aggregated_2012,cdl_developed_aggregated_2012_fips,by="FIPS")

dummy <-  cbind(dummy,"percentage" = dummy$DEVELOPED2012.x/dummy$DEVELOPED2012.y)

percent_developed_in_cbw <- dummy[,-c(4,5,6)]
percent_developed_in_cbw2012 <- percent_developed_in_cbw

### Animal Allocation ###
# This section corresponds to allocation of animals to the land river segments. That should be done using the cropland as a proxy (one of the Booth approach).
allnames <- names(lrs_cdl_2012)
noncropnames <- c("Open Water","Developed/Open Space","Developed/Low Intensity","Developed/Med Intensity","Developed/High Intensity","Barren",
                   "Deciduous Forest","Evergreen Forest","Mixed Forest","Shrubland","Woody Wetlands","Herbaceous Wetlands")
cropnames2012 <- allnames[!allnames %in% c(noncropnames,"OBJECTID","LNDRVRSEG","FIPS","Region")]

cdl_cropland_aggregated <- data.frame("OBJECTID"=lrs_cdl_2012$OBJECTID,
                                      "LNDRVRSEG"=lrs_cdl_2012$LNDRVRSEG,
                                      "FIPS"=lrs_cdl_2012$FIPS,
                                      "REGION"=lrs_cdl_2012$Region,
                                      "CROPLAND"=rowSums(lrs_cdl_2012[,cropnames2012]),
                                      "NONCROP"=rowSums(lrs_cdl_2012[,noncropnames]),
                                      "CORN"=lrs_cdl_2012[,"Corn"])

cdl_cropland_aggregated_fips <- cdl_cropland_aggregated %>% group_by(FIPS) %>% summarize(across(CROPLAND,sum))
cdl_corn_aggregated_fips <- cdl_cropland_aggregated %>% group_by(FIPS) %>% summarize(across(CORN,sum))

dummy <- merge(cdl_cropland_aggregated,cdl_cropland_aggregated_fips,by="FIPS")

dummy <-  cbind(dummy,"percentage2012" = dummy$CROPLAND.x/dummy$CROPLAND.y)
percent_crop_lrs <- dummy[,-c(5,6,7)]
percent_crop_in_cbw <- dummy[dummy$REGION == "Chesapeake Bay Watershed",]
percent_crop_in_cbw <- percent_crop_in_cbw[,-c(4,5,6,7,8)]
percent_crop_in_cbw2012 <- percent_crop_in_cbw

dummy_corn <- merge(cdl_cropland_aggregated,cdl_corn_aggregated_fips,by="FIPS")

dummy_corn <-  cbind(dummy_corn,"percentage.corn2012" = dummy_corn$CORN.x/dummy_corn$CORN.y)
percent_corn_lrs <- dummy_corn[,-c(5,6,7,8)]
percent_corn_in_cbw <- percent_corn_lrs[percent_corn_lrs$REGION == "Chesapeake Bay Watershed",]
percent_corn_in_cbw <- percent_corn_in_cbw[,-c(4)]
percent_corn_in_cbw[is.na(percent_corn_in_cbw)] <- 0
percent_corn_in_cbw2012 <- percent_corn_in_cbw

### Crop Allocation ##
# This section corresponds to allocation of crop to the land river segments. 
Corn_pixels <- rowSums(lrs_cdl_2012[,grep("Corn",names(lrs_cdl_2012))])
Soybean_pixels <- rowSums(lrs_cdl_2012[,grep("Soybean",names(lrs_cdl_2012))])
Sorghum_pixels <- rowSums(lrs_cdl_2012[,grep("Sorghum",names(lrs_cdl_2012))])
Wheat_pixels <- rowSums(lrs_cdl_2012[,grep("Wheat",names(lrs_cdl_2012))])
Oats_pixels <- rowSums(lrs_cdl_2012[,grep("Oats",names(lrs_cdl_2012))])
Barley_pixels <- rowSums(lrs_cdl_2012[,grep("Barley",names(lrs_cdl_2012))])

lrs_cdl_crop_pxl <- data.frame("OBJECTID"=lrs_cdl_2012$OBJECTID,
                               "LNDRVRSEG"=lrs_cdl_2012$LNDRVRSEG,
                               "FIPS"=lrs_cdl_2012$FIPS,
                               "REGION"=lrs_cdl_2012$Region,
                               "Corn" = Corn_pixels,
                               "Sorghum" = Sorghum_pixels,
                               "Soybeans" = Soybean_pixels,
                               "Peanuts" = lrs_cdl_2012[,8],
                               "Barley" = Barley_pixels,
                               "Wheat" = Wheat_pixels,
                               "Rye" = lrs_cdl_2012[,15],
                               "Oats" = Oats_pixels,
                               "Alfalfa" = lrs_cdl_2012[,21],
                               "Other Hay/Non Alfalfa" = lrs_cdl_2012[,22],
                               "Potatoes" = lrs_cdl_2012[,26],
                               "Grass/Pasture" = lrs_cdl_2012[,58]) 

lrs_cdl_crop_pxl_groupby_cty <- lrs_cdl_crop_pxl %>% group_by(lrs_cdl_crop_pxl$FIPS) %>% summarize(across(colnames(lrs_cdl_crop_pxl[,5:16]),sum))
colnames(lrs_cdl_crop_pxl_groupby_cty)[1] <- "FIPS"

lrs_cdl_crop_pxl_merged <- merge(lrs_cdl_crop_pxl, lrs_cdl_crop_pxl_groupby_cty, by="FIPS")

lrs_cdl_crop_pxl_merged[,29:40] <- lrs_cdl_crop_pxl_merged[,5:16]/lrs_cdl_crop_pxl_merged[,17:28]

lrs_cdl_percent <-  data.frame("FIPS"=lrs_cdl_crop_pxl_merged$FIPS,
                               "LNDRVRSEG"=lrs_cdl_crop_pxl_merged$LNDRVRSEG,
                               "OBJECTID"=lrs_cdl_crop_pxl_merged$OBJECTID,
                               "REGION"=lrs_cdl_crop_pxl_merged$REGION,
                               "Corn" = lrs_cdl_crop_pxl_merged[,29],
                               "Sorghum" = lrs_cdl_crop_pxl_merged[,30],
                               "Soybeans" = lrs_cdl_crop_pxl_merged[,31],
                               "Peanuts" = lrs_cdl_crop_pxl_merged[,32],
                               "Barley" = lrs_cdl_crop_pxl_merged[,33],
                               "Wheat" = lrs_cdl_crop_pxl_merged[,34],
                               "Rye" = lrs_cdl_crop_pxl_merged[,35],
                               "Oats" = lrs_cdl_crop_pxl_merged[,36],
                               "Alfalfa" = lrs_cdl_crop_pxl_merged[,37],
                               "Other Hay/Non Alfalfa" = lrs_cdl_crop_pxl_merged[,38],
                               "Potatoes" = lrs_cdl_crop_pxl_merged[,39],
                               "Grass/Pasture" = lrs_cdl_crop_pxl_merged[,40])
lrs_cdl_percent[is.na(lrs_cdl_percent)] <- 0

lrs_cdl_percent <- lrs_cdl_percent %>% arrange(FIPS,LNDRVRSEG)
lrs_cdl_percent2012 <- lrs_cdl_percent

### 2017 ####
lrs_cdl_2017 <- as.data.frame(read_and_preprocess_cdl(table2017,code_class_cdl,lrs_shp)[1],check.names=FALSE)
cbw_lrs_cdl_2017 <- as.data.frame(read_and_preprocess_cdl(table2017,code_class_cdl,lrs_shp)[2],check.names=FALSE)
cbw_lrs_cdl_2017 <- cbw_lrs_cdl_2017 %>% arrange(FIPS,LNDRVRSEG)

### Population Allocation ##
# This section corresponds to allocation of population to the land river segments. The variable percent_developed_in_cbw will be used in the population_data.R
# The variable has to be restricted to cbw only because the populationcty is already calculated for the counties inside of cbw, which means just the 1925 LRSs
cdl_developed_aggregated_2017 <- data.frame("OBJECTID"=cbw_lrs_cdl_2017$OBJECTID,
                                       "LNDRVRSEG"=cbw_lrs_cdl_2017$LNDRVRSEG,
                                       "FIPS"=cbw_lrs_cdl_2017$FIPS,
                                       "REGION"=cbw_lrs_cdl_2017$Region,
                                       "DEVELOPED2017"=rowSums(cbw_lrs_cdl_2017[,49:52]))

cdl_developed_aggregated_2017_fips <- cdl_developed_aggregated_2017 %>% group_by(FIPS) %>% summarize(across(DEVELOPED2017,sum))

dummy <- merge(cdl_developed_aggregated_2017,cdl_developed_aggregated_2017_fips,by="FIPS")

dummy <-  cbind(dummy,"percentage" = dummy$DEVELOPED2017.x/dummy$DEVELOPED2017.y)

percent_developed_in_cbw <- dummy[,-c(4,5,6)]
percent_developed_in_cbw2017 <- percent_developed_in_cbw

### Animal Allocation ###
# This section corresponds to allocation of animals to the land river segments. That should be done using the cropland as a proxy (one of the Booth approach).
allnames <- names(lrs_cdl_2017)
noncropnames <- c("Open Water","Developed/Open Space","Developed/Low Intensity","Developed/Med Intensity","Developed/High Intensity","Barren",
                  "Deciduous Forest","Evergreen Forest","Mixed Forest","Shrubland","Woody Wetlands","Herbaceous Wetlands")
cropnames2017 <- allnames[!allnames %in% c(noncropnames,"OBJECTID","LNDRVRSEG","FIPS","Region")]

cdl_cropland_aggregated <- data.frame("OBJECTID"=lrs_cdl_2017$OBJECTID,
                                      "LNDRVRSEG"=lrs_cdl_2017$LNDRVRSEG,
                                      "FIPS"=lrs_cdl_2017$FIPS,
                                      "REGION"=lrs_cdl_2017$Region,
                                      "CROPLAND"=rowSums(lrs_cdl_2017[,cropnames2017]),
                                      "NONCROP"=rowSums(lrs_cdl_2017[,noncropnames]),
                                      "CORN"=lrs_cdl_2017[,3])

cdl_cropland_aggregated_fips <- cdl_cropland_aggregated %>% group_by(FIPS) %>% summarize(across(CROPLAND,sum))
cdl_corn_aggregated_fips <- cdl_cropland_aggregated %>% group_by(FIPS) %>% summarize(across(CORN,sum))

dummy <- merge(cdl_cropland_aggregated,cdl_cropland_aggregated_fips,by="FIPS")

dummy <-  cbind(dummy,"percentage2017" = dummy$CROPLAND.x/dummy$CROPLAND.y)
percent_crop_lrs <- dummy[,-c(5,6,7)]
percent_crop_in_cbw <- dummy[dummy$REGION == "Chesapeake Bay Watershed",]
percent_crop_in_cbw <- percent_crop_in_cbw[,-c(4,5,6,7,8)]
percent_crop_in_cbw2017 <- percent_crop_in_cbw

dummy_corn <- merge(cdl_cropland_aggregated,cdl_corn_aggregated_fips,by="FIPS")

dummy_corn <-  cbind(dummy_corn,"percentage.corn2017" = dummy_corn$CORN.x/dummy_corn$CORN.y)
percent_corn_lrs <- dummy_corn[,-c(5,6,7,8)]
percent_corn_in_cbw <- percent_corn_lrs[percent_corn_lrs$REGION == "Chesapeake Bay Watershed",]
percent_corn_in_cbw <- percent_corn_in_cbw[,-c(4)]
percent_corn_in_cbw[is.na(percent_corn_in_cbw)] <- 0
percent_corn_in_cbw2017 <- percent_corn_in_cbw

### Crop Allocation ##
# This section corresponds to allocation of crop to the land river segments. 
Corn_pixels <- rowSums(lrs_cdl_2017[,grep("Corn",names(lrs_cdl_2017))])
Soybean_pixels <- rowSums(lrs_cdl_2017[,grep("Soybean",names(lrs_cdl_2017))])
Sorghum_pixels <- rowSums(lrs_cdl_2017[,grep("Sorghum",names(lrs_cdl_2017))])
Wheat_pixels <- rowSums(lrs_cdl_2017[,grep("Wheat",names(lrs_cdl_2017))])
Oats_pixels <- rowSums(lrs_cdl_2017[,grep("Oats",names(lrs_cdl_2017))])
Barley_pixels <- rowSums(lrs_cdl_2017[,grep("Barley",names(lrs_cdl_2017))])

lrs_cdl_crop_pxl <- data.frame("OBJECTID"=lrs_cdl_2017$OBJECTID,
                            "LNDRVRSEG"=lrs_cdl_2017$LNDRVRSEG,
                            "FIPS"=lrs_cdl_2017$FIPS,
                            "REGION"=lrs_cdl_2017$Region,
                            "Corn" = Corn_pixels,
                            "Sorghum" = Sorghum_pixels,
                            "Soybeans" = Soybean_pixels,
                            "Peanuts" = lrs_cdl_2017[,8],
                            "Barley" = Barley_pixels,
                            "Wheat" = Wheat_pixels,
                            "Rye" = lrs_cdl_2017[,15],
                            "Oats" = Oats_pixels,
                            "Alfalfa" = lrs_cdl_2017[,21],
                            "Other Hay/Non Alfalfa" = lrs_cdl_2017[,22],
                            "Potatoes" = lrs_cdl_2017[,26],
                            "Grass/Pasture" = lrs_cdl_2017[,58]) 

lrs_cdl_crop_pxl_groupby_cty <- lrs_cdl_crop_pxl %>% group_by(lrs_cdl_crop_pxl$FIPS) %>% summarize(across(colnames(lrs_cdl_crop_pxl[,5:16]),sum))
colnames(lrs_cdl_crop_pxl_groupby_cty)[1] <- "FIPS"

lrs_cdl_crop_pxl_merged <- merge(lrs_cdl_crop_pxl, lrs_cdl_crop_pxl_groupby_cty, by="FIPS")

lrs_cdl_crop_pxl_merged[,29:40] <- lrs_cdl_crop_pxl_merged[,5:16]/lrs_cdl_crop_pxl_merged[,17:28]

lrs_cdl_percent <-  data.frame("FIPS"=lrs_cdl_crop_pxl_merged$FIPS,
                               "LNDRVRSEG"=lrs_cdl_crop_pxl_merged$LNDRVRSEG,
                               "OBJECTID"=lrs_cdl_crop_pxl_merged$OBJECTID,
                               "REGION"=lrs_cdl_crop_pxl_merged$REGION,
                               "Corn" = lrs_cdl_crop_pxl_merged[,29],
                               "Sorghum" = lrs_cdl_crop_pxl_merged[,30],
                               "Soybeans" = lrs_cdl_crop_pxl_merged[,31],
                               "Peanuts" = lrs_cdl_crop_pxl_merged[,32],
                               "Barley" = lrs_cdl_crop_pxl_merged[,33],
                               "Wheat" = lrs_cdl_crop_pxl_merged[,34],
                               "Rye" = lrs_cdl_crop_pxl_merged[,35],
                               "Oats" = lrs_cdl_crop_pxl_merged[,36],
                               "Alfalfa" = lrs_cdl_crop_pxl_merged[,37],
                               "Other Hay/Non Alfalfa" = lrs_cdl_crop_pxl_merged[,38],
                               "Potatoes" = lrs_cdl_crop_pxl_merged[,39],
                               "Grass/Pasture" = lrs_cdl_crop_pxl_merged[,40])
lrs_cdl_percent[is.na(lrs_cdl_percent)] <- 0

lrs_cdl_percent <- lrs_cdl_percent %>% arrange(FIPS,LNDRVRSEG)

lrs_cdl_percent2017 <- lrs_cdl_percent

### Adjusting variables for multiple years ####
# Population
percent_developed_in_cbw <- percent_developed_in_cbw2007 %>%  left_join(percent_developed_in_cbw2012, by="LNDRVRSEG") %>%  left_join(percent_developed_in_cbw2017, by="LNDRVRSEG")
percent_developed_in_cbw <- data.frame("FIPS"=percent_developed_in_cbw$FIPS.x,
                                       "OBJECTID"=percent_developed_in_cbw$OBJECTID.x,
                                       "LNDRVRSEG"=percent_developed_in_cbw$LNDRVRSEG,
                                       "percentage2007"=percent_developed_in_cbw$percentage.x,
                                       "percentage2012"=percent_developed_in_cbw$percentage.y,
                                       "percentage2017"=percent_developed_in_cbw$percentage)

# Animal allocation
percent_crop_in_cbw <- percent_crop_in_cbw2007 %>% left_join(percent_crop_in_cbw2012,by="LNDRVRSEG") %>% left_join(percent_crop_in_cbw2017, by="LNDRVRSEG")
percent_crop_in_cbw <- data.frame("FIPS"=percent_crop_in_cbw$FIPS.x,
                                  "OBJECTID"=percent_crop_in_cbw$OBJECTID.x,
                                  "LNDRVRSEG"=percent_crop_in_cbw$LNDRVRSEG,
                                  "percentage1997"=percent_crop_in_cbw$percentage2007,
                                  "percentage2002"=percent_crop_in_cbw$percentage2007,
                                  "percentage2007"=percent_crop_in_cbw$percentage2007,
                                  "percentage2012"=percent_crop_in_cbw$percentage2012,
                                  "percentage2017"=percent_crop_in_cbw$percentage2017)

percent_crop_in_cbw <- percent_crop_in_cbw %>% arrange(FIPS,LNDRVRSEG) # To keep the order of FIPS and LRS the same as original variables

percent_corn_in_cbw <- percent_corn_in_cbw2007 %>% left_join(percent_corn_in_cbw2012,by="LNDRVRSEG") %>% left_join(percent_corn_in_cbw2017, by="LNDRVRSEG")
percent_corn_in_cbw <- data.frame("FIPS"=percent_corn_in_cbw$FIPS.x,
                                  "OBJECTID"=percent_corn_in_cbw$OBJECTID.x,
                                  "LNDRVRSEG"=percent_corn_in_cbw$LNDRVRSEG,
                                  "percentage1997c"=percent_corn_in_cbw$percentage.corn2007,
                                  "percentage2002c"=percent_corn_in_cbw$percentage.corn2007,
                                  "percentage2007c"=percent_corn_in_cbw$percentage.corn2007,
                                  "percentage2012c"=percent_corn_in_cbw$percentage.corn2012,
                                  "percentage2017c"=percent_corn_in_cbw$percentage.corn2017)

percent_corn_in_cbw <- percent_corn_in_cbw %>% arrange(FIPS,LNDRVRSEG) # To keep the order of FIPS and LRS the same as original variables

# Crop allocation
lrs_cdl_percents <- array(0, c(2058,16,5))
lrs_cdl_percents[,,1] <- as.matrix(lrs_cdl_percent2007)
lrs_cdl_percents[,,2] <- as.matrix(lrs_cdl_percent2007)
lrs_cdl_percents[,,3] <- as.matrix(lrs_cdl_percent2007)
lrs_cdl_percents[,,4] <- as.matrix(lrs_cdl_percent2012)
lrs_cdl_percents[,,5] <- as.matrix(lrs_cdl_percent2017)

lrs_cdl_percents <- list(lrs_cdl_percent2007,lrs_cdl_percent2007,lrs_cdl_percent2007,lrs_cdl_percent2012,lrs_cdl_percent2017)

# as.data.frame(a[1],check.names=FALSE)