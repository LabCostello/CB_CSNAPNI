lrs_cdl_2017 <- read_excel("RawData/Table_LRS_CDL_2017.xlsx")
code_class_cdl <- read_excel("RawData/code_class_cdl.xlsx")

# Drop column value_0
lrs_cdl_2017 <- lrs_cdl_2017[,-3]

# Change the name of the columns to the crops
old_colnames <- colnames(lrs_cdl_2017[,-c(1,2)])
index_new_name <- match(old_colnames,code_class_cdl$CODE) # index of the new names for new_colnames

column_names <- c("OBJECTID","LNDRVRSEG",unlist(code_class_cdl[index_new_name,2]))
colnames(lrs_cdl_2017) <- column_names


# Checking the percentage per county 
lrs_cdl_2017 <- cbind(lrs_cdl_2017,"FIPS" = lrs_shp$FIPS, "Region" = lrs_shp$Region)

cbw_lrs_cdl_2017 <- lrs_cdl_2017[lrs_cdl_2017$Region == "Chesapeake Bay Watershed",] 
cbw_lrs_cdl_2017 <- cbw_lrs_cdl_2017[order(cbw_lrs_cdl_2017$FIPS),] # Ascending order for FIPS

### Population Allocation ####
# This section corresponds to allocation of population to the land river segments. The variable percent_developed_in_cbw will be used in the population_data.R
# The variable has to be restricted to cbw only because the populationcty is already calculated for the counties inside of cbw, which means just the 1925 LRSs
cdl_developed_aggregated <- data.frame("OBJECTID"=cbw_lrs_cdl_2017$OBJECTID,
                                       "LNDRVRSEG"=cbw_lrs_cdl_2017$LNDRVRSEG,
                                       "FIPS"=cbw_lrs_cdl_2017$FIPS,
                                       "REGION"=cbw_lrs_cdl_2017$Region,
                                       "DEVELOPED"=rowSums(cbw_lrs_cdl_2017[,48:51]))

cdl_developed_aggregated_fips <- cdl_developed_aggregated %>% group_by(FIPS) %>% summarize(across(DEVELOPED,sum))

dummy <- merge(cdl_developed_aggregated,cdl_developed_aggregated_fips,by="FIPS")

dummy <-  cbind(dummy,"percentage" = dummy$DEVELOPED.x/dummy$DEVELOPED.y)

percent_developed_in_cbw <- dummy[,-c(4,5,6)]

### Animal Allocation ####
# This section corresponds to allocation of animals to the land river segments. That should be done using the cropland as a proxy (one of the Booth approach).
cdl_cropland_aggregated <- data.frame("OBJECTID"=lrs_cdl_2017$OBJECTID,
                                      "LNDRVRSEG"=lrs_cdl_2017$LNDRVRSEG,
                                      "FIPS"=lrs_cdl_2017$FIPS,
                                      "REGION"=lrs_cdl_2017$Region,
                                      "CROPLAND"=rowSums(lrs_cdl_2017[,c(3:46,58,61:83)]),
                                      "NONCROP"=rowSums(lrs_cdl_2017[,c(47:57,59,60)]),
                                      "CORN"=lrs_cdl_2017[,3])

cdl_cropland_aggregated_fips <- cdl_cropland_aggregated %>% group_by(FIPS) %>% summarize(across(CROPLAND,sum))
cdl_corn_aggregated_fips <- cdl_cropland_aggregated %>% group_by(FIPS) %>% summarize(across(CORN,sum))

dummy <- merge(cdl_cropland_aggregated,cdl_cropland_aggregated_fips,by="FIPS")

dummy <-  cbind(dummy,"percentage" = dummy$CROPLAND.x/dummy$CROPLAND.y)
percent_crop_lrs <- dummy[,-c(5,6,7)]
percent_crop_in_cbw <- dummy[dummy$REGION == "Chesapeake Bay Watershed",]
percent_crop_in_cbw <- percent_crop_in_cbw[,-c(4,5,6,7,8)]

dummy_corn <- merge(cdl_cropland_aggregated,cdl_corn_aggregated_fips,by="FIPS")

dummy_corn <-  cbind(dummy_corn,"percentage.corn" = dummy_corn$CORN.x/dummy_corn$CORN.y)
percent_corn_lrs <- dummy_corn[,-c(5,6,7,8)]
percent_corn_in_cbw <- percent_corn_lrs[percent_corn_lrs$REGION == "Chesapeake Bay Watershed",]
percent_corn_in_cbw <- percent_corn_in_cbw[,-c(4)]
percent_corn_in_cbw[is.na(percent_corn_in_cbw)] <- 0


### Crop Allocation ####
# This section corresponds to allocation of crop to the land river segments. 
Corn_pixels <- lrs_cdl_2017[,3] + lrs_cdl_2017[,10] + lrs_cdl_2017[,71] + lrs_cdl_2017[,72] + lrs_cdl_2017[,75] + lrs_cdl_2017[,78]
Soybean_pixels <- lrs_cdl_2017[,6] + lrs_cdl_2017[,14] + lrs_cdl_2017[,77] + lrs_cdl_2017[,78] + lrs_cdl_2017[,83] + lrs_cdl_2017[,78]
Sorghum_pixels <- lrs_cdl_2017[,5] + lrs_cdl_2017[,74]
Wheat_pixels <- lrs_cdl_2017[,5] + lrs_cdl_2017[,74]
Oats_pixels <- lrs_cdl_2017[,16] + lrs_cdl_2017[,75] + lrs_cdl_2017[,77]
Barley_pixels <- lrs_cdl_2017[,11] + lrs_cdl_2017[,75] + lrs_cdl_2017[,83]

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

CROPS <- colnames(lrs_cdl_crop_pxl[,5:16])

lrs_cdl_crop_pxl_groupby_cty <- lrs_cdl_crop_pxl %>% group_by(lrs_cdl_crop_pxl$FIPS) %>% summarize(across(CROPS,sum))
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