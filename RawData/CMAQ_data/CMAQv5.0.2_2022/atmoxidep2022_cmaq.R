# Atmospheric deposition for 2022 (fixing) ####
library(sp)
library(raster)
library(sf)
library(terra)

cbwshp <- st_read("RawData/CMAQ_data/CMAQv5.0.2_2022/cbw.shp")
raster <- rast("RawData/CMAQ_data/CMAQv5.0.2_2022/EQUATES_CMAQv532_2019_V1_TOTDEP_OXN_12US1.tif")

# Get the CRS of the raster
raster_crs <- crs(raster)

# Transform the shapefile to the CRS of the raster
shapefile_transformed <- st_transform(cbwshp, crs = raster_crs)

for (i in 1:nrow(shapefile_transformed)) {
  print(i)
  example <- shapefile_transformed[i,]
  
  # Crop the raster to the extent of the shapefile
  raster_cropped <- crop(raster,example)

  shapefile_transformed$atmdep[i] <- mean(values(raster_cropped)) 
}

# total deposition oxidezided N in 2019 by LRS in kg N/km2
atmdep2022 <- data.frame("FIPS"=shapefile_transformed$FIPS,
                         "LRS"=shapefile_transformed$LndRvrSeg,
                         "AtmDep2019"=shapefile_transformed$atmdep*100*areaws)

write.csv(atmdep2022, file = "atmdep2019.csv")
