output_lrs_coefficients <- read_excel("C:/Users/lds5498/OneDrive - The Pennsylvania State University/Desktop/Code/SourceData.xlsx", sheet = "DeliveryFactors")

#Filter the ones that is just in CBW
output_lrs_coefficients <- output_lrs_coefficients[output_lrs_coefficients$LandRiverSegment %in% CBW_lrs_shp$LndRvrSeg,]

LU_for_lrs_outputcoef <- unique(output_lrs_coefficients$LoadSource) 

list_LU <- LU_for_lrs_outputcoef[c(2:14)]

output_lrs_coef_filt <- output_lrs_coefficients[output_lrs_coefficients$LoadSource %in% list_LU,]

out_lrs_N <- data.frame("LRS" = output_lrs_coef_filt[,1],
                        "LU" = output_lrs_coef_filt[,2],
                        "NOut" = output_lrs_coef_filt[,3]*output_lrs_coef_filt[,6]*output_lrs_coef_filt[,9])

out_lrs_N$CombinedColumn <- paste(out_lrs_N$LandRiverSegment,out_lrs_N$LandToWater_TN_Factor,sep = "_")

not_duplicated_rows <- out_lrs_N[!duplicated(out_lrs_N$CombinedColumn), ] # There are some LRS that has 2 coefficients and others that just one for all land uses

Nout_lrs <- not_duplicated_rows %>% group_by(LandRiverSegment) %>% summarize(mean(LandToWater_TN_Factor))

Nout_lrs <- Nout_lrs[order(match(Nout_lrs$LandRiverSegment,CBW_lrs_shp$LndRvrSeg)),]

RvrNexport <- NANIBtotsum[,5]*Nout_lrs$`mean(LandToWater_TN_Factor)`
RvrNexport2 <- (TNsum[,5]-rowSums(CNprod[,,5]))*Nout_lrs$`mean(LandToWater_TN_Factor)`

RvrNexportarea <- RvrNexport/area[,1]
RvrNexportarea <- RvrNexportarea[RvrNexportarea>0]

a <- NANIBtotsum[,5]

b <- testabb$`2017 Progress_NLoadEOT`*0.453592 #transforming lbs to kg and then dividing by km2

testabb2 <- read_excel("C:/Users/lds5498/Downloads/bb2.xlsx",sheet = 3)

testabb2 <- testabb2[testabb2$LNDRVS %in% CBW_lrs_shp$LndRvrSeg,]
testabb2 <- testabb2[match(CBW_lrs_shp$LndRvrSeg,testabb2$LNDRVS),]

p <- list()

c <- aggregate(CBW_lrs_shp$Acres,by=list(CBW_lrs_shp$MinBas),FUN=sum)
c <- c$x*0.0040468564224 #Transforming Acres to km2

for (i in 1:6) {
  RvrNexport <- NANIBtotsum[,i]
  
  a <- data.frame(CBW_lrs_shp$MinBas, RvrNexport)
  b <- data.frame(CBW_lrs_shp$MinBas,testabb2[,30+i]*0.453592)
  #b <- data.frame(CBW_lrs_shp$MinBas,(TNsum[,i]-rowSums(CNprod[,,i]))*Nout_lrs$`mean(LandToWater_TN_Factor)`)
  
  a <- aggregate(a$RvrNexport,by=list(a$CBW_lrs_shp.MinBas),FUN=sum)
  b <- aggregate(b[,2], by=list(b$CBW_lrs_shp.MinBas),FUN=sum)
  
  a <- a$x/c
  b <- b$x/c
  
  df <- data.frame(x=a,y=b)
  
  model <- lm(y ~ x, data=df)
  summary(model)
  
  # Extract the coefficients
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  
  # Create the regression equation as a string
  equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", "\n", " (R2 = ", round(summary(model)$r.squared, 3), ")", sep = "")
  
  
  # Create the plot
  
  p[[i]] <- ggplot(df, aes(x = x, y = y)) +
    geom_point() + # Scatter plot of the data
    geom_smooth(method = "lm", se = FALSE, col = "blue") + # Regression line
    annotate("text", x = Inf, y = Inf, label = equation, hjust = 1.1, vjust = 7, size = 5, colour = "red") + # Add the equation
    theme_minimal() +
    labs(title = paste("Scatter Plot with Regression Line", year[i]), x = "NANI (kg N/km2 yr-1)", y = "Riverine TN Flux (kg N/km2 yr-1)")
}
# Print all plots in one image
library(gridExtra)
grid.arrange(grobs = p, ncol = 3)
