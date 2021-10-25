install.packages("FedData")

library(FedData)
library(raster)
library(dplyr)


PRW_crs<-'+proj=utm +datum=NAD83 +zone=11'
PRW_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)
PRW_ext_shape <- polygon_from_extent(raster::extent(PRW_ext), proj4string=PRW_crs)
#fix name NHD

plot(PRW_ext_shape)



#PRW_NHD <- get_NED(template=PRW_ext_shape, label='PRW_NHD')


AEC_raw<-raster("RAW/AEC/dynamicAec_20180307.tif")

#use NED_PRW, need NED script
AEC1<-projectRaster(AEC_raw, NED_PRW, method="ngb") #this is a long time step 

AEC2<- AEC1%>%
  crop(NED_PRW)%>%
  mask(NED_PRW)

writeRaster(AEC2,"processed_data/ACE_PRW")
AEC2 <- raster("processed_data/ACE_PRW")
plot(AEC2)

hist(test_cropped)

reclass_df<-c(101,1,
              201,1,
              103,3,
              203,3,
              104,4,
              204,4,
              105,5,
              205,5,
              106,6,
              206,6,
              107,7,
              207,7,
              109,9,
              209,9,
              111,11.1,
              211,11.2,
              112,12.1,
              212,12.2,
              113,13.1,
              213,13.2,
              114,14,
              214,14,
              115,15,
              215,15,
              150,50,
              250,50,
              151,51,
              251,52)

reclass_m <- matrix(reclass_df,
                    ncol = 2, 
                    byrow = T)


AEC3<- reclassify(AEC2,reclass_m)#? reclassify

hist(AEC3, xlab="ACE")
writeRaster(AEC3, "Processed_data/MAN_PRW")
AEC3<-raster("Processed_data/MAN_PRW")

ACE_col<- rgb(runif(17),runif(17),runif(17))
plot(AEC3, xlab="Easting (m)", ylab= "northing (m)", main = "Managment")
#plot(PRW, add =T)
unique(AEC3)

#unique(test_reclass)
 
# 
# 
# 
# 
# colMedians(xyFromCell(test_reclass, which(test_reclass[]==11.00)))
# ??colMedians
# 
# df<-test_reclass
# #find the mean and median centroid 
# Raster_centroid<-function(df){
#   values<-unique(df)
#   Centroid_mean<- as.data.frame(t(mapply(x=values,
#                                          function(x) apply(xyFromCell(test_reclass, which(test_reclass[]==x)),2, FUN=mean)
#   )))
#   Centroid_mean$AEC<-values 
#   Centroid_median<- as.data.frame(t(mapply(x=values,
#                                            function(x) apply(xyFromCell(test_reclass, which(test_reclass[]==x)),2, FUN=median)
#   )))
#   Centroid_median$AEC<-values
#   return(Centroid_median)
# }
# 
# 
# apply(xyFromCell(test_reclass, which(test_reclass[]==3)),2, FUN=median)
# #fix return climates for 3,11,11.1,11.2,12,12.1,12.2,13,13.1,13.2 
# 
# Raster_centroid(test_reclass)
# 
# sapply(x=values, FUN = x*2)
# plot(test_cropped)
# plot(PRW_shape, add=T)
# summary(test)
# 
# unique(test)
# # 1   3   4   5   6   7  11  12  13  14  15 101 103 104 105 106 107 111 112 113 115 201 203 204 205 206 207 211 212 213 215
# 
