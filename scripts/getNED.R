library(FedData)
library(raster)
library (dplyr)


PRW_crs<-'+proj=utm +datum=NAD83 +zone=11'
PRW_ext<- c(380000.00,550000.000,5000000.00,5300000.00)
#PRW_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)
PRW_ext_shape <- polygon_from_extent(raster::extent(PRW_ext), proj4string=PRW_crs)


#plot(PRW_ext_shape)
#get NED DEM at 1 acr ~30m for the PRW extent, mosaic the tiles and 
NED <- get_ned(template=PRW_ext_shape, 
                       label='PRW',
                       force.redo = F)

#PRW_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)

#Define the PRW Extent -- this should be based on the watershed polygon.... fixed 4.4.2019
#PRW_crs<-'+proj=utm +datum=NAD83 +zone=11'
#PRW2_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)
#PRW_shape <- polygon_from_extent(raster::extent(PRW2_ext), proj4string=PRW_crs)

PRW<-shapefile("RAW/extent/PRW_shape")
str(PRW)
extent(PRW)
crs(PRW)

NED<-raster("EXTRACTIONS/PRW_NED/NED/PRW_NED_NED_1.tif")

#croping NED down to PRW
PRW_NED<-raster::projectRaster(NED, crs=crs(PRW),res=30)
NED_PRW<- PRW_NED%>%
  crop(PRW)%>%
  mask(PRW)

rm(PRW_NED)
NED_out<- PRW_NED%>%
  crop(PRW)


writeRaster(NED_PRW,"processed_data/NED_PRW.grd", overwrite=T)
#NED_PRW <- raster("processed_data/NED_PRW.grd")
#plot(PRW_NED_clip, main="PRW NED DEM 30m", xlab="Easting", ylab="Northing")                              
                           
plot(NED_PRW)
 


