library(FedData)
library(raster)


PRW_crs<-'+proj=utm +datum=NAD83 +zone=11'
PRW_ext<- c(380000.00,550000.000,5000000.00,5300000.00)
#PRW_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)
PRW_ext_shape <- polygon_from_extent(raster::extent(PRW_ext), proj4string=PRW_crs)


#plot(PRW_ext_shape)

PRW_NLCD <- get_nlcd(template=PRW_ext_shape, 
                   label='PRW_NLCD',
                   year=2011,
                   dataset ="landcover",
                   raw.dir = "RAW/NLCD",
                   extraction.dir = "EXTRACTIONS/NLCD",
                   raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9",
                                      "INTERLEAVE=BAND"), 
                   force.redo = F)
#fix name NHD
plot(PRW_ext_shape)

plot(PRW_NLCD$NHDFlowline, add=T)
plot(PRW_NLCD$NHDLine, add=T)
plot(PRW_NLCD$NHDArea, col='black', add=T)
plot(PRW_HCD$NHDWaterbody, col='black', add=T)
