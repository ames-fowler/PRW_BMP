
install.packages("FedData", dependencies = TRUE)
install.packages("raster", dependencies = TRUE)
install.packages("gdal")
library(FedData)
library(raster)

PRW_crs<-'+proj=utm +datum=NAD83 +zone=11'
PRW_ext<- c(380000.00,550000.000,5000000.00,5300000.00)
#PRW_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)
PRW_ext_shape <- polygon_from_extent(raster::extent(PRW_ext), proj4string=PRW_crs)
                               
plot(PRW_ext_shape)



PRW_NHD <- get_nhd(template=PRW_ext_shape, label='PRW_NHD')

plot(PRW_NHD$`_Line`)
plot(PRW_NHD)
plot(PRW_NHD$NHDLine, add=T)
plot(PRW_NHD$NHDArea, col='black', add=T)
plot(PPRW_NHD$NHDWaterbody, col='black', add=T)

plot(PRW_HCD$Line)

