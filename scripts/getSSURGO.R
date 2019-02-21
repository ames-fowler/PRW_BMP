library(FedData)
library(raster)

PRW_shape<- shapefile("rawdata/extent/PRW_shape.shp")
crs(PRW_shape)
PRW_crs<-'+proj=utm +datum=NAD83 +zone=11'
PRW_ext<- c(380000.00,550000.000,5100000.00,5300000.00)
#PRW_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)
PRW_ext_shape <- polygon_from_extent(raster::extent(PRW_ext), proj4string=PRW_crs)
extent(PRW_ext_shape)


#plot(PRW_ext_shape)

SOIL <- get_ssurgo(template=PRW_ext_shape, 
                     label='PRW',
                     force.redo = F)


#project the croped area to UTM 
pr1 <- spTransform(SOIL$spatial, CRSobj =crs(PRW_ext_shape))
plot(pr1)
#Define the PRW Extent -- this should be based on the watershed polygon....2/28/19
PRW_crs<-'+proj=utm +datum=NAD83 +zone=11'
PRW2_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)
PRW_shape <- polygon_from_extent(raster::extent(PRW2_ext), proj4string=PRW_crs)

#croping NED down to PRW
PRW_SOIL<-crop(pr1,PRW_shape)
rm(pr1,SOIL)

plot(PRW_NED)
