library(FedData)
library(raster)
library(gdalUtils)
library(tictoc)



PRW_crs<-'+proj=utm +datum=NAD83 +zone=11'
PRW_ext<- c(380000.00,550000.000,5000000.00,5300000.00)
#PRW_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)
PRW_ext_shape <- polygon_from_extent(raster::extent(PRW_ext), proj4string=PRW_crs)

PRW<-shapefile("RAW/extent/PRW_shape")


#plot(PRW_ext_shape)

NLCD <- get_nlcd(template=PRW_ext_shape, 
                   label='PRW',
                   year=2011,
                   dataset ="landcover",
                   raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9",
                                      "INTERLEAVE=BAND"), 
                   force.redo = F)
#fix name NHD






NLCD<-raster("EXTRACTIONS/PRW/NLCD/PRW_NLCD_2011_landcover.tif") 

#croping NED down to PRW

# tic()
# PRW_NLCD<-raster::projectRaster(NLCD, crs=crs(PRW),res=30) #so slow 111 secounds for bilinear, ngb won't run... 
# toc()




tic() #YAYAYAYAYAYAY!!!!! got it 3.62 secounds 
testit <- gdalwarp("PRW_NLCD_2011_landcover.tif",dstfile="test.tif",
                   t_srs='+proj=utm +zone=11 +datum=NAD83',tr=(c(30,30)),output_Raster=TRUE,
                   overwrite=TRUE,verbose=TRUE)
toc()

PRW_NLCD <-testit

res(testit) #resolution issues 

NLCD_PRW <- PRW_NLCD%>%
  crop(PRW)%>%
  mask(PRW)

raster::origin(NLCD_PRW) <- raster::origin(NED_PRW)
xmin(NLCD_PRW)<-xmin(NED_PRW)
xmax(NLCD_PRW)<-xmax(NED_PRW)
ymin(NLCD_PRW)<-ymin(NED_PRW)
ymax(NLCD_PRW)<-ymax(NED_PRW)

plot(NLCD_PRW)

writeRaster(NLCD_PRW,"processed_data/PRW_NLCD.tif", overwrite=F)
NLCD_PRW <- raster("processed_data/PRW_NLCD.tif")

extent(NLCD_PRW)
extent(NED_PRW)

plot(NLCD_PRW)


# rm(PRW_NLCD)
# 
# plot(PRW_NLCD$NHDFlowline, add=T)
# plot(PRW_NLCD$NHDLine, add=T)
# plot(PRW_NLCD$NHDArea, col='black', add=T)
# plot(PRW_HCD$NHDWaterbody, col='black', add=T)
