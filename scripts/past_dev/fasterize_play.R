setwd("E:/Dropbox/active/PCD_work/PRW_BMP")


library(dplyr)
library(raster)
library(fasterize)
library(sf)


mapunit<-shapefile("EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_Mapunits.shp")

test<- spTransform(mapunit,CRS("+proj=utm +zone=11 +datum=NAD83 +units=m
                               +no_defs +ellps=GRS80 +towgs84=0,0,0"))%>%
  crop(NED_PRW)

test$MUKEY<- as.numeric(test$MUKEY)

extent(NED_PRW)

# test the rasterize
#ample<-extent(c(390674,391874,5145869,5147069))
#smalltest <- crop(test,sample)
#smallR<-crop(NED_PRW,sample)
#small_r<-rasterize(smalltest,smallR,smalltest$MUKEY)
#plot(small_r)
#plot(smalltest, col=smalltest$MUKEY)

#requires to run get NED.R prior
r <- raster(ncol=ncol(NED_PRW), nrow=nrow(NED_PRW),vals=1,crs=crs(NED_PRW),res=res(NED_PRW), ext=extent(NED_PRW))

NED<-raster("processed_data/NED_PRW")
testit<- st_read("EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_Mapunits.shp")
typeof(testit$MUKEY)

testit2<- st_transform(testit,"+proj=utm +zone=11 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0")

testit3<-st_crop(testit2,NED)

t<- fasterize(testit, NED, field=testit$MUKEYfun = "last", background = NA_real_,
              by = NULL)
?fasterize

Mapunit_r<-raster(test, res=30, vals=test$MUKEY)
Mapunit_r<-rasterize(test,NED_PRW,test$MUKEY)
?sf()
Mapunit_r<-fasterize(test,r,test$MUKEY)
?fasterize




as.geomerty
sf:::as_Spatial(test)

typeof(test)

typeof(test)
head(test)
vignette(package="raster")
test$MUKEY
?rasterize




test<- spTransform(mapunit,CRS("+proj=utm +zone=11 +datum=NAD83 +units=m
+no_defs +ellps=GRS80 +towgs84=0,0,0"))
