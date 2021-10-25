
#code from 
#https://casoilresource.lawr.ucdavis.edu/software/r-advanced-statistical-package/aggregating-ssurgo-data-r/
#3/18/19

install.packages("dplyr")
install.packages("fasterize")


# need this for ddply()
library(dplyr)
library(raster)
library(fasterize)
library(sf)
library(data.table)
library(microbenchmark)

# load horizon and component data
#chorizon <- read.csv('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_chorizon.csv')
chorizon <- fread('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_chorizon.csv')

# only keep some of the columns from the component table
component <- fread('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_component.csv')[,c('mukey','cokey','comppct.r','compname','majcompflag')]

#load corestrictions data

restrictions <- fread('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_corestrictions.csv')[,c('cokey','resdept.r')]

my_chorizon <-  chorizon %>% 
  group_by(cokey)%>%
  slice(which.min(ksat.r))%>%
  dplyr::select(ksat.r,cokey,hzdept.r)

depth <-  chorizon %>% 
  group_by(cokey)%>%
  slice(which.max(hzdepb.r))%>%
  dplyr::select(cokey,hzdepb.r)


data_chorizon <- restrictions %>%
  merge(my_chorizon, all.y=T)%>%
  merge(depth, all.y=T)%>%
  rename(ksat_min=ksat.r, hz_ksat_min_depth=hzdept.r, hz_soil_depth=hzdepb.r)
  
data_chorizon$resdept.r[is.na(data_chorizon$resdept.r)]<-158
  
comp_r<- data_chorizon%>%
  merge(component, all.y=T)
head(comp_r)  

test<- data_chorizon%>%
  merge(component, all.y=T)
head(test)  

#Find NAs - all are Rocks, pits, river wash etc. 
NA_test <- subset(comp_r,is.na(ksat_min)==T)
#View(NA_test) 

comp_r$soil_depth = ifelse(comp_r$ksat_min/0.277778 < 10.0 & comp_r$hz_ksat_min_depth < 50, "shallow",
                        ifelse(comp_r$resdept.r < 100, "medium", "deep"))
#write out comp_r for future use ------------------
fwrite(comp_r, 'processed_data/comp_r.txt')
#test <- microbenchmark(c(

#dumb mapping and old scripts  ------------------
#hist(as.integer(na.omit(comp_r$ksat_min))/0.277778, breaks=100)


#comp_r$ksat_min/0.277778 < 10
#(comp_r)
# plot1<-ggplot(comp_r)+
#   geom_histogram(aes(x=comp_r$hz_soil_depth, col =comp_r$soil_depth))
# 

# For	the	HCT,	the	“shallow”	soils	in	the	eastern	region	were	therefore	defined	
# as	those	identified	to	have	a	saturated	hydraulic	conductivity	less	than	10	mm/hr	
# within	the	upper	0.5	m	of	the	soil	profile.	“Moderate”	soils	were	defined	as	soils	having 24
# a	restrictive	layer	at	a	depth	of	less	than	1	m
# 
rm(my_chorizon,depth,data_chorizon)

#comp_r$soil_depth


#hist(comp_r$resdept.r)
#hist(comp_r$ksat_min)  #um/s = 8.64 cm/day = 0.864 m/day
#hist(comp_r$hz_soil_depth)
#hist(comp_r$comppct.r)

#typeof(comp_r$resdept.r)

# Manage soil shape file - fail  ------------------------------------------
# this was actually completed in Arc GIS

PRW_shape<- shapefile("RAW/extent/PRW_shape.shp")

mapunit<-shapefile("EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_Mapunits.shp")
map <- st_read("EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_Mapunits.shp")

CRS_PRW <- crs(NED_PRW)
map_p <- st_transform(map, " +proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
map_p$MUKEY<- as.numeric(as.character(map_p$MUKEY))
r <- raster(ncol=ncol(NED_PRW), nrow=nrow(NED_PRW),vals=1,crs=crs(NED_PRW),res=res(NED_PRW), ext=extent(NED_PRW))
tic()
mapunit_r <- fasterize(map_p,r,field = "MUKEY", fun = "first")
toc()

#mapunit_r<- spTransform(mapunit,CRS("+proj=utm +zone=11 +datum=NAD83 +units=m
#+no_defs +ellps=GRS80 +towgs84=0,0,0"))%>%
 # crop(NED_PRW)

# test the rasterize
#ample<-extent(c(390674,391874,5145869,5147069))
#smalltest <- crop(test,sample)
#smallR<-crop(NED_PRW,sample)
#small_r<-rasterize(smalltest,smallR,smalltest$MUKEY)
#plot(small_r)
#plot(smalltest, col=smalltest$MUKEY)

#requires to run get NED.R prior
#r <- raster(ncol=ncol(NED_PRW), nrow=nrow(NED_PRW),vals=1,crs=crs(NED_PRW),res=res(NED_PRW), ext=extent(NED_PRW))

#Mapunit_r<-rasterize(mapunit_r,r,field=4)
#Mapunit_r<-rasterize(mapunit_r, res=30, vals=mapunit_r$MUKEY) #slow

#Mapunit_r<-rasterize(test,NED_PRW,test$MUKEY)

#writeRaster(Mapunit_r, "sratchspace/Mapunit_r.tif")
Mapunit_r_C<- mask(mapunit_r, NED_PRW)

writeRaster(Mapunit_r_C, "processed_data/Mapunit_r_7-3_test.tif", overwrite=T)###Rename this in processed data when convinced it works. 
Mapunit_r_C <- raster("processed_data/Mapunit_r_7-3_test.tif")
plot(Mapunit_r_C)


plot(Mapunit_r_test)

###help
#mapit<-raster("processed_data/mapunit_2.tif")
#mapit2<-mask(mapit,NED_PRW)

# switch_att <- function (r, att){
#   r[] <- levels(r)[[1]][values((r)),att]
#   r
# }
# mapit3<- switch_att(mapit2,"MUKEY")
# 
# plot(mapit3)
# testit<- as.data.frame(mapit3)
#?sf()
##Mapunit_r<-fasterize(test,r,test$MUKEY)
#?fasterize

###
# # 
# as.geomerty
# sf:::as_Spatial(test)
# 
# typeof(test)
# 
#  typeof(test)
# head(test)
# vignette(package="raster")
# test$MUKEY
# ?rasterize
# 
# 
# 
# 
# test<- spTransform(mapunit,CRS("+proj=utm +zone=11 +datum=NAD83 +units=m
# +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# 
# 
# test.2<-crop(test,PRW_shape)
# plot(test.2)
# 
# ?spTransform
# (mapunit)
# 
# # custom function for calculating a weighted mean
# # values passed in should be vectors of equal length
# wt_mean <- function(property, weights)
#   {
#   # compute thickness weighted mean, but only when we have enough data
#   # in that case return NA
#   
#   # save indices of data that is there
#   property.that.is.na <- which( is.na(property) )
#   property.that.is.not.na <- which( !is.na(property) )
#   
#   if( length(property) - length(property.that.is.na) >= 1)
#     prop.aggregated <- sum(weights[property.that.is.not.na] * property[property.that.is.not.na], na.rm=TRUE) / sum(weights[property.that.is.not.na], na.rm=TRUE)
#   else
#     prop.aggregated <- NA
#   
#   return(prop.aggregated)
# }
# 
# wt_min <- function(property)
# {
#   # compute min, but only when we have enough data
#   # in that case return NA
#   
#   # save indices of data that is there
#   property.that.is.na <- which( is.na(property) )
#   property.that.is.not.na <- which( !is.na(property) )
#   
#   if( length(property) - length(property.that.is.na) >= 1)
#     prop.aggregated <- min(property[property.that.is.not.na])
#   else
#     prop.aggregated <- NA
#   
#   return(prop.aggregated)
# }
# 
# profile_total <- function(property, thickness)
# {
#   # compute profile total
#   # in that case return NA
#   
#   # save indices of data that is there
#   property.that.is.na <- which( is.na(property) )
#   property.that.is.not.na <- which( !is.na(property) )
#   
#   if( length(property) - length(property.that.is.na) >= 1)
#     prop.aggregated <- sum(thickness[property.that.is.not.na] * property[property.that.is.not.na], na.rm=TRUE)
#   else
#     prop.aggregated <- NA
#   
#   return(prop.aggregated)
# }
# 
# # define a function to perfom hz-thickness weighted aggregtion
# component_level_aggregation <- function(i)
# {
#   
#   # horizon thickness is our weighting vector
#   hz.thick <- i$hzdepb.r - i$hzdept.r
#   
#   # compute wt.mean aggregate values
#   clay <- wt_mean(i$claytotal.r, hz.thick)
#   silt <- wt_mean(i$silttotal.r, hz.thick)
#   sand <- wt_mean(i$sandtotal.r, hz.thick)
#   ksat_avg <- wt_mean(i$ksat.r, hz.thick)
#   OM   <- wt_mean(i$om.h, hz.thick)
#   #BD   <- wt_mean(dbovendry.r, hz.thick)
#   
#   # compute wt.min aggregate values
#  # Ksat_min <- wt_min(i$ksat.r)
#   
#   # compute wt.max aggregate values
#   #Ksat_max <- wt_min(i$ksat.r)
#   
#   # compute profile sum values
#   water_storage <- profile_total(i$awc.r, hz.thick)
#   
#   # make a new dataframe out of the aggregate values
#   d <- data.frame(cokey=unique(i$cokey), clay=clay, silt=silt, sand=sand, ksat_avg=ksat_avg, OM=OM, water_storage=water_storage)
#   
#   return(d)
# }
# 
# mapunit_level_aggregation <- function(i)
# {
#   # component percentage is our weighting vector
#   comppct <- i$comppct_r
#   
#   # wt. mean by component percent
#   clay <- wt_mean(i$clay, comppct)
#   silt <- wt_mean(i$silt, comppct)
#   sand <- wt_mean(i$sand, comppct)
#   water_storage <- wt_mean(i$water_storage, comppct)
#   
#   # make a new dataframe out of the aggregate values
#   d <- data.frame(mukey=unique(i$mukey), clay=clay, silt=silt, sand=sand, water_storage=water_storage)
#   
#   return(d)
# }
# 
# wt_mean <- function(property, weights)
# {
#   # compute thickness weighted mean, but only when we have enough data
#   # in that case return NA
#   
#   # save indices of data that is there
#   property.that.is.na <- which( is.na(property) )
#   property.that.is.not.na <- which( !is.na(property) )
#   
#   if( length(property) - length(property.that.is.na) >= 1)
#     prop.aggregated <- sum(weights[property.that.is.not.na] * property[property.that.is.not.na], na.rm=TRUE) / sum(weights[property.that.is.not.na], na.rm=TRUE)
#   else
#     prop.aggregated <- NA
#   
#   return(prop.aggregated)
# }
# 
# # aggregate horizon data to the component level
# chorizon.agg <- ddply(chorizon, .(cokey), .fun=component_level_aggregation, .progress='text')
# 
# # join up the aggregate chorizon data to the component table
# comp.merged <- merge(component, chorizon.agg, by='cokey')
# 
# # aggregate component data to the map unit level
# component.agg <- ddply(comp.merged, .(mukey), .fun=mapunit_level_aggregation, .progress='text')
# 
# # save data back to CSV
# write.csv(component.agg, file='something.csv', row.names=FALSE)
