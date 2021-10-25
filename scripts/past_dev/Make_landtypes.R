library(FedData)
library(raster)
library(prism)
library(tidyverse)
library(rgdal)
library(gdalUtils)
library(sf)
library(data.table)

##### load data ------------------
AEC <- raster("processed_data/ACE_PRW")
Pre_ann_PRW <- raster("processed_data/Pre_ann_PRW")
mapunit_r <- raster("processed_data/mapunit_r.tif")
NLCD_PRW <- raster("processed_data/PRW_NLCD.tif")
NED_PRW <- raster("processed_data/NED_PRW.tif")

##### Prism - cliamte layer ----------------------

m <- quantile(Pre_ann_PRW, probs = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))

#hist(Pre_ann_PRW, breaks =m, col=rev(heat.colors(10)), main="hist of quantile breaks annual precip", xlab="precip (mm)")
m<- matrix(c(m[1:length(m)-1], m[2:length(m)], 1:10),ncol=3)

Cli_PRW<- reclassify(Pre_ann_PRW,m,include.lowest=T, right=T)#? reclassify

#find the mean and median centroid 
Raster_centroid<-function(df){
  values<-unique(df)
  Centroid_mean<- as.data.frame(t(mapply(x=values,
                                         function(x) apply(xyFromCell(df, which(df[]==x)),2, FUN=mean)
  )))
  Centroid_mean$AEC<-values 
  Centroid_median<- as.data.frame(t(mapply(x=values,
                                           function(x) apply(xyFromCell(df, which(df[]==x)),2, FUN=median)
  )))
  Centroid_median$AEC<-values
  return(Centroid_median)
}

writeRaster(Cli_PRW,"processed_data/Cli_PRW")

Cli_PRW <- raster("processed_data/Cli_PRW")

cli_centroids<-Raster_centroid(Cli_PRW)
fwrite(cli_centroids,"processed_data/centroid")
# plot(x=cli_centroids$x,y=cli_centroids$y, add=T)
# spatial(cli_centroids)=T
# cli_centroids_P <- SpatialPoints(cli_centroids,CRS)

#### management ----------------------------------

reclass_df<-c(10,13,4, #water
              20,25,1, #urban
              30,32,2, #barren to grass
              30,33,10, #rock/other ###AF 2.12.20
              40,50,3, #trees
              51,53,2, # shrub to grass
              70,81.5,2, #pasture to grass
              81.5,82.5,6, # crops (continuos)
              89,96,5) #wet lands
 
reclass_m <- matrix(reclass_df,
                    ncol = 3, 
                    byrow = T)       
PRW_NLCD <- raster("processed_data/PRW_NLCD.tif")
NLCD_class_PRW<- reclassify(as.integer(PRW_NLCD),reclass_m)#? reclassify

###-------------
#Knit NLCD and AEC together 
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
              111,11,
              211,11,
              112,12,
              212,12,
              113,13,
              213,13,
              114,14,
              214,14,
              115,15,
              215,15,
              150,50,
              250,50,
              151,51,
              251,52)

reclass_df<-c(0.5 , 1.5 , 1.0, #urban
              2.4 , 3.5 , 2.0, #range/grass
              3.5 , 4.5 , 3.0, #forest
              4.5 , 5.5 , 4.0, #water
              5.5 , 6.5 , 5.0, #wetlands
              10.5 , 11.5 , 6.0, #annual
              11.5 , 12.5 , 7.0, #transition 
              12.5 , 13.5 , 8.0, #crop fallow 
              13.5 , 15.5 , 9.0) #irrigated ag/orchards



reclass_m <- matrix(reclass_df,
                    ncol = 2, 
                    byrow = T)

AEC3<- reclassify(AEC2,reclass_m)#? reclassify


#plot(NLCD_class_PRW)
unique(NLCD_class_PRW)
MAN_PRW <- raster("Processed_data/MAN_PRW")

unique(MAN_PRW)

reclass_df<-c(0.5 , 1.5 , 1.0, #urban
              2.4 , 3.5 , 2.0, #range/grass
              3.5 , 4.5 , 3.0, #forest
              4.5 , 5.5 , 4.0, #water
              5.5 , 6.5 , 5.0, #wetlands
              10.5 , 11.5 , 6.0, #annual
              11.5 , 12.5 , 7.0, #transition 
              12.5 , 13.5 , 8.0, #crop fallow 
              13.5 , 15.5 , 9.0) #irrigated ag/orchards


reclass_m <- matrix(reclass_df,
                    ncol = 3, 
                    byrow = T)       

man_sim_PRW<- reclassify(MAN_PRW,reclass_m)#? reclassify
#plot(man_sim_PRW)
#unique(man_sim_PRW)
