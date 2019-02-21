
install.packages("prism")
library(prism)
library(raster)
library(dplyr)
library(rgdal)
# get prism normals tmean monthly and prcp annual 
prism.path<-"rawdata/prism"
get_prism_normals(type="tmean","800m",mon=c(1,2,3,4,5,6,7,8,9,10,11,12), annual=F)

get_prism_normals(type="ppt","800m",mon=NULL, annual=T)


# load prism data

US_ann_pre<-raster("RAW/prismtmp/PRISM_ppt_30yr_normal_800mM2_annual_bil/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")

# need NED

Pre_ann_PRW <- projectRaster(US_ann_pre, NED_PRW) %>%
  crop(NED_PRW) %>%
  mask(NED_PRW)


m<-quantile(Pre_ann_PRW, probs = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))

hist(Pre_ann_PRW, breaks =m, col=rev(heat.colors(9)), main="hist of quantile breaks annual precip", xlab="precip (mm)")

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
writeRaster(Pre_ann_PRW,"processed_data/Pre_ann_PRW")

cli_PRW <- raster("processed_data/Cli_PRW")

Raster_centroid(Cli_PRW)


