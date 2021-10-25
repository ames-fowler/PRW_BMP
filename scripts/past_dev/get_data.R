packages <- c("FedData", "raster", "prism", "raster","tidyverse", "rgdal", "gdalUtils", "fasterize")
lapply(packages, require, character.only = TRUE)
lapply(packages, library, character.only = TRUE)
# library(FedData)
# library(raster)
# library(prism)
# library(tidyverse)
# library(rgdal)
# library(gdalUtils)
# library(sf)
## ask matt for a standard code block here 

# Set the extent and projection -------------------------------------------
PRW <- shapefile("RAW/extent/PRW_shape") #We haven't used this extent to avoid ext issues 
#PRW_ext<- c(390665.227700,541802.227000,5145863.712500,5275644.958500)

PRW_crs <-crs(PRW)@projargs #'+proj=utm +datum=NAD83 +zone=11'
PRW_ext <- c(380000.00,550000.000,5100000.00,5300000.00)

PRW_ext_shape <- polygon_from_extent(raster::extent(PRW_ext), proj4string=PRW_crs)
#extent(PRW_ext_shape)


# Inital data retrieval  ---------------------------------------------------

#Get the ssurgo data ====================================
SOIL <- get_ssurgo(template=PRW_ext_shape, 
                   label='PRW',
                   force.redo = F)

#Get landcover Data===============================
NLCD <- get_nlcd(template=PRW_ext_shape, 
                 label='PRW',
                 year=2011,
                 force.redo = F)

#Get DEM DATA=========================================
NED <- get_ned(template=PRW_ext_shape, 
               label='PRW',
               force.redo = F)

#Get prism normals tmean monthly and prcp annual at 800m grids  =============================
prism.path <- "RAW/prism"
get_prism_normals(type="ppt","800m", mon=NULL, annual=T)

# Cut and crop data  -------------------------------------------------------
NED <- raster("EXTRACTIONS/PRW/NED/PRW_NED_1.tif")
NLCD <- raster("EXTRACTIONS/PRW/NLCD/PRW_NLCD_2011_landcover.tif") 
US_ann_pre <- raster("RAW/prismtmp/PRISM_ppt_30yr_normal_800mM2_annual_bil/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")%>%
  crop(NED)

writeRaster(US_ann_pre,"processed_data/gis_scratch/PRW_ann_pre.tif")
# test <- raster("processed_data/gis_scratch/PRW_ann_pre.grd")
# plot(test)

soil <- st_read("EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_Mapunits.shp")
AEC_raw<-raster("RAW/AEC/dynamicAec_20180307.tif")  # downloaded manually fromXXXXX

#croping NED down to PRW shape and reprojecting to UTMs ====================
# PRW_NED <- raster::projectRaster(NED, crs=crs(PRW),res=30)
# PRW_NLCD <- raster::projectRaster(NLCD, crs=crs(PRW),method= 'ngb' ,res=30)

### do i want to make this a function 
#PRW_UTM <- function(raster_in, mask_raster){ 

#reporject data - maintain cataorgial or continous data type with interolation  method. "r = "near"" for cat. data  
#
# scratch folder?
#
NED <- gdalwarp("EXTRACTIONS/PRW/NED/PRW_NED_1.tif",dstfile="processed_data/scratch/NED.tif",
                   t_srs='+proj=utm +zone=11 +datum=NAD83',tr=(c(30,30)),output_Raster=TRUE,
                   overwrite=TRUE,verbose=TRUE)

Pre_ann_PRW <- gdalwarp("processed_data/gis_scratch/PRW_ann_pre.tif",
                        dstfile="processed_data/gis_scratch/precip.tif",
                        t_srs='+proj=utm +zone=11 +datum=NAD83',tr=(c(30,30)),
                        output_Raster=TRUE,overwrite=TRUE,verbose=TRUE)

NLCD <- gdalwarp("EXTRACTIONS/PRW/NLCD/PRW_NLCD_2011_landcover.tif",dstfile="processed_data/gis_scratch/test_nlcd.tif", 
                 t_srs='+proj=utm +zone=11 +datum=NAD83',tr=(c(30,30)),
                     r="near",output_Raster=TRUE,overwrite=TRUE,verbose=TRUE)

AEC <- gdalwarp("RAW/AEC/dynamicAec_20180307.tif",dstfile="processed_data/gis_scratch/AEC.tif",
                      t_srs='+proj=utm +zone=11 +datum=NAD83',tr=(c(30,30)),
                      r="near",output_Raster=TRUE,overwrite=TRUE,verbose=TRUE)


### Fix the  sub cell origin issue ----------------- 
origin(NLCD) <- origin(NED)
origin(Pre_ann_PRW) <- origin(NED)
origin(AEC) <- origin(NED)

##### soil data to raster using the st formatting ---------------------------
CRS_PRW <- crs(NED_PRW)
map_p <- st_transform(soil, " +proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
map_p$MUKEY<- as.numeric(as.character(map_p$MUKEY))
r <- raster(ncol=ncol(NED_PRW), nrow=nrow(NED_PRW),vals=1,crs=crs(NED_PRW),res=res(NED_PRW), ext=extent(NED_PRW))
mapunit_r <- fasterize(map_p,r,field = "MUKEY", fun = "first")

### crop to wshed boundary ------------------------------ 
NED_PRW <- NED %>%
  crop(PRW) %>%
  mask(PRW)

NLCD_PRW <- NLCD %>%
  crop(PRW) %>%
  mask(PRW)

Pre_ann_PRW <- Pre_ann_PRW %>%
  crop(PRW) %>%
  mask(PRW)

mapunit_r <- mapunit_r %>%
  crop(PRW) %>%
  mask(PRW)

AEC<- AEC %>%
  crop(PRW) %>%
  mask(PRW)


### get terrain layers --------------
# This also assumes that MPICH2 is properly installed on your machine and that TauDEM command line executables exist
# MPICH2.  Obtain from http://www.mcs.anl.gov/research/projects/mpich2/
# Install following instructions at http://hydrology.usu.edu/taudem/taudem5.0/downloads.html.  
# It is important that you install this from THE ADMINISTRATOR ACCOUNT.

# TauDEM command line executables.  
# If on a PC download from http://hydrology.usu.edu/taudem/taudem5.0/downloads.html
# The install package will install to c:\program files\taudem or c:\program files (x86)\taudem set a 
# system path.  If you want to do this manually you can download the command line executables and place where you wish.
# If on a different system, download the source code and compile for your system.

# Set working directory to your location
#setwd("C:/Users/dtarb/Scratch/Logan")

z=NED_PRW
plot(z)

# Pitremove
system("mpiexec -n 8 pitremove -z -fel processed_data/NED_PRW_fill.tif")
system("mpiexec -n 8 pitremove -z -fel processed_data/gis_scratch/NED_PRW_fill.tif")
fel=raster("processed_data/gis_scratch/")

# D8 flow directions
system("mpiexec -n 8 D8Flowdir -p processed_data/gis_processed/flow_direction.tif 
       -sd8 processed_data/gis_processed/slope_d8_PRW.tif -fel processed_data/NED_PRW_fill.tif",
       show.output.on.console=F,invisible=F)

# Contributing area
system("mpiexec -n 8 AreaD8 -p processed_data/NED_PRW_p.tif -ad8 processed_data/gis_processed/Flow_d_PRW.tif")


####rasters out --------- 

#Write out rasters 

writeRaster(NLCD_PRW,"processed_data/gis_processed/NLCD_PRW.tif", overwrite=F)
writeRaster(NED_PRW,"processed_data/gis_processed/NED_PRW.tif", overwrite=F)
writeRaster(Pre_ann_PRW,"processed_data/gis_processed/Pre_ann_PRW.tif", overwrite=F)
writeRaster(mapunit_r,"processed_data/gis_processed/mapunit_r.tif", overwrite=F)
writeRaster(AEC,"processed_data/gis_processed/ACE_PRW.tif", overwrite=F)

rm(list.files("processed_data/gis_scratch/"))

# AEC <- raster("processed_data/ACE_PRW.tif")
# Pre_ann_PRW <- raster("processed_data/Pre_ann_PRW.tif")
# mapunit_r <- raster("processed_data/mapunit_r.tif")
# NLCD_PRW <- raster("processed_data/PRW_NLCD.tif")
# NED_PRW <- raster("processed_data/NED_PRW.tif")