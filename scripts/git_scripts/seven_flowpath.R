##
##identify and build flow path index 
##

library(raster)
#library(geoR)
library(ggplot2)
library(latticeExtra)
library(tidyverse)
#library(fasterize)
library(sf)
library(data.table)
#library(RSAGA)
library(spatialEco)
library(purrr)

# load shit1  -------------------------------------------------------------
slope_PRW <- raster("processed_data/NED_PF_PRWsd8.tif")  # Slope 
NED_PRW   <- raster("processed_data/NED_PRW.tif")        # DEM raster 
huc12     <- shapefile("RAW/extent/HUC12")               # NHD huc 12s
PRW       <- shapefile("RAW/extent/PRW_shape.shp")       # full PRW
# catch     <- raster("EXTRACTIONS/NED_catch")             # SAGA or Grass catchments CHECK and incoporate
# NED_PRW_p <- as.data.table(rasterToPoints(NED_PRW))      # DEM as table 

# load data ---------------------------------------------------------------

landtype_slope <-raster("processed_data/landtype_091720_1.tif") # slope upper limit increase to add 25 and 35 not just XX200917
landtype_man <-raster("processed_data/landtype_091720_2.tif")
landtype_cli<-raster("processed_data/landtype_091720_3.tif")
landtype_soil<-raster("processed_data/landtype_091720_4.tif")

# landtype_slope <- raster("processed_data/landtype_1.tif")
# landtype_man <- raster("processed_data/landtype_2.tif")
# landtype_cli <- raster("processed_data/landtype_3.tif")
# landtype_soil <- raster("processed_data/landtype_4.tif")


# huc12_r <- raster("processed_data/huc12_r.tif")          # NDH huc 12 as raster 
PRW_curve <- curvature(NED_PRW, "profile")               
writeRaster(PRW_curve,"processed_data/PRW_curve") #2019_10_11 af
PRW_curve <- raster("processed_data/PRW_curve")
names(PRW_curve) <- "curve"
#ang  <- raster("processed_data/NED_PRW_ang.tif")
dem <- raster("processed_data/NED_PRW_fill.tif")     # DEM filled
slp <- raster("processed_data/NED_PRW_slp.tif")      # SLope d8 
p <- raster("processed_data/NED_PRW_p.tif")          # D8 flow direction 
sd8  <- raster("processed_data/NED_PRW_sd8.tif")     # this looks like slope? d8 
ad8 <- raster("processed_data/NED_PRW_ad8.tif")      # flow accumultation d8?
#sca  <- raster("processed_data/NED_PRW_sca.tif")
fd <- terrain(dem, opt = "flowdir")

terrain_brick <- brick(dem,slp,p,ad8,fd,PRW_curve,
                       landtype_slope, landtype_man, 
                       landtype_cli, landtype_soil)

rm(dem,slp,p,ad8,fd,PRW_curve,
   landtype_slope, landtype_man, 
   landtype_cli, landtype_soil)
#crs(terrain_brick) <- "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# Subsetting a test basin ---------------
# plot <- spplot(huc12, zcol = "OBJECTID")
# lables <- layer(sp.text(coordinates(huc12), txt = huc12$OBJECTID, pos=0))
# plot + lables

obj <- (huc12$OBJECTID)
# j = 35 
# j=82 # paradise creek 
# g <- subset(huc12, OBJECTID == obj[j])
# plot(g)
### FUNC ### =-----------------------------

watershed <- function (watershed= wsheds, terrain_brick = terrain_brick){
  ter_crop <- crop(terrain_brick, wsheds) %>%
    mask(wsheds)
  # DEM_crop <- crop(NED_PRW, wsheds[[i]]) %>%
  #   mask(wsheds[[i]])
  
  # Subset all flow accum == 0 ------------ 
  para_ter <- as.data.table(rasterToPoints(ter_crop)) %>%
    mutate(xy = paste0(x,y))
  
  # Walk the points ---------
  starts    <- para_ter %>% subset(para_ter$NED_PRW_ad8==1)
  starts_2  <- para_ter %>% subset(para_ter$NED_PRW_ad8==2)
  stream    <- para_ter %>% subset(para_ter$NED_PRW_ad8>100)
  
  
  ## move to trace function
  xy_list <- trace(start_cells = starts,
                   flow_direction =  ter_crop$flowdir) #xy_list <-  xy_list
  
  ## Check Boundary Isues
  # lots (553) of 2nd cells have start cells outside of the boundary.... 
  #  corner start cells (9 of them fall outside the boundary too) how to fix?
  
  starts    <- starts   %>% subset(!(starts_xy$xy %in% xy_list$xy))
  starts_2  <- starts_2 %>% subset(!(starts_2$xy %in% xy_list$xy))
  
  #plot(starts_xy$x, starts_xy$y)
  
  list_2    <- trace(starts_2, fd)
  list_2    <- list_2 %>% 
    mutate(profile = profile + max(xy_list$profile))
  
  xy_list_t <- rbind(xy_list,list_2) 
  xy_list_t <- xy_list_t %>% 
    subset(!(xy_list_t$xy %in% stream$xy)) %>% subset(steps<16)
  
  
  #### add in other information -------------
  #slope, length, curve?, Aspect, 
  xy_list_t <- xy_list_t %>% 
    merge.data.table(para_ter, all.x = T) %>%
    mutate(duplicate = duplicated(xy_list_t$xy))
  return(xy_list_t)
}

#trace every flow path starting from a 0 accumulation cell. -------

trace <- function(start_cells, flow_direction){
  #require packages (raster)
  list <- list(0)
  i <- 1
  j <- 1
  #tic()
  for(i in seq(1:nrow(start_cells))){ #
    path <- flowPath(flow_direction, 
                     p = c(start_cells$x[i],
                           start_cells$y[i]))
    
    #plot(ter_brick_crop$NED_PRW_slp)
    if(is.null(path)==F){
      xy_temp <- xyFromCell(flow_direction,path)
      # add profile steps and prof ID 
      
      xy_temp <- list(x = xy_temp[,1], 
                      y = xy_temp[,2] ,
                      profile = rep(j,length(xy_temp)/2),
                      steps = (seq(1,length(xy_temp)/2)),
                      path = path) 
      list[[j]] <- as.data.table(xy_temp)
      j = j +1
    }
  }
  #toc()
  xy_list = data.table::rbindlist(list) %>% mutate(xy  = paste0(x,y))
  return(xy_list)
}
getmode <- function(v) { ###https://www.tutorialspoint.com/r/r_mean_median_mode.htm
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

profile_sum <- function(prof_list=xy_list_t){
  short_list <- prof_list %>% group_by(profile, huc12) %>%
    summarise(steps          = steps       %>% max(), 
              slope_max      = NED_PRW_slp %>% max(na.rm = T), 
              aspect         = NED_PRW_p   %>% mean(na.rm = T),
              accum          = NED_PRW_ad8 %>% max(na.rm = T),
              curve_max      = curve       %>% max(na.rm = T),
              curve_min      = curve       %>% min(na.rm = T),
              landtype_slope = landtype_slope %>% max(na.rm = T), #run as median but shiuld be max () 
              landtype_man   = landtype_man   %>% median(na.rm = T) %>% floor(),
              landtype_soil  = landtype_soil  %>% min(na.rm = T),
              landtype_cli   = landtype_cli   %>% min(na.rm = T),
              duplicate      = duplicate   %>% sum())
  return(short_list)
}


# 34,57 not wsheds, arrifact in NHD?
k = 39
### solve -------------------------

for(k in seq(1,length(obj)-1,1)[-c(34, 57)]){  #length(obj)-1
   wsheds <- subset(huc12, OBJECTID == obj[k]) #wsheds <- xy_list
   wshed_prof <- watershed(wsheds,terrain_brick) #wshed_prof <- xy_list_t
   wshed_sum <- profile_sum(wshed_prof)
   fwrite(wshed_prof, paste0("processed_data/gis_processed/wshed_tr_",k))
   fwrite(wshed_sum, paste0("processed_data/gis_processed/wshed_sum_",k,".txt"))
   rm(wshed_prof, wsheds)
}

 
k <- "PRW_21_06_28"
wsheds <- PRW
wshed_prof <- watershed(wsheds,terrain_brick) #wshed_prof <- xy_list_t
wshed_sum <- profile_sum(wshed_prof)
fwrite(wshed_prof, paste0("processed_data/gis_processed/wshed_tr_",k))
fwrite(wshed_sum, paste0("processed_data/gis_processed/wshed_sum_",k,".txt"))
rm(wshed_prof, wsheds)



# fixing the parameters issues 21-06-15 -----------
# in the sumation of the wshed data. 210614
# brick and raster to points corrected land type data. 
# will add slope type too, S-curve, straight, concave, convex. 

# terrain_brick <- brick(dem,slp,p,ad8,fd,PRW_curve,
#                          +                        landtype_slope, landtype_man, 
#                          +                        landtype_cli, landtype_soil)
# 
# 
# para_ter <- as.data.table(rasterToPoints(terrain_brick)) %>%
#   +     mutate(xy = paste0(x,y))
# 
# names(para_ter) <- c("x","y","dem","NED_PRW_slp","NED_PRW_p","NED_PRW_ad8","fd","curve",
#                                              "landtype_slope", "landtype_man", 
#                                              "landtype_cli", "landtype_soil", "xy")
# # pull back in full PRW xy tabe  
# wshed_flow <- fread(file = "processed_data/gis_processed/whsed_tr_PRW_092320")
# 
# xy_list_t2 <- wshed_flow[,c(1:6,18)]
# 
# xy_list_t2 <- xy_list_t2 %>%                  #
#   merge.data.table(para_ter, by = "xy", all.x = T, no.dups =T) %>%
#   mutate(duplicate = duplicated(xy_list_t2$xy))
# 
# xy_list_t2 <- xy_list_t2 %>%
#   subset(., select = -c(x.y, y.y)) %>%
#   rename(., x = x.x)%>%
#   rename(., y = y.x)
#
#
#
# fwrite(xy_list_t2, file = "processed_data/gis_processed/whsed_tr_PRW_210614")
# 
# short_list <- xy_list_t2  %>% group_by(huc12, profile) %>%
#   summarise(steps = steps %>% max(),
#             slope_max = NED_PRW_slp %>%
#               max(na.rm = T),
#             aspect = NED_PRW_p  %>%  mean(na.rm = T),
#             accum = NED_PRW_ad8  %>% max(na.rm = T),
#             curve_max = curve  %>%   max(na.rm = T),
#             curve_min = curve  %>%   min(na.rm = T),
#             landtype_slope = landtype_slope %>% max(na.rm = T), #run as median but shiuld be max () 210614
#             landtype_man = landtype_man  %>% median(na.rm = T) %>% floor(), # for all even number of steps median gives average values == problem
#             landtype_soil = landtype_soil %>% min(na.rm = T),
#             landtype_cli = landtype_cli %>% min(na.rm = T),
#             duplicate = duplicate %>% sum())
# 
# 
# fwrite(short_list, file = "processed_data/gis_processed/whsed_tr_sum_210614")


# merging rasters 

# 
# t1 <- fread(paste0("processed_data/gis_processed/wshed_sum_",1))
# t2 <- fread(paste0("processed_data/gis_processed/wshed_tr_",2))
# 
# dt_test <- t1[t2,]
# rbind(t2, t2)
