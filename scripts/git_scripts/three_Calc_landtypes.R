library(dplyr)
library(raster)
library(fasterize)
library(sf)
library(data.table)
library(microbenchmark)
library(prism)
library(raster)
library(rgdal)

##Ssurrgo_AGG_script =======================================

## * gather soil data and define soil depth bins----------------
NED_PRW <- raster("processed_data/gis_processed/NED_PRW.tif") 
### load soil data 

#horizon and component data
chorizon <- fread('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_chorizon.csv')

# only keep some of the columns from the component table
component <- fread('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_component.csv')[,c('mukey','cokey','comppct.r','compname','majcompflag')]

#load restrictions data
restrictions <- fread('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_corestrictions.csv')[,c('cokey','resdept.r')]

# Gather the depth of the minimum Ksat value 
my_chorizon <-  chorizon %>% 
  group_by(cokey)%>%
  slice(which.min(ksat.r))%>%
  dplyr::select(ksat.r,cokey,hzdept.r)

# Gather the maximum depth per component
depth <-  chorizon %>% 
  group_by(cokey)%>%
  slice(which.max(hzdepb.r))%>%
  dplyr::select(cokey,hzdepb.r)

# merge the minimums and depth of the COKEYs 
data_chorizon <- restrictions %>%
  merge(my_chorizon, all.y=T)%>%
  merge(depth, all.y=T)%>%
  rename(ksat_min=ksat.r, hz_ksat_min_depth=hzdept.r, hz_soil_depth=hzdepb.r)

# add in a maximum value for the depth if missing 
data_chorizon$resdept.r[is.na(data_chorizon$resdept.r)]<-158

# build out the compent table with horizon data. 
comp_r<- data_chorizon%>%
  merge(component, all.y=T)
 

#Find NAs - all are Rocks, pits, river wash etc. 
NA_test <- subset(comp_r,is.na(ksat_min)==T)


## Calc soil depth based on below definition. 
comp_r$soil_depth = ifelse(comp_r$ksat_min/0.277778 < 10.0 & comp_r$hz_ksat_min_depth < 50, "shallow",
                           ifelse(comp_r$resdept.r < 100, "medium", "deep"))
fwrite(comp_r, 'processed_data/comp_r.txt')

# * Classifiy soil depths by mukey  -------------
soil_reclass <- cbind(as.numeric(as.character(comp_r$mukey)),
                      comp_r$soil_depth,comp_r$comppct.r,as.character(comp_r$majcompflag),
                      as.character(comp_r$compname)) %>% as.data.frame()

colnames(soil_reclass)<- c("mukey","soil_depth","percent","majcomp","name")
soil_reclass$percent <- as.numeric(as.character(soil_reclass$percent))
soil_reclass$mukey<-as.numeric(as.character(soil_reclass$mukey))

# adding in a tracking layer on the no depth ssurgo layers, and added in compent name 190705 AF
levels(soil_reclass$soil_depth) <- c(levels(soil_reclass$soil_depth),"rock")
soil_reclass$soil_depth[is.na(soil_reclass$soil_depth)==T] <- "rock"
#soil_reclass<- na.omit(soil_reclass)

### need to do this by cells. 
soil_reclass$soil_depth_num <- ifelse(soil_reclass$soil_depth =="shallow", 1,
                                      ifelse(soil_reclass$soil_depth == "medium", 2,
                                             ifelse(soil_reclass$soil_depth =="deep",3,4)))


soil_reclass_temp <- soil_reclass %>% group_by(mukey) %>%
  filter(percent == max(percent))

soil_reclass_conds <- soil_reclass %>% group_by(mukey) %>%
  filter(soil_depth_num == min((soil_depth_num)))


#soil_reclass$soil_depth[is.na(soil_reclass$soil_depth)] <- 0

# * Reclass the MUKEY map to soil depth ---- 
# maximum Component percentage
reclass_m <- data.matrix(soil_reclass_temp[,c(1,6)])
soil_depth_PRW <- reclassify(mapunit_r,reclass_m)#? reclassify
soil_depth_PRW@data@values[soil_depth_PRW@data@values==4] <-NA

#shallowest soils
reclass_m <- data.matrix(soil_reclass_conds[,c(1,6)])
soil_depth_PRW_cons <- reclassify(mapunit_r,reclass_m)#? reclassify
soil_depth_PRW_cons@data@values[soil_depth_PRW@data@values==4] <-NA



rm(my_chorizon,depth,data_chorizon)

# Climate ==========================

Pre_ann_PRW <- raster("processed_data/gis_processed/Pre_ann_PRW.tif")

# * build climate bins ------------
m <- quantile(Pre_ann_PRW, probs = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
m <- matrix(c(m[1:length(m)-1], m[2:length(m)], 1:10),ncol=3)

cli_PRW<- reclassify(Pre_ann_PRW,m,include.lowest=T, right=T)#? reclassify
# plot(cli_PRW)

#find the mean and median centroid 
Raster_centroid<-function(df){
  values<-unique(df)
  # Centroid_mean<- as.data.frame(t(mapply(x=values,
  #                                        function(x) apply(xyFromCell(df, which(df[]==x)),2, FUN=mean)
  # )))
  # Centroid_mean$AEC<-values 
  Centroid_median<- as.data.frame(t(mapply(x=values,
                                           function(x) apply(xyFromCell(df, which(df[]==x)),2, FUN=median)
  )))
  Centroid_median$AEC<-values
  return(Centroid_median)
}

cli_centroids<-Raster_centroid(cli_PRW)

#write out the centriods and the cli_PRW cliamte land type
fwrite(cli_centroids, "processed_data/gis_processed/centroids.csv")
writeRaster(cli_PRW,"processed_data/gis_processed/cli_PRW.tif", overwrite=F)

#cli_PRW <- raster("processed_data/cli_PRW")
# mapunit_r <- raster("processed_data/gis_processed/mapunit_r.tif)

# Combine Climate and Slope ===============================
s<-stack(mapunit_r,cli_PRW)
s_table<-as.data.frame(s, xy = TRUE)%>%
  na.omit()
names(s_table)<-c("x","y","mapunit","cli")

cell_counts<-(s_table[,3:4]) %>% group_by(cli) %>% count(mapunit)  #from maps_by_cli
comp_r_counts <- merge (cell_counts, comp_r, all.x=T, by.x = "mapunit", by.y = "mukey")

#Weights column is the number of cells represented by that component. This may not be a dominate component so if  
# more cells are represented by a non-dominate component then said non-dom soil could represent a dominat soil? could check? 
# There are 4 non dominate soil types selected. these need to be repalced manualy if using WEPPcloud to build soil files. 
comp_r_counts$weights <-comp_r_counts$n*comp_r_counts$comppct.r/100

# * gather the soil type with maximum abundance for each soil depth and climate bin ----------  
soil_groups <- comp_r_counts %>% 
  group_by(cli, soil_depth)%>%
  slice(which.max(weights))%>%
  na.omit()

# * write out soil groups -----------
fwrite(soil_groups, 'processed_data/gis_processed/soil_groups.txt') 
fwrite(s_table, 'processed_data/gis_processed/s_table.txt')


# Management ---------- 
# *Reclass management -----
reclass_df<-c(11,4, #water
              21,1, #urban
              22,1, #urban
              23,1, #urban
              24,1, #urban
              31,2, #barren to grass 
              41,3, #trees
              42,3, #trees
              43,3, #trees
              52,2, # shrub to grass
              71,2, #grass to grass
              81,2, #pasture to grass
              82,6, # crops (continuous)
              90,2, #wet lands to grass
              95,2) #wet lands to grass


reclass_m <- matrix(reclass_df,
                    ncol = 2, 
                    byrow = T)       
PRW_NLCD <- raster("processed_data/gis_processed/NLCD_PRW.tif")
NLCD_class_PRW <- reclassify(as.integer(PRW_NLCD),reclass_m)#? reclassify
# plot(NLCD_class_PRW)

reclass_df<-c(1,1,# Urban 1, 101, 202 
              101,1, # Urban 1, 101, 202 
              102,1, # Urban 1, 101, 202 
              201,1, # Urban 1, 101, 202 
              3,2,   # Rangeland, 3, 103, 203 to grass
              103,2, # Rangeland, 3, 103, 203 to grass
              203,2, # Rangeland, 3, 103, 203 to grass
              4,4, # Forest, 4, 104, 204 to forest
              104,4, # Forest, 4, 104, 204 to forest
              204,4, # Forest, 4, 104, 204 to forest
              5,4,   # Water, 5, 105, 205
              105,4, # Water, 5, 105, 205
              205,4, # Water, 5, 105, 205 
              6,2,   # Wetlands, 6, 106, 206 TO GRASS
              106,2, # Wetlands, 6, 106, 206 TO GRASS
              206,2, # Wetlands, 6, 106, 206 TO GRASS
              7,2,   # Barren, 7, 107, 207 TO GRASS
              107,2, # Barren, 7, 107, 207 TO GRASS 
              207,2, # Barren, 7, 107, 207 TO GRASS
              9,2,   # Wilderness, 9, 109, 209 TO GRASS
              109,2, # Wilderness, 9, 109, 209 TO GRASS
              209,2, # Wilderness, 9, 109, 209 TO GRASS
              11,6,   # Annual, 11, 111, 211 TO wbp
              111,6, # Annual, 11, 111, 211 TO wbp
              211,6, # Annual, 11, 111, 211 TO wbp
              12,7,  # Transition, 12, 112, 212 TO wpf
              112,7, # Transition, 12, 112, 212 TO wpf
              212,7, # Transition, 12, 112, 212 TO wpf
              13,8,  # Grain-fallow, 13, 113, 213 TO wf
              113,8, # Grain-fallow, 13, 113, 213 TO wf
              213,8, # Grain-fallow, 13, 113, 213 TO wf
              14,9,  # Irrigated, 14, 114, 214 TO IRRIGATED/ORCHARD 
              114,9, # Irrigated, 14, 114, 214 TO IRRIGATED/ORCHARD 
              214,9, # Irrigated, 14, 114, 214 TO IRRIGATED/ORCHARD 
              15,9,  # Orchard, 14, 114, 214 TO IRRIGATED/ORCHARD 
              115,9, # Orchard, 14, 114, 214 TO IRRIGATED/ORCHARD
              215,9, # Orchard, 14, 114, 214 TO IRRIGATED/ORCHARD
              50, 9, # Agriculture, 50, 150, 250 TO IRRIGATED/ORCHARD 
              150,9, # Agriculture, 50, 150, 250 TO IRRIGATED/ORCHARD 
              250,9, # Agriculture, 50, 150, 250 TO IRRIGATED/ORCHARD 
              151,4,# Water and Other, 51, 151, 251 to water 
              251,4)# Water and Other, 51, 151, 251


reclass_m <- matrix(reclass_df,
                    ncol = 2, 
                    byrow = T)

AEC_class<- reclassify(AEC,reclass_m)#? reclassify

################ * ncld OVERLAY -------
man_class_PRW <-merge(AEC_class, NLCD_class_PRW, overlap = T)

# Elevation Bins ======================================

slope_PRW <- raster("processed_data/gis_processed/slope_d8_PRW.tif") 


reclass_df<-c(0,0.035,2,
              0.035,0.065,5,
              .065,.10,8,
              .10,.15,12,
              .15,.30,25, ### 9/17/20 add in the 25% slope AF _ check breaks...
              .30,2.00,35)

reclass_m <- matrix(reclass_df,
                    ncol = 3, 
                    byrow = T)       

slope_class_PRW <- reclassify(slope_PRW,reclass_m)#? reclassify
#plot(slope_PRW)
#plot(slope_class_PRW)

#plot(man_class_PRW)

## print Land types===================== 

landtype_brick <- brick(slope_class_PRW,man_class_PRW, cli_PRW, soil_depth_PRW)
names(landtype_brick)<-c("landtype_slope","landtype_man", "landtype_cli", "landtype_soil")

writeRaster(landtype_brick, filename = "processed_data/gis_processed/landtype_210401",bylayer=TRUE, format="GTiff", overwrite=TRUE)
rm(slope_class_PRW,man_class_PRW, cli_PRW, soil_depth_PRW)

names(landtype_brick)<-c("slope_class_PRW","MAN_PRW", "cli_PRW", "soil_depth_PRW")

landtype_data <- as.data.frame(landtype_brick,xy = TRUE) %>% as.data.table()

fwrite(landtype_data, "processed_data/gis_processed/reclass_df_210401.csv", row.names = F)

# ### make inputs -----------------
# 
# 
# 
# soil_cli<-Soil_groups[c(1,2,4,12)] #row shift to get soil depth 11->12 2019/10/02
# 
# soil<-as.data.frame(list.files(path ="processed_data/soil/", pattern = "*.sl",full.names = F))
# names(soil)<- "soil" 
# ###soil$soil[grep("68533", soil$soil)]
# 
# soil_df <- soil%>%as.data.frame()%>%
#   mutate( ofe = 
#             ifelse(grepl("_1",soil),1,3))
# 
# soil_df$mapunit <-mapply(x=soil,  function(x)strsplit(as.character(x),'_'))%>%
#   lapply('[[',1)%>%
#   as.numeric()
# 
# #####soil_cli contains 68559 <- which need to change to #Garfield 68533 as #Garfield  is 35% but most abundant 
# soil_cli$mapunit[soil_cli$mapunit==68559] <- c(68533)
# 
# 
# soil_cli_df <- merge (soil_cli, soil_df)
# soil_cli_df$cli_file<-paste0('cli_zone_', soil_cli_df$cli,'.cli')
# 
# slope<-as.data.frame(list.files(path ="processed_data/slope/", pattern = "*.slp",full.names = F))
# names(slope)<- "slope"
# 
# Slope_df <- slope %>% as.data.frame() %>%
#   mutate( ofe = 
#             ifelse(grepl("300",slope),3,1))
# 
# Man<- as.data.frame(list.files(path ="processed_data/man/", pattern = "*.man",full.names = F))
# names(Man)<- "Man"
# 
# Man_df <- Man %>% as.data.frame() %>%
#   mutate( ofe = 
#             ifelse(grepl("_1",Man),1,3))
# #changed f2 to f3 7.3.2019
# man_code <- c("f1","f3","g1","g3","h1","h3","s1","s3","wbp_mt1","wbp_mt3","wbf_ct1",
#               "wbf_ct3","wbf_mt1","wbf_mt3","wbf_nt1","wbf_nt3", "wbp_ct1", "wbp_ct3", 
#               "wbp_nt1", "wbp_nt3", "wf_ct1", "wf_ct3", "wf_mt1", "wf_mt3", "wf_nt1", "wf_nt3")
# #f = forest, g = grass, h = hermada (srubs), mt = mulch till, nt = notill, ct = conventional till, wbf = wheat barley fallow
# # wbp = wheat barley peas, wf = wheat fallow. 
# 
# 
# Man_df$code<-man_code
# slope_man<-merge (Slope_df, Man_df, by="ofe")
# 
# inputs <- merge(slope_man,soil_cli_df)
# 
# 
# fwrite(inputs,"processed_data/inputs_19_10_15") #with hard coded fix of garfield, see note above. 
# head(inputs)
# 
# inputs<-fread("processed_data/inputs_19_10_15")
# View(inputs)
# 
# 
