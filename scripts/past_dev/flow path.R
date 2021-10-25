##
##identify and build flow path index 
##

library(raster)
library(geoR)
library(ggplot2)
library(latticeExtra)
library(tidyverse)
library(fasterize)
library(sf)
library(data.table)
library(RSAGA)
library(spatialEco)
library(purrr)



# load shit1  -------------------------------------------------------------
slope_PRW <- raster("processed_data/NED_PF_PRWsd8.tif")  # Slope ??
NED_PRW <- raster("processed_data/NED_PRW.tif")          # DEM raster 
huc12 <- shapefile("RAW/extent/HUC12")                   # NHD huc 12s
PRW <- shapefile("RAW/extent/PRW_shape.shp")             # full PRW
catch <- raster("EXTRACTIONS/NED_catch")                 # SAGA or Grass catchments CHECK and incoporate
NED_PRW_p <- as.data.table(rasterToPoints(NED_PRW))      # DEM as table 


# junk1 -------------------------------------------------------------------


# reproject data
#streams_utm <- spTransform(streams,
#                             crs(huc12))%>%crop(PRW)
#streams_utm <- crop(streams_utm,PRW)


#huc12_r <- rasterize(huc12, NED_PRW) ## ao slow
#streams_R <- rasterize(streams_utm, NED_PRW, field = 'FlowDir') ## ao slow
#writeRaster(streams_R,"processed_data/PRW_streams.tif") #2019_10_11 af
#writeRaster(huc12_r,"processed_data/huc12_r.tif") #2019_10_11 af


# streams_R <- raster("processed_data/PRW_streams.tif")

# plot <- spplot(huc12, zcol = "OBJECTID")
# lables <- layer(sp.text(coordinates(huc12), txt = huc12$OBJECTID, pos=0))
# plot + lables

# j=19
# i <- (huc12$OBJECTID)
# g <- subset(huc12, OBJECTID == i[j])
# Kam_NED <- NED_PRW %>% crop(g) %>% mask(g)
# Kam_sd8 <- sd8 %>% crop(g) %>% mask(g)
# Kam_streams <- streams_R %>% crop(g)%>% mask(g)
# Kam_huc12 <- huc12_r %>% crop(g)%>% mask(g)
#plot(Kam_huc12)
#plot(Kam_streams)
# #??rasterize()
# k_streams_R <- streams_R %>% crop(g) %>% mask(g)
# #plot(k_streams_R)
# k_stream_cells <- rasterToPoints(k_streams_R)
# k_stream_cells <- as.data.table(k_stream_cells)[,c("x","y")][
#   ,c("xy"):=list(paste0(x,y))]

# load shit2 ---------------------------------------------------------------

huc12_r <- raster("processed_data/huc12_r.tif")          # NDH huc 12 as raster 
PRW_curve <- curvature(NED_PRW, "profile")               
writeRaster(PRW_curve,"processed_data/PRW_curve") #2019_10_11 af
PRW_curve <- raster("processed_data/PRW_curve")

#ang  <- raster("processed_data/NED_PRW_ang.tif")
slp <- raster("processed_data/NED_PRW_slp.tif")           # SLope too _ check 
p <- raster("processed_data/NED_PRW_p.tif")               # this looks like 1-8 direction 
sd8  <- raster("processed_data/NED_PRW_sd8.tif")          # this looks like slope? d8 
ad8 <- raster("processed_data/NED_PRW_ad8.tif")           # flow accumultation d8?
#sca  <- raster("processed_data/NED_PRW_sca.tif")


r_brick <- brick(NED_PRW, slp, p,                         # put all the rasters together to save time 
                 sd8, ad8, PRW_curve, huc12_r,catch)      # DEM, slope, flow direction, slope, flow accum, huc12, catch)    

#raster to datatable
master_t <- as.data.table(rasterToPoints(r_brick))[       # making the bricka  table 
  ,c("xy"):=list(paste0(x,y))]
fwrite(master_t, "processed_data/raster_points.csv")      # Writing out the table 

####################################################################Can start here 
master_t <- fread("processed_data/raster_points.csv")
master_t <- na.omit(master_t)                             # chopping out all the area out of PRW 

#t <- as.data.table(rasterToPoints(ad8)) ### PRW FLOW ACCUNULATION 
##t_stream <- as.data.table(rasterToPoints(ad8>=100)) ### PRW FLOW ACCUNULATION 
##t2 <- as.data.table(rasterToPoints(ad8==2))[layer==1] ### second cell flows for catching issues 
# p_p <- as.data.table(rasterToPoints(p)) #PRW FLOW DIRECTION
# huc12_p <- as.data.table(rasterToPoints(huc12_r)) #PRW WATERSHED
# slp_p <- as.data.table(rasterToPoints(slp))[,c("x","y","xy"):=list(x,y,paste0(x,y))]
# curve_P <- as.data.table(rasterToPoints(PRW_curve))[,c("x","y","xy"):=list(x,y,paste0(x,y))]


master_t <- master_t[NED_PRW_p %in% 1,c("dx","dy"):=list(30,0)][ #calculate the dx and dy coordinate change out 
  NED_PRW_p %in% 2,c("dx","dy"):=list(30,30)][                   # side of the loop - could change 30 to res(p_p)     
    NED_PRW_p %in% 3,c("dx","dy"):=list(0,30)][
      NED_PRW_p %in% 4, c("dx","dy"):=list(-30,30)][
        NED_PRW_p %in% 5,c("dx","dy"):=list(-30,0)][
          NED_PRW_p %in% 6,c("dx","dy"):=list(-30,-30)][
            NED_PRW_p %in% 7,c("dx","dy"):=list(0,-30)][
              NED_PRW_p %in% 8,c("dx","dy"):=list(30,-30)]
# 
#test<- master_t[,"NED_catch"]                                   # A way to check catchment cell numbers
#View(test[, .N, by=NED_catch])                                  # smallest = 1 (weird) largest 118982 (also weird)

sub_t <- master_t[NED_catch %in% 1278] # nine cell wshed for the check... 
t <- sub_t
## finding all bounaries                        #########################  CHECK  ########################
#writeRaster(catch, "processed_data/NED_catch.tif", overwrite=T)
# streams <- shapefile("EXTRACTIONS/PRW/NHD/PRW_NHD_Flowline") 

#streams <- st_read("EXTRACTIONS/PRW/NHD/PRW_NHD_Flowline.shp")
# write.sgrd(data = NED_PRW, file = "processed_data/dem", header = dem$header,
#            env = env)


# test<- merge(huc12_p,p_p)                                                      # add in the move
# test <- test[,c("xdx","ydy","index"):=list(x+dx,y+dy, seq(1:nrow(test)))]      # take a step and add an index
# test2 <- test[,c("xdx", "ydy","index")]                                        # remove old coordinates and steps
# names(test2) <- c("x", "y","index")                                            # rename
# test3 <- test[,1:3]                                                            # subset test
# test4 <- merge(test2, test3, by=c('x','y'))[,3:4]                              # sub set the lager
# names(test4) <- c("index", "layer_t")                                          # fix names
# test5 <- merge(test, test4, by = "index")                                      # ???
# 
# hp_fix <- test5[,c("x","y","layer_t","NED_PRW_p","dx","dy")]
# # 
# # hp_fix_K <-hp_fix[layer_t ==19] 
# # str_k <- k_stream_cells
# # str_k <- str_k[,c("x","y","xy"):=list(x,y,paste0(x,y))]#[, names(str_k) := lapply(.SD, as.integer)]
# 
# names(t) <- c("x","y","flow_s")
# t <- merge(test5, t, by=c('x','y')) 
# 
# rm(test,test2,test3,test4,test5)
# t_k <- t[layer_t == 19,]


# define streams and starts -----------------------------------------------

stream_cells <- t[NED_PRW_ad8>100,][,c("x","y","xy","profile"):=list(x,y,paste0(x,y),"s")]

ggplot(data =  t, aes(x = x, y = y)) + 
  geom_tile(aes(fill = t$NED_PRW_ad8), colour = "white")

t_t <- t[NED_PRW_ad8==1,][
  ,c("x","y","xy","profile","next_id"):=list(x,y,paste0(x,y),seq_along(x),seq_along(x))][
    ,c("x","y","xy","profile","next_id")]
# t_k_int<- t_k[, names(t_k) := lapply(.SD, as.integer)]
# t_0 <- t_t[,c("x","y")][,profile := seq_along(x)]
# t_0_s <- t[1:15,][,c("x","y","xy"):=list(x,y,paste0(x,y))][
#   , next_id:=min(profile),by=xy] 
# hp_k <- hp_fix_K[,c(1:2,5:6)]
hp <- t[,c(11:13)]


# workit out --------------------------------------------------------------
# t_1 <- merge(t_t,hp, all.x=T)[
#   ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
#     , next_id:=min(profile),by=xy][
#       , c("dx","dy"):=NULL] 
# t_0_s <- t_t[,next_id := ifelse(t_1$xy %in% cell_list$xy,cell_list[cell_list$xy%in%t_1$xy]$profile,t_t$next_id)]
# t_1 <- t_1 [ ((!t_1$xy %in% cell_list$xy)),]
# cell_list <- unique(rbind(cell_list,t_1[,c(12,1)]))

# t_1 <- merge(t_0_s,hp_k, all.x=T)[
#   ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
#     , next_id:=min(profile),by=xy][
#       ,c("x","y","xy","profile","next_id")]
# t_0_s <- t_0_s[,next_id := ifelse(t_1$xy %in% cell_list$xy,cell_list[cell_list$xy%in%t_1$xy]$profile,t_0_s$next_id)]
# t_1 <- t_1 [ ((!t_1$xy %in% cell_list$xy)),]
# cell_list <- unique(rbind(cell_list,t_1[,3:4]))
# 
# t_2 <- merge(t_1,hp_k, all.x=T)[
#   ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
#     , next_id:=min(profile),by=xy][
#       ,c("x","y","xy","profile","next_id")]
# t_1 <- t_1[,next_id := ifelse(t_2$xy %in% cell_list$xy,cell_list[cell_list$xy%in%t_2$xy]$profile,t_1$next_id)]
# t_2 <- t_2 [ ((!t_2$xy %in% cell_list$xy)),]
# cell_list <- unique(rbind(cell_list,t_2[,3:4]))
# 
# t_3 <- merge(t_2,hp_k, all.x=T)[
#   ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
#     , next_id:=min(profile),by=xy][
#       ,c("x","y","xy","profile","next_id")]
# t_2 <- t_2[,next_id := ifelse(t_3$xy %in% cell_list$xy,cell_list[cell_list$xy%in%t_3$xy]$profile,t_2$next_id)]
# t_3 <- t_3 [ ((!t_3$xy %in% cell_list$xy)),]
# cell_list <- unique(rbind(cell_list,t_3[,3:4]))
# 
# t_4 <- merge(t_3,hp_k, all.x=T)[
#   ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
#     , next_id:=min(profile),by=xy][
#       ,c("x","y","xy","profile","next_id")]
# t_3 <- t_3[,next_id := ifelse(t_4$xy %in% cell_list$xy,cell_list[cell_list$xy%in%t_4$xy]$profile,t_3$next_id)]
# t_4 <- t_4 [ ((!t_4$xy %in% cell_list$xy)),]
# cell_list <- unique(rbind(cell_list,t_4[,3:4]))
# 
# 
# t_5 <- merge(t_4,hp_k, all.x=T)[
#   ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
#     , next_id:=min(profile),by=xy][
#       ,c("x","y","xy","profile","next_id")]
# t_4 <- t_4[,next_id := ifelse(t_4$xy %in% cell_list$xy,cell_list[cell_list$xy%in%t_5$xy]$profile,t_4$next_id)]
# t_5 <- t_5 [ ((!t_5$xy %in% cell_list$xy)),]
# cell_list <- unique(rbind(cell_list,t_5[,3:4]))
# 
# t_6 <- merge(t_5,hp_k, all.x=T)[
#   ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
#     , next_id:=min(profile),by=xy][
#       ,c("x","y","xy","profile","next_id")]
# t_5 <- t_5[,next_id := ifelse(t_5$xy %in% cell_list$xy,cell_list[cell_list$xy%in%t_6$xy]$profile,t_5$next_id)]
# t_6 <- t_6 [ ((!t_6$xy %in% cell_list$xy)),]
# cell_list <- unique(rbind(cell_list,t_6[,3:4]))
# 
# t_7 <- merge(t_6,hp_k, all.x=T)[
#   ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
#     , next_id:=min(profile),by=xy][
#       ,c("x","y","xy","profile","next_id")]
# t_6 <- t_6[,next_id := ifelse(t_6$xy %in% cell_list$xy,cell_list[cell_list$xy%in%t_7$xy]$profile,t_6$next_id)]
# t_7 <- t_7 [ ((!t_7$xy %in% cell_list$xy)),]
# cell_list <- unique(rbind(cell_list,t_7[,3:4]))
# #Add stream cells and boundary cells to
# 
# points(t_4$x,t_4$y)
# ###xlim=c(444030,444290), ylim=c(5218210,5218420)
# 
# # tic()
# # t_0_sf <- t_0[1:5,][,c("x","y","xy"):=list(x,y,paste0(x,y))]
# # toc()

# work --------------------------------------------------------------------

walk <- function(xy_list,dxdy_list){                                 # this function walks down a DEM starting from
   step <- merge(xy_list,dxdy_list, all.x=T)[                        # flow accumulation == 1 (does it??? )         
    ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
      , next_id:=min(profile),by=xy][
        , c("dx","dy"):=NULL]
  
  return(step)
}  
# t_1 <- merge(t_t,hp, all.x=T)[
#   ,c("x","y","xy"):=list(x+dx,y+dy,paste0((x+dx),(y+dy)))][
#     , next_id:=min(profile),by=xy][
#       , c("dx","dy"):=NULL] 
# t_0_s <- t_t[,next_id := ifelse(t_1$xy %in% cell_list$xy,cell_list[cell_list$xy%in%t_1$xy]$profile,t_t$next_id)]
# t_1 <- t_1 [ ((!t_1$xy %in% cell_list$xy)),]
# cell_list <- unique(rbind(cell_list,t_1[,c(12,1)]))

tic()
cell_list <- stream_cells[,list(profile,xy)]
step <- list(t_t)
step[[1]]$it <- 1
step[[1]]$next_it <- 0
#step[[1]][,c("dx","dy"):=NULL]
#cell_list <- unique(rbind(cell_list,step[[1]][,c("profile","xy")]))
i=2
i=12

for (i in seq(2,50,1)){  ###need to know which cell the next cell goes to - a flag - for where a profile hit the old profile. 
  step[[i]]<- walk(step[[i-1]],hp)[                                            # take a step x+dx y+dy 
    ,it:=list(i)]
  step[[i-1]] <- step[[i-1]][,c("next_id","next_it"):= list(                   # look back a step, to where we just were - see if we are currentlly in a stream cell or repeated cell.
                                                                               # Lable the next profile id and the cell of intersection 
    ifelse(step[[i]]$xy %in% cell_list$xy , 
           cell_list[cell_list$xy%in%step[[i]]$xy]$profile , 
           step[[i-1]]$next_id),
    
    ifelse(step[[i]]$xy %in% cell_list$xy ,
           cell_list[cell_list$xy%in%step[[i]]$xy]$xy , 
           0)
    )]
  step[[i]] <- step[[i]] [((!step[[i]]$xy %in% cell_list$xy)),]               # check that our current cells are not in a know cell and remove if true
  #cell_list <- unique(rbind(cell_list,step[[i]][,c("profile","xy")]))         # add current cells to the cell list. 
  if(nrow(step[[i]])==0){
    print(i)
    break
  }
}

toc()


all_steps <- rbindlist(step, use.names = T)
sum(is.na(all_steps$next_it))

all_steps <- all_steps[,count:=1][, node:=ifelse(next_id == profile|next_id =="s",0,1)]
# fwrite(all_steps,"processed_data/all_steps_11-16.csv")
# all_steps <- fread("processed_data/all_steps_11-16.csv")

#grab list segments

node_list <- all_steps[node==1]

i=1
seg_list <- all_steps[
  profile == node_list[i,next_id]&it>=all_steps[profile == node_list[i,next_id]&xy==node_list[i,next_it],it]][
    ,profile:=node_list[i,profile]
  ]
          

i=2
for(i in seq(2,nrow(node_list))){
  seg_list_temp <-(all_steps[
    profile == node_list[i,next_id]&it>=all_steps[profile == node_list[i,next_id]&xy==node_list[i,next_it],it]][
      ,profile:=node_list[i,profile]
      ]
    )
  
  seg_list <- list(seg_list, seg_list_temp)
}

test <- rbindlist(seg_list)
#fwrite(test,"processed_data/profile_data_11-16.csv")
test <- fread("processed_data/profile_data_11-16.csv")

flowpath_metrics <- test[,.(max.slp=max(NED_PRW_slp),max.curve=max(layer),curve=sum(layer),min.curve=min(layer),length=length(it)), by=profile]
# fwrite(flowpath_metrics,"processed_data/flowpath_metrics_11-16.csv")
# test_met <- fread("processed_data/flowpath_metrics_11-16.csv")

# Testing data  -----------------------------------------------------------
#look at the data 
hist(flowpath_metrics$max.slp*45) #log normal 
hist(log(flowpath_metrics$max.slp*45)) #negative screw

hist(test_met$length)
hist(log(test_met$length))
?hist

hist(flowpath_metrics$min.curve)
hist(log(test_met$min.curve))

hist(test_met$max.curve)
hist(log(test_met$max.curve))

hist(test_met$max.slp[test_met$max.curve<.0005&test_met$min.curve>(-.0005)], breaks=7)
hist(test_met$max.slp[test_met$max.curve<.0001&test_met$min.curve>(-.0001)], breaks=7)

#slope 5, curveature 4, lenth 10
### vectorizaion

stream <- ggplot(stream_cells, aes(x=x,y=y))+ geom_point()

start <- ggplot(step[[1]], aes(x=x,y=y))+ geom_point()
start+stream

w <- ggplot(t%>%subset(NED_PRW_ad8>100))+
  geom_point(aes(x=x,y=y))
  
e <- ggplot(t%>%subset(NED_PRW_ad8==1))+
  geom_point(aes(x=x,y=y))

w+e
