library(readr)
library(raster)
library(microbenchmark)
library(data.table)
library(purrr)
library(rpart)
library(tidyr)


# getting wepp runs as profiles and turning them into a data table --------


x <- paste0(path,"/",file)
res <- fread(x)

file_list <- list.files(path)
file <- list.files[1]

substrLeft <- function(x, n){
  substr(x, 1, nchar(x)-n+1)
}
x=file
n = nchar(file)-2
rm(dataset)
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- (fread(paste0(path,"/",file)))
    dataset$name <- substrLeft(file, 5)
    dataset <- list(dataset)
    i <- 2
    
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <- fread(paste0(path,"/",file))
    temp_dataset$name <- substrLeft(file, 5)
    dataset[[i]] <- temp_dataset
    i <- i+1
    rm(temp_dataset)
  }
  
}
#View(dataset)
dataset <- rbindlist(dataset)


fwrite(dataset,("processed_data/191014_PRW_MASTER_LIST"),quote=FALSE, row.names = FALSE, col.names = TRUE)
  
dataset <- fread("processed_data/191014_PRW_MASTER_LIST", header=T)
# dataset <- as.data.table(dataset)

# sensitivity analysis ---------------------------------------------------

# three OFE slopes 
dataset_three <- dataset[grepl("t3|f3|h3|g3|s3", name)]

dataset_three[, g := .GRP, by=.(name)]

dataset_three[, c("climate", "soil","length","S1","S2","S3","rotation","tillage"):= tstrsplit(name, "_", fixed=TRUE)][
  ,managment := paste(rotation,"_",tillage)][
    , soil_n := .GRP, by=.(soil)][
      , slope_n := .GRP, by=.(S2)][
        , man_n := .GRP, by=.(managment)][
          , rot_n := .GRP, by=.(rotation)][
            , til_n := .GRP, by=.(tillage)][
              , climate:= as.numeric(climate)]

tokeep <- which(sapply(dataset_three,is.numeric)) #grab the numeric columns and build a new data frame
test<-dataset_three[ , tokeep, with=FALSE][,c("OFE","erosion_kgm2_mean","erosion_kgm2_max","Q","UpStrmQ", "latqcc","climate","soil_n","slope_n","man_n","rot_n","til_n")] 

#Grab the 2nd OFE
dataset_ofe2<-dataset_three[OFE %in% 2,]
test_ofe2<-test[OFE %in% 2,]
#plot(test_ofe2)

# 
# figs <- map(.x = c(unique(whitman_interest$iconic_taxon_name)),             .f = ~ whitman_interest %>%
#               filter(iconic_taxon_name == .x) %>%
#               ggplot(aes(x = month_obs, y = n)) +
#               geom_boxplot(outlier.shape = NA) +
#               geom_jitter(aes(color = factor(year_obs))) +
#               scale_color_viridis_d(name = "Year", drop = FALSE) +
#               scale_x_discrete(drop = FALSE) +
#               theme_bw() +
#               theme(axis.text.x = element_text(angle = 45)) +
#               ggtitle(label = .x) +
#               ylab(label = "Count") +
#               xlab(label = "Month observed"))
ggplot(test_ofe2, aes(x=climate, y=erosion_kgm2_mean))+
  geom_point()

#strsplit(as.character(dataset_three$name), "_")[[1]][1]
# hist(log10(dataset$erosion_kgm2_mean[dataset$OFE==2]))
# NROW(dataset)
test <- na.omit(test_ofe2)
datacor<- cor(test)
cor_str <- plot(test)

# fit <- rpart(erosion_kgm2_mean ~ climate + soil_n + slope_n + man_n, data = test_ofe2)
# fit_1 <- rpart(erosion_kgm2_mean ~ climate + soil_n + man_n, data = test_ofe2)
# fit_2 <- rpart(erosion_kgm2_mean ~ soil_n + slope_n + man_n, data = test_ofe2)
# fit_3 <- rpart(erosion_kgm2_mean ~ climate + soil_n + slope_n , data = test_ofe2)
# fit_4 <- rpart(erosion_kgm2_mean ~ climate + slope_n + man_n, data = test_ofe2)
# 
# 
# plot(fit)
# text(fit, use.n = TRUE)
# 
# plot(fit_1)
# text(fit_1, use.n = TRUE)
# 
# plot(fit_2)
# text(fit_2, use.n = TRUE)
# 
# plot(fit_3)
# text(fit_3, use.n = TRUE)
# 
# plot(fit_4)
# text(fit_4, use.n = TRUE)


# ## building the map set  ------------------------------------------------

cli_PRW <- raster("processed_data/CLI_PRW.grd")
soil_PRW <- raster("processed_data/Mapunit_r_7-3_test.tif")  # raster processed_data/Mapunit_r.tif is the problem
# soil_PRW <- soil_PRW %>% mask(cli_PRW)
# head(soil_PRW)



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
soil_reclass <- soil_reclass %>% group_by(mukey) %>% 
  filter(percent == max(percent))

##should I group by dominant/ or most soil depth or as is   
  
#soil_depth_cat <- soil_reclass

soil_reclass$soil_depth_num <- ifelse(soil_reclass$soil_depth =="shallow", 1,
                                 ifelse(soil_reclass$soil_depth == "medium", 2,
                                        ifelse(soil_reclass$soil_depth =="deep",3,0)))

#soil_reclass$soil_depth[is.na(soil_reclass$soil_depth)] <- 0

#head(soil_reclass)

reclass_m <- data.matrix(soil_reclass[,c(1,6)])
#reclass_mcat <- data.matrix(soil_depth_cat[,1:2])
soil_depth_PRW <- reclassify(soil_PRW,reclass_m)#? reclassify
#soil_depth_PRW_cat <- reclassify(soil_PRW,reclass_mcat)#? reclassify
soil_depth_PRW@data@values[soil_depth_PRW@data@values>4] <-NA
#soil_depth_PRW_cat@data@values[soil_depth_PRW@data@values>4] <-NA
#plot(soil_depth_PRW)



slope_PRW <- raster("processed_data/NED_PF_PRWsd8.tif") 


reclass_df<-c(0,0.035,2,
              0.035,0.065,5,
              .065,.10,8,
              .10,.20,12,
              .20,2.00,35)

reclass_m <- matrix(reclass_df,
                    ncol = 3, 
                    byrow = T)       

slope_class_PRW<- reclassify(slope_PRW,reclass_m)#? reclassify
#plot(slope_PRW)
#plot(slope_class_PRW)

reclass_df<-c(10,13,4, #water
              20,25,1, #urban
              30,32,2, #barren to grass
              40,50,3, #trees
              51,53,2, # shrub to grass
              70,81.5,2, #pasture to grass
              81.5,82.5,6, # crops (tansitition)
              89,96,3) #trees/shrubs

reclass_m <- matrix(reclass_df,
                    ncol = 3, 
                    byrow = T)       
PRW_NLCD <- raster("processed_data/PRW_NLCD.tif")
NLCD_class_PRW<- reclassify(as.integer(PRW_NLCD),reclass_m)#? reclassify

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

################ncld OVERLAY
NLCD_class_PRW <- resample(NLCD_class_PRW, MAN_PRW, method = "ngb")
man_class_PRW <-merge(man_sim_PRW, NLCD_class_PRW, overlap = T)

#plot(man_class_PRW)


extent(MAN_PRW)
extent(NLCD_PRW)
extent(NED_PRW)

### okay to here 

landtype_brick <- brick(slope_class_PRW,man_class_PRW, cli_PRW, soil_depth_PRW)
writeRaster(landtype_brick, filename = "processed_data/landtype",bylayer=TRUE, format="GTiff", overwrite=TRUE)
rm(slope_class_PRW,man_class_PRW, cli_PRW, soil_depth_PRW)

names(landtype_brick)<-c("slope_class_PRW","MAN_PRW", "cli_PRW", "soil_depth_PRW")

#writeRaster(landtype_brick, "processed_data/landtype.tif") #problem saving a raster stake
#landtype_brick<-raster("processed_data/landtype.tif")
landtype_data <- as.data.frame(landtype_brick,xy = TRUE)
#test4<-as.data.frame(test3)
#head(test4)
names(landtype_data) <- c("x","y","slope_class_PRW","MAN_PRW", "cli_PRW", "soil_depth_PRW")
#names(test4) <- c("x","y","slope_class_PRW","MAN_PRW", "cli_PRW", "soil_depth_PRW")
#r <- rasterFromXYZ(test2)
fwrite(landtype_data, "processed_data/reclass_df.csv", row.names = F)

#landtype_data<- read.csv("processed_data/reclass_df.csv")
#unique(test4$slope_class_PRW)
#plot(test4$slope_class_PRW)


##REWRITE AS DATA TABLE
landtype_data$slope <- ifelse(landtype_data$slope_class_PRW==0|landtype_data$slope_class_PRW == 2, "300_2_2_2",
                      ifelse(landtype_data$slope_class_PRW==5, "300_2_5_2",
                             ifelse(landtype_data$slope_class_PRW==8, "300_2_8_2",
                                    ifelse(landtype_data$slope_class_PRW==12, "300_5_12_5",
                                           ifelse(landtype_data$slope_class_PRW==35, "300_5_35_5",NA)))))

landtype_data$soil <- ifelse(landtype_data$soil_depth_PRW == 3, "d",
                     ifelse(landtype_data$soil_depth_PRW == 2,"m",
                            ifelse(landtype_data$soil_depth_PRW == 1,"s",
                                   ifelse(landtype_data$soil_depth_PRW == 0,"r",NA))))

landtype_data$man_ct <- ifelse(landtype_data$MAN_PRW == 6.0, "wbp_ct",
                       ifelse(landtype_data$MAN_PRW == 7.0, "wbf_ct",
                              ifelse(landtype_data$MAN_PRW == 8.0, "wf_ct",
                                     ifelse(landtype_data$MAN_PRW == 2.0|landtype_data$MAN_PRW == 9.0, "g",
                                            ifelse(landtype_data$MAN_PRW == 3.0, "f",
                                                   ifelse(landtype_data$MAN_PRW == 5.0|landtype_data$MAN_PRW== 4.0 , "w",
                                                          ifelse(landtype_data$MAN_PRW == 1.0 , "u",NA)))))))


#unique(landtype_data$MAN_PRW)# 0.5 , 1.5 , 1.0, #urban
# 2.4 , 3.5 , 2.0, #range/grass
# 3.5 , 4.5 , 3.0, #forest
# 4.5 , 5.5 , 4.0, #water
# 5.5 , 6.5 , 5.0, #wetlands
# 10.5 , 11.5 , 6.0, #annual
# 11.5 , 12.5 , 7.0, #transition 
# 12.5 , 13.5 , 8.0, #crop fallow 
# 13.5 , 15.5 , 9.0) #irrigated ag/orchards

landtype_data$man_mt <- ifelse(landtype_data$MAN_PRW == 6.0, "wbp_mt",
                       ifelse(landtype_data$MAN_PRW == 7.0, "wbf_mt",
                              ifelse(landtype_data$MAN_PRW == 8.0, "wf_mt",
                                     ifelse(landtype_data$MAN_PRW == 2.0|landtype_data$MAN_PRW == 9.0, "g",
                                            ifelse(landtype_data$MAN_PRW == 3.0, "f",
                                                   ifelse(landtype_data$MAN_PRW == 5.0|landtype_data$MAN_PRW== 4.0 , "w",
                                                          ifelse(landtype_data$MAN_PRW == 1.0 , "u",NA)))))))
                       # ifelse(test2$MAN_PRW >= 11.0 & test2$MAN_PRW < 12, "wbp_mt",
                       #  ifelse(test2$MAN_PRW >= 12.0 & test2$MAN_PRW < 13, "wbf_mt",
                       #         ifelse(test2$MAN_PRW >= 13.0 & test2$MAN_PRW < 14, "wf_mt",
                       #                ifelse(test2$MAN_PRW == 3.0 |test2$MAN_PRW == 6.0 , "g",
                       #                       ifelse(test2$MAN_PRW == 4.0, "f",NA)))))

landtype_data$man_nt <-  ifelse(landtype_data$MAN_PRW == 6.0, "wbp_nt",
                        ifelse(landtype_data$MAN_PRW == 7.0, "wbf_nt",
                               ifelse(landtype_data$MAN_PRW == 8.0, "wf_nt",
                                      ifelse(landtype_data$MAN_PRW == 2.0|landtype_data$MAN_PRW == 9.0, "g",
                                             ifelse(landtype_data$MAN_PRW == 3.0, "f",
                                                    ifelse(landtype_data$MAN_PRW == 5.0|landtype_data$MAN_PRW== 4.0 , "w",
                                                           ifelse(landtype_data$MAN_PRW == 1.0 , "u",NA)))))))
                        # ifelse(test2$MAN_PRW >= 11.0 & test2$MAN_PRW < 12, "wbp_nt",
                        # ifelse(test2$MAN_PRW >= 12.0 & test2$MAN_PRW < 13, "wbf_nt",
                        #        ifelse(test2$MAN_PRW >= 13.0 & test2$MAN_PRW < 14, "wf_nt",
                        #               ifelse(test2$MAN_PRW == 3.0 |test2$MAN_PRW == 6.0 , "g",
                        #                      ifelse(test2$MAN_PRW == 4.0, "f",NA)))))


#man_sim_PRW

                       
landtype_data$ct <- (paste0(landtype_data$cli_PRW,"_",landtype_data$soil,"_",landtype_data$slope,"_",landtype_data$man_ct,"3"))  
landtype_data$ct <- sub("NA",NA,landtype_data$ct)

landtype_data$mt <- (paste0(landtype_data$cli_PRW,"_",landtype_data$soil,"_",landtype_data$slope,"_",landtype_data$man_mt,"3"))
landtype_data$mt <- sub("NA",NA,landtype_data$mt)

landtype_data$nt <- (paste0(landtype_data$cli_PRW,"_",landtype_data$soil,"_",landtype_data$slope,"_",landtype_data$man_nt,"3"))
landtype_data$nt <- sub("NA",NA,landtype_data$nt)


#names(landtype_data)

cols <- c(1:2,12:14)

run_table <- landtype_data[,cols]
run_table <- as.data.table(run_table)

#sum(is.na(df$col))

fwrite(run_table, "processed_data/xynames_2.csv", row.names = F)  #change to Fread 
run_table<- fread("processed_data/xynames_2.csv")

# merging data and spatial ------------------------------------------------


dt <- data.table(run_table, key = names(run_table$ct))

dataset2 <- subset(dataset, (OFE == 2))  ## fix f2 - fixed but still read in to profiles sorted here
dataset2$name <- as.character(dataset2$name)
dt2 <- data.table(dataset2, key = names(dataset2$name))

# testit_ct<-merge(x=dt, y=dt2, by.x="ct", by.y="name")
# testit_ct[,c("mt","nt"):=NULL]
# 
# testit_mt<-merge(x=dt, y=dt2, by.x="mt", by.y="name")
# testit_mt[,c("ct","nt"):=NULL]
# 
# testit_nt<-merge(x=dt, y=dt2, by.x="nt", by.y="name")
# testit_nt[,c("mt","ct"):=NULL]


#pick up from here 
# write.table(testit_ct,("processed_data/190729_ct_merged.txt"),quote=FALSE, row.names = FALSE, col.names = TRUE)
# write.table(testit_mt,("processed_data/190729_mt_merged.txt"),quote=FALSE, row.names = FALSE, col.names = TRUE)
# write.table(testit_nt,("processed_data/190729_nt_merged.txt"),quote=FALSE, row.names = FALSE, col.names = TRUE)


#fit<- raster("procecssed_data/190729_ct_merged.txt")
NED_PRW <- raster("processed_data/NED_PRW")
#t <- subset(testit, (OFE == 2))
#t[,c("ct"):=NULL]
testit_ct<-merge(x=dt, y=dt2, by.x="ct", by.y="name")
testit_ct[,c("mt","nt"):=NULL]
rm(raster_ct)
raster_ct <- rasterFromXYZ(testit_ct[,c(2:3,5)],crs= crs(NED_PRW)) #
raster_ct_Q <- rasterFromXYZ(testit_ct[,c(2:3,13)],crs= crs(NED_PRW))
raster_ct_lat <- rasterFromXYZ(testit_ct[,c(2:3,15)],crs= crs(NED_PRW))
# spg<- testit_ct[,c(2:3,5,13,14))]
# coordinates(spg) <- ~ x+y 
# #spggridded(spg) <- True
# gridded(spg) <- TRUE
# crs(spg) <- crs(NED_PRW)
# raster_ct <- raster(spg)
# rm(spg)
writeRaster(raster_ct, filename=("processed_data/raster_ct_19_10_15.tif"), format="GTiff", overwrite=TRUE)# fix not all one soil, and 7_s/8_s 
writeRaster(raster_ct_Q, filename=("processed_data/raster_ct_Q_19_10_15.tif"), format="GTiff", overwrite=TRUE)
writeRaster(raster_ct_lat, filename=("processed_data/raster_ct_lat_19_10_15.tif"), format="GTiff", overwrite=TRUE)


testit_mt<-merge(dt, dt2, by.x="mt", by.y="name") #all.y=F, allow.cartesian=TRUE
testit_mt[,c("ct","nt"):=NULL]

raster_mt <- rasterFromXYZ(testit_mt[,c(2:3,5)],crs= crs(NED_PRW)) #
raster_mt_Q <- rasterFromXYZ(testit_mt[,c(2:3,13)],crs= crs(NED_PRW))
raster_mt_lat <- rasterFromXYZ(testit_mt[,c(2:3,15)],crs= crs(NED_PRW))

writeRaster(raster_mt, filename=("processed_data/raster_mt_19_10_15.tif"), format="GTiff", overwrite=TRUE)# fix not all one soil, and 7_s/8_s 
writeRaster(raster_mt_Q, filename=("processed_data/raster_mt_Q_19_10_15.tif"), format="GTiff", overwrite=TRUE)
writeRaster(raster_mt_lat, filename=("processed_data/raster_mt_lat_19_10_15.tif"), format="GTiff", overwrite=TRUE)

# spg<- testit_mt[,c(2:3,5,13,14)]
# coordinates(spg) <- ~ x+y 
# gridded(spg) <- TRUE
# crs(spg) <- crs(NED_PRW)
# raster_mt <- raster(spg)
# rm(spg)
# writeRaster(raster_mt, "processed_data/raster_mt_19_08_01.tif", override = T)
  
testit_nt<-merge(dt, dt2, by.x="nt", by.y="name", allx=T, ally=F)
testit_nt[,c("ct","mt"):=NULL]


raster_nt <- rasterFromXYZ(testit_nt[,c(2:3,5)],crs= crs(NED_PRW)) #
raster_nt_Q <- rasterFromXYZ(testit_nt[,c(2:3,13)],crs= crs(NED_PRW))
raster_nt_lat <- rasterFromXYZ(testit_nt[,c(2:3,15)],crs= crs(NED_PRW))

writeRaster(raster_nt, filename=("processed_data/raster_nt_19_10_15.tif"), format="GTiff", overwrite=TRUE)# fix not all one soil, and 7_s/8_s 
writeRaster(raster_nt_Q, filename=("processed_data/raster_nt_Q_19_10_15.tif"), format="GTiff", overwrite=TRUE)
writeRaster(raster_nt_lat, filename=("processed_data/raster_nt_lat_19_10_15.tif"), format="GTiff", overwrite=TRUE)

# plotting raster - move to plotting  -------------------------------------


# testnt <- rasterFromXYZ(testit_nt[,c(2:3,5)]) 
# spg<- testit_nt[,c(2:3,5,13,14)]
# coordinates(spg) <- ~ x+y 
# gridded(spg) <- TRUE
# crs(spg) <- crs(NED_PRW)
# raster_nt <- raster(spg)
# rm(spg)
# writeRaster(raster_nt, "processed_data/raster_nt_19_08_01.tif")
# 
# 
# raster_ct <- raster("processed_data/raster_ct_19_08_01.tif")
# raster_mt <- raster("processed_data/raster_mt_19_08_01.tif")
# raster_nt <- raster("processed_data/raster_nt_19_08_01.tif")


raster_plot<- function(raster,xname,yname){
  rp <- raster/maxValue(raster)*100
  plot(rp, xlab = "xname", ylab= "yname", col = YlrRd)
}

ct_plot<-raster_plot(raster_ct, "Easting (m)", "Northing (m)",norm=TRUE)


ct <- raster_ct/maxValue(raster_ct)*100
mt <- raster_mt/maxValue(raster_ct)*100
nt <- raster_nt/maxValue(raster_ct)*100


c <- c( 'moccasin', 'khaki3', 'yellowgreen', 'yellow3', 'salmon3', 'salmon4', 'orangered')
breaks <- c(0,2,4,6,10,20,200)
breaks_names <- c("0-2","2-4","4-5","5-8","8-15","15-25",">25")
#c<-terrain.colors(9)


plot(raster_ct*4.49, col = (c), breaks = breaks, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Conventional Tillage")
plot(PRW, lwd = .001, add=TRUE)


plot(raster_mt*4.49, col = (c), xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Mulch Tillage")
plot(PRW, lwd = .001, add=TRUE)

plot(raster_nt*4.49, col = c, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="No Tillage")
plot(PRW, lwd = .001, add=TRUE)


c[1:3]<-c("#8B2323","#CD3333","#FF8C00")

maxValue(raster_ct)
extent(raster_nt) <- extent(raster_ct)

raster_all<- brick(raster_ct, raster_mt, raster_nt)
huc12<-shapefile("RAW/extent/HUC12")
PRW<-shapefile("RAW/extent/PRW_shape")



i <- (huc12$OBJECTID)
j=1
g <- subset(huc12, OBJECTID == i[j])
x <- stack()


Raster<-raster_ct 
shape_mask <- huc12

extent(raster_nt)<- extent(raster_ct)
l <- (huc12)
extent(l)<- extent(raster_ct)

Raster_mask<- function(Raster, shape_mask, name){
  i <- (shape_mask$OBJECTID)
  for (j in i[1]:i[length(i)]){
    g <- subset(shape_mask, OBJECTID == i[j])
    f <- Raster %>% crop(g) %>% mask(g)
    nam <- paste0("processed_data/raster_out/",name,"_",j,".tif")
    writeRaster(f,nam)
  }
}

Raster_stack<- function(Raster, shape_mask, name){
  i <- (shape_mask$OBJECTID)
  x <- stack()
  for (j in i[1]:i[length(i)]){
    g <- subset(shape_mask, OBJECTID == i[j])
    f <- Raster %>% crop(g) %>% mask(g)
    nam <- paste0("processed_data/raster_out/",name,"_",j,".tif")
    stack(x,f)
  }
  return(x)
}

tt <- Raster_stack(raster_ct,huc12,"PRW_ct")

str(tt)

j=19
g <- subset(huc12, OBJECTID == i[j])
f_ct <- raster_ct %>% crop(g) %>% mask(g)
f_mt <- raster_mt %>% crop(g) %>% mask(g)
f_nt <- raster_nt %>% crop(g) %>% mask(g)
f_man <- man_sim_PRW %>% crop(g) %>% mask(g)
f_slope <- slope_class_PRW %>% crop(g) %>% mask(g)
f_cli <- cli_PRW %>% crop(g) %>% mask(g)
f_soil <- soil_depth_PRW %>% crop(g) %>% mask(g)
plot(f_ct)

h <- test %>% crop(g) %>% mask(g)
plot(f, colNA="gray")
plot(h$slope_class_PRW)
plot(h$soil_depth_PRW)
plot(h)


plot(f/maxValue(Raster)*100, xlab = "Easting (m)", ylab = "Northing (m", main ="Thorn Creek", col=pal(7))
plot(g, add=T)

#,colNA="blue"
plot(f$soil_depth_PRW)
library(latticeExtra)
plot <- spplot(huc12, zcol = "OBJECTID")
lables <- layer(sp.text(coordinates(huc12), txt = huc12$OBJECTID, pos=0))
plot + lables

Raster_mask(raster_ct,huc12,"PRW_ct")
 
h<-Raster_mask(raster_ct,huc12
               ) 

g <- subset(huc12, OBJECTID =="52")
f<- raster_ct %>% clip(g) %>% mask(g)

g <- subset(huc12, OBJECTID =="52")
  
#####################mergeing landtypes 
# soil dpeth, slope steepness, landuse (ag and other)
landtype_ct_pie <- testit_ct[,c("ct","erosion_kgm2_mean")][,LT:=round(exp(V1),2)] 
names(landtype_ct_pie) <- c("name", "erosion_kgm2_mean")
datasetsimple <- dataset_ofe2[,c("name","soil_n","slope_n","man_n")]
 
LT_data <- merge(landtype_ct_pie, datasetsimple)
LT_data$soil_LT<- ifelse(LT_data$soil_n==3 , "shallow",ifelse(LT_data$soil_n == 2 , "medium", "deep")) 
LT_data$man_LT<- ifelse(LT_data$man_n > 4 , "CT","Other")
LT_data$slope_LT<- ifelse(LT_data$slope_n < 3 , "flat",ifelse(LT_data$slope_n > 4 , "steep", "moderate")) 

DT[,.(V4.Sum=sum(V4)),  by=.(V1,V2)] #Calculate sum of V4 for every group in V1
DT[,.N,by=V1]
LT <- LT_data[,.(LT_avg=mean(erosion_kgm2_mean),area=.N),by=c("soil_LT","man_LT","slope_LT")][man_LT=="CT",]
total_e <- sum(LT$LT_avg*LT$area*900) 
total_a <- sum(LT$area)

LT$LT_area_p  <-  LT$area/total_a*100
LT$LT_erosion_p  <-  LT$area*LT$LT_avg*900/total_e*100
cols <- c("soil_LT", "slope_LT")
LT$group <- apply(LT[, cols],1, paste, collapse= "-")
data$x <- apply( data[ , cols ] , 1 , paste , collapse = "-" )
LT[,group:=paste(soil_LT,slope_LT, sep=" and ")]
fwrite(LT, "processed_data/landtype_groups")

bp_area <- ggplot(data=LT, aes(x="", y=LT_area_p, fill=group))+ geom_bar(width = 1, stat = "identity")
pie_area <- bp + coord_polar("y", start=0)+ theme_minimal() 
pie_area

bp_erosion <- ggplot(data=LT, aes(x="", y=LT_erosion_p, fill=group))+ geom_bar(width = 1, stat = "identity")
pie_erosion <- bp_erosion + coord_polar("y", start=0)+ theme_minimal() 
pie_erosion

