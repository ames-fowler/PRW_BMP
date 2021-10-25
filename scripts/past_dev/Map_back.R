library(readr)
library(data.table)

# getting wepp runs as profiles and turning them into a data table
path <- "processed_data/profile"


file_list <- list.files(path, "*.txt")


substrLeft <- function(x, n){
  substr(x, 1, nchar(x)-n+1)
}
rm(dataset)
x=file
n = nchar(file)-2
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

dataset <- rbindlist(dataset)
head(dataset)
dataset

fwrite(dataset,("processed_data/190719_PRW_MASTER_LIST"),quote=FALSE, row.names = FALSE, col.names = TRUE)
  
dataset <- read.table("processed_data/190719_PRW_MASTER_LIST", header=T)
# hist(dataset$erosion_kgm2_mean[dataset$OFE==2])
# NROW(dataset)

## building the map set 


cli_PRW <- raster("processed_data/CLI_PRW.grd")
soil_PRW <- raster("processed_data/Mapunit_r_7-3_test.tif")
#soil_PRW <- raster("processed_data/Mapunit_r.tif") --- old file-likely the same..
# soil_PRW <- soil_PRW %>% mask(cli_PRW)
# head(soil_PRW)



soil_reclass<-cbind(as.numeric(as.character(comp_r$mukey)),comp_r$soil_depth,comp_r$comppct.r,as.character(comp_r$majcompflag),as.character(comp_r$compname)) %>% as.data.frame()
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

#soil_depth_PRW_t <- reclassify(test,reclass_m)
## this test sugest there is an error in "soil_depth_PRW" 9/22/2019 - there is no missing data here - where is it introducted 


slope_PRW <- raster("processed_data/NED_PF_PRWsd8.tif") 


reclass_df<-c(-Inf,0.035,2,
              0.035,0.065,5,
              .065,.10,8,
              .10,.20,12,
              .20,2.00,35)
#2019.09.22 - was introducing error 0 value 

reclass_m <- matrix(reclass_df,
                    ncol = 3, 
                    byrow = T)       

slope_class_PRW<- reclassify(slope_PRW,reclass_m)#? reclassify
plot(slope_PRW)
plot(slope_class_PRW)

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

NLCD_class_PRW<- reclassify(as.integer(NLCD_PRW),reclass_m)#? reclassify

plot(NLCD_class_PRW)
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
plot(man_sim_PRW)
#unique(man_sim_PRW)

################ncld OVERLAY
NLCD_class_PRW <- resample(NLCD_class_PRW, MAN_PRW, method = "ngb")
man_class_PRW <-merge(man_sim_PRW, NLCD_class_PRW, overlap = T)

plot(man_class_PRW)


extent(MAN_PRW)
extent(NLCD_PRW)
extent(NED_PRW)

### okay to here 


test <- brick(slope_class_PRW,man_class_PRW, cli_PRW, soil_depth_PRW)
names(test)<-c("slope_class_PRW","MAN_PRW", "cli_PRW", "soil_depth_PRW")

writeRaster(test, "processed_data/landtype.tif")
test<-raster("processed_data/landtype.tif")
#test3 <- rasterToPoints(test)
test2 <- as.data.frame(test,xy = TRUE)
#test4<-as.data.frame(test3)
#head(test4)
names(test2) <- c("x","y","slope_class_PRW","MAN_PRW", "cli_PRW", "soil_depth_PRW")
#names(test4) <- c("x","y","slope_class_PRW","MAN_PRW", "cli_PRW", "soil_depth_PRW")
#r <- rasterFromXYZ(test2)
write.csv(test2, "processed_data/reclass_df.csv", row.names = F)

test2<- read.csv("processed_data/reclass_df.csv")
unique(test4$slope_class_PRW)
#plot(test4$slope_class_PRW)

test2$slope <- ifelse(test2$slope_class_PRW==0|test2$slope_class_PRW == 2, "300_2_2_2",
                      ifelse(test2$slope_class_PRW==5, "300_2_5_2",
                             ifelse(test2$slope_class_PRW==8, "300_2_8_2",
                                    ifelse(test2$slope_class_PRW==12, "300_5_12_5",
                                           ifelse(test2$slope_class_PRW==35, "300_5_35_5",NA)))))

test2$soil <- ifelse(test2$soil_depth_PRW == 3, "d",
                     ifelse(test2$soil_depth_PRW == 2,"m",
                            ifelse(test2$soil_depth_PRW == 1,"s",
                                   ifelse(test2$soil_depth_PRW == 0,"r",NA))))

test2$man_ct <- ifelse(test2$MAN_PRW == 6.0, "wbp_ct",
                       ifelse(test2$MAN_PRW == 7.0, "wbf_ct",
                              ifelse(test2$MAN_PRW == 8.0, "wf_ct",
                                     ifelse(test2$MAN_PRW == 2.0|test2$MAN_PRW == 9.0, "g",
                                            ifelse(test2$MAN_PRW == 3.0, "f",
                                                   ifelse(test2$MAN_PRW == 5.0|test2$MAN_PRW== 4.0 , "w",
                                                          ifelse(test2$MAN_PRW == 1.0 , "u",NA)))))))


unique(test2$MAN_PRW)# 0.5 , 1.5 , 1.0, #urban
# 2.4 , 3.5 , 2.0, #range/grass
# 3.5 , 4.5 , 3.0, #forest
# 4.5 , 5.5 , 4.0, #water
# 5.5 , 6.5 , 5.0, #wetlands
# 10.5 , 11.5 , 6.0, #annual
# 11.5 , 12.5 , 7.0, #transition 
# 12.5 , 13.5 , 8.0, #crop fallow 
# 13.5 , 15.5 , 9.0) #irrigated ag/orchards

test2$man_mt <- ifelse(test2$MAN_PRW == 6.0, "wbp_mt",
                       ifelse(test2$MAN_PRW == 7.0, "wbf_mt",
                              ifelse(test2$MAN_PRW == 8.0, "wf_mt",
                                     ifelse(test2$MAN_PRW == 2.0|test2$MAN_PRW == 9.0, "g",
                                            ifelse(test2$MAN_PRW == 3.0, "f",
                                                   ifelse(test2$MAN_PRW == 5.0|test2$MAN_PRW== 4.0 , "w",
                                                          ifelse(test2$MAN_PRW == 1.0 , "u",NA)))))))
                       # ifelse(test2$MAN_PRW >= 11.0 & test2$MAN_PRW < 12, "wbp_mt",
                       #  ifelse(test2$MAN_PRW >= 12.0 & test2$MAN_PRW < 13, "wbf_mt",
                       #         ifelse(test2$MAN_PRW >= 13.0 & test2$MAN_PRW < 14, "wf_mt",
                       #                ifelse(test2$MAN_PRW == 3.0 |test2$MAN_PRW == 6.0 , "g",
                       #                       ifelse(test2$MAN_PRW == 4.0, "f",NA)))))

test2$man_nt <-  ifelse(test2$MAN_PRW == 6.0, "wbp_nt",
                        ifelse(test2$MAN_PRW == 7.0, "wbf_nt",
                               ifelse(test2$MAN_PRW == 8.0, "wf_nt",
                                      ifelse(test2$MAN_PRW == 2.0|test2$MAN_PRW == 9.0, "g",
                                             ifelse(test2$MAN_PRW == 3.0, "f",
                                                    ifelse(test2$MAN_PRW == 5.0|test2$MAN_PRW== 4.0 , "w",
                                                           ifelse(test2$MAN_PRW == 1.0 , "u",NA)))))))
                        # ifelse(test2$MAN_PRW >= 11.0 & test2$MAN_PRW < 12, "wbp_nt",
                        # ifelse(test2$MAN_PRW >= 12.0 & test2$MAN_PRW < 13, "wbf_nt",
                        #        ifelse(test2$MAN_PRW >= 13.0 & test2$MAN_PRW < 14, "wf_nt",
                        #               ifelse(test2$MAN_PRW == 3.0 |test2$MAN_PRW == 6.0 , "g",
                        #                      ifelse(test2$MAN_PRW == 4.0, "f",NA)))))


#man_sim_PRW

                       
test2$ct <- (paste0(test2$cli_PRW,"_",test2$soil,"_",test2$slope,"_",test2$man_ct,"3"))  
test2$ct <- sub("NA",NA,test2$ct)

test2$mt <- (paste0(test2$cli_PRW,"_",test2$soil,"_",test2$slope,"_",test2$man_mt,"3"))
test2$mt <- sub("NA",NA,test2$mt)

test2$nt <- (paste0(test2$cli_PRW,"_",test2$soil,"_",test2$slope,"_",test2$man_nt,"3"))
test2$nt <- sub("NA",NA,test2$nt)


names(test2)

cols <- c(1:2,12:14)

test5 <- test2[,cols]
test5 <- as.data.table(test5)

#sum(is.na(df$col))

write.csv(test5, "processed_data/xynames_2.csv", row.names = F)
test5<- read.csv("processed_data/xynames_2.csv")

#test6 <- merge(test5, dataset, all.x=T, by.x="ct",by.y="name")


#testit <- left_join(test5, dataset.tb, c("ct","name"))


## Fix up your example data.frame so that the columns aren't all factors
## (not necessary, but shows that data.table can now use numeric columns as keys)
#cols <- c(1:5, 7:10)
#test[cols] <- lapply(cols, FUN=function(X) as.numeric(as.character(test[[X]])))
#test[11] <- as.logical(test[[11]])

## Create two data.tables with which to demonstrate a data.table merge
#dt <- data.table(test, key=names(test))


dt <- data.table(test5, key = names(test5$ct))

dataset2 <- subset(dataset, (OFE == 2))  ## fix f2 
dataset2$name <- as.character(dataset2$name)
dt2 <- data.table(dataset2, key = names(dataset2$name))

testit_ct<-merge(dt, dt2, by.x="ct", by.y="name", all.x=T, all.y=F)
testit_ct[,c("mt","nt"):=NULL]

#pick up from here 
write.table(testit_ct,("processed_data/190729_ct_merged.txt"),quote=FALSE, row.names = FALSE, col.names = TRUE)
write.table(testit_mt,("processed_data/190729_mt_merged.txt"),quote=FALSE, row.names = FALSE, col.names = TRUE)
write.table(testit_nt,("processed_data/190729_nt_merged.txt"),quote=FALSE, row.names = FALSE, col.names = TRUE)


fit<- raster("procecssed_data/190729_ct_merged.txt")

#t <- subset(testit, (OFE == 2))
#t[,c("ct"):=NULL]

spg<- testit_ct[,c(2:3,5)]
coordinates(spg) <- ~ x+y 
#spggridded(spg) <- True
gridded(spg) <- TRUE
crs(spg) <- crs(NED_PRW)
raster_ct <- raster(spg)
writeRaster(raster_ct, "processed_data/raster_ct_19_08_01.tif")

testit_mt<-merge(dt, dt2, by.x="mt", by.y="name", all.x=T, all.y=F, allow.cartesian=TRUE)
testit_mt[,c("ct","nt"):=NULL]

spg<- testit_mt[,c(2:3,5)]
coordinates(spg) <- ~ x+y 
gridded(spg) <- TRUE
crs(spg) <- crs(NED_PRW)
raster_mt <- raster(spg)
writeRaster(raster_mt, "processed_data/raster_mt_19_08_01.tif", override = T)
  
testit_nt<-merge(dt, dt2, by.x="nt", by.y="name", allx=T, ally=F)
testit_nt[,c("ct","mt"):=NULL]

spg<- testit_nt[,c(2:3,5)]
coordinates(spg) <- ~ x+y 
gridded(spg) <- TRUE
crs(spg) <- crs(NED_PRW)
raster_nt <- raster(spg)
writeRaster(raster_nt, "processed_data/raster_nt_19_08_01.tif")


raster_ct <- raster("processed_data/raster_ct_19_08_01.tif")
raster_mt <- raster("processed_data/raster_mt_19_08_01.tif")
raster_nt <- raster("processed_data/raster_nt_19_08_01.tif")


raster_plot<- function(raster,xname,yname){
  rp <- raster/maxValue(raster)*100
  plot(rp, xlab = "xname", ylab= "yname", col = YlrRd)
}

ct_plot<-plot(raster_ct, "Easting (m)", "Northing (m)")


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

i <- (huc12$OBJECTID)
j=1
g <- subset(huc12, OBJECTID == i[j])
x <- stack()
plot(g)

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
  
  
df3[, c("foo","bar"):=NULL] 
typeof(dt$ct)

typeof(dt2$name)


## Add to each one a unique non-keyed column
dt$key <- dt$ct
dt2$keY <- dt2$name

## Merge them based on the keyed columns (in both cases, all but the last) to ...
## (1) create a new data.table
dt3 <- dt[dt2]
## (2) or (poss. minimizing memory usage), just add column Y from dt2 to dt
dt[dt2,Y:=Y]
