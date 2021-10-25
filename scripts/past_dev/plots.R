library(raster)
# Libraries for GIS
library('sp')
library(tigris)
# Library for plot
library(latticeExtra)
library(tmap)
library(viridis)
library(rasterVis)
library(maptools)
library(ggplot2)
library(data.table)
library(rgdal)
#raster ploting 
#install.packages("tigris")
#install.packages("tigris")
PRW <- shapefile("raw/extent/PRW_shape")
PRW_DF <- PRW %>% fortify
PRW_LL <- spTransform(PRW,CRS("+proj=longlat"))
plot(pnw)
plot(PRW_DF)

plot(PRW_LL)
# reclass to kg/m2 --------------------------------------------------------
test <- raster_ct[raster_ct>=20]
df<-as.data.frame(unique(raster_ct)[unique(raster_ct)<20])

#? reclassify
mt_re<- reclassify(raster_mt,reclass_m)
nt_re<- reclassify(raster_nt,reclass_m)

# #reload data ------------------------------------------------------------


PRW_SSURGO_shape<-shapefile('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_Mapunits.shp')

landtypes_slope<-raster('processed_data/landtype_1.tif') ##check names - bridge lengend from map_back 
landtypes_man<-raster('processed_data/landtype_2.tif')
landtypes_cli<-raster('processed_data/landtype_3.tif')
landtypes_soil<-raster('processed_data/landtype_4.tif')

cli_PRW<-raster('processed_data/Pre_ann_PRW.gri')
NED_PRW <- raster('processed_data/NED_PRW.gri')
slope_PRW <- terrain(NED_PRW,'slope')
aspect_PRW <- terrain(NED_PRW,'aspect')
hill_PRW <- hillShade(slope_PRW,aspect_PRW)

  #ACE_PRW <- raster('processed_data/ACE_PRW.gri')

Pre_ann_PRW <- raster('processed_data/Pre_ann_PRW.gri')
huc12<-shapefile("RAW/extent/HUC12")
huc12 <- readOGR(dsn = file.path("./RAW/extent/HUC12.shp"), stringsAsFactors = F)

huc12_r <- raster("processed_data/huc12_r.tif")
PRW<-shapefile("RAW/extent/PRW_shape")

#mapunit <- raster('processed_data/Mapunit_r_7-3_test.tif')

raster_ct <- raster("processed_data/raster_ct_19_10_15.tif")
raster_ct_Q <- raster("processed_data/raster_ct_Q_19_10_15.tif")
raster_ct_lat <- raster("processed_data/raster_ct_lat_19_10_15.tif")

raster_mt <- raster("processed_data/raster_mt_19_10_15.tif")
raster_mt_Q <- raster("processed_data/raster_mt_Q_19_10_15.tif")
raster_mt_lat <- raster("processed_data/raster_mt_lat_19_10_15.tif")

raster_nt <- raster("processed_data/raster_nt_19_10_15.tif")
raster_nt_Q <- raster("processed_data/raster_nt_Q_19_10_15.tif")
raster_nt_lat <- raster("processed_data/raster_nt_lat_19_10_15.tif")


data <- brick(raster_ct,raster_ct_lat, raster_ct_Q, raster_mt, 
              raster_mt_lat, raster_mt_Q, raster_nt, raster_nt_lat, raster_nt_Q)
huc12_p <- rasterToPoints(huc12_r)%>%as.data.table()
data_p <- rasterToPoints(data)
data_p <- as.data.table(data_p)

data_p12 <- merge(data_p,huc12_p)
data_aux <- brick(NED_PRW, hill_PRW, slope_PRW, 
                  landtypes_cli, landtypes_man, landtypes_slope, landtypes_soil)
rm(raster_ct,raster_ct_lat, raster_ct_Q, raster_mt, 
   raster_mt_lat, raster_mt_Q, raster_nt, raster_nt_lat, raster_nt_Q,NED_PRW, Pre_ann_PRW, hill_PRW, slope_PRW, 
   landtypes_cli, landtypes_man, landtypes_slope, landtypes_soil)


  
pnw <- map_data("state", c("washington", "oregon", "idaho"))
#proj4string(pnw) <- CRS("+proj=longlat")

#crs(pnw)
xy <- pnw[,c(1,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data = pnw,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

PRW_LL <- spTransform(PRW, crs(spdf))
pnw_utm <- spTransform(spdf,crs(PRW))
pnw_utm@data <- fortify(pnw_utm@data)
ggplot(pnw, aes(x=long, y=lat)) +
    geom_polygon(aes(group = group), fill = NA, colour = "grey60")+theme_bw() 
    

library(ggmap)
mal <- get_map('Malabo', zoom = 12, maptype = 'satellite')
ggmap(mal)
###############################################spatial stats
ct_huc12_mean <- data_p12[,.(ct_mean=mean(raster_ct_19_10_15)),.(huc12_r)]
ct_huc12_median <- data_p12[,.(ct_median=median(raster_ct_19_10_15)),.(huc12_r)]
ct_huc12_max <- data_p12[,.(ct_max=max(raster_ct_19_10_15)),.(huc12_r)]

csa <- function (x){
    sum(x*4.49>=5)
}
  
ct_huc12_csa <- data_p12[,.(csa(raster_ct_19_10_15)),.(huc12_r)]

huc12@data = data.frame(huc12@data, 
                        ct_huc12_csa[match(huc12@data$OBJECTID, ct_huc12_csa$huc12_r),],
                        ct_huc12_mean[match(huc12@data$OBJECTID, ct_huc12_mean$huc12_r),],
                        ct_huc12_median[match(huc12@data$OBJECTID, ct_huc12_median$huc12_r),],
                        ct_huc12_max[match(huc12@data$OBJECTID, ct_huc12_max$huc12_r),])
huc12@data$csa_area <- huc12@data$V1*30/1000*30/1000
#names(huc12@data)[2] <- "id"
huc12@data$id = rownames(huc12@data)
huc12.points = fortify(huc12, region="id")
huc12.df = merge(huc12.points, huc12@data, by="id")

writeOGR(obj=huc12, dsn="processed_data/huc12.shp", layer="csa_area", driver="ESRI Shapefile")

csa <- ggplot(huc12.df,aes(long,lat,group=group,fill=csa_area)) + 
  geom_polygon(color="White") +
  geom_path(color="white") +
  coord_equal() +  
  scale_fill_gradient(low="white", high ="red",na.value = "white",name = expression("km"^2)) +
  labs(x="\nEasting (m)", y="Northing(m)\n",title="CSA Area")+
  plot.theme.map

max <- ggplot(huc12.df,aes(long,lat,group=group,fill=ct_max)) + 
  geom_polygon(color="White") +
  geom_path(color="white") +
  coord_equal() +  
  scale_fill_gradient(low="white", high ="red", name = expression("kg/m"^2),na.value = "white") +
  labs(x="\nEasting (m)", y="Northing(m)\n", title="Maximum Erosion")+
  plot.theme.map

mean <- ggplot(huc12.df,aes(long,lat,group=group,fill=ct_mean)) + 
  geom_polygon(color="White") +
  geom_path(color="white") +
  coord_equal() +  
  scale_fill_gradient(low="white", high ="red", name = expression("kg/m"^2),na.value = "white") +
  labs(x="\nEasting (m)", y="Northing(m)\n",title="Mean Erosion")+
  plot.theme.map

median <- ggplot(huc12.df,aes(long,lat,group=group,fill=ct_median)) + 
  geom_polygon(color="White") +
  geom_path(color="white") +
  coord_equal() +  
  scale_fill_gradient(low="white", high ="red", name = expression("kg/m"^2),na.value = "white") +
  labs(x="\nEasting (m)", y="Northing(m)\n", title="Median Erosion")+
  plot.theme.map



csa+n+h
ggsave("Figures/huc12_csa_19_12_1.jpeg", csa+n+h, dpi=300)
ggsave("Figures/huc12_max_19_12_1.jpeg", max+n+h, dpi=300)
ggsave("Figures/huc12_mean_19_12_1.jpeg", mean+n+h, dpi=300)
ggsave("Figures/huc12_median_19_12_1.jpeg", median+n+h, dpi=300)
##shp <- readOGR(dsn = file.path(tempdir(), "GBR_adm1.shp"), stringsAsFactors = F) ### use this for all shape files 
map <- ggplot() + 
  geom_polygon(data = huc12, aes(x = long, y = lat, group = group, fill = "v1"), colour = "black")
map
TEST <- ggplot(huc12, aes(fill = id)) +
  theme_minimal() +
  geom_sf() +
  scale_fill_brewer(type = "qual")


plot(huc12[, "OBJECTID"])

# crop sub regions  -------------------------------------------------------
Zak_fields <- shapefile("RAW/extent/Zakarisons/Zakarisons/Fields.shp")
Zak_fields <- spTransform(Zak_fields,crs(PRW))
Zak_fields <- fortify(Zak_fields)

Union_flat <- readOGR(dsn = file.path("/RAW/extent/Union_Fla_Spring_Creek_Shapet/Union_Flat"), stringsAsFactors = F)
Union_flat <- shapefile("Raw/extent/Union_Fla_Spring_Creek_Shapet/Union_Flat")
  
clark_fields <- shapefile("RAW/extent/clarks/Clark_Field.shp")
clark_fields2 <- shapefile("RAW/extent/clarks/Clark_Field.shp")
clark_fields <- spTransform(clark_fields,crs(PRW))
clark_fields <- fortify(clark_fields)

long <- mean(extent(clark_fields2)[1:2])
lat <- mean(extent(clark_fields2)[3:4])
clark_gmap <- get_map(c(long,lat))

?get_map
roads<- tigris::roads("WA","075")
roads <- spTransform(roads,crs(data))

plot(clark_fields)
test <- crop(data,clark_fields)
aux <- crop(data_aux,clark_fields)
plot(test)
?crop
################################################write up this farm function 
clark_hills <- crop(hill_PRW,clark_fields)
plot(clark_hills, col=grey.colors(100))
plot(test$raster_ct_19_10_15, add=T, alpha=.5)
#plot(roads, add=T)
plot(Zak_fields, add=T)
??rasterVis::gplot

rasterVis::gplot(data=test) +
  geom_raster(aes(fill = raster_ct))
test
gplot(cli_PRW)+geom_raster(aes(x,y, fill=value))



###this works well - need legend to work too legend(x='topright', legend = c("Other", "Range","Forest","water", "WW-B-P", "WW-B-F", "WW-F", "Irrigated"), fill = my_col_man)
# theme -------------------------------------------------------------------

plot.theme.map <- theme(
  #axis.text  = element_text(color = 'black', size = 14,face="bold"),
  #axis.title = element_text(size = 14,face="bold"),
  axis.ticks = element_line(color = 'black'),
  plot.title= element_text(size = 18,face="bold",hjust = 0.5), # plot title
  legend.key = element_rect(fill = NA),
  legend.key.size = unit(2, "lines"),
  legend.key.width = unit(0.5, "cm"), ## here to control the legend to be smaller 
  legend.title = element_text(size = 16,face="bold"),
  legend.text = element_text(size = 16,face="bold"),
  # axis.text.x = element_text(angle = 0, vjust = 0.5),
  panel.grid.major  = element_blank(),#element_line(colour = "grey10", size = 0.5, linetype = 'dashed'),
  legend.background = element_rect(fill = NA),panel.border = element_blank(),
  panel.background  = element_blank(),
  axis.line.x = element_line(color = "black"),
  axis.line.y = element_line(color = "black"),
  panel.grid.minor= element_blank(),#element_line(colour = "grey10", size = 0.5, linetype = 'dashed'),
  # panel.grid.minor.y = element_blank(),
  panel.ontop = T,
  strip.text=element_blank(),
  axis.text.x = element_text(size=16), axis.text.y = element_text(size=16), 
  axis.title=element_text(size=18),
  plot.margin= unit(c(3,1,1,1),"cm")
)
  

# ggplot Landtypes --------------------------------------------------------
#background
n <- geom_polygon(data= PRW_DF,aes(x =long,y= lat) , fill=NA , col='grey45')
h <- geom_polygon(data= huc12,aes(x =long,y= lat) , fill=NA , col='grey45')
h_75<- geom_polygon(data= subset(huc12,OBJECTID==75),aes(x =long,y= lat) , fill=NA , col='black', size=1)
zf <-  geom_path(data= Zak_fields,aes(x =long,y= lat, group=group) ,col='grey45')
cf <- geom_path(data= clark_fields,aes(x =long,y= lat, group=group) ,col='grey45')


####PNW pllot
pnw_theme <- theme(line  = element_blank(),#element_line(colour = "grey10", size = 0.5, linetype = 'dashed'),
rect  = element_blank(),
text  = element_blank())
pnw_plot <- ggplot()+
  geom_path(data=pnw, aes(x=long, y=lat, group=group))+
  geom_polygon(data=PRW_LL, aes(x=long, y=lat, group=group), 
               fill="grey50")+pnw_theme

#managment
my_col_man <-c("#F2F2F2FF" ,"#EFC2B3FF", "burlywood4",
               "steelblue","lightblue1", "#00A600FF", "#3EBB00FF", 
               "#8BD000FF", "#E6E600FF")

my_col_man_labs <- c("Other", "Range","Forest","Water","Wetlands", "WW-B-P", "WW-B-F", "WW-F", "Irrigated")

m <- gplot(data_aux$landtype_2,maxpixels=30000000) + geom_raster(aes(fill = factor(value))) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=my_col_man, na.value = "white", labels= my_col_man_labs, name= "Management") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  plot.theme.map


my_col_cli= rev(terrain.colors(11))
my_col_breaks=seq(1,10,1)
c <- gplot(data_aux$landtype_3,maxpixels=1e7) + geom_raster(aes(fill = factor(value))) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=rev(viridis(11)), na.value = "white", name= "Precipitation  \nquantile") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  plot.theme.map

my_col_slope <- rev(terrain.colors(6))
my_col_slope_labs <- c("0.0", "0-3.5","3.5-6.5","6.5-10","10-20", "20+")


s <- gplot(data_aux$landtype_1,maxpixels=1e7) +
  geom_raster(aes(fill = factor(value))) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=rev(plasma(6)), na.value = "white", labels= my_col_slope_labs,name= "Slope [%]") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  plot.theme.map


my_col_soil = rev(terrain.colors(n = 4))
my_col_soil_labs <- c("NA", "Shallow", "Medium",  "Deep")
d <- gplot(data_aux$landtype_4,maxpixels=1e7) +
  geom_raster(aes(fill = factor(value))) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=rev(viridis(4)), labels= my_col_soil_labs,name= "Slope [%]", na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  plot.theme.map

NED <- gplot((NED_PRW),maxpixels=300000) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=viridis(10), name = "Elevation [m]",na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  plot.theme.map

pre_col <- c("#F7FBFF" , "#C6DBEF" ,"#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")
pre <- gplot((cli_PRW),maxpixels=300000) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=pre_col, name = "Precipitation [mm]",na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  plot.theme.map


ggsave("Figures/pnw.jpeg", pnw, dpi=300)  
ggsave("Figures/man_19_12_3.jpeg", m+n, dpi=300)
ggsave("Figures/cli_19_12_3.jpeg", c+n, dpi=300)
ggsave("Figures/slope_19_12_3.jpeg", s+n, dpi=300)
ggsave("Figures/soil_19_12_3.jpeg", d+n, dpi=300)

ggsave("Figures/ned_19_12_1.jpeg", NED+n, dpi=300)
ggsave("Figures/pre_19_12_1.jpeg", pre+n, dpi=300)

# ggplot PRW results NT MT CT XXX let's do this with huc 12 catchments---------------------------------------------

col_erosion <- rev(magma(7))#c('yellowgreen', 'yellow3', 'moccasin', 'khaki3',  'salmon3', 'salmon4', 'orangered')
breaks <- c(0,2,4,6,10,20,200)
breaks_names <- c("0-2","2-4","4-6","6-8","8-20",">20")


ct <- gplot((ct_re),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,20), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="Conventional Tillage")+
  plot.theme.map

mt <- gplot((mt_re),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,55), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="Mulch Tillage")+
  plot.theme.map

nt <- gplot((nt_re),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,55), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="No Tillage")+
  plot.theme.map

ggsave("Figures/ct_19_12_2.jpeg", ct+n, dpi=300)
ggsave("Figures/mt_19_12_2.jpeg", mt+n, dpi=300)
ggsave("Figures/nt_19_12_2.jpeg", nt+n, dpi=300)


i <- (huc12$OBJECTID)
#thorn = 28, kamiache = 19
j=93
g <- subset(huc12, OBJECTID == i[j])
wat_93 <- raster_ct %>% crop (g) %>% mask(g)
f_ct <- ct_re %>% crop(g) #%>% mask(g)
z_ct <- ct_re %>% crop(Zak_fields)
f_mt <- mt_re %>% crop(g) #%>% mask(g)
f_nt <- nt_re %>% crop(g) #%>% mask(g)

cf_ct <- ct_re %>% crop(clark_fields) #%>% mask(g)
cf_mt <- mt_re %>% crop(clark_fields) #%>% mask(g)
cf_nt <- nt_re %>% crop(clark_fields) #%>% mask(g)

ct75 <- gplot((f_ct),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,20), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="Conventional Tillage")+
  plot.theme.map

mt75 <- gplot((f_mt),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,55), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="Mulch Tillage")+
  plot.theme.map

nt75 <- gplot((f_nt),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,55), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="No Tillage")+
  plot.theme.map

ct_zf <- gplot((z_ct),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,20), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="Conventional Tillage")+
  plot.theme.map

ct_cf_r <- gplot((cf_ct),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,20), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="Conventional Tillage")+
  plot.theme.map

mt_cf_r  <- gplot((cf_mt),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,55), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="Mulch Tillage")+
  plot.theme.map

nt_cf_r  <- gplot((cf_nt),maxpixels=1e7) +
  geom_raster(aes(fill = (value))) +#
  facet_wrap(~ variable) +
  scale_fill_gradientn(colors=rev(magma(10)),limits=c(0,55), name = expression("kg/m"^2),na.value = "white") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", title ="No Tillage")+
  plot.theme.map


ct_cf_r+cf
ct75+h_75+zf
ct_zf+zf
ggsave("Figures/ct_cf_19_12_2.jpeg",ct_cf_r+cf, dpi=300)
ggsave("Figures/mt_cf_19_12_2.jpeg", ct_cf_r+cf, dpi=300)
ggsave("Figures/nt_cf_19_12_2.jpeg", ct_cf_r+cf, dpi=300)

ggsave("Figures/ct75_19_12_2.jpeg",ct75+h_75, dpi=300)
ggsave("Figures/ct75_zf_19_12_2.jpeg", ct75+h_75+zf, dpi=300)
ggsave("Figures/ct_zf_19_12_2.jpeg", ct_zf+zf, dpi=300)
# land type plots ---------------------------------------------------------

## Example data
r <- raster(ncol=4, nrow=2)
r[] <- sample(1:4, size=ncell(r), replace=TRUE)
r <- as.factor(r)

## Add a landcover column to the Raster Attribute Table
landtypes_man <- as.factor(landtypes_man)
rat <- levels(landtypes_man)[[1]]
rat[["landcover"]] <- c(1,2,3,4,5,6,7,8,9)#c("Urban","Range/grass", "Forest","Water", "Wetlands", "AG - Continuous", "AG - Transition","AG - Fallow", "AG - Irrigated")
levels(landtypes_man) <- rat


## Plot
#levelplot(landtypes_man, xlab="", ylab="")
##########rev(brewer.pal(9,"Paired")

color_test <- c("#FB9A99", "#33A02C", "#FDBF6F", "#E31A1C", "#CAB2D6", "#FF7F00", "#B2DF8A", "#1F78B4", "#A6CEE3")
color_test <- c("pink", "#EBB25EFF", "tan3", "lightblue3" ,"lightblue2", "#00A600FF", "#3EBB00FF", "#8BD000FF","#E6E600FF")
mapTheme <- rasterTheme(region = color_test)
plot(landtypes_man,par.settings=mapTheme, xlab= 'Easting (m)',
     ylab="Northing (m)")

level_man <- levelplot(data_aux$landtype_2,maxpixel=1000000, margin=F, par.settings=mapTheme, xlab= 'Easting (m)',
                 ylab="Northing (m)", main = "Land Use")

mapTheme <- rasterTheme(region = brewer.pal(8,"Blues"))
level_pre<- levelplot(Pre_ann_PRW, margin=F, par.settings=mapTheme, xlab= 'Easting (m)',cex=.25,
                       ylab="Northing (m)", main="Annual Precipitation (mm)")
level_ned <- levelplot(NED_PRW, margin=F, xlab= 'Easting (m)',
                       ylab="Northing (m)", main="Elevation (m)")
level_man+PRW_plot
level_pre+PRW_plot
level_ned+PRW_plot


pdf('Figures/level_man_PRW.pdf', width=200, height=100)
level_man+PRW_plot
dev.off()


grid.arrange(level_man,level_pre, level_ned)
###############
?terrain.colors

n <- length(v1$NAME_1)
plot(v1, col=rainbow(n), main = 'Administrative boundary: level 1')
# Now add name of the indiviudal regions to the map
text(v1, v1$NAME_1, cex=0.75)
pre_topo$level

plot(pre_topo, col= blues9)
text(pre_topo, c("335.380596923828", "372.095788574219", "394.9326171875", "427.315588378906",
                 "448.435150146484", "481.565960693359","510.952166748047", "557.317456054687", "681.315454101562"), cex=0.75)

plot(PRW)
text(PRW,"test")

plot(huc12) 
text(huc12,huc12$OBJECTID)
# HUC12 plots -------------------------------------------------------------

PRW_plot<-sp::plot(PRW, zcol="OBJECTID", colorkey=F)
huc12plot<-spplot(huc12, zcol="Id", colorkey=F)
huc12_lables<-layer(sp.text(huc12),txt=huc12$Id)
## "#00A600FF" "#E6E600FF" "#EAB64EFF" "#EEB99FFF" "#F2F2F2FF"
huc12plot+huc12_lables

#get roads 
dfw <- tracts(state = 'WA', county = c('Whitman'))

plot(dfw)
crs(dfw)<-crs(huc12)

plot <- spplot(huc12, zcol = "OBJECTID", colorkey=F)
lables <- layer(sp.text(loc = coordinates(huc12), 
                        txt = huc12$OBJECTID, pos = 1 ))
plot + lables

?layer(0)

i=huc12$OBJECTID
j=19
# kam is 19
# thorn is 28 
Kam_mask <- subset(huc12, OBJECTID == i[j])
plot(Kam_mask)
map_p2<-as(map_p, 'Spatial')
test<-crop(mapunit,Kam_mask)%>%
  mask(Kam_mask)
test_u$mukey <- na.omit(unique(test))

test_r <- as.data.table(comp_r)[mukey %in% test_u]

mbreakpoints <- na.omit(unique(test))
plot(test,breaks=breakpoints,col=rainbow(35))

plot(test)

plot(raster_ct*4.49, col = (c), breaks = breaks, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Conventional Tilalge")
plot(PRW, lwd = .001, add=TRUE)
plot(raster_mt*4.49, col = c, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Mulch Tilalge")
plot(PRW, lwd = .001, add=TRUE)
plot(raster_nt*4.49, col = c, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="No Tilalge")
plot(PRW, lwd = .001, add=TRUE)

c <- c( 'moccasin', 'khaki3', 'yellowgreen', 'yellow3', 'salmon3', 'salmon4', 'orangered')
breaks <- c(0,2,4,6,10,20,200)

# PRW plots ---------------------------------------------------------------


##########
mapTheme <- rasterTheme(region=brewer.pal(8,"Greens"))
plt <- levelplot(raster_ct, margin=T, par.settings=mapTheme)
plt + layer(sp.lines(PRW, col="gray", lwd=0.5))
###############


viridis(7)

c <- rev(magma(7))#c('yellowgreen', 'yellow3', 'moccasin', 'khaki3',  'salmon3', 'salmon4', 'orangered')
breaks <- c(0,2,4,6,10,20,200)
breaks_names <- c("0-2","2-4","4-6","6-8","8-20",">20")
  
# 1. Open jpeg file
jpeg("Figures/ct.jpg", width = 300, height = 300, res = 300)


plot(aux$layer,xlab= ("Easting (m)"),
     ylab= ("Northing (m)"), main ="Conventional Tillage", legend=F, col=grey.colors(50))
plot(test$raster_ct_19_10_15*4.49, col = (c), breaks = breaks, alpha=.7, add=T, legend=F)
plot(Zak_fields, lwd = .001, add=TRUE)
legend(x='topright', legend = breaks_names, fill = c, title="ton/acre")

pdf("Plot_ct.pdf", width = 13, height = 10)
plot(raster_ct, col = (c), breaks = breaks, xlab= ("Easting (m)"),
     ylab= ("Northing (m)"), main ="Conventional Tillage [kg/m2]", legend=F)
plot(PRW, lwd = .001, add=TRUE)
legend(x='topright', legend = breaks_names, fill = c, title="ton/acre")
dev.off()

pdf("Plot_mt.pdf", width = 13, height = 10)
plot(raster_mt*4.49, col = (c), breaks = breaks, xlab= ("Easting (m)"),
     ylab= ("Northing (m)"), main ="Mulch Tillage", legend=F)
plot(PRW, lwd = .001, add=TRUE)
legend(x='topright', legend = breaks_names, fill = c, title="ton/acre")
dev.off()     
     
pdf("Plot_nt.pdf", width = 13, height = 10)
plot(raster_nt, col = (c), breaks = breaks, xlab= ("Easting (m)"),
     ylab= ("Northing (m)"), main ="No Tillage", legend=F)
plot(PRW, lwd = .001, add=TRUE)
legend(x='topright', legend = breaks_names, fill = c, title="ton/acre")
dev.off()

color.legend(12,220,13,80,rev(ddf$VAL),rev(col.pal),gradient="y")

# 2. Create the plot
plot(x = my_data$wt, y = my_data$mpg,
     pch = 16, frame = FALSE,
     xlab = "wt", ylab = "mpg", col = "#2E9FDF")
# 3. Close the file
dev.off()


# Land type plots ---------------------------------------------------------


my_col = rev(terrain.colors(n = 4))
#landtype_brick$
pdf("Plot_soil.pdf", width = 13, height = 10)
plot(soil_depth_PRW, xlab="Easting (m)", ylab= "Northing (m)", main = "Soil Depth",legend=F)
legend(x='topright', legend = c("NA", "Shallow", "Medium",  "Deep"), fill = my_col, title="Soil Depth")
plot(PRW, add =T)
dev.off()



m<- c(266.5785 , 335.3806,  372.0958,  394.9326,  427.3156,  448.4352,  481.5660 , 510.9522  ,557.3175 , 681.3155 ,1277.5782)
my_col_cli= rev(terrain.colors(11))
my_col_breaks=seq(1,10,1)


pdf("Plot_cli.pdf", width = 13, height = 10)
m_in=round(m/25.4,0)
plot(cli_PRW, xlab="Easting (m)", ylab= "Northing (m)", main = "Annual Precipitation",
     legend = F, col = my_col_cli)
plot(PRW, add =T)
legend(x='topright', legend = m_in, fill = my_col_cli, title="Prcp (in)")
dev.off()


my_col_slope <- rev(terrain.colors(6))
my_slope <- unique(slope_class_PRW)
pdf("Plot_slope.pdf", width = 13, height = 10)
plot(slope_class_PRW, xlab="Easting (m)", ylab= "Northing (m)", main = "Slope", legend =F)
plot(PRW, add =T)
legend(x='topright', legend = my_slope, fill = my_col_slope, title="Slope (%)")
dev.off()

my_col_man <-c("#F2F2F2FF" ,"#EFC2B3FF", "burlywood4", "#E9BD3AFF", "#00A600FF", "#3EBB00FF", "#8BD000FF", "#E6E600FF")
pdf("Plot_man.pdf", width = 13, height = 10)
plot(man_class_PRW, col=my_col_man, xlab="Easting (m)", ylab= "Northing (m)", main = "Land use", legend =F)
plot(PRW, add =T)
legend(x='topright', legend = c("Other", "Range","Forest","water", "WW-B-P", "WW-B-F", "WW-F", "Irrigated"), fill = my_col_man)
dev.off()

# bar plot ----------------------------------------------------------------

head(test5)

ctmean <- mean(na.omit(testit$erosion_kgm2_mean))*4.49
mtmean <- mean(na.omit(testit_mt$erosion_kgm2_mean))*4.49
ntmean <- mean(na.omit(testit_nt$erosion_kgm2_mean))*4.49

PRW_30yr_mean<- c(ctmean, mtmean,ntmean)
barplot(PRW_30yr_mean, main = "PRW 30 yr. average erosion (ton/acre)", ylab = "Average erosion (ton/acre)", legend = c("CT", "MT", "NT"), col = c( 'orangered', 'yellow3','yellowgreen'))

ctmax <- max(na.omit(testit$erosion_kgm2_mean))*4.49
mtmax <- max(na.omit(testit_mt$erosion_kgm2_mean))*4.49
ntmax <- max(na.omit(testit_nt$erosion_kgm2_mean))*4.49

PRW_30yr_max<- c(ctmax, mtmax,ntmax)
barplot(PRW_30yr_max, main = "PRW max erosion (ton/acre)", ylab = "Average erosion (ton/acre)", legend = c("CT", "MT", "NT"), col = c( 'orangered', 'yellow3','yellowgreen'))


ct_dp_mean <- mean(na.omit(testit$Dp))
mt_dp_mean <- mean(na.omit(testit_mt$Dp))
nt_dp_mean <- mean(na.omit(testit_nt$Dp))
   
ct_lat_mean <- mean(na.omit(testit$latqcc))
mt_lat_mean <- mean(na.omit(testit_mt$latqcc))
nt_lat_mean <- mean(na.omit(testit_nt$latqcc))

ct_Q_mean <- mean(na.omit(testit$Q))
mt_Q_mean <- mean(na.omit(testit_mt$Q))
nt_Q_mean <- mean(na.omit(testit_nt$Q))



# thorn/kamiache plots -------------------------------------------------------------

huc12<-shapefile("RAW/extent/HUC12")
PRW<-shapefile("RAW/extent/PRW_shape")



i <- (huc12$OBJECTID)
#thorn = 28, kamiache = 19
j=28
g <- subset(huc12, OBJECTID == i[j])
f_ct <- raster_ct %>% crop(g) #%>% mask(g)
f_mt <- raster_mt %>% crop(g) %>% mask(g)
f_nt <- raster_nt %>% crop(g) %>% mask(g)
f_man <- landtypes_man %>% crop(g) %>% mask(g)
f_slope <- landtypes_slope %>% crop(g) %>% mask(g)
f_cli <- landtypes_cli %>% crop(g) %>% mask(g)
f_soil <- landtypes_soil %>% crop(g) %>% mask(g)
#f_dfw <- dfw %>% crop(g)
#plot(f_nt*4.49, col=c)
##########

p.strip <- list(cex=1.5, lines=2, fontface='bold')
ckey <- list(labels=list(cex=1.5))
x.scale <- list(cex=1.25)
y.scale <- list(cex=1.25)

levelplot(s, colorkey=ckey, par.strip.text=p.strip,
          scales=list(x=x.scale, y=y.scale))

mapTheme <- rasterTheme(region=rev(magma(10)))

plt <- levelplot((f_ct), margin=F, par.settings=mapTheme, xlab= list('Easting (m)', cex=1.5),
                 ylab=list("Northing (m)", cex = 1.5),colorkey=ckey,scales=list(x=x.scale, y=y.scale))
plt + latticeExtra::layer(sp.lines(g, col="gray10", lwd=1))

breaks <- seq(0, 55, by = 1)


prw_lev <- latticeExtra::layer(sp.lines(PRW, col="gray10", lwd=1))

plt_ct <- levelplot((raster_ct),maxpixels = 1e6, margin=F, par.settings=mapTheme, xlab= list('Easting (m)', cex=1.5),
                    ylab=list("Northing (m)", cex = 1.5),colorkey=ckey,scales=list(x=x.scale, y=y.scale),at=breaks,
                    main = expression("Conventional Tillage, kg/m"^2))

plt_mt <- levelplot((raster_mt),maxpixels = 1e6, margin=F, par.settings=mapTheme, xlab= list('Easting (m)', cex=1.5),
                   ylab=list("Northing (m)", cex = 1.5),colorkey=ckey,scales=list(x=x.scale, y=y.scale),at=breaks,
                   main = expression("Mulch Tillage, kg/m"^2))

plt_nt <- levelplot((raster_nt),maxpixels = 1e6, margin=F, par.settings=mapTheme, xlab= list('Easting (m)', cex=1.5),
                   ylab=list("Northing (m)", cex = 1.5),colorkey=ckey,scales=list(x=x.scale, y=y.scale),at=breaks,
                   main = expression("No Tillage, kg/m"^2))

ggsave("Figures/ct_19_12_2.jpeg", plt_ct + prw_lev, dpi=300)
ggsave("Figures/mt_19_12_2.jpeg", levelplot(plt_mt), dpi=300)
ggsave("Figures/nt_19_12_2.jpeg", plt_nt + prw_lev, dpi=300)

trellis.device(device="jpeg", filename="Figures/ct_19_12_2.jpeg")
print(plt_ct + prw_lev)
dev.off()

trellis.device(device="jpeg", filename="Figures/mt_19_12_2.jpeg")
print(plt_mt + prw_lev)
dev.off()

trellis.device(device="jpeg", filename="Figures/nt_19_12_2.jpeg")
print(plt_nt + prw_lev)
dev.off()


###############


pdf("Plot_Kam_ct.pdf", width = 13, height = 10)
plot(f_ct*4.49, col = (c),  xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Conventional Tillage", legend=T)
plot(g, lwd = .001, add=TRUE)
dev.off()

legend(x='bottomright', legend = breaks_names, fill = c, title="ton/acre")



plot(f_mt*4.49, col = (c), xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Mulch Tillage", legend=T)
plot(g, lwd = .001, add=TRUE)

legend(x='bottomright', legend = breaks_names, fill = c, title="ton/acre")


plot(f_nt*4.49, col = (c), breaks = breaks, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="No Tillage", legend=F)
plot(g, lwd = .001, add=TRUE)
legend(x='bottomright', legend = breaks_names, fill = c, title="ton/acre")


#landtypes
pdf("Plot_Kam_cli.pdf", width = 13, height = 10)
plot(f_cli, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Annual Precipitation", legend = F, col = my_col_cli, breaks= my_col_breaks)
plot(g, add =T)
legend(x='bottomright', legend = m_in, fill = my_col_cli, title="Prcp (in)")
dev.off()

pdf("Plot_Kam_slope.pdf", width = 13, height = 10)
plot(f_slope, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Slope", legend = F)
plot(g, add =T)
legend(x='bottomright', legend = my_slope, fill = my_col_slope, title="Slope (%)")
dev.off()

pdf("Plot_Kam_soil.pdf", width = 13, height = 10)
plot(f_soil, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Soil Depth", legend = F)
plot(g, add =T)
legend(x='bottomright', legend = c("NA", "Shallow", "Medium",  "Deep") , fill = my_col)
dev.off()

pdf("Plot_Kam_man.pdf", width = 13, height = 10)
plot(f_man, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Land use", legend = F, col=my_col_man)
plot(g, add =T)
legend(x='bottomright', legend = c("Other", "Range","Forest","water", "WW-B-P", 
                                   "WW-B-F", "WW-F", "Irrigated") , fill = my_col_man)
dev.off()



#thorn = 28, kamiache = 19
j=28
g <- subset(huc12, OBJECTID == i[j])
f_ct <- raster_ct %>% crop(g) %>% mask(g)
f_mt <- raster_mt %>% crop(g) %>% mask(g)
f_nt <- raster_nt %>% crop(g) %>% mask(g)
f_man <- man_sim_PRW %>% crop(g) %>% mask(g)
f_slope <- slope_class_PRW %>% crop(g) %>% mask(g)
f_cli <- cli_PRW %>% crop(g) %>% mask(g)
f_soil <- soil_depth_PRW %>% crop(g) %>% mask(g)
plot(f_ct*4.49)

pdf("Plot_thorn_ct.pdf", width = 13, height = 10)
plot(f_ct*4.49, col = (c),  xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Conventional Tillage", legend=T)
plot(g, lwd = .001, add=TRUE)
dev.off()

legend(x='bottomright', legend = breaks_names, fill = c, title="ton/acre")



plot(f_mt*4.49, col = (c), xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Mulch Tillage", legend=T)
plot(g, lwd = .001, add=TRUE)

legend(x='bottomright', legend = breaks_names, fill = c, title="ton/acre")


plot(f_nt*4.49, col = (c), breaks = breaks, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="No Tillage", legend=F)
plot(g, lwd = .001, add=TRUE)
legend(x='topright', legend = breaks_names, fill = c, title="ton/acre")


#landtypes
pdf("Plot_thorn_cli.pdf", width = 13, height = 10)
plot(f_cli, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Annual Precipitation", legend = F, col = my_col_cli, breaks= my_col_breaks)
plot(g, add =T)
legend(x='topright', legend = m_in, fill = my_col_cli, title="Prcp (in)")
dev.off()

pdf("Plot_thorn_slope.pdf", width = 13, height = 10)
plot(f_slope, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Slope", legend = F)
plot(g, add =T)
legend(x='topright', legend = my_slope, fill = my_col_slope, title="Slope (%)")
dev.off()

pdf("Plot_thorn_soil.pdf", width = 13, height = 10)
plot(f_soil, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Soil Depth", legend = F)
plot(g, add =T)
legend(x='topright', legend = c("NA", "Shallow", "Medium",  "Deep") , fill = my_col)
dev.off()

pdf("Plot_thorn_man.pdf", width = 13, height = 10)
plot(f_man, xlab= ("Easting (m)"),ylab= ("Northing (m)"), main ="Land use", legend = F, col=my_col_man)
plot(g, add =T)
legend(x='bottomright', legend = c("Other", "Range","Forest","water", "WW-B-P", 
                                   "WW-B-F", "WW-F", "Irrigated") , fill = my_col_man)
dev.off()


# Zak_fields --------------------------------------------------------------

c <- rev(magma(7))#c('yellowgreen', 'yellow3', 'moccasin', 'khaki3',  'salmon3', 'salmon4', 'orangered')
breaks <- c(0,2,4,6,10,20,200)
breaks_names <- c("0-2","2-4","4-6","6-8","8-20",">20")

# 1. Open jpeg file
jpeg("Figures/zak_ct.jpg", width = 300, height = 300, res = 300)

pdf("Figures/clark_ct.pdf", width = 13, height = 10)
plot(aux$layer,xlab= ("Easting (m)"),
     ylab= ("Northing (m)"), main ="Conventional Tillage", legend=F, col=grey.colors(50))
plot((test$raster_ct_19_10_15*4.49+.01), col = (c), breaks = breaks, alpha=.5, add=T, legend=F)
plot(clark_fields, lwd = 2, add=TRUE)
legend(x='bottomright', legend = breaks_names, fill = c, title="ton/acre",bty="o",bg="white")
dev.off()

pdf("Figures/clark_mt2.pdf", width = 13, height = 10)
plot(aux$layer,xlab= ("Easting (m)"),
     ylab= ("Northing (m)"), main ="Mulch Tillage", legend=F, col=grey.colors(50))
plot((test$raster_mt_19_10_15*4.49+0.001), col = (c), breaks = breaks, alpha=.7, add=T, legend=F)
plot(clark_fields, lwd = 2, add=TRUE)
legend(x='bottomright', legend = breaks_names, fill = c, title="ton/acre",bty="o",bg="white")
dev.off()

pdf("Figures/clark_nt.pdf", width = 13, height = 10)
plot(aux$layer,xlab= ("Easting (m)"),
     ylab= ("Northing (m)"), main ="No Tillage", legend=F, col=grey.colors(50))
plot(test$raster_nt_19_10_15*4.49, col = (c), breaks = breaks, alpha=.7, add=T, legend=F)
plot(clark_fields, lwd = 2, add=TRUE)
legend(x='bottomright', legend = breaks_names, fill = c, title="ton/acre",bty="o",bg="white")
dev.off()

my_col_soil = rev(terrain.colors(4))
pdf("Figures/clark_soil.pdf", width = 13, height = 10)
plot(aux$layer,xlab="Easting (m)", ylab= "Northing (m)", main = "Soil Depth", legend=F, col=grey.colors(50))
plot(aux$landtype_4, legend=F, alph= .7,add=T)
legend(x='bottomright', legend = c("NA", "Shallow", "Medium",  "Deep"), fill = my_col_soil,
       title="Soil Depth", bg="white")
plot(clark_fields, lwd = 2, add=TRUE)
dev.off()

m<- c(266.5785 , 335.3806,  372.0958,  394.9326,  427.3156,  448.4352,  481.5660 , 510.9522  ,557.3175 , 681.3155 ,1277.5782)
my_col_cli= rev(terrain.colors(11))
my_col_breaks=seq(1,10,1)
m_in=round(m/25.4,0)

pdf("Figures/clark_cli.pdf", width = 13, height = 10)
plot(aux$layer,xlab="Easting (m)", ylab= "Northing (m)", main = "Annual Precipitation", legend=F,
     col=grey.colors(50))
plot(aux$landtype_3,  legend = F, col = my_col_cli, add=T, alpha=.7)
plot(clark_fields, lwd = 2, add=TRUE)
legend(x='bottomright', legend = m_in, fill = my_col_cli, title="Prcp (in)",bg="white")
dev.off()

my_col_slope <- rev(terrain.colors(6))
my_slope <- unique(aux$landtype_1)
pdf("Figures/clark_slope.pdf", width = 13, height = 10)
plot(aux$layer,xlab="Easting (m)", ylab= "Northing (m)", main = "Slope", legend=F, col=grey.colors(50))
plot(aux$landtype_1, legend =F, add=TRUE, alpha=.7)
plot(clark_fields, lwd = 2, add=TRUE)
legend(x='bottomright', legend = my_slope, fill = my_col_slope, title="Slope (%)",bg="white")
dev.off()

my_col_man <-c("#F2F2F2FF" ,"#EFC2B3FF", "burlywood4", "#E9BD3AFF", "#00A600FF", "#3EBB00FF", "#8BD000FF", "#E6E600FF")
pdf("Figures/clark_man.pdf", width = 13, height = 10)
plot(aux$layer,xlab="Easting (m)", ylab= "Northing (m)", main = "Land use", legend=F, col=grey.colors(50))
plot(aux$landtype_2, legend =F, add=T, alpha=.7)
plot(clark_fields, lwd = 2, add=TRUE)
legend(x='bottomright', legend = c("Other", "Range","Forest","water", "WW-B-P", "WW-B-F", "WW-F", "Irrigated"),
       fill = my_col_man,bg="white")
dev.off()



# HUC12 agregated ---------------------------------------------------------

for (i in 1:length(grids)){
  ex <- extract(s, poly, fun=sum, na.rm=TRUE, df=TRUE)
}


ct_huc12_mean <- extract(raster_ct,huc12,fun=mean, df=T, na.omit=T)
ct_huc12_median <- extract(raster_ct,huc12,fun=median, d=T, na.omit=T)
ct_huc12_max <- extract(raster_ct,huc12,fun=max, df=T, na.omit=T)
ct_huc12_csa <- extract(raster_ct,huc12,fun=(function(x) sum(x*4.49>=5)),cellnumbers=T, df=T, na.omit=T)


?countif
