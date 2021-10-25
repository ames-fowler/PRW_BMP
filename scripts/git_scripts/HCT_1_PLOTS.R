

# PLot for Man 2 ---------------
# ames fowler

#libs -------------
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
library(cartomisc)
library(ggplot2)
library(gridExtra)
library(data.table)
library(rgdal)
library(rgeos)
library(tidyverse)
library(cowplot)
library(ggpubr)
#functions --------
gplot_data <- function(x, maxpixels = 1e7)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
    ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- data.frame(coords, dat)#dplyr::as_tibble()
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  return(dat)
}  # from https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r

grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }

# 0) load data-----

# o load CT rasters ---------------
master_list <- fread("processed_data/batch/210719_PRW_MASTER_LIST_shape")
raster_ct_max <- raster("processed_data/batch/geo_scratch/210615/CT.max.Errosion.tif")
raster_ct_mean <- raster("processed_data/batch/geo_scratch/210615/CT.mean.Errosion.tif")
raster_ct_sum <- raster("processed_data/batch/geo_scratch/210615/CT.sum.Errosion.tif")
raster_ct_sed <- raster("processed_data/batch/geo_scratch/210615/CT.sum.sed_t_ha.tif")
raster_ct_Q <- raster("processed_data/batch/geo_scratch/210615/CT.sum.Q.tif")
raster_ct_lat <- raster("processed_data/batch/geo_scratch/210615/CT.sum.Lat.tif")
# raster_ct_Rm <- raster("processed_data/batch/geo_scratch/210615/CT.RM.tif")

raster_ct_mean_shape <- raster("processed_data/batch/geo_scratch/210719_shape/mean.Errosion.tif")
raster_ct_max_shape <- raster("processed_data/batch/geo_scratch/210719_shape/max.Errosion.tif")
raster_ct_sum_shape <- raster("processed_data/batch/geo_scratch/210719_shape/sum.Errosion.tif")

data <- brick(raster_ct_sum,raster_ct_mean,raster_ct_max,
              raster_ct_sed, raster_ct_lat, raster_ct_Q,
              raster_ct_mean_shape,raster_ct_max_shape,raster_ct_sum_shape)
rm(list = c("raster_ct_sum","raster_ct_mean","raster_ct_max",
            "raster_ct_sed", "raster_ct_lat", "raster_ct_Q",
            "raster_ct_mean_shape","raster_ct_max_shape","raster_ct_sum_shape"))
## o land types =============
landtypes_slope <- raster("processed_data/gis_processed/landtype_210401_1.tif")
landtypes_man <- raster("processed_data/gis_processed/landtype_210401_2.tif")
landtypes_cli <- raster("processed_data/gis_processed/landtype_210401_3.tif")
landtypes_soil <- raster("processed_data/gis_processed/landtype_210401_4.tif")
landtypes <-  brick (landtypes_slope,landtypes_man,landtypes_cli,landtypes_soil)
names(landtypes) <- c("slope", "man", "cli", "soil")


## o extents--------------------

# o get streams and urban 
Urban <- landtypes_man
Urban[Urban!=1]<-NA

ad8 <- raster("processed_data/NED_PRW_ad8.tif")      # flow accumultation d8?
streams <- ad8 
streams[streams<100]<-NA
streams[streams>=100]<-1


#ACE_PRW <- raster('processed_data/ACE_PRW.gri')
# 
# Pre_ann_PRW <- raster('processed_data/Pre_ann_PRW.gri')
huc12<-shapefile("RAW/extent/HUC12")
huc12 <- readOGR(dsn = file.path("./RAW/extent/HUC12.shp"), stringsAsFactors = F)
huc12 <- huc12[huc12$Shape_Area<max(huc12$Shape_Area),]# remove bounding box shape
#plot(huc12)
estes <- readOGR(dsn = file.path("./processed_data/gis_scratch/estes_field_utm/estes_field-polygon.shp"),
                 stringsAsFactors = F)

huc12_r <- raster("processed_data/huc12_r.tif")
PRW<-shapefile("RAW/extent/PRW_shape")

PRW <- shapefile("raw/extent/PRW_shape")
PRW_DF <- PRW %>% fortify

## o mask out sub extents ==========

i <- (huc12$OBJECTID)
#thorn = 28, kamiache = 19, four mile = 74 
j=28
go =subset(huc12, OBJECTID == i[75]) 
g = subset(huc12, OBJECTID == i[j])
streams_79 <- streams %>% crop(g) %>% mask(g)
streams_75 <- streams %>% crop(go) %>% mask(go)
streams_28 <- streams %>% crop(subset(huc12, OBJECTID == i[28])) %>% mask(subset(huc12, OBJECTID == i[28]))
urban_79   <- Urban %>% crop(g) %>% mask(g)
urban_75   <- Urban %>% crop(go) %>% mask(go)
urban_28 <- Urban %>% crop(subset(huc12, OBJECTID == i[28])) %>% mask(subset(huc12, OBJECTID == i[28]))

streams_gp <- gplot_data(streams)
urban_gp   <- gplot_data(Urban)

streams_79gp <- gplot_data(streams_79)
urban_79gp   <- gplot_data(urban_79)

streams_28gp <- gplot_data(streams_28)
urban_28gp   <- gplot_data(urban_28)

data_28 <- data %>% crop (subset(huc12, OBJECTID == i[28])) %>% 
  mask(subset(huc12, OBJECTID == i[28]))
data_79 <- data %>% crop (subset(huc12, OBJECTID == i[79])) %>% 
  mask(subset(huc12, OBJECTID == i[79]))

c_fields <- shapefile("RAW/extent/clarks/Clark_Field.shp")
c_fields <- spTransform(c_fields,crs(PRW))
data_C <- data %>% crop(c_fields)%>% mask(c_fields)

ad8_cf <- ad8 %>% crop(c_fields)
ad8_gp <- gplot_data(ad8_cf, maxpixels = 1e7)


streams_cf <- streams %>% crop(c_fields)
urban_cf   <- Urban %>% crop(c_fields)

c_fields <- fortify(c_fields)
cf <- geom_path(data= c_fields,aes(x =long,y= lat, group=group) ,col='grey45')

streams_75 <- streams %>% crop(go) %>% mask(go)
urban_79   <- Urban %>% crop(g) %>% mask(g)


# map plot theme: 

plot.theme.map <- theme(
  axis.ticks = element_line(color = 'black'),
  plot.title= element_text(size = 12,face="bold",hjust = 0.5), # plot title
  legend.key = element_rect(fill = NA),
  legend.key.size = unit(2, "lines"),
  legend.key.width = unit(0.5, "cm"), ## here to control the legend to be smaller 
  legend.title = element_text(size = 12,face="bold"),
  legend.text = element_text(12),
  panel.grid.major  = element_blank(),#element_line(colour = "grey10", size = 0.5, linetype = 'dashed'),
  panel.background  = element_blank(),
  axis.line.x = element_line(color = "black"),
  axis.line.y = element_line(color = "black"),
  panel.grid.minor= element_blank(),#element_line(colour = "grey10", size = 0.5, linetype = 'dashed'),
  panel.ontop = T,
  strip.text=element_blank(),
  axis.text.x = element_text(size=12, angle = -45,hjust = -.2), axis.text.y = element_text(size=12), 
  axis.title=element_text(size=12),
)


# FIGURE 1) land type plots -----------------------------------------------------------------


c_fields_p <- c_fields$lat %>% mean %>% as.data.frame()
c_fields_p$long <- c_fields$long %>% mean
names(c_fields_p) <- c("y","x")
cf <- geom_path(data= c_fields, aes(x =long,y= lat, group=group), color = grey(.65))
ggplot()+cf

cf_point <- geom_point(data= c_fields_p,
                   aes(x =x, y=y), size = 4, shape = 18, 
                       color='orange') 

thorn <- geom_polygon(data = subset(huc12, OBJECTID == i[28]),
                   aes(x =long,y= lat, group=group), fill = "red")


n <- geom_polygon(data= PRW_DF,aes(x =long,y= lat) , fill=NA , col='grey45')

my_col_man <-c("#F2F2F2FF" ,"#EFC2B3FF", "burlywood4",
               "lightblue1","#00A600FF", "#3EBB00FF", 
               "#8BD000FF", "#E6E600FF")

my_col_man_labs <- c("Other", "Range","Forest","Water", 
                     "WW-B-P", "WW-B-F", "WW-F", "Irrigated")
p_man <- gplot((landtypes$man),maxpixels=1e7) +
  geom_raster(aes(fill =as.factor(value))) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=my_col_man,  na.translate = FALSE, labels= my_col_man_labs, name= "Management")+ 
  coord_equal()+ plot.theme.map +
  labs(x="\nEasting (m)", y="Northing(m)\n")+  plot.theme.map + 
  theme(legend.key.size = unit(7/16, "cm"))


my_col_cli= rev(terrain.colors(11))
my_col_breaks=seq(1,10,1)
p_cli <- gplot(landtypes$cli,maxpixels=1e7) + geom_raster(aes(fill = factor(value))) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=rev(viridis(11)), na.translate = FALSE, name= "Precipitation  \nquantile") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+  plot.theme.map + 
  theme(legend.key.size = unit(5/16, "cm"))




my_col_slope_labs <- c( "0","0-3.5","3.5-6.5","6.5-10","10-20", "20-30", "30+")

p_slope <- gplot(landtypes_slope,maxpixels=1e7) +
  geom_raster(aes(fill = factor(as.integer(value)))) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=(plasma(7)), breaks = c(0,2,5,8,12,25,35), na.translate = FALSE, labels= my_col_slope_labs,name= "Slope [%]") +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+  plot.theme.map + 
  theme(legend.key.size = unit(7/16, "cm"))


# my_col_soil = rev(terrain.colors(n = 4))

my_col_soil_labs <- c("Shallow", "Medium",  "Deep")
p_soil <- gplot(landtypes$soil,maxpixels=1e7) +
  geom_raster(aes(fill = factor(value))) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=rev(viridis(4)), labels= my_col_soil_labs,name= "Soil", na.translate = FALSE,) +  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+  plot.theme.map +  plot.theme.map + 
  theme(legend.key.size = unit(7/16, "cm"))


# o arrange and out put -----
LT <- ggarrange(p_slope+n, p_cli+n+thorn+cf_point, p_soil+n, p_man+n,
                heights = c(3, 3, 3, 3),
                widths  = c(3,3,3, 3),
                labels  = c("A", "B", "C", "D"),
                ncol    = 2, nrow = 2)

ggsave("processed_data/batch/figures/2109/LT.jpeg", LT, dpi = 300, width = 10, height = 8, unit ="in")

rm(p_man, p_slope, p_soil, p_cli, LT)

# FIGURE 4) Results and SCALE ----------


# o plot out extents ----
#background
n <- geom_polygon(data= PRW_DF,aes(x =long,y= lat) , fill=NA , col='grey45')
n_w <- geom_polygon(data= PRW_DF,aes(x =long,y= lat) , fill="white" , col='grey45')

h <- geom_path(data= (huc12),aes(x =long,y= lat,group=group) ,  col='grey45', size = .2)
h_75 <- geom_polygon(data= subset(huc12,OBJECTID==75),aes(x =long,y= lat) , fill=NA , col='grey45')
h_28 <- geom_polygon(data= subset(huc12,OBJECTID==28),aes(x =long,y= lat) , fill=NA , col='grey45')
h_28g <- geom_polygon(data= subset(huc12,OBJECTID==28),aes(x =long,y= lat) , fill=NA , col='grey65')

h_79 <- geom_polygon(data= subset(huc12,OBJECTID==c(79)),aes(x =long,y= lat) , fill=NA, col='grey45')
h_79_w <- geom_polygon(data= subset(huc12,OBJECTID==c(79)),aes(x =long,y= lat) , fill="grey95", col='black', size=1)


# o plot streams and urban----

str <- geom_tile(data = streams_gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y, ), fill = "lightblue3")
urb <- geom_tile(data = urban_gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y), fill = "lightgray")

str_79 <- geom_tile(data = streams_79gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y, ), fill = "lightblue3")
urb_79 <- geom_tile(data = urban_79gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y), fill = "lightgray")

str_28 <- geom_tile(data = streams_28gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y, ), fill = "lightblue3")
urb_28 <- geom_tile(data = urban_28gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y), fill = "lightgray")

# o plot the CPR CT max ----

CTgp <- gplot_data(data$CT.max.Errosion, maxpixels = 1e7) %>%
  dplyr::filter(., !is.na(value)) %>%
  mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10)))

ct_plot <- ggplot() +
  geom_tile(data = CTgp, aes(x = x, y = y, fill = value))+
  scale_fill_gradient2(low = "darkgreen", 
                       mid = magma(5)[5], 
                       high = "brown", midpoint = 0, 
                       name = expression("t/ha"))+  #what is this unit
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+plot.theme.map

CTgp_79 <- gplot_data(data_79$CT.max.Errosion, maxpixels = 1e7) %>%
  dplyr::filter(., !is.na(value)) %>%
  mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10)))

ct_plot_79 <- ggplot() +
  geom_tile(data = CTgp_79, aes(x = x, y = y, fill = value))+
  scale_fill_gradient2(low = "darkgreen", 
                       mid = magma(5)[5], 
                       high = "brown", midpoint = 0, 
                       name = expression("t/ha"))+  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_classic()

CTgp_28 <- gplot_data(data_28$CT.max.Errosion, maxpixels = 1e7) %>%
  dplyr::filter(., !is.na(value)) %>%
  mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10)))

ct_plot_28 <- ggplot() +
  geom_tile(data = CTgp_28, aes(x = x, y = y, fill = value))+
  scale_fill_gradient2(low = "darkgreen", 
                       mid = magma(5)[5], 
                       high = "brown", midpoint = 0, 
                       name = expression("t/ha"))+  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_classic()

ggsave("processed_data/batch/figures/2109/PRW_ct.jpeg", ct_plot+n+str+urb+h_28, dpi=300, width = 8, height = 5, unit = "in") 
ggsave("processed_data/batch/figures/2109/PRW_ct_huc12.jpeg", ct_plot+n+str+urb+h+h_28, dpi=300, width = 8, height = 5, unit = "in") 
ggsave("processed_data/batch/figures/2109/PRW_ct_28.jpeg", ct_plot_28+str_28+urb_28+h_28 + theme_void(), dpi=300, width = 4, height = 2.5, unit = "in") 


### 3) plot and compare the flow accumulation on c fields ---------
#written 210324. need stats to go with 

streams_cf_gp <- gplot_data(streams_cf)
urban_cf_gp <- gplot_data(urban_cf)

str_cf <- geom_tile(data = streams_cf_gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y, ), fill = "lightblue")
urb_cf <- geom_tile(data = urban_cf_gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y), fill = "lightgray")



CT_c_sum_gp <- gplot_data(data_C$CT.sum.Errosion) %>%
  dplyr::filter(., !is.na(value)) %>%
  mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10)))

CT_c_mean_gp <- gplot_data(data_C$CT.mean.Errosion) %>%
  dplyr::filter(., !is.na(value)) %>%
  mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10)))

CT_c_max_gp <- gplot_data(data_C$CT.max.Errosion) %>%
  dplyr::filter(., !is.na(value)) %>%
  mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10)))

#histogram 
# datatable_C <- c(data_C$CT.sum.Errosion, data_C$CT.mean.Errosion, data_C$CT.max.Errosion) %>% 
#   lapply(as.data.frame)%>% 
#   unlist(recursive = F) %>% 
#   as.data.table() %>% na.omit()%>%
#   pivot_longer(cols = c(CT.sum.Errosion, CT.mean.Errosion, CT.max.Errosion))
# 
# ggplot(datatable_C, aes(4.46*value, fill= name))+xlim(-300,300)+
#   geom_histogram(binwidth=30, position = "dodge")+
#   # geom_density(alpha=0.6)+
#   scale_y_continuous(trans = "log10")+
#   theme_classic()

#o spatial plots -------------
ct_c_sum_plot <- ggplot() +
  geom_tile(data = CT_c_sum_gp, aes(x = x, y = y, fill = value))+
  scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0, 
                       name = expression("t/ha"))+
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_void()

ct_c_max_plot <- ggplot() +
  geom_tile(data = CT_c_max_gp, aes(x = x, y = y, fill = value))+
  scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0, 
                       name = expression("t/ha"))+
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_void()+ theme(legend.position  = "none")

ct_c_mean_plot <- ggplot() +
  geom_tile(data = CT_c_mean_gp, aes(x = x, y = y, fill = value))+
  scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0, 
                       name = expression("t/ha"))+
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_void()
# ct_c_mean_plot
# ct_c_max_plot
# ct_c_sum_plot+ theme(legend.position  = "none")

leg <- get_legend(ct_c_sum_plot)
as_ggplot(leg)

ct_mean <- (ct_c_mean_plot+ str_cf+ urb_cf +cf)

# ag_compare_cf <- grid.arrange(ct_c_mean_plot+ theme(legend.position  = "none")+ str_cf+ urb_cf +cf, 
#                               ct_c_max_plot+ str_cf+ urb_cf +cf, as_ggplot(leg),
#                               ct_c_sum_plot+ theme(legend.position  = "none")+ str_cf+ urb_cf +cf,
#                               nrow= 3, layout_matrix = rbind(c(1,1,NA), c(2,2,NA), c(4,4,3)))
# 
# ggsave("processed_data/batch/Figures/2109/cf_agg_compare.jpeg", 
#        ag_compare_cf, dpi=300, 
#        height = 4, width = 3)

#o differences from mean -----
CT_c_sum_dif_gp <- gplot_data(data_C$CT.sum.Errosion - data_C$CT.mean.Errosion) %>%
  dplyr::filter(., !is.na(value)) #%>%
  # mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10)))

CT_c_max_dif_gp <- gplot_data(data_C$CT.max.Errosion - data_C$CT.mean.Errosion) %>%
  dplyr::filter(., !is.na(value)) #%>%
  # mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10)))

CT_c_max_sum_dif_gp <- gplot_data(data_C$CT.sum.Errosion - data_C$CT.max.Errosion) %>%
  dplyr::filter(., !is.na(value)) #%>%
  # mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10)))

merged_agg <- CT_c_sum_dif_gp[,1:3] %>% merge(.,ad8_gp[,1:3], by=c("x","y"))
names(merged_agg) <- c("x","y","mean_sum_res","flow_accum")


merged_agg <- merged_agg %>% merge(.,CT_c_max_dif_gp[,1:3], by=c("x","y"))
names(merged_agg) <- c("x","y","mean_sum_res","flow_accum","mean_max_res")

merged_agg <- merged_agg %>% merge(.,CT_c_max_sum_dif_gp[,1:3], by=c("x","y"))
names(merged_agg) <- c("x","y","mean_sum_res","flow_accum","mean_max_res","max_sum_res")

wshed_flow    <-  fread(file = "processed_data/gis_processed/whsed_tr_PRW_210614")
temp_dup_sum <- wshed_flow[, lapply(.SD, sum, na.rm=TRUE), by=c("x","y"), .SDcols=c("duplicate")]
rm(wshed_flow)

merged_agg1 <- merged_agg %>% merge(., temp_dup_sum, by=c("x","y"))
merged_agg <- merged_agg1 %>% as.data.table()
## could add in the other differences here.... 
#o plot by points ==========

#max
# max_res <- ggplot(data = merged_agg %>% subset(.,abs(mean_max_res )>0))+
#   geom_point(aes(x= duplicate , y = (abs(mean_max_res )*10)),bins = 20)+
#   scale_fill_viridis()+theme_classic()+
#   xlab("duplicates")+ ylab("\n abs. difference (t/ha)")+
#   scale_y_continuous(trans = "log10")#+scale_x_continuous(trans = "log10")

max_res <- ggplot(data= merged_agg[abs(mean_max_res)>0]) + 
  geom_boxplot(aes(y = (mean_max_res*10), x = duplicate, group = duplicate))+
  scale_y_continuous(trans = "log10", limits = c(1e-3,1e+4))+theme_classic()+
  xlab("duplicates")+ ylab("\n abs. difference (t/ha)")
sum_res <- ggplot(data= merged_agg[abs(mean_sum_res)>0]) + 
  geom_boxplot(aes(y = abs(mean_sum_res*10), x = duplicate, group = duplicate))+
  scale_y_continuous(trans = "log10", limits = c(1e-3,1e+4)) +theme_classic()+
  xlab("duplicates")+ ylab("\n abs. difference (t/ha)")
max_sum_res <- ggplot(data= merged_agg[abs(max_sum_res)>0]) + 
  geom_boxplot(aes(y = abs(max_sum_res*10), x = duplicate, group = duplicate)) +
  scale_y_continuous(trans = "log10", limits = c(1e-3,1e+4))+theme_classic()+
  xlab("duplicates")+ ylab("\n abs. difference (t/ha)")
# 
# #sum
# sum_res <- ggplot(data = merged_agg %>% subset(.,abs(mean_sum_res)>0))+
#   geom_point(aes(x= duplicate, y = (mean_sum_res %>% abs )*10))+
#   scale_fill_viridis()+theme_classic()+
#   xlab("duplicates")+ ylab("\n abs. difference (tons/ha)")+
#   scale_y_continuous(trans = "log10")#+scale_x_continuous(trans = "log10")
# 
# #sum max
# max_sum_res <- ggplot(data = merged_agg %>% subset(.,abs(max_sum_res)>0))+
#   geom_point(aes(x= duplicate, y = (max_sum_res*10) %>% abs ),bins = 40)+
#   scale_fill_viridis()+theme_classic()+
#   xlab("duplicates")+ ylab("\n abs. difference (tons/ha)")+
#   scale_y_continuous(trans = "log10")#+scale_x_continuous(trans = "log10")


ggsave("processed_data/batch/Figures/2109/res_sum_dif_points.jpeg", res_by_accum,
       height = 4, width = 4) 

ggsave("processed_data/batch/Figures/2109/res_max_dif_points.jpeg", res_by_accum_max,
       height = 4, width = 4) 
# plot((merged_agg %>% subset(.,value.x!=0))$value.x %>% abs(),
#      log((merged_agg %>% subset(.,value.x!=0))$value.y),bins = 60)
#   xlab()

ct_c_sum_dif_plot <- ggplot() +
  geom_tile(data = CT_c_sum_dif_gp %>% 
              mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10))),
            aes(x = x, y = y, fill = value))+
  scale_fill_gradient2(low = cividis(5)[5], mid = "grey95", high = plasma(5)[1], midpoint = 0, 
                       name = expression("t/ha"))+
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_void()

ct_c_max_dif_plot <- ggplot() +
  geom_tile(data = CT_c_max_dif_gp%>% 
              mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10))),
            aes(x = x, y = y, fill = value))+
  scale_fill_gradient2(low = cividis(5)[5], mid = "grey95", high = plasma(5)[1], midpoint = 0, 
                       name = expression("t/ha"))+
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_void()

ct_c_max_sum_dif_plot <- ggplot() +
  geom_tile(data = CT_c_max_sum_dif_gp %>% 
              mutate(value = ifelse(value*10>20, 20, ifelse(value*10<(-20), -20, value*10))), 
            aes(x = x, y = y, fill = value))+
  scale_fill_gradient2(low = cividis(5)[5], mid = "grey95", high = plasma(5)[1], midpoint = 0, 
                       name = expression("t/ha"))+
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_void()
# 
# ggplot() +
#   geom_tile(data = merged_agg,aes(x = x, y = y, fill = duplicate))+
#   coord_equal()+
#   labs(x="\nEasting (m)", y="Northing(m)\n")+
#   scale_fill_viridis(direction=-1)+
#   theme_void()
# 
# ggplot() +
#   geom_tile(data = merged_agg,aes(x = x, y = y, fill = flow_accum ))+
#   coord_equal()+
#   labs(x="\nEasting (m)", y="Northing(m)\n")+
#   scale_fill_viridis(direction=-1)+
#   theme_void()


sum_dif     <- (ct_c_sum_dif_plot     + str_cf + urb_cf + cf)
max_dif     <- (ct_c_max_dif_plot     + str_cf + urb_cf + cf)
max_sum_dif <- (ct_c_max_sum_dif_plot + str_cf + urb_cf + cf)
# 
# res_compare_cf <- ggarrange(max_dif,sum_dif, max_sum_dif,
#                                align = "v",nrow= 3,
#                             labels = c("A", "B", "C"))
# 
# ggsave("processed_data/batch/Figures/2107/cf_agg_res_compare.jpeg", 
#        res_compare_cf, dpi=300, 
#        height = 6, width = 4)
# 
# 
# res_compare_plus_cf <- ggarrange(ct_mean, max_dif,sum_dif, max_sum_dif,
#                             align = "v",
#                             nrow= 4,
#                             labels = c("A", "B", "C", "D"))
# 
# ggsave("processed_data/batch/Figures/2109/cf_agg_res_plus_compare.jpeg", 
#        res_compare_plus_cf, dpi=300, 
#        height =8, width = 4)



res_compare_res <- ggarrange(ct_mean,NA,  max_dif, max_res, sum_dif, sum_res, max_sum_dif, max_sum_res,
                                 align = "v",
                                 nrow= 4,
                                 ncol = 2,
                                 labels = c("A",NA, "B","E", "C","F", "D", "G"))

ggsave("processed_data/batch/Figures/2109/cf_agg_res_compare.jpeg", 
       res_compare_res, dpi=300, 
       height =10, width = 7.5)

# res_compare_total <- ggarrange(p_slope+n, 
#                                p_cli+n, 
#                                p_soil+n, 
#                                p_man+n,
#                 heights = c(3, 3, 3, 3),
#                 widths  = c(3,3,3, 3),
#                 labels  = c("A", "B", "C", "D"),
#                 ncol    = 2, nrow = 4)

 
### 4) Plot figure 7 WEPP comparison -----------------

#load total data: 
total_data <- fread("processed_data/batch/210906_thorn_comp_data")
# total_data <- fread("processed_data/batch/210719_thorn_comp_data")
Thorn_hills <- shapefile("./weppcloud_thorn/export/arcmap/subcatchments.shp")
Thorn_channels <- shapefile("./weppcloud_thorn/export/arcmap/channels.shp")
total_data1$topaz_id[duplicated(total_data1$topaz_id)]
total_data %>% subset(., topaz_id==423)   



library(broom)
spdf_fortified <- tidy(Thorn_hills, region = "TopazID") %>% rename(TopazID=id)
spdf_fortified <-merge(spdf_fortified, total_data, by.x ="TopazID", by.y ="topaz_id")
ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = group, fill=duplicated(TopazID))) +
  coord_equal() +
  theme_void() 

# o the big oh f moment --------
fu <- ggplot(data = total_data )+ #%>% subset(slope_shape == "uniform")
  geom_line(aes(x = Erosion_T_ha1HCT.man.cli**0*1:7, y = Erosion_T_ha1HCT.man.cli**0*1:7),
            col = "red", size=1.2)+ 
  geom_point(aes(x = Erosion_T_ha1HCT.man.cli.soils, y = sed_CT_mean, col= soil_LT, alpha = 0.7))+
  scale_color_viridis_c()+
  coord_equal()+ theme_minimal()

fu

# o step wise comparison: ---------
# https://stackoverflow.com/questions/19699858/ggplot-adding-regression-line-equation-and-r2-with-facet
lm_eqn <- function(x,y){
  m <- lm(y ~ x);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
 

eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(r)^2~"="~r2*","~~italic(p)~"="~p,#italic(y) == a + b %.% italic(x)*","~~
                 list(a = format(coef(m)[1], digits = 1),
                      b = format(coef(m)[2], digits = 1),
                      r2 = format(summary(m)$r.squared, digits = 2),
                      p = format(summary(m)$coefficients[,4][2], digits = 2)))
    )
  )
}

eq1 <- lm_eqn(total_data$Erosion_T_ha1HCT.man, total_data$Erosion_T_ha1HCT.man.cli)


sw1_text <-geom_text(x = 25, y = 300, 
                     label = lm_eqn(total_data$Erosion_T_ha1HCT.man,
                                    total_data$Erosion_T_ha1HCT.man.cli), 
                     parse = TRUE)


p1 <- sw1 + geom_text(x = 25, y = 300, label = lm_eqn(total_data$Erosion_T_ha1HCT.man,
                                                      total_data$Erosion_T_ha1HCT.man.cli),
                      parse = TRUE)


sw1+sw1_text


stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
             label.x.npc = "right", label.y.npc = 0.15,
             formula = formula, parse = TRUE, size = 3)

library(ggpmisc)

sw1 <- ggplot(data = total_data[Erosion_T_ha1HCT.man!=0])+
  geom_line(aes(x = Erosion_T_ha1HCT.man.cli**0*1:20, y = Erosion_T_ha1HCT.man.cli**0*1:20),
            col = "red", size=1.2)+
  geom_point(aes(x = Erosion_T_ha1HCT.man, y = Erosion_T_ha1HCT.man.cli), size=1.6,)+
  geom_smooth(aes(x = Erosion_T_ha1HCT.man, y = Erosion_T_ha1HCT.man.cli), 
              method = lm, show.legend = T)+
  labs(x = " \nSediment Delivery (t/ha/yr) \n s-HCT: man",
       y =  "\nSediment Delivery (t/ha/yr) \n s-HCT: man + cli\n") +
  theme_minimal() + coord_equal()+theme(text = element_text(size=12))+ 
  scale_fill_viridis_c(direction = 1)+xlim(0,25)+ylim(0,25)+
  geom_text(x = 10, y = 20, label = eq(total_data$Erosion_T_ha1HCT.man,
                                       total_data$Erosion_T_ha1HCT.man.cli), parse = TRUE)
# ggplot()+t1
# sw1+t1

sw2 <- ggplot(data = total_data[Erosion_T_ha1HCT.man!=0])+
  geom_line(aes(x = Erosion_T_ha1HCT.man.cli**0*1:20, y = Erosion_T_ha1HCT.man.cli**0*1:20),
            col = "red", size=1.2)+ 
  geom_point(aes(x = Erosion_T_ha1HCT.man, y = Erosion_T_ha1HCT.man.cli.soils), size=1.6)+
  geom_smooth(aes(x = Erosion_T_ha1HCT.man, y = Erosion_T_ha1HCT.man.cli.soils), method = lm, show.legend = T)+
  labs(x = " \nSediment Delivery (t/ha/yr) \n s-HCT: man",
       y =  "\nSediment Delivery (t/ha/yr) \n s-HCT: man + cli+ soils\n") +
  theme_minimal() + coord_equal()+theme(text = element_text(size=12))+ 
  scale_fill_viridis_d(direction = -1)+xlim(0,25)+ylim(0,25)+
  geom_text(x = 10, y = 20, label = eq(total_data$Erosion_T_ha1HCT.man,
                                      total_data$Erosion_T_ha1HCT.man.cli.soils), parse = TRUE)


sw3 <- ggplot(data = total_data[Erosion_T_ha1HCT.man.cli!=0])+
  geom_line(aes(x = Erosion_T_ha1HCT.man.cli**0*1:20, y = Erosion_T_ha1HCT.man.cli**0*1:20),
            col = "red", size=1.2)+ 
  geom_point(aes(x = Erosion_T_ha1HCT.man.cli, y = Erosion_T_ha1HCT.man.cli.soils), size=1.6)+
  geom_smooth(aes(x = Erosion_T_ha1HCT.man.cli, y = Erosion_T_ha1HCT.man.cli.soils), method = lm, show.legend = T)+
  labs(x = " \nSediment Delivery (t/ha/yr) \n s-HCT: man + cli",
       y =  "\nSediment Delivery (t/ha/yr) \n s-HCT: man + cli + soils\n") +
  theme_minimal() + coord_equal()+theme(text = element_text(size=12))+xlim(0,25)+ylim(0,25)+
  geom_text(x = 10, y = 20, label = eq(total_data$Erosion_T_ha1HCT.man.cli,
                                      total_data$Erosion_T_ha1HCT.man.cli.soils), parse = TRUE)


sw4 <- ggplot(data = total_data)+
  geom_line(aes(x = Erosion_T_ha1HCT.man.cli**0*1:20, y = Erosion_T_ha1HCT.man.cli**0*1:20),
            col = "red", size=1.2)+ 
  geom_point(aes(x = Erosion_T_ha1HCT.man.cli.soils, y = Erosion_T_ha1.scurve), size=1.6)+
  geom_smooth(aes(x = Erosion_T_ha1HCT.man.cli.soils, y = Erosion_T_ha1.scurve), method = lm, show.legend = T)+
  labs(x = " \nSediment Delivery (t/ha/yr) \n s-HCT: man + cli + soils",
       y =  "\nSediment Delivery (t/ha/yr) \n s-HCT: man + cli + soils + slope\n") +
  theme_minimal() + coord_equal()+theme(text = element_text(size=12))+
  geom_text(x = 2, y = 20, label = eq(total_data$Erosion_T_ha1HCT.man.cli.soils,
                                      total_data$Erosion_T_ha1.scurve), parse = TRUE)

sw5 <- ggplot(data = total_data)+
  geom_line(aes(x = Erosion_T_ha1HCT.man.cli**0*1:20, y = Erosion_T_ha1HCT.man.cli**0*1:20),
            col = "red", size=1.2)+ 
  geom_point(aes(x = Erosion_T_ha1HCT.man.cli.soils, y = Erosion_T_ha1.shaped), size=1.6)+
  geom_smooth(aes(x = Erosion_T_ha1HCT.man.cli.soils, y = Erosion_T_ha1.shaped), method = lm, show.legend = T)+
  labs(x = " \nSediment Delivery (t/ha/yr) \n s-HCT: man + cli + soils",
       y =  "\nSediment Delivery (t/ha/yr) \n s-HCT: man + cli + soils + slope + shapes\n") +
  theme_minimal() + coord_equal()+theme(text = element_text(size=12))


""
sw_T <- ggarrange(sw1, sw2, sw3, 
                  heights = c(3, 3, 3),
                  widths = c(3,3,3),
                  align = "h",
                  labels = c("A", "B", "C"),
                  ncol = 3, nrow = 1)


ggsave(path ="processed_data/batch/figures/2109/",
       filename = "21.09.29.stepwise.jpeg",device = "jpeg",plot = sw_T, width = 9.0, height = 3.5, units = "in") 

# o wepp hillslope compare points----
sed_comp <- ggplot(total_data)+
  geom_point(aes(x=Erosion_T_ha1HCT.man.cli.soils, y = total_data$Erosion_T_ha1.scurve, 
                 color = as.numeric(lowwer_s)/as.numeric(upper_s)))+
  geom_line(data = cbind(0:10, 0:10) %>% as.data.frame(), aes(V1,V2))+
  scale_color_viridis(direction = -1)+
  labs(x="Sediment delivery\nHCT.man.cli.soils (t/ha)",
       y = "\nSediment delivery\nS-HCT s-curve (t/ha)",
       color = "upper to lowwer \nslope ratio")+
  # ggtitle("WEPP-CLOUD verse maximum aggregation - Sediment kg/ha")+
  theme_minimal()+coord_equal()+xlim(0,14)

sed_comp_shape <- ggplot(total_data)+
  geom_point(aes(x=Erosion_T_ha1HCT.man.cli.soils, y = total_data$Erosion_T_ha1.shaped, 
                 color = as.numeric(lowwer_s)/as.numeric(upper_s)))+
  geom_line(data = cbind(0:10, 0:10) %>% as.data.frame(), aes(V1,V2))+
  scale_color_viridis(direction = -1)+
  labs(x="Sediment delivery\nHCT.man.cli.soils (t/ha)",
       y = "\nSediment delivery\nS-HCT with shapes (t/ha)",
       color = "upper to lowwer \nslope ratio")+
  coord_equal() +theme_minimal()+xlim(0,14)

sed_comp_shape_comp <- ggplot(total_data)+
  geom_point(aes(x=total_data$Erosion_T_ha1.scurve, y = total_data$Erosion_T_ha1.shaped, 
                 color = as.numeric(lowwer_s)/as.numeric(upper_s)))+
  geom_line(data = cbind(0:10, 0:10) %>% as.data.frame(), aes(V1,V2))+
  scale_color_viridis(direction = -1)+
  labs(x="Sediment delivery\nS-HCT s-curve (t/ha)",
       y = "\nSediment delivery\nS-HCT with shapes (t/ha)",
       color = "upper to lowwer \nslope ratio")+
  coord_equal() +theme_minimal()+xlim(0,14)

# o wepp hillslope compare space----
hill_wepp <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = TopazID, fill=Erosion_T_ha1HCT.man.cli.soils)) +
  scale_fill_gradientn(colors = magma(5, direction = -1), na.value = "transparent", breaks = c(0, 5, 10), 
                       labels = c("0","5","10+"), name = expression("Sediment\nDelivery\n (t/ha)"))+#scale_fill_viridis(direction= -1, option = "A", name = expression("Sediment\nDelivery\n (t/ha)"))+
  coord_equal() +
  theme_void() 

hill_CT_SED <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = group, fill= Erosion_T_ha1.scurve)) +
  scale_fill_gradientn(colors = magma(5, direction = -1), na.value = "transparent", breaks = c(0, 5, 10),
                       labels = c("0","5","10+"),limits = c(0,10), name = expression("Sediment\nDelivery\n (t/ha)"))+
  coord_equal() +
  theme_void() 


hill_CT_shape_SED <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = group, fill=Erosion_T_ha1.shaped)) +
  scale_fill_gradientn(colors = magma(5, direction = -1), na.value = "transparent", breaks = c(0, 5, 10), 
                       labels = c("0","5","10+"), limits = c(0,10), name = expression("Sediment\nDelivery\n (t/ha)"))+
  coord_equal() +
  theme_void()

hill_CT_sed_res <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = group, fill=Erosion_T_ha1.scurve-Erosion_T_ha1.shaped))+
  scale_fill_gradientn(colors = c("green",magma(5, direction = -1)), na.value = "transparent", breaks = c(0, 5, 10), 
                       labels = c("0","5","10+"), limits = c(-3,10), name = expression("Difference\n(t/ha)"))+
  coord_equal() +theme_void()

# save out figure 7 ----
wepp_hills_compare <- ggarrange(hill_wepp+h_28g, NA,hill_CT_SED+h_28g, sed_comp, hill_CT_shape_SED+h_28g, 
                                 sed_comp_shape, hill_CT_sed_res+h_28g,
                                sed_comp_shape_comp,
                                 align = "v",
                                 nrow = 4,
                                 ncol = 2, 
                                 labels = c("A", NA,"B","E",
                                            "C","F","D","G"))

ggsave("processed_data/batch/Figures/2109/thorn_wepp_slope_compare.jpeg", 
       wepp_hills_compare, dpi=300, 
       height =11, width = 8)


# FIGURE 9) relative abundance--------------
table(master_list$soil)

## wroung max - i did this again this spring... 
dt_merge_ct <- fread(file = "D:/Dropbox/active/PCD_work/PRW_BMP/processed_data/batch/geo_scratch/ct_data_201228.csv") #includes missing runs (996)

dt_merge_ct

#did not save a master list of land shapes??? what!!! 
land_types_df <- dt_merge_ct[, c("ID_cli", "ID_soil","ID_slp","ID_man", "ID_till", "ID_len", "ID_buf") := tstrsplit(ct, "_", fixed=TRUE)]
cli_table <- table(land_types_df$ID_cli)
soil_table <- table(land_types_df$ID_soil)
slp_table <- table(land_types_df$ID_slp)
man_table <- table(land_types_df$ID_man)
len_table <- table(land_types_df$ID_len)


## FIGURE 6) plot figure XX hydro--------
# #load data 
# fwrite(flow_sum_net$CT$max, "processed_data/batch/geo_scratch/net_flow_max_ct.txt")
# fwrite(flow_sum_net$MT$max, "processed_data/batch/geo_scratch/net_flow_max_mt.txt")                                                
# fwrite(flow_sum_net$NT$max, "processed_data/batch/geo_scratch/net_flow_max_nt.txt")                                                
# flowit <- fread("processed_data/batch/geo_scratch/net_flow.txt")
# flow_sum_net_CT <- fread("processed_data/batch/geo_scratch/net_flow_max_ct.txt")


# build a thing to facet ( max sum Net , ct)
ct_ids <-flow_sum_net_CT[,c("x","y","huc12", "RM")]
ct_flows <- flow_sum_net$CT$max[,c("x","y", "QOFE","net_flow")] %>%
 merge(.,flow_sum_net$CT$sum[,c("x","y", "QOFE")], by =c("x","y")) %>% 
  merge(.,flow_sum_net$CT$mean[,c("x","y", "QOFE")], by =c("x","y"))

setnames(ct_flows,
         c("x","y", "QOFE.x","net_flow", "QOFE.y","QOFE"), 
         c("x","y", "max","net_flow", "sum","mean"))

ct_flow_longer <- ct_flows %>% merge(., ct_ids)%>%
  pivot_longer(cols = c("max","net_flow", "sum", "mean")) %>% 
  as.data.table 

fwrite(ct_flow_longer,"processed_data/batch/geo_scratch/ct_flow_longer")
ct_flow_longer <- fread("processed_data/batch/geo_scratch/ct_flow_longer")
# 
# percentages by name 

# extract to c field then plot 


coordinates(ct_flow_longer)<-~x+y
crs(ct_flow_longer) <- crs(c_fields )
c_fields_flows <-ct_flow_longer %>% crop(.,c_fields)
c_fields_flows <- as.data.table(c_fields_flows)

#percentage by name 
max(c_fields_flows[name == "mean"]$value/c_fields_flows[name == "mean"]$RM) # 2.33
max(c_fields_flows[name == "max"]$value/c_fields_flows[name == "max"]$RM) # 2.5
max(c_fields_flows[name == "sum"]$value/c_fields_flows[name == "sum"]$RM) # 56.5
max(c_fields_flows[name == "net_flow"]$value/c_fields_flows[name == "net_flow"]$RM) # 11.97


c_filed_streams <- streams %>% crop(.,c_fields) %>% mask(., c_fields) %>%  
  as.data.frame(., xy =T)

c_field_stream_plot <- geom_tile(data = c_filed_streams%>% dplyr::filter(., !is.na(NED_PRW_ad8)), 
                                 aes(x = x, y = y, fill = NED_PRW_ad8), fill= "lightblue") 
# ggplot()+c_field_stream_plot
# geom_tile(data = urban_cf_gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y))

name_list <- c(
  mean = "A. Mean",
  max = "B. Max",
  net_flow = "C. Net flow",
  sum = "D. Sum"
  )

c_field_Q_plot <- ggplot(c_fields_flows %>%  
                           mutate(name = factor(name, levels=c('mean','max','net_flow','sum'))))+
  geom_tile(aes(x = x, y=y, fill = ((value)/RM)))+ 
  facet_wrap(~name, labeller = labeller(name = name_list))+
  scale_fill_viridis(direction = -1, limits = c(0,10))+ theme_minimal()+coord_equal()+
  labs(x = "Easting (m)", y="Northing (m)", fill ="Q/P")




ggsave((c_field_Q_plot + c_field_stream_plot+cf),
       file = "processed_data/batch/figures/2109/man1_cfield_runoff.jpeg", 
       dpi = 300, width = 6, height = 6, unit ="in")


# FIGURE 7 - Rpart--------

data          <- fread("processed_data/batch/201222_PRW_MASTER_LIST")
sed_data      <-  fread("processed_data/batch/sed_out_201210.txt")
data_shape    <- fread("processed_data/batch/210719_PRW_MASTER_LIST_shape")

data_total <- merge(data, sed_data)
rm(list=c("data","sed_data"))



data_shape_unique <- data_shape[,c(1,3,19)][
  , lapply(.SD, max, na.rm=TRUE), by = ID, .SDcols=c("Errosion_kg/m2", "Erosion_T_ha1")][
    , c("shape","climate", "soil","slope","rotation","tillage","length","buffer"):= tstrsplit(ID, "_", fixed=TRUE)][
      length %>% as.numeric<16,][tillage =="ct",][tillage=="ct"][
        ,slope:= as.numeric(slope)][
          ,climate:= as.numeric(climate)][
            ,length:= as.numeric(length)]

data_unique <- data_total[,c(1,3,12)][
  , lapply(.SD, max, na.rm=TRUE), by = ID, .SDcols=c("Errosion_kg/m2", "Erosion_T_ha1")][
    , c("climate", "soil","slope","rotation","tillage","length","buffer"):= tstrsplit(ID, "_", fixed=TRUE)][
      length %>% as.numeric<16,][tillage =="ct",][tillage=="ct"][
        ,slope:= as.numeric(slope)][
          ,climate:= as.numeric(climate)][
            ,length:= as.numeric(length)]

fit_data <- rpart(`Errosion_kg/m2` ~ length + climate + soil + slope + rotation,
                  data = data_unique)
plot_data <- rpart.plot(fit_data, tweak	=1.4)


fit_data_shape <- rpart(`Errosion_kg/m2` ~ shape+length + climate + soil + slope + rotation ,
                        data = data_shape_unique)
plot_data <- rpart.plot(fit_data_shape, tweak	=1.4)


data_rf <- randomForest(`Errosion_kg/m2` ~ 
                          length + climate + soil + slope + rotation ,
                        data = data_unique , ntree= 2000, importance = TRUE)
t<- importance(data_rf)
str(t)
plot(t)

tryit <- measure_importance(data_rf)

data_shape_rf <- randomForest(`Errosion_kg/m2` ~ 
                          shape + length + climate + soil + slope + rotation,
                          data = data_shape_unique , ntree= 2000, importance = TRUE)

importance(data_shape_rf)
# FIGURE 8. thorn erosn shape comparison -----

data_28_df <- (data_28 %>% 
  as.data.frame(., xy=T) %>% 
  as.data.table %>%  na.omit)[,difference_shaped := CT.max.Errosion - max.Errosion]



CTgp_28 <- gplot_data(data_28$CT.max.Errosion, maxpixels = 1e7) %>%
  dplyr::filter(., !is.na(value)) %>%
  mutate(value = ifelse(value*4.46>20, 20, ifelse(value*4.46<(-20), -20, value*4.46)))

CTgp_28_shaped <- gplot_data(data_28$max.Errosion, maxpixels = 1e7) %>%
  dplyr::filter(., !is.na(value)) %>%
  mutate(value = ifelse(value*4.46>20, 20, ifelse(value*4.46<(-20), -20, value*4.46))) %>% 
  mutate(difference = CTgp_28_shaped$value-CTgp_28$value)

ct_plot_28 <- ggplot() +
  geom_tile(data = CTgp_28, aes(x = x, y = y, fill = value))+
  scale_fill_gradientn(colors = c("darkgreen",magma(5, direction = -1)), 
                       na.value = "transparent", breaks = c(0, 5, 10), 
                       labels = c("0","5","10+"), limits = c(-2.5,10), 
                       name = expression("erosion\n(t/ha)"))+
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_classic()

ct_plot_28_shaped <- ggplot() +
  geom_tile(data = CTgp_28_shaped, aes(x = x, y = y, fill = value))+
  scale_fill_gradientn(colors = c("darkgreen",magma(5, direction = -1)), 
                       na.value = "transparent", breaks = c(0, 5, 10), 
                       labels = c("0","5","10+"), limits = c(-2.5,10), 
                       name = expression("erosion\n(t/ha)"))+
  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_classic()

# testit <- (land_types_df %>% subset(huc12==28) %>% fortify())
# 
# 
# flow_length_plot <- ggplot(data = testit)+
#   geom_tile(aes(x = x, y = y, fill = ID_len %>% as.numeric()))+theme_minimal()+lims(fill=c(12,15))
# 
# plot(testit$ID_len, testit$`Errosion_kg/m2`)
# plot(testit$ID_len, testit$Erosion_T_ha1)

# scale_fill_gradient2(low = "darkgreen", 
#                      mid = magma(5)[5], 
#                      high = "brown", midpoint = 0, 
#                      name = expression("tons/acre"))+
bb <- as(raster::extent(473000, 476000, 5218000, 5221000), "SpatialPolygons")
crs(bb) <- crs(data_28)

data_bb_df <- (data_28 %>% 
                 crop(., bb) %>% 
                 as.data.frame(., xy=T) %>% 
                 as.data.table)[
                   ,difference_shaped := CT.max.Errosion - max.Errosion]
plot_bb <-geom_path(data = bb %>% fortify(), aes(x = long, y=lat))                                                                                  
str_28 <- geom_tile(data = streams_28gp %>% dplyr::filter(., !is.na(value)), aes(x = x, y = y, ), fill = "lightblue")
str_bb <- geom_tile(data = streams_28 %>% crop(.,bb) %>% as.data.frame(.,xy=T) %>%
                      dplyr::filter(., !is.na(NED_PRW_ad8)), aes(x = x, y = y, ), fill = "lightblue")


ct_plot_28_difference <- ggplot() +
  geom_tile(data = data_28_df,
            aes(x = x, y = y,
                fill = ifelse(difference_shaped*10>20,20,
                              ifelse(difference_shaped*10<(-20),-20,difference_shaped*10))))+
  scale_fill_gradientn(colors = c("darkgreen","gray95","brown"),
                       na.value = "transparent", breaks = c(-20,-10,0, 10, 20), 
                       labels = c("-20-", "-10","0","10","20+"), 
                       limits = c(-20,20), name = expression("erosion\n(t/ha)"))+
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_classic()

ct_plot_bb_difference <- ggplot() +
  geom_tile(data = data_bb_df, aes(x = x, y = y, 
                                   fill = ifelse(difference_shaped*10>20,20,
                                                 ifelse(difference_shaped*10<(-20),-20,difference_shaped*10))))+
  scale_fill_gradientn(colors = c("darkgreen","gray95","brown"),
                       na.value = "transparent", breaks = c(-20,-10,0, 10, 20), 
                       labels = c("-20-", "-10","0","10","20+"), 
                       limits = c(-20,20), name = expression("erosion\n(t/ha)"))+
  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_void()+theme(legend.position = "none")

ct_plot_bb_max <- ggplot() +
  geom_tile(data = data_bb_df, aes(x = x, y = y,
                                   fill = ifelse(CT.max.Errosion*10>20,20,
                                                 ifelse(CT.max.Errosion*10<(-20),-20,CT.max.Errosion*10))))+
  scale_fill_gradientn(colors = c("darkgreen","gray95","brown"),
                       na.value = "transparent", breaks = c(-20,-10,0, 10, 20), 
                       labels = c("-20-", "-10","0","10","20+"), 
                       limits = c(-20,20), name = expression("erosion\n(t/ha)"))+
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_void()+theme(legend.position = "none")

ct_plot_bb_shape <- ggplot() +
  geom_tile(data = data_bb_df, aes(x = x, y = y, fill = ifelse(max.Errosion*10>20,20,
                                                               ifelse(max.Errosion*10<(-20),-20,max.Errosion*10))))+
  scale_fill_gradientn(colors = c("darkgreen","gray95","brown"),
                       na.value = "transparent", breaks = c(-20,-10,0, 10, 20), 
                       labels = c("-20-", "-10","0","10","20+"), 
                       limits = c(-20,20), name = expression("erosion\n(t/ha)"))+
  
  coord_equal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+
  theme_void()+theme(legend.position = "none")

ct_plot_28_points_max <- ggplot() +
  geom_point(data = data_28_df[CT.max.Errosion!=0], aes(x = CT.max.Errosion*10, y = max.Errosion*10),
             size = .8, alpha = .5)+
  labs(x = "S-HCT s_curve \nerosion (t/ha)", y = "S-HCT with shapes \nerosion (t/ha)")+
  scale_fill_viridis()+ 
  coord_equal()+
  theme_minimal()

ct_plot_28_points_mean <- ggplot() +
  geom_hex(data = data_28_df, aes(x = CT.mean.Errosion, y = mean.Errosion), bins = 120)+
  labs(x = "S-HCT s_curve \nerosion (t/ha)", y = "S-HCT with shapes \nerosion (t/ha)")+
  scale_fill_viridis()+ 
  coord_equal()+
  theme_minimal()

ct_plot_28_points_sum <- ggplot() +
  geom_hex(data = data_28_df %>% na.omit() %>% subset(.,CT.sum.Errosion!=0), 
           aes(x = CT.sum.Errosion, y = sum.Errosion), bins = 120)+
  labs(x = "S-HCT s_curve \nerosion (t/ha)", y = "S-HCT with shapes \nerosion (t/ha)")+
  scale_fill_viridis()+ 
  coord_equal()+
  theme_minimal()



lay <- rbind(c(1,2,3,4,4),
             c(5,5,5,4,4),
             c(5,5,5,4,4))

thorn_shape_comp_arr <- grid.arrange(grobs = list(ct_plot_bb_max + str_bb+plot_bb, 
                                                  ct_plot_bb_difference + str_bb+plot_bb, 
                                                  ct_plot_bb_shape + str_bb+plot_bb,
                                                  ct_plot_28_points_max,
                                                  ct_plot_28_difference+str_28+h_28g+plot_bb), 
             layout_matrix = lay)


thorn_shape_comp <- ggarrange(ct_plot_28_shaped, ct_plot_28_shaped,NA, ct_plot_28_points_max,
                              align = "h",
                              nrow = 2,
                              ncol = 2, 
                              labels = c("A","B",NA,"C"))

thorn_shape_comp_diff <- ggarrange(ct_plot_28_difference+h_28g, ct_plot_28_points_max,
                                   widths = c(2,1), 
                                   labels = c("A","B"))
ggsave("processed_data/batch/Figures/2109/thorn_Shape_compare.jpeg", 
       thorn_shape_comp, dpi=300, 
       height =4, width = 8)

ggsave("processed_data/batch/Figures/2109/thorn_Shape_compare_diff.jpeg", 
       thorn_shape_comp_arr, dpi=300, 
       height =4, width = 8)
hist(CTgp_28_shaped$difference)

### Note: there is helpful story here. I am not sure exactly what it is. 
# maybe in writing it will be more clear - the difference in many slopes with slope shape
# mean being wrong much less - flow paths with v. steep slopes are common at 30m and not at 
# the hill slope scale. (I need to show this very clearly.) It makes sense to me that they occur 
# at the head cut of the valley... == point of maximum  


# ggplot()+c_field_stream_plot
# ### SCRAP -----------------
# ## ploting erosion of thorn to move ------------------------
# 
# ggplot() +
#   geom_tile(data = hct_thorn_erosion2, aes(x = x, y = y, fill = CT_mean))+
#   scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0,na.value = NA,
#                        name = expression("tons/acre"))+
#   theme_void()
# 
# ggplot() +
#   geom_tile(data = hct_thorn_erosion2, aes(x = x, y = y, fill = CT_shape_mean))+
#   scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0,na.value = NA,
#                        name = expression("tons/acre"))+
#   theme_void()
# 
# ggplot() +
#   geom_tile(data = hct_thorn_erosion2, aes(x = x, y = y, fill = CT_shape_mean-CT_mean))+
#   scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0,na.value = NA,
#                        name = expression("tons/acre"))+
#   theme_void()
# 
# 
# 
# ggplot() +
#   geom_tile(data = hct_thorn_erosion2, aes(x = x, y = y, fill = sed_CT_mean))+
#   scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0,na.value = NA,
#                        name = expression("tons/acre"))+
#   theme_void()
# 
# ggplot() +
#   geom_tile(data = hct_thorn_erosion2, aes(x = x, y = y, fill = sed_CT_shape_mean))+
#   scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0,na.value = NA,
#                        name = expression("tons/acre"))+
#   theme_void()
# 
# ggplot() +
#   geom_tile(data = hct_thorn_erosion2, aes(x = x, y = y, fill = sed_CT_shape_mean-sed_CT_mean))+
#   scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0,na.value = NA,
#                        name = expression("tons/acre"))+
#   theme_void()
# 
# ggplot() +
#   geom_tile(data = hct_thorn_erosion_GP$CT_shape_mean, aes(x = x, y = y, fill = value))+
#   scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0,na.value = NA,
#                        name = expression("tons/acre"))+
#   theme_void()
# 
# 
# ggplot() +
#   geom_tile(data = hct_thorn_erosion_GP$CT_shape_mean-, aes(x = x, y = y, fill = value))+
#   scale_fill_gradient2(low = "darkgreen", mid = magma(5)[5], high = "brown", midpoint = 0,na.value = NA,
#                        name = expression("tons/acre"))+
#   theme_void()

