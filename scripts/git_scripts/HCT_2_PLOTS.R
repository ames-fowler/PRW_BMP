


# Read in data: ------------

#
dt_merge_ct <- fread(file = "processed_data/batch/geo_scratch/ct_data_210909.csv")
dt_merge_ct_buf <- fread(file = "processed_data/batch/geo_scratch/ct_buf_data_210909.csv")

dt_merge_mt <- fread(file = "processed_data/batch/geo_scratch/mt_data_210909.csv")
dt_merge_mt_buf <- fread(file = "processed_data/batch/geo_scratch/mt_buf_data_210909.csv")

dt_merge_nt <- fread(file = "processed_data/batch/geo_scratch/nt_data_210909.csv")
dt_merge_nt_buf <- fread(file = "processed_data/batch/geo_scratch/nt_buf_data_210909.csv")

profiles <- dt_merge_ct[,c(1,3,4,5,6)]

merged <- list("CT"=(dt_merge_ct),"MT"=(dt_merge_mt), "NT"=(dt_merge_nt),"CT_buf"=(dt_merge_ct_buf),"MT_buf"=(dt_merge_mt_buf), "NT_buf"=(dt_merge_nt_buf))

# i don't know why the "9_d_25_g_no_13_0" failed but i have manually fixed it here... 
Clean_up <- function(dt_xy){
  indexit <- dt_xy[,2]=="9_d_25_g_no_13_0" %>% unlist
  dt_xy[c(indexit),7] <- 0
  return(dt_xy)
}

merged_clean <- merged %>% map(.,Clean_up) 
rm(list = c("merged","dt_merge_ct",
    "dt_merge_mt","dt_merge_mt_buf",
    "dt_merge_nt","dt_merge_nt_buf"))
gc()


### figure 1 site map 
landtypes_man <- raster("processed_data/gis_processed/landtype_210401_2.tif")

my_col_man <-c("#F2F2F2FF" ,"#EFC2B3FF", "burlywood4",
               "lightblue1","#00A600FF", "#3EBB00FF", 
               "#8BD000FF", "#E6E600FF")

my_col_man_labs <- c("Other", "Range","Forest","Water",
                     "WW-B-P", "WW-B-F", "WW-F", "Irrigated")
p_man <- gplot((landtypes_man),maxpixels=1e7) +
  geom_raster(aes(fill =as.factor(value))) +#
  # facet_wrap(~ variable) +
  scale_fill_manual(values=my_col_man,  na.translate = FALSE, labels= my_col_man_labs, name= "Management")+ 
  coord_equal()+labs(x="\nEasting (m)", y="Northing(m)\n")+theme_minimal()

c_fields_p <- fortify(c_fields)$lat %>% mean %>% as.data.frame()
c_fields_p$long <- fortify(c_fields)$long %>% mean
names(c_fields_p) <- c("y","x")

cf_point <- geom_point(data= c_fields_p,
                       aes(x =x, y=y), size = 4, shape = 18, 
                       color='black') 
PRW <- shapefile("raw/extent/PRW_shape")
PRW_DF <- PRW %>% fortify
huc12<-shapefile("RAW/extent/HUC12")
i <- (huc12$OBJECTID)
MFC <- geom_polygon(data = subset(huc12, OBJECTID == i[79]),
                      aes(x =long,y= lat, group=group), fill = "black")


n <- geom_polygon(data= PRW_DF,aes(x =long,y= lat) , fill=NA , col='grey65')

ggsave((p_man + cf_point + MFC+n), file ="processed_data/batch/figures/2109/man_site.jpeg", 
       height = 5, width = 8, dpi = 300)

### figure 2 bar plot ------------------------------


#this function combines the overlapping cells with Mean- max- and average 
#updated the the sum algorithm to solve for a most mass balanced flow. 
sum_it <- function (x){ 
  temp_sum <- x[, lapply(.SD, sum, na.rm=TRUE), by=c("x","y"), 
                .SDcols=c("huc12","Errosion_kg/m2","Erosion_T_ha1","total_Q","QOFE", "UpStrmQ", "total_Lat","RM","latqcc")] # removeNA = T before this? 
  
  
  
  temp_max <- x[, lapply(.SD, max, na.rm=TRUE), by=c("x","y"), 
                .SDcols=c("huc12","Errosion_kg/m2","Erosion_T_ha1","total_Q","QOFE", "total_Lat","RM","latqcc")]
  temp_mean <- x[, lapply(.SD, mean, na.rm=TRUE), by=c("x","y"), 
                 .SDcols=c("huc12","Errosion_kg/m2","Erosion_T_ha1","total_Q","QOFE", "total_Lat","RM","latqcc")]
  ID <- x[, lapply(.SD, function(x) x[which.max(Erosion_T_ha1)]), by=c("x","y"), .SDcol = 1:4]
    # x[, lapply(.SD,first), by = c("x","y"), .SDcol = 1:4] #add huc12 -- this is old news had problems with iding weird grouping 
  temp_sum <- merge(temp_sum,ID)
  temp_max <- merge(temp_max,ID)
  temp_mean <- merge(temp_mean,ID)
  temp <- list(temp_sum,temp_max, temp_mean)
  names(temp) <-c("sum", "max", "mean")
  return(temp)
}
# tic()
# testing <- head(merged$CT,10000)[, lapply(.SD, function(x) x[which.max(Erosion_T_ha1)]), by=c("x","y"), .SDcol = 1:4]
# toc() 10x faster than the straight up which max - vectorized... 
# tic()
# testing2 <- head(merged$CT,10000)[, .SD[which.max(Erosion_T_ha1)], by=c("x","y"), .SDcol = 1:4]
# toc()
# testing==testing2


sum_dat <- merged_clean %>% map(sum_it) %>% unlist(recursive =F) 
# sum_dat_flat <- do.call(rbind, sum_dat)
gc()

# fwrite(sum_dat,file = "processed_data/batch/geo_scratch/210915_sum_dat")
# sum_dat <- fread(file = "processed_data/batch/geo_scratch/210915_sum_dat")

sum_dat_max <- sum_dat[grepl("max", names(sum_dat))]
## note the selection of the first ID here tosses out noag aggragated with ag. 
sum_dat_max$CT.max <- sum_dat_max$CT.max[!grepl("no", ct)]
sum_dat_max$MT.max <- sum_dat_max$MT.max[!grepl("no", mt)]
sum_dat_max$NT.max <- sum_dat_max$NT.max[!grepl("no", nt)]

sum_dat_max$NT_buf.max[grepl("NA", nt_buf)]

# results, percentage of land area under CT that is below the threshold: -----
(sum_dat$CT.max[,sum_dat$CT.max$`Errosion_kg/m2`>1.12] %>% sum)/nrow(sum_dat$CT.max)*100


testit2[nrow(testit2)*.25]  # 0.098231kg/m2 = 0.98 kg/m2 = 0.4381103



sum_noag<- function (x){
  erosion <- sum(x[,4]*900/1000)  #TONNES or Mg
  sed_delivery <- sum(x[,5]*900/10000) #TONNES or Mg
  total_lat <- sum(x[,8]/x[,9])
  total_Q <- sum(x[,6]/x[,9])
  temp_out <- list(erosion,sed_delivery, total_lat, total_Q)
  names(temp_out) <-c("Erosion_Mg", "Sed_Mg", "Lateral_percent", "Q_percent")
  return(temp_out)
}
# x<- sum_dat_max$CT.max %>% head(100) %>% list()

test <- sum_dat_max %>% map(sum_noag)
testit2 <- test %>% unlist %>% 
  matrix(nrow =4, ncol = 6) %>%
  t() %>% as.data.frame()%>% 
  mutate(v5 = c("CT", "MT","NT","CT Buffer","MT Buffer","NT Buffer" ))
colnames(testit2) <- c("Erosion_Mg", "Sed_Mg", "Lateral_percent", "Q_percent", "Management")

# percent reduce CT to MT 
((testit2 %>% as.data.table)[Management =="CT"]$Erosion_Mg-
    (testit2 %>% as.data.table)[Management =="MT"]$Erosion_Mg)/
  (testit2 %>% as.data.table)[Management =="CT"]$Erosion_Mg
# percent reduce MT to NT 
((testit2 %>% as.data.table)[Management =="MT"]$Erosion_Mg-
    (testit2 %>% as.data.table)[Management =="NT"]$Erosion_Mg)/
  (testit2 %>% as.data.table)[Management =="MT"]$Erosion_Mg


# percent reduce MT to NT 
((testit2 %>% as.data.table)[Management =="CT"]$Erosion_Mg-
    (testit2 %>% as.data.table)[Management =="NT"]$Erosion_Mg)/
  (testit2 %>% as.data.table)[Management =="CT"]$Erosion_Mg

## Figure 2 HUC CSA area -------------
max_an_erosion <- ggplot(data = testit2)+
  geom_col(aes(x = Management, y = Erosion_Mg, fill= Management))+ theme_classic()+
  ylab('Erosion (Mg/year)')+scale_fill_manual(values=viridis(6))

max_an_sediment <- ggplot(data = testit2)+
  geom_col(aes(x =Management, y = Sed_Mg, fill= Management))+ theme_classic()+
  ylab('Sediment delivery (Mg/year)')+scale_fill_manual(values=viridis(6))



cum_sedxy <- sum_dat_max$CT.max[,c(1,2,4)] %>% 
  merge(sum_dat_max$MT.max[,c(1,2,4)], by=c("x","y"), suffixes = c(".CT", ".MT")) %>%
  merge(sum_dat_max$NT.max[,c(1,2,4)], by=c("x","y")) %>%
  merge(sum_dat_max$CT_buf.max[,c(1,2,4)], by=c("x","y"), suffixes = c(".NT", ".CT_buf")) %>%
  merge(sum_dat_max$MT_buf.max[,c(1,2,4)], by=c("x","y")) %>%
  merge(sum_dat_max$NT_buf.max[,c(1,2,4)], by=c("x","y"), suffixes = c(".MT_buf", ".NT_buf"))

names(cum_sedxy)<- c("x","y","CT", "MT","NT","CT_Buf","MT_Buf","NT_Buf")
cum_sedxy[,area_ha := .09]
testit2<- cum_sedxy[order(CT, decreasing = T)][,c("x","y"):= NULL]
testit25<- cum_sedxy[CT>0][order(CT, decreasing = T)][,c("x","y"):= NULL]
testit3 <-  testit25[, lapply(.SD, cumsum)]
testit4 <- testit3[, lapply(.SD, function(x){x/max(x)})][sample(.N,40000)] %>% pivot_longer(.,names(.)[-7],"Management")
testit5 <- testit3[sample(.N,40000)] %>% pivot_longer(.,names(.)[-7],"Management")

#percent reductions from CT to NT for relative CSA 
(testit3[nrow(testit3)*.25]$CT-testit3[nrow(testit3)*.25]$MT)/testit3[nrow(testit3)*.25]$CT
(testit3[nrow(testit3)*.25]$CT-testit3[nrow(testit3)*.25]$NT)/testit3[nrow(testit3)*.25]$CT

plot_cumu_erosion_rel <- ggplot(testit4, aes(x = area_ha/max(area_ha)*100, y = value, group = Management))+
  geom_line(aes(color = Management), size =1)+
  scale_color_manual(values=viridis(6))+theme_minimal()+
  xlab("Percent area")+ylab("Cumulative annual erosion (Mg) \n")

plot_cumu_erosion <- ggplot(testit5, aes(x = area_ha/max(area_ha)*100, y = value, group = Management))+
  geom_line(aes(color = Management), size =1)+
  scale_color_manual(values=viridis(6))+theme_minimal()+
  xlab("Percent area")+ylab("Cumulative annual erosion (Mg) \n")

percent <- 0.15
ttt <- testit3[round(nrow(testit3)*percent),]*.09/round(nrow(testit3)*percent)
ttt

(ttt$CT*ttt$area_ha/(nrow(testit3)*percent)) %>% mean()

relative_erosion <- ggarrange(max_an_erosion+ theme(legend.position = "none") ,
                              max_an_sediment+ theme(legend.position = "none") ,
                              plot_cumu_erosion_rel + 
                                theme(legend.position = c(0.75, 0.5),
                                      legend.background = element_rect(fill="white",
                                                                       linetype="solid")),
                             align = "h",
                             nrow= 1,
                             ncol = 3,
                             labels = c("A","B","C"),
                             vjust=20, 
                             hjust = -2)
# relative_erosion-----

ggsave("processed_data/batch/Figures/2109/man_2_relative_erosion.jpeg", 
       relative_erosion, dpi=300, 
       height =3, width = 10)


## figure XXX threshold -----
# results, percentage of land area under CT that is above the threshold: 
sum_dat_max$CT.max[,sum_dat_max$CT.max $`Errosion_kg/m2`>1.12] %>% sum/nrow(sum_dat_max$CT.max) 

# (merged$CT[,merged$CT$`Errosion_kg/m2`>1.12] %>% sum)/nrow(merged$CT)*100
testit25[(.25*(nrow(testit25))) %>% round]
# CT      MT NT  CT_Buf  MT_Buf  NT_Buf area_ha
# 1: 1.01094 0.09276  0 0.64499 0.01941 0.00087    0.09

# sim_xy <- merged$CT

unique_IDs <- function(sim_xy){
  temp <- sim_xy[,c(2:3,7:8)][sim_xy$`Errosion_kg/m2`>1.12]  
  names(temp) <- c("ID", "steps","Errosion_kg/m2", "Erosion_T_ha1")
  temp2 <- temp[, c("climate", "soil","slope","rotation","tillage","length","buffer"):= tstrsplit(temp$ID, "_", fixed=TRUE)]
 
    #split it up 
  return(temp)}

ID_max <- merged %>% map(., unique_IDs)
ID_max_tot <- do.call(rbind, ID_max)
fwrite(ID_max_tot, file = "processed_data/batch/geo_scratch/210915_ID_max_tot")

ID_max_tot <- fread("processed_data/batch/geo_scratch/210915_ID_max_tot")
ID_max_tot[grepl(pattern = "9_d_25_g_no_13_0",ID_max_tot$ID)] <- NA # DROP error grass values 

ID_max_tot$steps <- as.numeric(as.character(ID_max_tot$steps))
ID_max_tot$climate <- as.numeric(as.character(ID_max_tot$climate))
ID_max_tot$slope <- as.numeric(as.character(ID_max_tot$slope))
ID_max_tot$length <- as.numeric(as.character(ID_max_tot$length))
ID_max_tot$buffer <- as.numeric(as.character(ID_max_tot$buffer))

# one box per variety
play <- ID_max_tot[buffer==0] %>% 
  mutate(climate_group = ifelse(climate <6, "<465mm", 
                                                 ifelse(climate <8 &climate >5, 
                                                        "465-534mm", ">534mm")),
         climate_group=factor(climate_group,levels=c("<465mm","465-534mm", ">534mm")),
         length_group = ifelse(length <6, "<150m", 
                               ifelse(length <11 & length >5, 
                                      "150-300m",">300m")),
         length_group=factor(length_group,levels=c("<150m","150-300m", ">300m")),
         slope_group = ifelse(slope <6, "flat", 
                               ifelse(slope>6 & slope< 12, "moderate","steep"))%>% 
           as.factor(),
         soil_group = ifelse(soil == "d", "deep", 
                             ifelse(soil == "m" , "medium","shallow")),
         soil_slope = paste0(soil_group," and ", slope_group),
         tillage=factor(tillage,levels=c("ct","mt", "nt", "no")), 
         prop = )
           
# total_temp <- merged %>% map(., unique_IDs) # this needs to be the total land area 
# total_temp <-  do.call(rbind, total_temp)     
total_temp <- sum_dat_max$CT.max[
  , c("climate", "soil","slope","rotation","tillage","length","buffer"):=
    tstrsplit(ct, "_", fixed=TRUE)]

total_temp_mt <- sum_dat_max$MT.max[
  , c("climate", "soil","slope","rotation","tillage","length","buffer"):=
    tstrsplit(mt, "_", fixed=TRUE)]

total_temp_nt <- sum_dat_max$NT.max[
  , c("climate", "soil","slope","rotation","tillage","length","buffer"):=
    tstrsplit(nt, "_", fixed=TRUE)]

total_temp <- total_temp  %>%
  mutate(climate = climate %>% as.numeric(),
    climate_group = ifelse(climate <6, "<465mm", 
                                ifelse(climate <8 &climate >5, 
                                       "465-534mm", ">534mm")),
         climate_group=factor(climate_group,levels=c("<465mm","465-534mm", ">534mm")),
    length = length %>% as.numeric(),
         length_group = ifelse(length <6, "<150m", 
                               ifelse(length <11,"150-300m",">300m")),
         length_group=factor(length_group,levels=c("<150m","150-300m", ">300m")),
    slope = slope %>% as.numeric(),
         slope_group = ifelse(slope <6, "flat", 
                              ifelse(slope>6 & slope< 12, "moderate","steep"))%>% 
           as.factor(),
         soil_group = ifelse(soil == "d", "deep", 
                             ifelse(soil == "m" , "medium","shallow")),
         soil_slope = paste0(soil_group," and ", slope_group),
         tillage=factor(tillage,levels=c("ct","mt", "nt", "no")))

# % shallow 
(total_temp[soil == "s"] %>% nrow)/(total_temp %>% nrow)
(total_temp[soil == "s"][`Errosion_kg/m2`>0]$`Errosion_kg/m2` %>% sum)/
  (total_temp[`Errosion_kg/m2`>0]$`Errosion_kg/m2` %>% sum)

(total_temp_mt[soil == "s"] %>% nrow)/(total_temp_mt %>% nrow)
(total_temp_mt[soil == "s"][`Errosion_kg/m2`>0]$`Errosion_kg/m2` %>% sum)/
  (total_temp_mt[`Errosion_kg/m2`>0]$`Errosion_kg/m2` %>% sum)

(total_temp_nt[soil == "s"][`Errosion_kg/m2`>0]$`Errosion_kg/m2` %>% sum)/
  (total_temp_nt[`Errosion_kg/m2`>0]$`Errosion_kg/m2` %>% sum)



total_temp_agg <- total_temp[buffer==0] %>% 
            na.omit %>% 
            group_by(soil_slope, climate_group) %>% 
            count %>%  as.data.frame

total_temp_agg <- total_temp_agg%>% as.data.table()
total_temp_agg[,total_n := list(sum=sum(n)), by=c("climate_group")]

threshold_col <-c(viridis(9,alpha = .7)[3],viridis(9, alpha = .8)[2] ,viridis(9)[1],
           viridis(9,alpha = .8)[6],viridis(9, alpha = .8)[5],viridis(9)[4],
           viridis(34,alpha = .8)[32],viridis(34, alpha = .8)[33],viridis(34)[34])



landtype_plot <- ggplot(total_temp_agg) +
  geom_bar(aes(x=2, y= n/total_n, fill = soil_slope), 
           position = "stack",width = 1, stat = "identity", col = grey(.50))+
  scale_fill_manual(values = threshold_col)+
  coord_polar(theta = "y", start = 0)+
  facet_grid(~climate_group, margins = 1)+
  theme_void()+
  xlim(0.5, 2.5)
  

CSA_rel_count <- ggplot(play %>% na.omit(),
       aes(fill=soil_slope, x=tillage), color = "white") + 
  geom_bar() + facet_grid(length_group ~ climate_group)+theme_minimal()+
  scale_fill_manual(values = threshold_col[2:length(threshold_col)])+
  labs(fill = "Soil and Slope")

#stats
(play[soil=="s"] %>% nrow)/(play %>% nrow)
(ID_max_tot[tillage == "mt"][soil=="s"] %>% nrow())/(ID_max_tot[tillage == "mt"] %>% nrow()) ### 0.09671372
(ID_max_tot[tillage == "ct"][soil=="s"] %>% nrow())/(ID_max_tot[tillage == "ct"] %>% nrow()) ### 0.0484723
(ID_max_tot[tillage == "nt"][soil=="s"] %>% nrow())/(ID_max_tot[tillage == "nt"] %>% nrow()) ### 0.7935531

(ID_max_tot[tillage == "ct"][soil=="s"]$`Errosion_kg/m2` %>% sum())/(ID_max_tot[tillage == "ct"]$`Errosion_kg/m2` %>% sum()) ### 0.2101307
(ID_max_tot[tillage == "mt"][soil=="s"]$`Errosion_kg/m2` %>% sum())/(ID_max_tot[tillage == "mt"]$`Errosion_kg/m2` %>% sum()) ### 0.3315815
(ID_max_tot[tillage == "nt"][soil=="s"]$`Errosion_kg/m2` %>% sum())/(ID_max_tot[tillage == "nt"]$`Errosion_kg/m2` %>% sum()) ### 0.872226
# csa_look <- ggarrange(landtype_plot+ theme(legend.position = "none"), CSA_rel_count,
#                       align = "v",
#                               nrow= 2,
#                                ncol = 1)

ggsave("processed_data/batch/Figures/2109/CSA_rel_count_5T.jpeg", 
       CSA_rel_count, dpi=300, height =10, width = 8) 
       
ggsave("processed_data/batch/Figures/2109/CSA_rel_landtype_plot.jpeg", 
       landtype_plot+ theme(legend.position = "none"), dpi=300, height =4, width = 8) 


fit_1 <- rpart(`Errosion_kg/m2` ~ steps+ length + climate + soil + slope + rotation +
                 tillage+buffer, data = ID_max_tot)
plot_1 <- rpart.plot(fit_1, tweak	=1.5)

  #### not to self: high erosion from 9_D_25_g_no_0 is an error from NA 
  #### temp2$`Errosion_kg/m2`[grepl("no",temp2$ct)] %>% unique
  #### no tillage values are due to low roughness(I think)
  
  last_buf <- last_buf[, c("climate", "soil","slope","rotation","tillage","length","buffer"):= tstrsplit(ID, "_", fixed=TRUE)][buffer==10][OFE>(as.numeric(length)-1)]
  


ID_CT_CSA <- (sum_dat_max$CT.max$ct[sum_dat_max$CT.max$`Errosion_kg/m2`>1.12]%>%
  unique())[, c("climate", "soil","slope","rotation","tillage","length","buffer"):= tstrsplit(ct, "_", fixed=TRUE)]
ID_MT_CSA <- (merged$MT[merged$MT$`Errosion_kg/m2`>1.12])$mt %>% unique()
ID_NT_CSA <- (merged$NT[merged$NT$`Errosion_kg/m2`>1.12])$nt %>% unique()

ID_CT_buf_CSA <- (merged$CT[merged$CT$`Errosion_kg/m2`>1.12])$ct %>% unique()
ID_MT_buf_CSA <- (merged$MT[merged$MT$`Errosion_kg/m2`>1.12])$mt %>% unique()
ID_NT_CSA <- (merged$NT[merged$NT$`Errosion_kg/m2`>1.12])$nt %>% unique()



## looking at sums and averages ----
tempit <- sum_dat$CT.max[, c("climate", "soil","slope","rotation","tillage","length","buffer"):= tstrsplit(ct, "_", fixed=TRUE)]
tempit[tillage!="no"][slope>5][steps.x/as.numeric(length)<.66]

tempit[tillage!="no"][slope>5][climate==9]$Erosion_T_ha1 %>% summary
(tempit[huc12==79][`Errosion_kg/m2`>1.12] %>% nrow)/(tempit[huc12==79] %>% nrow)

(sum_dat_max$CT.max$`Errosion_kg/m2`- sum_dat_max$CT_buf.max$`Errosion_kg/m2`) %>% 
  sum/sum(sum_dat_max$CT.max$`Errosion_kg/m2`)


####PLOT huc 12  -----------------
# load data 
huc12 <- sf::st_read("processed_data/huc12.shp")
# huc12 <- huc12[,c("OBJECTID", "Id", "gridcode",    "Shape_Leng",    "Shape_Area")]

by_huc12_cells <- function(x){
  area <- x[, .(area_m2 = .N*900), by = huc12]
  area_CSA_erosion_5t <- x[, .(csa_5t_m2 = sum(`Errosion_kg/m2` > (5/4.46))*900/10000), by = huc12]
  area_CSA_erosion_4t <- x[, .(csa_4t_m2 = sum(`Errosion_kg/m2` > (4/4.46))*900/10000), by = huc12]
  area_CSA_erosion_3t <- x[, .(csa_3t_m2 = sum(`Errosion_kg/m2` > (3/4.46))*900/10000), by = huc12]
  area_CSA_erosion_2t <- x[, .(csa_2t_m2 = sum(`Errosion_kg/m2` > (2/4.46))*900/10000), by = huc12]
  area_CSA_erosion_438t <- x[, .(csa_438t_m2 = sum(`Errosion_kg/m2` > (.101))*900/10000), by = huc12]
  temp <- merge(area, area_CSA_erosion_5t) %>%  merge(area_CSA_erosion_4t)%>%
    merge(area_CSA_erosion_3t) %>% merge(area_CSA_erosion_2t) %>% merge(area_CSA_erosion_438t)
  temp$huc12 <- as.character(temp$huc12)
  temp <- list(temp)
  return(temp)
}

## by pulling from flow paths, the total values are double counting, relative values should be okay. 

sum_dat$CT.max$`Errosion_kg/m2`[(sum_dat$CT.max$ct=="9_d_25_g_no_13_0" )] <- 0
huc12_data_ct_cells <- list(sum_dat$CT.max) %>% map(by_huc12_cells)


huc12 <- huc12 %>% merge(.,huc12_data_ct_cells, by.x = "OBJECTID", by.y = "huc12" )

CSA_CT_5t <- ggplot(huc12) +
  geom_sf(aes(fill = csa_5t_m2/Shape_Area *10000))+
  coord_sf(datum = st_crs(huc12))+
  scale_fill_viridis_c(option = "B", direction = -1)+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = -45))+
  labs(x = "\nEasting (m)", y = "Northing (m)\n", fill = "Absolute CSA Area (%)")


CSA_CT_438t <- ggplot(huc12) +
  geom_sf(aes(fill = csa_438t_m2/Shape_Area *10000))+
  coord_sf(datum = st_crs(huc12))+
  scale_fill_viridis_c(option = "B", direction = -1)+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = -45))+
  labs(x = "\nEasting (m)", y = "Northing (m)\n", fill = "Relative CSA Area (%)")


huc12_CSA_percent <- ggarrange(CSA_CT_5t, CSA_CT_438t,
                              align = "h",
                              nrow= 1,
                              ncol = 2,
                              labels = c("A","B"),
                              vjust=17, 
                              hjust = -2)

ggsave("processed_data/batch/Figures/2109/CSA_comp_percent.jpeg", 
       huc12_CSA_percent, dpi=300, 
       height =3, width = 10)
### Table xx pull average errotion rates per crop land by region for slopes of a type ---------
# The absolute values of erosion underpredict past erosion studies with discrepancies in the definition of erodible hillslopes area. Comparison of S-HCT model results are presented to previous studies of erosion values in table XX. Early studies of erosion in the basin use the XXX rill measurement method and RUSLE modeling for representative hillslopes (cite). Following work of Kok et al., (2009) employed  RUSLE 2 and XX field overservation. Geener 2006 applied the WEPP model to plot data over one season. All comparisons require extrapolation of representative data and representative simulations to the watershed scale on the basis of average slope and average slope length and are only applicable to the “erosive portion” of a hill slope and may or may not include a sediment delivery ratio (“cite”). To compare the S-HCT results which include the toe slope simulation and calculated deposition, we defined the erosion-able portion of the hillslope are all area with slope gradients >8%  and selected the top tow thirds the profile run to remove the simulated toe slope. Values after this modification are shown in table X. The average CT erosion rate for climate zone 8 and 9 under continuous cropping was XXX, ^ and 7 under WW-b-F was XXX, and 5  under WW-F was XXX. The S-HCT simulation has the advantage of integrating the best available public spatial data, and so integrates heterogeneity for regional estimates instead of averaging the heterogeneity away. The use of immerging, meso scale measurements of soil loss – edging toward feasibility with Plane and Unoccupied, Aerial vehicles (UVA) hosted Lidar and structure from motion photogrammetry – might finally give use the spatial data sets to validate erosion models before board extrapolation.       
# Table 1. Palouse erosion historic estimates 
# Study 	Climate zone		notes
# <=5	6 to 7	>= 8 	
# S-HCT	2.36	4.16	13.92	slopes>=12, upper 2/3 slopes, uncalibrated WEPP simulations. 
# Samrat et al 2021	X	X	X	Wepp model, represtnative calibrated to XXX 
# kok et al 2009 	21	28	40	
# Greer et al 2006			69.7	plot
# McCool and Roe (2005)		53.8		manual field measurements 
# A.J. Busacca, C.A. Cook and D.J. Mulla			11.6	plot
# 1978		31.4	20.6	manual field measurements 

upslope_erosion <- sum_dat_max$CT.max[
  , c("climate", "soil","slope","rotation","tillage","length","buffer"):= tstrsplit(ct, "_", fixed=TRUE)][
    steps.x/as.numeric(length)<.66][as.numeric(slope)>8]
upslope_erosion_mean <- upslope_erosion$`Errosion_kg/m2`%>% mean*10

upslope_erosion_low_prcip <- ((upslope_erosion[(climate %>% as.numeric())<6])$`Errosion_kg/m2` )%>% mean*10
upslope_erosion_high_prcip <- ((upslope_erosion[(climate %>% as.numeric())>7])$`Errosion_kg/m2` )%>% mean*10
upslope_erosion_mid_prcip <- ((upslope_erosion[(climate %>% as.numeric())<8&(climate %>% as.numeric())>5])$`Errosion_kg/m2` )%>% mean*10

upslope_erosion_wwf <- ((upslope_erosion[rotation  =="wf"])$`Errosion_kg/m2` )%>% mean*10
upslope_erosion_wwbf <- ((upslope_erosion[rotation  =="wbf"])$`Errosion_kg/m2` )%>% mean*10
upslope_erosion_wwbp <- ((upslope_erosion[rotation  =="wbp"])$`Errosion_kg/m2` )%>% mean*10


### Figure XXX plot missouri flat creek erosion rates deltas ------------------
#load_rasters 
raster_ct_max <- raster("processed_data/batch/geo_scratch/210615/CT.max.Errosion.tif")
raster_ct_mean <- raster("processed_data/batch/geo_scratch/210615/CT.mean.Errosion.tif")
raster_ct_sum <- raster("processed_data/batch/geo_scratch/210615/CT.sum.Errosion.tif")
raster_ct_sed <- raster("processed_data/batch/geo_scratch/210615/CT.max.sed_t_ha.tif")
raster_ct_buf_sed <- raster("processed_data/batch/geo_scratch/210615/CT_buf.max.sed_t_ha.tif")
raster_ct_Q <- raster("processed_data/batch/geo_scratch/210615/CT.sum.Q.tif")
raster_ct_lat <- raster("processed_data/batch/geo_scratch/210615/CT.sum.Lat.tif")
# raster_ct_Rm <- raster("processed_data/batch/geo_scratch/210615/CT.RM.tif")


raster_mt <- raster("processed_data/batch/geo_scratch/210615/mt.max.Errosion.tif")
raster_mt_sed <- raster("processed_data/batch/geo_scratch/210615/mt.max.sed_t_ha.tif")
raster_mt_buf_sed <- raster("processed_data/batch/geo_scratch/210615/MT_buf.max.sed_t_ha.tif")
raster_mt_Q <- raster("processed_data/batch/geo_scratch/210615/mt.sum.Q.tif")
raster_mt_lat <- raster("processed_data/batch/geo_scratch/210615/mt.sum.Lat.tif")
# raster_mt_Rm <- raster("processed_data/batch/geo_scratch/210615/MT.RM.tif")


raster_nt <- raster("processed_data/batch/geo_scratch/210615/nt.max.Errosion.tif")
raster_nt_sed <- raster("processed_data/batch/geo_scratch/210615/nt.max.sed_t_ha.tif")
raster_nt_buf_sed <- raster("processed_data/batch/geo_scratch/210615/NT_buf.max.sed_t_ha.tif")
raster_nt_Q <- raster("processed_data/batch/geo_scratch/210615/nt.sum.Q.tif")
raster_nt_lat <- raster("processed_data/batch/geo_scratch/210615/nt.sum.Lat.tif")
# raster_nt_Rm <- raster("processed_data/batch/geo_scratch/210615/NT.RM.tif")


data <- brick(raster_ct_sum,raster_ct_mean,raster_ct_max,raster_ct_sed, raster_ct_lat, raster_ct_Q, raster_mt, 
              raster_mt_sed,raster_mt_lat, raster_mt_Q, raster_nt, raster_nt_sed, 
              raster_nt_lat, raster_nt_Q)#, raster_mt_buf_sed, raster_ct_buf_sed,raster_nt_buf_sed)#, raster_ct_Rm, raster_mt_Rm, raster_nt_Rm)

data_buf <- brick(raster_mt_buf_sed, raster_ct_buf_sed,raster_nt_buf_sed)#, raster_ct_Rm, raster_mt_Rm, raster_nt_Rm)

tryitall <- raster_ct_buf_sed %>% crop(.,raster_ct_sed)

compareRaster(tryitall,raster_ct_sed)  
  
((raster_ct_buf_sed %>% is.na)*raster_ct_sed+raster_ct_buf_sed)

extent(raster_ct_buf_sed) <- extent(raster_ct_sed)
origin(raster_ct_buf_sed) <- origin(raster_ct_sed)

# crop out missour flat 
i <- (huc12$OBJECTID)
#thorn = 28, kamiache = 19, four mile = 74, paradise = 79, missouri flat = 75 
j=75
g <- subset(huc12, OBJECTID == i[j])
data_75 <- data %>% crop (g) %>% mask(g)
data_buf_75 <- data_buf %>% crop (g) %>% mask(g)
data_df <- (as.data.frame(data_75, xy =T) %>% as.data.table %>% na.omit)
data_buf_df <- (as.data.frame(data_buf_75, xy =T) %>% as.data.table %>% na.omit)
data_df <- data_df %>% merge(., data_buf_df, by =c("x","y"), all.x = T) %>% 
  mutate(CT_buf.max.sed_t_ha = ifelse(is.na(CT_buf.max.sed_t_ha),
                                      CT.max.sed_t_ha, 
                                      CT_buf.max.sed_t_ha),
         MT_buf.max.sed_t_ha = ifelse(is.na(MT_buf.max.sed_t_ha) ,
                                      MT.max.sed_t_ha, 
                                      MT_buf.max.sed_t_ha),
         NT_buf.max.sed_t_ha = ifelse(is.na(NT_buf.max.sed_t_ha),
                                      NT.max.sed_t_ha, 
                                      NT_buf.max.sed_t_ha))

summary(data_df$CT_buf.max.sed_t_ha)
PRW <- shapefile("raw/extent/PRW_shape")
ad8 <- raster("processed_data/NED_PRW_ad8.tif")      # flow accumultation d8?
streams <- ad8 
streams[streams<100]<-NA
streams[streams>=100]<-1

landtypes_man <- raster("processed_data/gis_processed/landtype_210401_2.tif")
Urban <- landtypes_man
Urban[Urban!=1]<-NA


streams_75 <- streams %>% crop(g) %>% mask(g) %>% as.data.frame(.,xy=T)
urban_75 <- Urban %>% crop(g) %>% mask(g) %>% as.data.frame(.,xy=T)


str_75 <- geom_tile(data = streams_75 %>% dplyr::filter(., !is.na(NED_PRW_ad8)), aes(x = x, y = y, ), fill = "lightblue3")
urb_75 <- geom_tile(data = urban_75 %>% dplyr::filter(., !is.na(landtype_210401_2)), aes(x = x, y = y), fill = "lightgray")


delta_ct_mt <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((CT.max.Errosion-MT.max.Errosion)*10) %>%
                  ifelse(.>11.2,11.2,.)%>%ifelse(.<0,0,.)  ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "Delta \nerosion \n(t/ha)")

# delta_ct_mt <- ggplot(sum_dat_max$NT.max[huc12==79])+
#   geom_tile(aes(x = x, y=y, fill = ((`Errosion_kg/m2`)*10) %>%
#                   ifelse(.>11.2,11.2,.)%>%ifelse(.<0,0,.)  ))+ 
#   scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
#   labs(x="\nEasting (m)", y="Northing(m)\n", fill = "Delta \nerosion \n(t/ha)")
# 
# 
#   

delta_mt_nt <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((MT.max.Errosion-NT.max.Errosion)*10) %>%
                  ifelse(.>11.2,11.2,.)%>%ifelse(.<0,0,.)  ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "Delta \nerosion \n(t/ha)")

ggplot(huc12 %>% subset(gridcode==75)) +
  geom_sf()+
  coord_sf(datum = st_crs(huc12))

# Redefire as shapefile not sf ugh tomany way of doing things
huc12 <- shapefile("processed_data/huc12.shp")
h_75<- geom_polygon(data= subset(huc12,OBJECTID==79),aes(x =long,y= lat) , fill=NA , col='black', size=1)

delta_erosion <- ggarrange(delta_ct_mt+str_75+urb_75+h_75,
                           delta_mt_nt+str_75+urb_75+h_75 ,
                              align = "h",
                              nrow= 1,
                              ncol = 2,
                              labels = c("A","B"),
                              vjust=20, 
                              hjust = -2)

ggsave("processed_data/batch/Figures/2109/delta_erosion_MF.jpeg", 
       delta_erosion, dpi=300, 
              height =3, width = 10)
# ggplot(data_df)+
#   geom_tile(aes(x = x, y=y, fill = ((MT.max.Errosion)*10) %>%ifelse(.>25,25,.)%>%ifelse(.<0,0,.)  ))+ 
#   scale_fill_viridis_c(option="B", direction=-1)+
#   labs(x="\nEasting (m)", y="Northing(m)\n", fill = "Delta \nerosion \n(t/ha)")
# 
# mean(data_df$CT.max.Errosion)*4.46
# 

delta_mt_nt$data$
delta_mt_nt <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((CT.)*10) %>%
                  ifelse(.>11.2,11.2,.)%>%ifelse(.<0,0,.)  ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "Delta \nerosion \n(t/ha)")


# FFigure XXX. sediment delivery and buffer effects ----------
  (test$CT.max$Sed_Mg-test$CT_buf.max$Sed_Mg)/test$CT.max$Sed_Mg* 100
  (test$MT.max$Sed_Mg-test$MT_buf.max$Sed_Mg)/test$MT.max$Sed_Mg* 100
  (test$NT.max$Sed_Mg-test$NT_buf.max$Sed_Mg)/test$NT.max$Sed_Mg* 100

((data_df$CT.max.sed_t_ha%>% 
    na.omit %>% sum/10000-data_df$CT_buf.max.sed_t_ha%>% 
    na.omit %>% sum/10000)/(data_df$CT.max.sed_t_ha%>% 
    na.omit %>% sum/10000))*100

((data_df$MT.max.sed_t_ha%>% 
    na.omit %>% sum-data_df$MT_buf.max.sed_t_ha%>% 
    na.omit %>% sum)/(data_df$MT.max.sed_t_ha%>% 
                              sum))*100

((data_df$NT.max.sed_t_ha%>% 
    na.omit %>% sum/10000-data_df$NT_buf.max.sed_t_ha%>% 
    na.omit %>% sum/10000)/(data_df$NT.max.sed_t_ha%>% 
                              na.omit %>% sum/10000))*100

ordered_data_frame <- data_df[order(CT.max.sed_t_ha)]
n <- nrow(ordered_data_frame)
s_ct <- sum(ordered_data_frame$CT.max.sed_t_ha)
s_mt <- sum(ordered_data_frame$MT.max.sed_t_ha)
s_nt <- sum(ordered_data_frame$NT.max.sed_t_ha)
(s_ct-sum(ordered_data_frame$CT.max.sed_t_ha[(n*.5):n]))/s_ct
(s_mt-sum(ordered_data_frame$MT.max.sed_t_ha[(n*.5):n]))/s_mt
(s_nt-sum(ordered_data_frame$NT.max.sed_t_ha[(n*.5):n]))/s_nt

sday75 <- sum_dat$CT.max[huc12==79][order(Erosion_T_ha1)][
  , c("climate", "soil","slope","rotation","tillage","length","buffer"):= 
    tstrsplit(ct, "_", fixed=TRUE)]
sdat_buf_75 <- sum_dat$CT_buf.max[huc12==79][order(Erosion_T_ha1)][
  , c("climate", "soil","slope","rotation","tillage","length","buffer"):= 
    tstrsplit(ct_buf, "_", fixed=TRUE)][Erosion_T_ha1>11]
ggplot(sdat_buf_75)+
  geom_bar(aes((soil)))+
  geom_bar(aes(slope))+
  geom_bar(aes(tillage))+
  geom_bar(aes(length))

nrow(sdat_buf_75[soil=="s"])/nrow(sdat_buf_75)


table(sdat_buf_75[,c("length","soil")])
hist(sdat_buf_75$length %>% as.numeric())
sday75[0:(nrow(sday75)*.5)]$tillage %>% table
sday75[0:(nrow(sday75)*.5)][,c("slope","tillage","soil")] %>% table
sday75[0:(nrow(sday75)*.5)]$length %>% table
sday75[0:(nrow(sday75)*.5)]$rotation %>% table

## asking where the low slope occur I can visualize with hist. low slopes for sure. 
ggplot(sday75)+
  geom_density(aes(slope))
hist(sday75$slope %>% as.numeric())
hist(sday75$slope[0:(nrow(sday75)*.5)] %>% as.numeric, col ="blue", add = T)

hist(sday75$length %>% as.numeric())
hist(sday75$length[0:(nrow(sday75)*.5)] %>% as.numeric, col ="blue", add = T)

hist(sday75$climate %>% as.numeric())
hist(sday75$climate[0:(nrow(sday75)*.5)] %>% as.numeric, col ="blue", add = T)

ggplot(sday75)+
  geom_tile(aes(x = x, y=y, fill = ((Erosion_T_ha1)) %>%
                  ifelse(.>11.2,11.2,.)%>%ifelse(.<0,0,.)  ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "(t/ha)")


ct_sed <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((CT.max.sed_t_ha)) %>%
                  ifelse(.>11.2,11.2,.)%>%ifelse(.<0,0,.)  ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "(t/ha)")

mt_sed <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((MT.max.sed_t_ha)) %>%
                  ifelse(.>11.2,11.2,.)%>%ifelse(.<0,0,.)  ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "(t/ha)")

nt_sed <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((NT.max.sed_t_ha)) %>%
                  ifelse(.>11.2,11.2,.)%>%ifelse(.<0,0,.)  ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "(t/ha)")

ctbuff_sed <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((CT_buf.max.sed_t_ha)) %>%
                  ifelse(.>11.2,11.2,.) ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "(t/ha)")

mtbuff_sed <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((MT_buf.max.sed_t_ha)) %>%
                  ifelse(.>11.2,11.2,.) ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "(t/ha)")

ntbuff_sed <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((NT_buf.max.sed_t_ha)) %>%
                  ifelse(.>11.2,11.2,.) ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "(t/ha)")



delta_ct_ctbuff_sed <- ggplot(data_df)+
  geom_tile(aes(x = x, y=y, fill = ((CT.max.sed_t_ha-CT_buf.max.sed_t_ha)) %>%
                  ifelse(.>11.2,11.2,.)%>%ifelse(.<0,0,.)  ))+ 
  scale_fill_viridis_c(option="B", direction=-1)+ coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n", fill = "(t/ha)")

MF_sed_delivery <- ggarrange(ct_sed+str_75+urb_75+h_75,ctbuff_sed+str_75+urb_75+h_75,
                           mt_sed+str_75+urb_75+h_75,mtbuff_sed+str_75+urb_75+h_75,
                           nt_sed+str_75+urb_75+h_75,ntbuff_sed+str_75+urb_75+h_75,
                           align = "h",
                           nrow= 3,
                           ncol = 2,
                           labels = c("A","D","B","E","C","F"),
                           vjust=13, 
                           hjust = -2)

ggsave("processed_data/batch/Figures/2109/MF_sed_delivery.jpeg", 
       MF_sed_delivery, dpi=300, 
       height =7, width = 10)


# Figure XXX hydroflows MF ============
CT <- fread("processed_data/batch/geo_scratch/net_flow_max_ct.txt")
MT <- fread("processed_data/batch/geo_scratch/net_flow_max_mt.txt")                                                
NT <- fread("processed_data/batch/geo_scratch/net_flow_max_nt.txt")
flow_dupe <- CT[huc12==79] %>% 
  merge(x= ., y = MT[huc12==79], by =c("x","y"),suffixes =c("_ct", "_mt")) %>%
  merge(x= ., y = NT[huc12==79], by =c("x","y"),suffixes =c("", "_nt"))

wshed_sum_flow <- wshed_sum[,c(1:2,13)]

flow_dupe_all <- CT%>% 
  merge(x= ., y = MT, by =c("x","y"),suffixes =c("_ct", "_mt")) %>%
  merge(x= ., y = NT, by =c("x","y"),suffixes =c("", "_nt")) %>% 
  merge(., profiles, by = c("x","y", "huc12"))


ggplot()+
  geom_tile(data = flow_dupe_all, aes(x=x, y=y, fill = net_flow_ct))

flow_average_Huc_ct <- merged_clean$CT [, lapply(.SD, mean, na.rm=TRUE), by=c("huc12"), 
              .SDcols=c("Q","latqcc","RM")][,"ID":= "ct"] # r
flow_average_Huc_mt <- merged_clean$MT [, lapply(.SD, mean, na.rm=TRUE), by=c("huc12"), 
                                        .SDcols=c("Q","latqcc","RM")][,"ID":= "mt"]
flow_average_Huc_nt <- merged_clean$NT [, lapply(.SD, mean, na.rm=TRUE), by=c("huc12"), 
                                        .SDcols=c("Q","latqcc","RM")][,"ID":= "nt"]

flow_average_Huc <- rbind(flow_average_Huc_ct,flow_average_Huc_mt,flow_average_Huc_nt)

huc12 <- sf::st_read("processed_data/huc12.shp")
# huc12 <- huc12[,c("OBJECTID", "Id", "gridcode",    "Shape_Leng",    "Shape_Area")]
flow_average_Huc_wide <- flow_average_Huc %>% pivot_wider(., id_cols=c("huc12"),
                                                          names_from = ID, values_from= c( "Q","latqcc","RM"),)

huc12_flow <- huc12 %>% merge(.,flow_average_Huc_wide,
                              by.x = "OBJECTID", by.y = "huc12" )

CT_flow <- ggplot(huc12_flow) +
  geom_sf(aes(fill = Q_ct/RM_ct))+
  coord_sf(datum = st_crs(huc12))+
  scale_fill_viridis_c(option = "B", direction = -1)+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = -45))+
  labs(x = "\nEasting (m)", y = "Northing (m)\n", fill = "Q/P")

CT_lat <- ggplot(huc12_flow) +
  geom_sf(aes(fill = latqcc_ct/RM_ct))+
  coord_sf(datum = st_crs(huc12))+
  scale_fill_viridis_c(option = "B", direction = -1)+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = -45))+
  labs(x = "\nEasting (m)", y = "Northing (m)\n", fill = "L/P")


PRW_flows <- ggarrange(CT_flow , 
                       CT_lat, labels = c("A","B"),
                         nrow = 1, ncol= 2 , align= "h")

ggsave(PRW_flows, file = "processed_data/batch/figures/2109/PRW_hydro.jpeg", 
       height = 4, width = 8)


  
ct_flow_plot <- ggplot(data = flow_dupe)+
  geom_tile(aes(x=x,y=y,fill = net_flow_ct/RM))+
  scale_fill_viridis(direction = -1)+ theme_minimal()+coord_equal()+
  labs(x = "Easting (m)", y="Northing (m)", fill ="Q/P")

ctmt_flow_plot <- ggplot(data = flow_dupe)+
  geom_tile(aes(x=x,y=y,fill = (net_flow_mt)/RM))+
  scale_fill_viridis(direction = -1)+ theme_minimal()+coord_equal()+
  labs(x = "Easting (m)", y="Northing (m)", fill ="Q/P")

ctnt_flow_plot <- ggplot(data = flow_dupe)+
  geom_tile(aes(x=x,y=y,fill = (net_flow)/RM))+
  scale_fill_viridis(direction = -1)+ theme_minimal()+coord_equal()+
  labs(x = "Easting (m)", y="Northing (m)", fill ="Q/P")

ct_lat_plot <- ggplot(data = flow_dupe)+
  geom_tile(aes(x=x,y=y,fill = latqcc_ct/RM))+
  scale_fill_viridis(direction = -1)+ theme_minimal()+coord_equal()+
  labs(x = "Easting (m)", y="Northing (m)", fill ="Q/P")

ctmt_lat_plot <- ggplot(data = flow_dupe)+
  geom_tile(aes(x=x,y=y,fill = (latqcc_ct-latqcc_mt)/RM))+
  scale_fill_viridis(direction = -1)+ theme_minimal()+coord_equal()+
  labs(x = "Easting (m)", y="Northing (m)", fill ="Q/P")

ctnt_lat_plot <- ggplot(data = flow_dupe)+
  geom_tile(aes(x=x,y=y,fill = (latqcc_ct-latqcc)/RM))+
  scale_fill_viridis(direction = -1)+ theme_minimal()+coord_equal()+
  labs(x = "Easting (m)", y="Northing (m)", fill ="Q/P")
hydro_flows <- ggarrange(ct_flow_plot , 
          ct_lat_plot, 
          ctmt_flow_plot, 
          ctmt_lat_plot, 
          ctnt_flow_plot,
          ctnt_lat_plot,
          nrow = 3, ncol= 2 , align= "v")

ggsave(hydro_flows, file = "processed_data/batch/figures/2109/MFC_hydro.jpeg", 
       height = 8, width = 8)



# Figure 8 c-field flows ----------------------------
c_fields <- shapefile("RAW/extent/clarks/Clark_Field.shp")

c_fields <- spTransform(c_fields,crs(PRW))

st_cf <- streams %>% crop(.,c_fields) %>% 
  mask(.,c_fields) %>% as.data.frame(.,xy=T)

CT <- fread("processed_data/batch/geo_scratch/net_flow_max_ct.txt")
MT <- fread("processed_data/batch/geo_scratch/net_flow_max_mt.txt")                                                
NT <- fread("processed_data/batch/geo_scratch/net_flow_max_nt.txt")
flow_dupe_all <- CT %>% 
  merge(x= ., y = MT, by =c("x","y"),suffixes =c("_ct", "_mt")) %>%
  merge(x= ., y = NT, by =c("x","y"),suffixes =c("", "_nt")) 

xy <- flow_dupe_all[,c(1,2)]

flow_dupe_s <- SpatialPointsDataFrame(coords = xy, data = flow_dupe_all,
                                      proj4string  = crs(c_fields))


data_C <- flow_dupe_s %>% 
  crop(x = ., y = c_fields) %>% 
  as.data.frame(., xy=T) %>% 
  as.data.table()

data_c_e <- data_C[,c("x","y", "Errosion_kg.m2_ct", "Errosion_kg.m2_mt", "Errosion_kg.m2")]
names(data_c_e)<- c("x","y", "CT", "MT", "NT")
data_c_e = melt(data_c_e,id.vars = c("x", "y"),
                measure.vars = c("CT", "MT", "NT"))


data_c_q <- data_C[,c("x","y", "net_flow_ct", "net_flow_mt", "net_flow", "RM")][
  ,net_flow_ct:=net_flow_ct/RM][,net_flow_mt:=net_flow_mt/RM][,net_flow:=net_flow/RM][,RM:=NULL]
names(data_c_q)<- c("x","y", "CT", "MT", "NT")
data_c_q = melt(data_c_q,id.vars = c("x", "y"),
                measure.vars = c("CT", "MT", "NT"))



data_c_l <- data_C[,c("x","y", "total_Lat_ct", "total_Lat_mt", "total_Lat", "RM")][
  ,total_Lat_ct:=total_Lat_ct/RM][,total_Lat_mt:=total_Lat_mt/RM][,total_Lat:=total_Lat/RM][,RM:=NULL]
names(data_c_l)<- c("x","y", "CT", "MT", "NT")
data_c_l = melt(data_c_l,id.vars = c("x", "y"),
                measure.vars = c("CT", "MT", "NT"))

cf_e_plot <- ggplot(data_c_e %>% mutate(value=ifelse(value*10>20, 20,
                                                     ifelse(value*10<(-20), -20, value*10))))+
  geom_tile(aes(x=x, y=y, fill =  value))+ facet_wrap(~variable)+
  scale_fill_gradient2(low = "darkgreen", 
                       mid = viridis(5)[5], 
                       high = "brown", midpoint = 0, 
                       name = expression("t/ha"))+ coord_equal()+ theme_minimal()+
  labs(y="Northing(m)\n") + theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank())


cf_q_plot <- ggplot(data_c_q)+
  geom_tile(aes(x=x, y=y, fill =  value))+ facet_wrap(~variable)+
  scale_fill_viridis(direction = -1, name = expression("Q/P"))+ 
  coord_equal()+ theme_minimal()+
  labs(y="Northing(m)\n") + theme(axis.title.x=element_blank(),
                                                     axis.text.x=element_blank(),
                                                     axis.ticks.x=element_blank(),
                                                      strip.background = element_blank(),
                                                       strip.text.x = element_blank(),
                                  plot.margin = margin(t=-1)
                                                     )

cf_l_plot <- ggplot(data_c_l)+
  geom_tile(aes(x=x, y=y, fill =  value))+ facet_wrap(~variable)+
  scale_fill_viridis(direction = -1, name = expression("L/P"))+ 
  coord_equal()+ theme_minimal()+
  labs(x="\nEasting (m)", y="Northing(m)\n")+ theme(strip.background = element_blank(),
                                                    strip.text.x = element_blank(),plot.margin = margin(t=-1))
c_fields <- fortify(c_fields)
cf <- geom_path(data= c_fields,aes(x =long,y= lat, group=group) ,col='grey45')
st_cf_p <- geom_tile(data= st_cf %>% na.omit ,aes(x =x,y= y, group=NED_PRW_ad8) ,fill='lightblue')

cfield_plot <- ggarrange(cf_e_plot+st_cf_p+cf,cf_q_plot+st_cf_p+cf,cf_l_plot+st_cf_p+cf,
                         nrow = 3, ncol= 1 , align= "hv",  labels = c("A","B","C"))

ggsave(plot = cfield_plot, filename = "processed_data/batch/figures/2109/cfield_all_flow.jpeg", height = 7, width = 8, units = "in", dpi = 300)
