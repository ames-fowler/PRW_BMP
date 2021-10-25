library(data.table)
library(raster)
library(tidyverse)
library(latticeExtra)
library(sf)
library(rgdal)

# map the spatial data and the spatial data back together 
# build the raster name as a column using the existing xy table 
# merge the simulation data back into the xy table (by tillage)
# summerize over lap, SUM, MEAN, MAX


# Load spatial data -----------------------------
# map the spatial data and the spatial data back together 
huc12 <- shapefile("processed_data/huc12")
huc12 <- huc12[,c("OBJECTID", "Id", "gridcode",    "Shape_Leng",    "Shape_Area")]


landtype_slope <-raster("processed_data/landtype_091720_1.tif") # slope upper limit increase to add 25 and 35 not just XX200917
landtype_management <-raster("processed_data/landtype_091720_2.tif")
landtype_climate<-raster("processed_data/landtype_091720_3.tif")
landtype_soil<-raster("processed_data/landtype_091720_4.tif")

# subset to paradise to test --------------------------------------
paradise_shape <- subset(huc12, huc12$OBJECTID == 82)

# read in the data tables --------------
#Landtypes xy table _ unused? 
#landtype_data <- fread("processed_data/reclass_df_091720.csv")

#profile sumerization for calling th eright file 
# paradise_flow_sum <- fread("processed_data/gis_processed/wshed_sum_82.txt")
# missoury_flat_sum <- fread("processed_data/gis_processed/wshed_sum_79.txt")


# call in all profile sum files and merge 
wshed_sum_list <- list.files(path ="processed_data/gis_processed/", pattern = "wshed_sum*",full.names = F)
wshed_flow_list <- list.files(path ="processed_data/gis_processed/", pattern = "wshed_tr*",full.names = F)

#profile xy table for calling the right OFE
# paradise_flow <- fread("processed_data/gis_processed/wshed_tr_82")[,c("x","y","profile","steps")]
# missoury_flat_flow <- fread("processed_data/gis_processed/wshed_tr_79")[,c("x","y","profile","steps")]


i=2
rm(wshed_sum)
rm(wshed_flow)
   
for(i in 1:length(wshed_sum_list)){
  temp   <-  fread(paste0("processed_data/gis_processed/",wshed_sum_list[i]))
  temp_2 <-  fread(paste0("processed_data/gis_processed/",wshed_flow_list[i]))
  
  huc_code <-parse_number(wshed_sum_list[i])
  
  temp <- temp %>% 
           mutate(huc12 = huc_code) #do I drop NAs
  temp_2 <- temp_2 %>% 
    mutate(huc12 = huc_code) #do I drop NAs
  
  if(!exists("wshed_sum")){
    wshed_sum <- temp
    wshed_flow <- temp_2
  }else{
    wshed_sum  <- rbind(wshed_sum,temp)
    wshed_flow <- rbind(wshed_flow,temp_2)
  }
}

rm(temp)
rm(temp_2)

wshed_flow <- na.omit(wshed_flow, cols=c("landtype_2"), invert=FALSE)

# fwrite(x = wshed_flow, file = "processed_data/gis_processed/whsed_tr_PRW_092320")
# fwrite(x = wshed_sum, file = "processed_data/gis_processed/whsed_sum_PRW_092320")
# wshed_flow <- fread(file = "processed_data/gis_processed/whsed_tr_PRW_092320")
# wshed_sum <- fread(file = "processed_data/gis_processed/whsed_sum_PRW_092320")
# wepp run profiles 

## ****Make the next script LOAD DATA HERE---------------------
# this is repeated in Cart_GSA_flowpath.R

# wshed_flow_OLD <- fread(file = "processed_data/gis_processed/whsed_tr_PRW_092320")
# wshed_sum_OLD <- fread(file = "processed_data/gis_processed/whsed_sum_PRW_092320")

wshed_flow    <-  fread(file = "processed_data/gis_processed/whsed_tr_PRW_210614")
wshed_sum     <-  fread(file = "processed_data/gis_processed/whsed_tr_sum_210614")

WEPP_data     <-  fread("processed_data/batch/201222_PRW_MASTER_LIST")
WEPP_data_buf <-  fread("processed_data/batch/201223_PRW_MASTER_LIST_buf")

sed_data      <-  fread("processed_data/batch/sed_out_201210.txt")
sed_data_buf  <-  fread("processed_data/batch/out_buf_201221.txt")

#### drop duplicates after confirming they are the same 201222------------------ 
WEPP_data <- WEPP_data[!duplicated(WEPP_data)==T]%>%
  mutate(ID_plus = paste0(ID,"_",OFE))

WEPP_data_buf <- WEPP_data_buf[!duplicated(WEPP_data_buf)==T]%>%
  mutate(ID_plus = paste0(ID,"_",OFE))

sed_data_buf <- sed_data_buf[, ID := substr(ID,32,nchar(ID))][!grepl("Test", sed_data_buf$ID),]


# Bind up sed data and weep data  =====
sed_data <- rbind(sed_data, sed_data_buf)
WEPP_data <- rbind(WEPP_data, WEPP_data_buf)
WEPP_data <- merge(WEPP_data, sed_data)

## pull and re-merge Buff OFE 201228 ==========
# combines the last two OFEs as a weighted average. 
last_buf <- WEPP_data

last_buf <- last_buf[, c("climate", "soil","slope","rotation","tillage","length","buffer"):= tstrsplit(ID, "_", fixed=TRUE)][buffer==10][OFE>(as.numeric(length)-1)]
noag_buf <- last_buf[tillage == "no"][,buffer := "10"][,ID:= paste0(climate,"_",soil,"_",slope,"_",rotation,"_",tillage,"_",length,"_",length,"_",buffer)]

#merge OFEs weighted by length. 
ID_list <- last_buf$ID_plus 
last_buf[,weight := ifelse(OFE==as.numeric(length),2/3,1/3)]

weight_list <- c("Errosion_kg/m2","RM","Q","ET", "Dp","UpStrmQ","SubRIn","latqcc","QOFE")
first_list <-  c("OFE","ID_plus", "Erosion_T_ha1", "area_ha")

last_buf_dat <- last_buf[, lapply(.SD, weighted.mean, w = weight), by=c("ID"), .SDcols = weight_list]
last_buf_first <- last_buf[, lapply(.SD, first), by=c("ID"), .SDcols = first_list]

last_buf_out <- merge(last_buf_dat, last_buf_first)
test <- WEPP_data[!(WEPP_data$ID_plus %in% ID_list)]
test[,c("climate", "soil","slope","rotation","tillage","length","buffer"):=NULL]

noag_buf[,c("climate", "soil","slope","rotation","tillage","length","buffer"):=NULL]

WEPP_data <- rbind(test, last_buf_out,noag_buf)

##make the "no" tillage buf same as mo buf

#replace WEPP data for patch -----------------------------
# WEPP_data_shortlist <- fread("processed_data/batch/201210_PRW_short_LIST")
# WEPP_data_shortlist <- WEPP_data_shortlist[!duplicated(WEPP_data_shortlist)==T]%>%
#   mutate(ID_plus = paste0(ID,"_",OFE))
# 
# WEPP_data <- WEPP_data[!duplicated(WEPP_data)==T]%>%
#   mutate(ID_plus = paste0(ID,"_",OFE))
# 
# # WEPP_data[ WEPP_data_shortlist$ID_plus %in% WEPP_data$ID_plus==T] <-  WEPP_data_shortlist$ID_plusWEPP_data_shortlist$ID_plus %in% WEPP_data$ID_plus)
# # WEPP_data_shortlist[WEPP_data_shortlist$ID_plus %in% WEPP_data$ID_plus==F ] 
# 
# WEPP_data[WEPP_data$ID_plus %in% WEPP_data_shortlist$ID_plus==T] <- WEPP_data_shortlist[WEPP_data_shortlist$ID_plus %in% WEPP_data$ID_plus ==T]
# 
# WEPP_data <- rbind(WEPP_data,WEPP_data_shortlist[WEPP_data_shortlist$ID_plus %in% WEPP_data$ID_plus==F])
# #fwrite(WEPP_data, "processed_data/batch/201215_PRW_master_list.txt")
# #identifying the SUm pathh

## watch the units - sum of total Q is wrong??? ###########fix####### 201210 


### wepp summing =======================

# build a aggregated  profile file. 
#Summing and dividing by length give the area total production. 210210

WEPP_data_sum <- WEPP_data[, lapply(.SD, sum), by=c("ID"),
                           .SDcols=c("Errosion_kg/m2","Q", "latqcc","RM","Dp","ET")][
                             , c("climate", "soil","slope","rotation","tillage","length","buffer") := tstrsplit(ID, "_", fixed=TRUE)]

temp <- WEPP_data_sum[, lapply(.SD, `/`, as.numeric(WEPP_data_sum$length)),.SDcols=c("Errosion_kg/m2","Q", "latqcc","RM","Dp","ET") ]

WEPP_data_sum = cbind(WEPP_data_sum[,-c("Errosion_kg/m2","Q", "latqcc","RM","Dp","ET")],temp)

WEPP_data_max <- WEPP_data[, total_Q := (Q + UpStrmQ)][, total_Lat := (SubRIn + latqcc)][
  , lapply(.SD, max, na.rm=TRUE), by=c("ID"), .SDcols=c("OFE","RM","Dp","ET","QOFE", "Errosion_kg/m2","total_Q", "total_Lat")]
names(WEPP_data_max) <- c("ID","OFE","RM_max","Dp_max","ET_max","QOFE", "Errosion_kgm2_max","total_Q_max", "total_Lat_max")


WEPP_data_total_path <- merge(WEPP_data_sum, WEPP_data_max) %>% merge(sed_data) 

WEPP_data_total_path <- WEPP_data_total_path[,ID_less := paste(climate, soil,slope,rotation,tillage,length, sep="_")][
  ,c("climate", "slope","length"):= lapply(.SD, as.numeric), .SDcols = c("climate", "slope","length")]  

# this is confusing, number verse character order verse factor 
WEPP_data_total_path <- WEPP_data_total_path[length<16]
## finding thresholds -----------
# 
# bins <- c(-Inf,seq(0,5,.1),Inf)
# bins <- c(seq(0,10,1))
# ggplot(data = WEPP_data_total_path[tillage != "no" & as.numeric(climate) > 5 ])+
#   # geom_histogram(aes(x = Erosion_T_ha1/2.2,y =..density..,
#   #                    fill = slope),
#   #                position="identity", alpha=.5,
#   #                breaks=bins)+
#   geom_density(aes(Erosion_T_ha1/2.2, col = (slope)), alpha=.2)
# xlim(.1,10)#+ ylim(0,2)
# #[tillage=="ct"][climate==9]
# 
# ggplot(data= WEPP_data_total_path[`Errosion_kg/m2`>0])+
#   geom_point(aes(x=`Errosion_kg/m2`*10, y= Erosion_T_ha1))
# 
# plot(WEPP_data_total_path$Errosion_kgm2_sum
#      [WEPP_data_total_path$Errosion_kgm2_sum>0]/length(WEPP_data_total_path$length[WEPP_data_total_path$Errosion_kgm2_sum>0]), WEPP_data_total_path$Erosion_T_ha1[WEPP_data_total_path$Errosion_kgm2_sum>0])
# 
# 
# 
# list = c(1,2,3,4,5,10,20,50,100)
# threshold_list <- function(list, data = WEPP_data_total_path){
#   temp <- unique(data[Erosion_T_ha1/2.24>list, ID])
#   return(temp)
# }
# 
# t <-  map(list, threshold_list)
# t5 <- as.data.table(t[5])[, c("climate", "soil","slope","rotation","tillage","length","buffer"):= tstrsplit(V1, "_", fixed=TRUE)]
# 
# unique(t5$climate)
# unique(t5$soil)
# unique(t5$slope)
# unique(t5$tillage)
# unique(t5$length)
# unique(t5[t5$slope >12]$length)
# 
# hist(t5$length%>% as.numeric())
# hist(t5$slope%>% as.numeric())
# hist(t5$climate%>% as.numeric())
# 
# table(t5$soil)
# table(t5$buffer)
# table(t5$rotation)

###fixed the summing of rates issue x + x + x + x = 4x not x, could not use an average because of the buffer, divided by length. 

# ggplot(WEPP_data_total_path %>% subset(length<16)%>% subset(buffer ==0), aes(x = total_Q_max, y = Errosion_kgm2_sum,col = tillage))+
#    geom_point() #+ xlab(i) +ylab(j)


# ggplot(WEPP_data_total_path, aes(x = QOFE , y = length, col = tillage))+
#   geom_point() #+ xlab(i) +ylab(j)
# ggsave(paste0("processed_data/batch/figures/pairs_nobuf_201217_",i,"_",j,".jpeg"), p1, dpi=300)  
  
#para_error_fix <- fread("processed_data/batch/para_errorfix_201014")
#####NEED TO FIX big discontinuities in the file suppressing erosion rates. Work up an example. 10/12/20

# WEPP_data <- rbind(WEPP_data, para_error_fix)
# build out names rebuild slope with the change.
# test <- tail(paradise_flow_sum)


# Build the names for each profile -----------------------------
# Check the slope XXX
# land type mean - now coded as median round to meaning full file. 
# watch the other and water man treatment. In place for legend/filling null values. will this work []
# practice aand slope reclass is happening like 6 times - fix once use dictionary.... 


#build out the scenarios ----------
dt <- wshed_sum
dt$landtype_cli <- floor(dt$landtype_cli) ### FIX THIS? 

dt<- dt %>% 
  mutate(landtype_slope  = ifelse((landtype_slope*45  < 3), 2,landtype_slope),
         practice = ifelse(grepl(pattern = "3", landtype_man), "f",   # the .5 should not be there function of mean not median 
                           ifelse(grepl(pattern = "2", landtype_man), "g", 
                                  ifelse(grepl(pattern = "4|5", landtype_man), "water",
                                         ifelse(grepl(pattern = "6", landtype_man), "wbp",
                                                ifelse(grepl(pattern = "7|9", landtype_man), "wbf", # well add irigated in to continous rotation for now 210615
                                                       ifelse(grepl(pattern = "8", landtype_man), "wf",
                                                              "other")))))),
         soil = ifelse(landtype_soil < 2, "s", 
                       ifelse(landtype_soil < 3, "m","d")),
         ct = paste(landtype_cli,soil,landtype_slope,practice,  # could be a little function 
                    ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "ct"),
                    steps,"0", sep = "_"),
         ct_buf = paste(landtype_cli,soil,landtype_slope,practice, 
                        ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "ct"),
                        steps,"10", sep = "_"),
         mt = paste(landtype_cli,soil,landtype_slope,practice, 
                    ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "mt"),
                    steps,"0", sep = "_"),
         mt_buf = paste(landtype_cli,soil,landtype_slope,practice, 
                        ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "mt"),
                        steps,"10", sep = "_"),
         nt = paste(landtype_cli,soil,landtype_slope,practice, 
                    ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "nt"),
                    steps,"0", sep = "_"),
         nt_buf = paste(landtype_cli,soil,landtype_slope,practice, 
                        ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "nt"),
                        steps,"10", sep = "_"),
         grass_all = paste(landtype_cli,soil,landtype_slope,"g","no", steps,"0", sep = "_"))
# earlier use of median lead to x.5 values it is now simplified 210615 -----
# dt<- dt %>% 
#   mutate(landtype_slope  = ifelse((slope_max*45  < 3), 2, #percent to degree
#                                   ifelse((slope_max*45 < 6), 5, 
#                                          ifelse((slope_max*45 < 9), 8,
#                                                 ifelse((slope_max*45 < 15), 12,
#                                                        ifelse((slope_max*45 < 35), 25, 35))))),
#         practice = ifelse(grepl(pattern = "3|3.5", landtype_man), "f",   # the .5 should not be there function of mean not median 
#                            ifelse(grepl(pattern = "2|2.5", landtype_man), "g", 
#                                   ifelse(grepl(pattern = "4|4.5|5", landtype_man), "water",
#                                          ifelse(grepl(pattern = "6|6.5", landtype_man), "wbp",
#                                                 ifelse(grepl(pattern = "7|7.5", landtype_man), "wbf",
#                                                        ifelse(grepl(pattern = "8|8.5", landtype_man), "wf",
#                                                               "other")))))),
#         soil = ifelse(landtype_soil < 2, "s", 
#                        ifelse(landtype_soil < 3, "m","d")),
#         ct = paste(landtype_cli,soil,landtype_slope,practice,  # could be a little function 
#                    ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "ct"),
#                    steps,"0", sep = "_"),
#         ct_buf = paste(landtype_cli,soil,landtype_slope,practice, 
#                    ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "ct"),
#                    steps,"10", sep = "_"),
#         mt = paste(landtype_cli,soil,landtype_slope,practice, 
#                    ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "mt"),
#                    steps,"0", sep = "_"),
#         mt_buf = paste(landtype_cli,soil,landtype_slope,practice, 
#                    ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "mt"),
#                    steps,"10", sep = "_"),
#         nt = paste(landtype_cli,soil,landtype_slope,practice, 
#                    ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "nt"),
#                    steps,"0", sep = "_"),
#         nt_buf = paste(landtype_cli,soil,landtype_slope,practice, 
#                    ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "nt"),
#                    steps,"10", sep = "_"),
#         grass_all = paste(landtype_cli,soil,landtype_slope,"g","no", steps,"0", sep = "_"))
# ct_all = paste(landtype_cli,soil,landtype_slope,"no", steps,"0", sep = "_"),
# mt_all = paste(landtype_cli,soil,landtype_slope,"g","no", steps,"0", sep = "_"),
# nt_all = paste(landtype_cli,soil,landtype_slope,"g","no", steps,"0", sep = "_"),
# ct_buf_all = paste(landtype_cli,soil,landtype_slope,"g","no", steps,"0", sep = "_"),
# mt_buf_all = paste(landtype_cli,soil,landtype_slope,"g","no", steps,"0", sep = "_"),
# nt_buf_all = paste(landtype_cli,soil,landtype_slope,"g","no", steps,"0", sep = "_")
        #,
        # ct = paste(landtype_cli,soil,landtype_slope,practice,ct, steps,"0", sep = "_"),
        # ct_buf = paste(landtype_cli,soil,landtype_slope,practice, 
        #                ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "ct"),
        #                steps,"10", sep = "_"),
        # mt = paste(landtype_cli,soil,landtype_slope,practice, 
        #            ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "mt"),
        #            steps,"0", sep = "_"),
        # mt_buf = paste(landtype_cli,soil,landtype_slope,practice, 
        #                ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "mt"),
        #                steps,"10", sep = "_"),
        # nt = paste(landtype_cli,soil,landtype_slope,practice, 
        #            ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "nt"),
        #            steps,"0", sep = "_"),
        # nt_buf = paste(landtype_cli,soil,landtype_slope,practice, 
        #                ifelse(grepl(pattern = "g|^f|water|other", practice), "no", "nt"),
        #                steps,"10", sep = "_"))


# make this a functioN ---------------------
### this step merges in the wshed_flow to get back to xy. 
# add in hillslope shape here 
dt_merge <- merge(wshed_flow, dt, by = c("huc12","profile"))[,c("huc12","steps.x","profile","duplicate.y", "x","y","ct","ct_buf","mt","mt_buf","nt","nt_buf", "grass_all")]   
gc()
#fwrite(x = dt_merge, file = "processed_data/gis_processed/dt_merge_210615.csv")
#dt_merge <- fread(file = "processed_data/gis_processed/dt_merge_210615.csv")

dt_merge_ct <- merge(dt_merge, WEPP_data, 
                       by.x = c("ct","steps.x"), by.y = c("ID", "OFE"),
                       all.x = T, all.y = F)[,c("huc12", "ct","steps.x","profile","x","y","Errosion_kg/m2","Erosion_T_ha1","RM","Q","ET","Dp","UpStrmQ","QOFE","SubRIn","latqcc")][
                         , total_Q := (Q + UpStrmQ)][, total_Lat := (SubRIn + latqcc)]

dt_merge_ct <- dt_merge_ct[!is.na(`Errosion_kg/m2`)] ### this step controls the migration of NA values for flow line passing through an NA cell 201209
#fwrite(dt_merge_ct, file = "processed_data/batch/geo_scratch/ct_data_201228.csv") #includes missing runs (996)
# dt_merge_ct <- fread(file = "D:/Dropbox/active/PCD_work/PRW_BMP/processed_data/batch/geo_scratch/ct_data_201228.csv") #includes missing runs (996)
# rm(dt_merge_ct)
gc()

dt_merge_ct_buf <- merge(dt_merge, WEPP_data, 
                     by.x = c("ct_buf","steps.x"), by.y = c("ID", "OFE"),
                     all.x = T, all.y = F)[,c("huc12", "ct_buf","steps.x","profile","x","y","Errosion_kg/m2","Erosion_T_ha1","RM","Q","ET","Dp","UpStrmQ","QOFE","SubRIn","latqcc")][
                       , total_Q := (Q + UpStrmQ)][, total_Lat := (SubRIn + latqcc)]
# dt_merge_ct_buf <- fread(file = "D:/Dropbox/active/PCD_work/PRW_BMP/processed_data/batch/geo_scratch/ct_buf_data_201228.csv") #includes missing runs (996)

dt_merge_ct_buf <- dt_merge_ct_buf[!is.na(`Errosion_kg/m2`)] ### this step controls the migration of NA values for flow line passing through an NA cell 201209
#fwrite(dt_merge_ct_buf, file = "processed_data/batch/geo_scratch/ct_buf_data_201228.csv") #includes missing runs (996)
# dt_merge_ct <- fread(file = "D:/Dropbox/active/PCD_work/PRW_BMP/processed_data/batch/geo_scratch/ct_data_201228.csv") #includes missing runs (996)

dt_merge_mt <- merge(dt_merge, WEPP_data, 
                     by.x = c("mt","steps.x"), by.y = c("ID", "OFE"),
                     all.x = T, all.y = F)[,c("huc12","mt","steps.x","profile","x","y","Errosion_kg/m2","Erosion_T_ha1","RM","Q","ET","Dp","QOFE","UpStrmQ","SubRIn","latqcc")][
                       , total_Q := (Q + UpStrmQ)][, total_Lat := (SubRIn + latqcc)]

dt_merge_mt <- dt_merge_mt[!is.na(`Errosion_kg/m2`)]
#fwrite(dt_merge_mt, file = "processed_data/batch/geo_scratch/mt_data_201228.csv")
# dt_merge_mt <- fread(file = "D:/Dropbox/active/PCD_work/PRW_BMP/processed_data/batch/geo_scratch/mt_data_201228.csv") #includes missing runs (996)

dt_merge_mt_buf <- merge(dt_merge, WEPP_data, 
                     by.x = c("mt_buf","steps.x"), by.y = c("ID", "OFE"),
                     all.x = T, all.y = F)[,c("huc12","mt_buf","steps.x","profile","x","y","Errosion_kg/m2","Erosion_T_ha1","RM","Q","ET","Dp","QOFE","UpStrmQ","SubRIn","latqcc")][
                       , total_Q := (Q + UpStrmQ)][, total_Lat := (SubRIn + latqcc)]

dt_merge_mt_buf <- dt_merge_mt_buf[!is.na(`Errosion_kg/m2`)]
#fwrite(dt_merge_mt_buf, file = "processed_data/batch/geo_scratch/mt_buf_data_201228.csv")
#dt_merge_mt_buf <- fread(file = "D:/Dropbox/active/PCD_work/PRW_BMP/processed_data/batch/geo_scratch/mt_buf_data_201228.csv")
# dt_merge_mt <- fread(file = "D:/Dropbox/active/PCD_work/PRW_BMP/processed_data/batch/geo_scratch/mt_data_201207.csv") 
gc()

dt_merge_nt <- merge(dt_merge, WEPP_data, 
                     by.x = c("nt","steps.x"), by.y = c("ID", "OFE"),
                     all.x = T, all.y = F)[,c("huc12","nt","steps.x","profile","x","y","Errosion_kg/m2","Erosion_T_ha1","RM","Q","ET","QOFE","Dp","UpStrmQ","SubRIn","latqcc")][
                       , total_Q := (Q + UpStrmQ)][, total_Lat := (SubRIn + latqcc)]

dt_merge_nt <- dt_merge_nt[!is.na(`Errosion_kg/m2`)]
# fwrite(dt_merge_nt, file = "processed_data/batch/geo_scratch/nt_data_201228.csv")
# dt_merge_nt <- fread(file = "D:/Dropbox/active/PCD_work/PRW_BMP/processed_data/batch/geo_scratch/nt_data_201228.csv") 
dt_merge_nt_buf <- merge(dt_merge, WEPP_data, 
                     by.x = c("nt_buf","steps.x"), by.y = c("ID", "OFE"),
                     all.x = T, all.y = F)[,c("huc12","nt_buf","steps.x","profile","x","y","Errosion_kg/m2","Erosion_T_ha1","RM","Q","ET","QOFE","Dp","UpStrmQ","SubRIn","latqcc")][
                       , total_Q := (Q + UpStrmQ)][, total_Lat := (SubRIn + latqcc)]

dt_merge_nt_buf <- dt_merge_nt_buf[!is.na(`Errosion_kg/m2`)]
# fwrite(dt_merge_nt_buf, file = "processed_data/batch/geo_scratch/nt_buf_data_201228.csv")
#dt_merge_nt_buf <- fread(file = "D:/Dropbox/active/PCD_work/PRW_BMP/processed_data/batch/geo_scratch/nt_buf_data_201228.csv")

# read back in the merged wshed wepp data  
#
# dt_merge_ct <- fread(file = "processed_data/batch/geo_scratch/ct_data_201228.csv")
# dt_merge_ct_buf <- fread(file = "processed_data/batch/geo_scratch/ct_buf_data_201228.csv")
# 
# dt_merge_mt <- fread(file = "processed_data/batch/geo_scratch/mt_data_201228.csv")
# dt_merge_mt_buf <- fread(file = "processed_data/batch/geo_scratch/mt_buf_data_201228.csv")
# 
# dt_merge_nt <- fread(file = "processed_data/batch/geo_scratch/nt_data_201228.csv")
# dt_merge_nt_buf <- fread(file = "processed_data/batch/geo_scratch/nt_buf_data_201228.csv")



merged <- list("CT"=(dt_merge_ct),"MT"=(dt_merge_mt), "NT"=(dt_merge_nt),"CT_buf"=(dt_merge_ct_buf),"MT_buf"=(dt_merge_mt_buf), "NT_buf"=(dt_merge_nt_buf))




#### testing missing in wepp data -----
# dt_merge_ct <- fread("processed_data/gis_processed/ct_data_201012.csv")
# make function by ct, nt, mt, 

# ct_sum <- dt_merge_ct[, lapply(.SD, sum, na.rm=TRUE), by=c("x","y"), .SDcols=c("Errosion_kg/m2","RM","Q","ET","Dp","UpStrmQ","SubRIn","latqcc")]
# ct_max <- dt_merge_ct[, lapply(.SD, max, na.rm=TRUE), by=c("x","y"), .SDcols=c("Errosion_kg/m2","RM","Q","ET","Dp","UpStrmQ","SubRIn","latqcc")]
# ct_mean <- dt_merge_ct[, lapply(.SD, mean, na.rm=TRUE), by=c("x","y"), .SDcols=c("Errosion_kg/m2","RM","Q","ET","Dp","UpStrmQ","SubRIn","latqcc")]

### **** add in here special summation for combining flow -------
# if duplicated = F Q = Qofe, Lat = Lat, gather Upslope Q.  
# if dulicated = T, Max Qofe + sum of run on from above.... 
# can I track the inflows. 

# ADD hydro sum sum_it, add Count to summations  


# sum_it <- function (x){
#   temp_sum <- x[, lapply(.SD, sum, na.rm=TRUE), by=c("x","y"), 
#                 .SDcols=c("huc12","Errosion_kg/m2","Erosion_T_ha1","total_Q","QOFE", "UpStrmQ", "total_Lat","RM","latqcc")] # removeNA = T before this? 
#   temp_max <- x[, lapply(.SD, max, na.rm=TRUE), by=c("x","y"), 
#                 .SDcols=c("huc12","Errosion_kg/m2","Erosion_T_ha1","total_Q","QOFE", "total_Lat","RM","latqcc")]
#   temp_mean <- x[, lapply(.SD, mean, na.rm=TRUE), by=c("x","y"), 
#                  .SDcols=c("huc12","Errosion_kg/m2","Erosion_T_ha1","total_Q","QOFE", "total_Lat","RM","latqcc")]
#   ID <- x[, lapply(.SD,first), by = c("x","y"), .SDcol = 2] #add huc12
#   temp_sum <- merge(temp_sum,ID)
#   temp_max <- merge(temp_max,ID)
#   temp_mean <- merge(temp_mean,ID)
#   temp <- list(temp_sum,temp_max, temp_mean)
#   names(temp) <-c("sum", "max", "mean")
#   return(temp)
# }


#updated the the sum algorithm to solve for a most mass balanced flow. 
sum_it <- function (x){
  temp_sum <- x[, lapply(.SD, sum, na.rm=TRUE), by=c("x","y"), 
                .SDcols=c("huc12","Errosion_kg/m2","Erosion_T_ha1","total_Q","QOFE", "UpStrmQ", "total_Lat","RM","latqcc")] # removeNA = T before this? 
  
  
  
  temp_max <- x[, lapply(.SD, max, na.rm=TRUE), by=c("x","y"), 
                .SDcols=c("huc12","Errosion_kg/m2","Erosion_T_ha1","total_Q","QOFE", "total_Lat","RM","latqcc")]
  temp_mean <- x[, lapply(.SD, mean, na.rm=TRUE), by=c("x","y"), 
                 .SDcols=c("huc12","Errosion_kg/m2","Erosion_T_ha1","total_Q","QOFE", "total_Lat","RM","latqcc")]
  ID <- x[, lapply(.SD,first), by = c("x","y"), .SDcol = 1:4] #add huc12
  temp_sum <- merge(temp_sum,ID)
  temp_max <- merge(temp_max,ID)
  temp_mean <- merge(temp_mean,ID)
  temp <- list(temp_sum,temp_max, temp_mean)
  names(temp) <-c("sum", "max", "mean")
  return(temp)
}
testing <- head(x,1000)[, .SD[which.max(steps.x)], by=c("x","y"), .SDcol = 1:4]


sum_dat <- merged %>% map(sum_it) %>% unlist(recursive =F)
# sub_ct_dat <- sum_it(dt_merge_ct)
# fwrite(merged, file = "processed_data/batch/geo_scratch/merged_210615")
# merged  <- fread(file = "processed_data/batch/geo_scratch/merged_210218")

gc()

sum_dat_sum <- sum_dat[grepl("sum", names(sum_dat))]
## note the selection of the first ID here tosses out noag aggragated with ag. 
sum_dat_sum$CT.sum <- sum_dat_sum$CT.sum[!grepl("no", ct)]
sum_dat_sum$MT.sum <- sum_dat_sum$MT.sum[!grepl("no", mt)]
sum_dat_sum$NT.sum <- sum_dat_sum$NT.sum[!grepl("no", nt)]

sum_noag<- function (x){
  erosion <- sum(x[,4]*900/1000)  #TONNES or Mg
  sed_delivery <- sum(x[,5]*900/10000) #TONNES or Mg
  total_lat <- sum(x[,8]/x[,9])
  total_Q <- sum(x[,6]/x[,9])
  temp_out <- list(erosion,sed_delivery, total_lat, total_Q)
  names(temp_out) <-c("Erosion_Mg", "Sed_Mg", "Lateral_percent", "Q_percent")
  return(temp_out)
 }
x<- sum_dat_sum$CT.sum %>% head(100) %>% list()

test <- sum_dat_sum %>% map(sum_noag)
testit2 <- test %>% unlist %>% 
  matrix(nrow =4, ncol = 6) %>%
  t() %>% as.data.frame()%>% 
  mutate(v5 = c("CT", "MT","NT","CT Buffer","MT Buffer","NT Buffer" ))
colnames(testit2) <- c("Erosion_Mg", "Sed_Mg", "Lateral_percent", "Q_percent", "Management")

## move to plots result figures-------------
# ggplot(data = testit2)+
#   geom_col(aes(x =Management, y = Sed_Mg, fill= Management))+ theme_classic()+
#   ylab('Total detachment (Mg/year)')+scale_fill_manual(values=viridis(6))
# #type_of_data <- c("UpStrmQ","Errosion_kg/m2","SubRIn")%>% rep(3)
# cum_sed <- as.data.table(cbind(sum_dat_sum$CT.sum$Erosion_T_ha1,
#               sum_dat_sum$MT.sum$Erosion_T_ha1,
#               sum_dat_sum$NT.sum$Erosion_T_ha1,
#               sum_dat_sum$CT_buf.sum$Erosion_T_ha1,
#               sum_dat_sum$MT_buf.sum$Erosion_T_ha1,
#               sum_dat_sum$NT_buf.sum$Erosion_T_ha1))
# 
# cum_sedxy <- sum_dat_sum$CT.sum[,c(1,2,5)] %>% 
#   merge(sum_dat_sum$MT.sum[,c(1,2,5)], suffixes = c(".CT", ".MT")) %>%
#   merge(sum_dat_sum$NT.sum[,c(1,2,5)]) %>%
#   merge(sum_dat_sum$CT_buf.sum[,c(1,2,5)], suffixes = c(".NT", ".CT_BUF")) %>%
#   merge(sum_dat_sum$MT_buf.sum[,c(1,2,5)]) %>%
#   merge(sum_dat_sum$NT_buf.sum[,c(1,2,5)], suffixes = c(".MT_Buf", ".NT_BUF")) %>%
#   as.data.table()
# names(cum_sedxy)<- c("x","y","CT", "MT","NT","CT_Buf","MT_Buf","NT_Buf")
# cum_sedxy[,area_ha := .09]
# 
# testit3<- cum_sedxy[order(CT, decreasing = T)][,c("x","y"):= NULL][, lapply(.SD, cumsum)]
# testit4 <- testit3[, lapply(.SD, function(x){x/max(x)})]
# testit5 <- testit3[sample(.N,40000)] %>% pivot_longer(.,names(.)[-7],"Management")
# 
# ggplot(testit5, aes(x = area_ha/max(area_ha)*100, y = value, group = Management))+
#   geom_line(aes(color = Management), size =1)+
#     scale_color_manual(values=viridis(6))+theme_minimal()+
#   xlab("Percent area")+ylab("Cumulative annual erosion (Mg) \n")
#   
# 
# 
# 
# ggplot(testit3)+
#   geom_line(aes(x = area_ha, y = (CT)),col= "red")+
#     geom_line(aes(x = area_ha, y = (MT)),col= "blue")+
#   geom_line(aes(x = area_ha, y = (NT)),col= "green")+
#   geom_line(aes(x = area_ha, y = (CT_Buf)),col= "red", linetype = "dashed")+
#   geom_line(aes(x = area_ha, y = (MT_Buf)),col= "blue", linetype = "dashed")+
#   geom_line(aes(x = area_ha, y = (NT_Buf)),col= "green", linetype = "dashed")+theme_classic()
# 
# 
# ggplot(testit3[sample(.N,10000)])+
#   geom_line(aes(x = log(CT), y = area_ha),col= "red")+
#   geom_line(aes(x = log(MT), y = area_ha),col= "blue")+
#   geom_line(aes(x = log(NT), y = area_ha),col= "green")+
#   geom_line(aes(x = log(CT_Buf), y = area_ha),col= "red", linetype = "dashed")+
#   geom_line(aes(x = log(MT_Buf), y = area_ha),col= "blue", linetype = "dashed")+
#   geom_line(aes(x = log(NT_Buf), y = area_ha),col= "green", linetype = "dashed")+
#   theme_classic()+ 
#   
# 
# ggplot(testit4[sample(.N,10000)])+
#   geom_line(aes(x = (CT), y = area_ha),col= "green")+
#   geom_line(aes(x = (CT_Buf), y = area_ha),col= "green", linetype = "dashed")+
#   theme_classic()
# 
# ggplot(testit4[sample(.N,10000)])+
#   geom_line(aes(x = area_ha, y = CT ),col= "green")+
#   geom_line(aes(x = area_ha, y = CT_Buf),col= "green", linetype = "dashed")+
#   theme_classic()
# 

# **** HUC 12 by the sum_dat Max(huc12, erosion, sed). Sum (hydros). ------

raster_it <- function (x, y, dir, huc12=huc12){
  
  ifelse(!dir.exists(dir),dir.create(dir),F)
  # soil erosion with different aggregation
  temp1 <- rasterFromXYZ(x[,c("x","y","Errosion_kg/m2")], crs = crs(huc12))
  tempname <- (paste0(dir,y, ".Errosion.tif"))
  writeRaster(temp1, file=tempname,overwrite=TRUE )
  
  # soil erosion with different aggregation
  temp1 <- rasterFromXYZ(x[,c("x","y","Erosion_T_ha1")], crs = crs(huc12))
  tempname <- (paste0(dir,y, ".sed_t_ha.tif"))
  writeRaster(temp1, file=tempname,overwrite=TRUE )
  
  # runoff with different aggregation
  temp2 <- rasterFromXYZ(x[,c("x","y","total_Q")], crs = crs(huc12))
  tempname <- (paste0(dir,y, ".Q.tif"))
  writeRaster(temp2, file=tempname,overwrite=TRUE )
  
  # runoff with different aggregation
  temp2 <- rasterFromXYZ(x[,c("x","y","QOFE")], crs = crs(huc12))
  tempname <- (paste0(dir,y, ".Qofe.tif"))
  writeRaster(temp2, file=tempname,overwrite=TRUE )
  
  # lateral flow with different aggregation
  temp3 <- rasterFromXYZ(x[,c("x","y","total_Lat")], crs = crs(huc12))
  tempname <- (paste0(dir,y, ".Lat.tif"))
  writeRaster(temp3, file=tempname,overwrite=TRUE )
  
  # RM flow with different aggregation
  temp4 <- rasterFromXYZ(x[,c("x","y","RM")], crs = crs(huc12))
  tempname <- (paste0(dir,y, ".Lat.tif"))
  writeRaster(temp4, file=tempname,overwrite=TRUE )
}

# temp4 <- rasterFromXYZ(sum_dat$CT_buf.sum[,c("x","y","RM")], crs = crs(huc12))
# tempname <- (paste0("processed_data/batch/geo_scratch/","CT_buf", ".RM.tif"))
# writeRaster(temp4, file=tempname,overwrite=TRUE )


test <-  map2 (.x = sum_dat, .y = names(sum_dat), .f =  ~raster_it(.x,.y, dir ="processed_data/batch/geo_scratch/210615/",   huc12))

# x  <- dt_merge_ct
# flowpath_it <- function (x,){
#   Sum <- x[,lapply(.SD,sum),by = profile,.SDcols=c("Q","latqcc", "Dp")] # in MM need account for area by 900 M^2 * steps
#   max <- x[,lapply(.SD,max),by = profile,.SDcols=c("steps.x","ET","RM")]
#   mean <- x[,lapply(.SD,max),by = profile,.SDcols=c("Errosion_kg/m2")]
#   plot(c$Q,)
# 
# }


### huc 12 polys 
# group by HUC12
#     area of erosion > 5 t/acre (by sediment delivery)
#     some king of lateral flow 



by_flowpath <- function (x){
  max <-x[, lapply(.SD, max), by=c("profile","huc12"), .SDcols=c("Erosion_T_ha1","steps.x","QOFE","total_Q", "total_Lat")]
  sum <- x[, lapply(.SD, sum), by = c("profile","huc12"), .SDcols = c("Errosion_kg/m2","RM","Dp","latqcc")] ## does this need to be over max(steps.x)
  temp <- merge(max,sum)%>%list()
  names(temp) <-c("flowpath")
  return(temp)
}

flowpath <- merged %>% map(by_flowpath) %>% unlist(recursive =F)
# flowpath_ct <- list(dt_merge_ct) %>% map(by_flowpath) %>% unlist(recursive =F)

fwrite(flowpath, file = "processed_data/batch/geo_scratch/flowpath_list_201228")

# flowpath <- fread("processed_data/batch/geo_scratch/flowpath_list_201228")

hydro_volume <- function(x,A){
  out <- sum(x/(A/900)/1000)
}
# 
# erosion_along_fun <- function(a,b){
#   out <- sum(a*900*b)/sum(a*900)  
# }
# 
# sed <- function(a,b){
#   out <- sum(a*900*b/10000)/sum(a*900/10000)  
# }
x<- flowpath$CT.flowpath

by_huc12 <- function(x){
  hydro <- x[, lapply(.SD, hydro_volume, A = steps.x), by = huc12, .SDcols=c("QOFE","total_Q","total_Lat")]
  erosion_along_kg <- x[, .(erosion_along_kg = sum(`Errosion_kg/m2`* steps.x*900)), by = huc12]
  sed_t <- x[, .(erosion_along = sum(Erosion_T_ha1*steps.x*900/10000)), by = huc12]
  area <- x[, .(area_m2 = sum(steps.x*900)), by = huc12]
  QP <- x[, (QP = mean(QOFE/RM)), by = huc12]
  LP <- x[, .(LP = mean(latqcc/RM)), by = huc12] 
  DP <- x[, .(DP = mean(Dp/RM)), by = huc12] 
  area_CSA_erosion <- x[, .(csa_m2 = sum(steps.x[`Errosion_kg/m2` > (5/4.46)]*900)), by = huc12]
  area_CSA_sed <- x[, .(csa_m2_sed = sum(steps.x[Erosion_T_ha1 > 11.21]*900)), by = huc12]
  erosion_area_kg_m <- x[, .(erosion_area_kg_m = sum(`Errosion_kg/m2`*steps.x*900)/sum(steps.x*900)*4.46),by = huc12] 
  temp <- merge(hydro, erosion_along_kg) %>% merge(sed_t) %>% merge(area) %>%  merge(QP)%>%  
    merge(LP)%>%merge(DP) %>% merge(area_CSA_erosion)%>% merge(area_CSA_sed) %>% merge(erosion_area_kg_m)
  temp$huc12 <- as.character(temp$huc12)
  temp <- list(temp)
  return(temp)
}


by_huc12_cells <- function(x){
  area <- x[, .(area_m2 = .N*900), by = huc12]
  area_CSA_erosion_5t <- x[, .(csa_5t_m2 = sum(`Errosion_kg/m2` > (5/4.46))*900), by = huc12]
  area_CSA_erosion_4t <- x[, .(csa_4t_m2 = sum(`Errosion_kg/m2` > (4/4.46))*900), by = huc12]
  area_CSA_erosion_3t <- x[, .(csa_3t_m2 = sum(`Errosion_kg/m2` > (3/4.46))*900), by = huc12]
  area_CSA_erosion_2t <- x[, .(csa_2t_m2 = sum(`Errosion_kg/m2` > (2/4.46))*900), by = huc12]
  temp <- merge(area, area_CSA_erosion_5t) %>%  merge(area_CSA_erosion_4t)%>%
    merge(area_CSA_erosion_3t) %>% merge(area_CSA_erosion_2t)
  temp$huc12 <- as.character(temp$huc12)
  temp <- list(temp)
  return(temp)
}

## by pulling from flow paths, the total values are double counting, relative values should be okay. 
huc12_data <- flowpath %>% map(by_huc12) %>% unlist(recursive =F)
huc12_data_ct <- flowpath_ct %>% map(by_huc12) %>% unlist(recursive =F) %>% as.data.table
huc12_data_ct_cells <- list(dt_merge_ct) %>% map(by_huc12_cells)
huc12_data_ct <- huc12_data_ct$flowpath %>% as.data.table() 
  merge(., huc12_data_ct_cells %>% as.data.frame())

huc12_data_names <- names(huc12_data_ct$)
# fwrite(flowpath, file = "processed_data/batch/geo_scratch/flowpath_list_201228")
#flowpath <- fread("processed_data/batch/geo_scratch/flowpath_list_201228")
huc12_data_names <- names(huc12_data$CT.flowpath)
x <- huc12_data$CT.flowpath
huc_it <- function(x,y, huc12=huc12){
  temp_out <- merge(huc12, x, by.x = "Id", by.y = "huc12")
  tempname <- (paste0("huc12_",y))
  writeOGR(obj=temp_out, dsn="processed_data/batch/geo_scratch/huc12_210615", layer=tempname, driver="ESRI Shapefile")
}

test <-  map2 (.x = huc12_data, .y = names(huc12_data), .f =  ~huc_it(.x, .y,  huc12))

########### Huc 12 figures _move to new script?? 210128-----------

scenarios<- names(huc12_data)
huc_names <- c("ersn_l_","ersn_ln", "area_m2", "V1","LP" ,
       "DP", "csa_m2","cs_m2_s", "ersn___")
huc_crossing <- crossing(scenarios, huc_names)

# x = huc_crossing$scenarios[1]
# y = huc_crossing$huc_names[2]

#### ct table CSA ------- 
ct_table <- cbind(c(">5 tons/acres",">4 tons/acres",">3 tons/acres", ">2 tons/acres", ">1 tons/acres"),
                  c(sum(sum_dat$CT.sum$`Errosion_kg/m2`>(5/4.46))/sum(!grepl("no", sum_dat$CT.sum$ct)),
                        sum(sum_dat$CT.sum$`Errosion_kg/m2`>(4/4.46))/sum(!grepl("no", sum_dat$CT.sum$ct)),
                            sum(sum_dat$CT.sum$`Errosion_kg/m2`>(3/4.46))/sum(!grepl("no", sum_dat$CT.sum$ct)),
                                sum(sum_dat$CT.sum$`Errosion_kg/m2`>(2/4.46))/sum(!grepl("no", sum_dat$CT.sum$ct)),
                                    sum(sum_dat$CT.sum$`Errosion_kg/m2`>(1/4.46))/sum(!grepl("no", sum_dat$CT.sum$ct)))) %>% 
  as.data.table()
names(ct_table) <-  c("threshold", "percent")

test4 <- WEPP_data[, total_Q := (Q + UpStrmQ)][, total_Lat := (SubRIn + latqcc)][
  , lapply(.SD, max, na.rm=TRUE), by=c("ID"), .SDcols=c("OFE","RM","Dp","ET", "Errosion_kg/m2","total_Q", "total_Lat")]

##### aggregate all--------------- 
dt_merge <-  fread(file = "processed_data/gis_processed/dt_merge_201228.csv")



##### testing how to plot everything 
dt_merge_test <- head(dt_merge,500)[,dup := duplicated(x,y)][,path:= length(unique(c(huc12, steps.x)))]  

unique(dt_merge_test[,c("huc12","profile")])

#### used id's 
tempit <- head(dt_merge_ct$ct, 1000)
ids <- dt_merge_ct$ct %>% 
  unique() %>% 
  lapply(function(x) (strsplit(x, '_')[[1]][1:4]%>% 
                             paste(collapse = "_"))) %>% 
  unlist(recursive = F)

id_regex <- paste((ids), collapse = "|")

### sum wepp data First? 
WEPP_ids <- WEPP_data$ID %>% 
  lapply(function(x) (strsplit(x, '_')[[1]][1:4]%>% 
                        paste(collapse = "_"))) %>% 
  unlist(recursive = F)

Used_runs <- (WEPP_ids) %in% (ids)
length(WEPP_ids)
nrow(WEPP_data)
used_wepp <- WEPP_data[Used_runs,]




#

temp_dup_sum <- wshed_flow[, lapply(.SD, sum, na.rm=TRUE), by=c("x","y"), .SDcols=c("duplicate")]
temp_dup_sum$duplicate %>% table





huc_fig <- function (x, y){
  testit <- shapefile(paste0("processed_data/batch/geo_scratch/huc12_210127/huc12_",x,".shp"))
  testit.points <- fortify(testit, region ="Id")
  testit.df <- merge(testit.points,testit@data, by.x="id", by.y = "Id")
  
  temp <-     ggplot(testit.df,aes(long,lat,group=group,fill=get(y))) + 
    geom_polygon() +
    geom_path()+
    theme_bw()+
    scale_fill_viridis(option="A", direction = )+ 
    labs(x = "Easting (m)", y = "Northing (m)", 
         fill = paste0(strsplit(x, split = "[.]")[[1]][1],"_",y)) 
  #   scale_fill_manual(values=rev(viridis(4)), na.value = "white")
  #   
  
  
  ggsave(paste0("processed_data/batch/figures/210128/",x,"_",y,".jpeg"),temp, width = 6, height = 4, dpi = 300)
}
test <-  map2 (.x = huc_crossing$scenarios, .y = huc_crossing$huc_names,
               .f =  ~huc_fig(.x, .y))

# library(rgdal)
# library(dplyr)
# map<-readOGR("C:/MAPS","33SEE250GC_SIR") 
# map<-subset(world, LON>-43.41 | LON < -43.1 & LAT>- 23.05 | LAT< -22.79)
# writeOGR(temp_out, "processed_data/batch/geo_scratch/", "test_this_shit", driver="ESRI Shapefile") 
# writeOGR(obj=temp_out, dsn=paste0("processed_data/batch/geo_scratch/huc12_",,".shp"), layer="csa_area", driver="ESRI Shapefile")
# test <- shapefile("processed_data/batch/geo_scratch/huc12_210127/huc12_CT.flowpath.sh.shp")
# ct_huc12_mean <- extract(raster_ct,huc12,fun=mean, df=T, na.omit=T)
# ct_huc12_median <- extract(raster_ct,huc12,fun=median, d=T, na.omit=T)
# ct_huc12_max <- extract(raster_ct,huc12,fun=max, df=T, na.omit=T)
# ct_huc12_csa <- extract(raster_ct,huc12,fun=(function(x) sum(x*4.49>=5)),cellnumbers=T, df=T)
# 
# test <- huc12[,c("OBJECTID", "Id", "gridcode",    "Shape_Leng",    "Shape_Area")]

sum_dat <- merged %>% map(sum_it) %>% unlist(recursive =F)
test3 <- WEPP_data[, total_Q := (Q + UpStrmQ)][, total_Lat := (SubRIn + latqcc)][
  , lapply(.SD, sum, na.rm=TRUE), by=c("ID"), .SDcols=c("Errosion_kg/m2","QOFE","total_Q", "total_Lat")]

# test3[, c("Cli", "Soil", "Slope", "crop","till","OFE", "Buf") := tstrsplit(ID, "_", fixed=TRUE)]

##################fix the 9_d_25_g_no_13/16/19_0
