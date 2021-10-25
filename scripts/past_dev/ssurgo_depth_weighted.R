
#Ames Fowler 
#
#10/17/19

# need this for ddply()
library(dplyr)
library(sf)
library(data.table)
library(microbenchmark)

# load horizon and component data
#chorizon <- read.csv('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_chorizon.csv')
mukey <- fread('RAW/mukey_Output.txt')
mukey_table <- fread('RAW/mukey_list.txt')
chorizon <- fread('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_chorizon.csv')

# only keep some of the columns from the component table
component <- fread('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_component.csv')[,c('mukey','cokey','comppct.r','compname','majcompflag')]

#load corestrictions data

restrictions <- fread('EXTRACTIONS/PRW/SSURGO/PRW_SSURGO_corestrictions.csv')[,c('cokey','resdept.r')]

#select the desired variables from the chorizon table. ksat.r is in um/s, depths are in cm 
comp_data <- chorizon[
  ,list(ksat.r,cokey,hzdepb.r,hzdept.r)]

# removing and ksat NA rows - this may be a limitation about 7% of profile data have missing Ksat data. 
# 398 of these are rock with no data from the surface. the rest are missing layers with no conductivity data.    
# test <- merge(comp_data, component)
# test2<-test[is.na(ksat.r)]
# the null data was omitted. 
#View(comp_data)

#The chorizpn data was merged with a restrictive layer database.
# a depth weighted Ksat and profile thickness variable was built 

comp_data <-  (merge(chorizon[
  ,list(ksat.r,cokey,hzdepb.r,hzdept.r)],restrictions, all=T))[
    , ksat_w := (ksat.r*(hzdepb.r-hzdept.r))][
      , ch_thick := ((hzdepb.r-hzdept.r))]


# layers were grouped into components. 
# 

comp_group <- comp_data[
  , list(ch_thick=sum(ch_thick),b_depth=max(hzdepb.r),
         ksat_w = sum(ksat_w),  resdept.r= mean(resdept.r)), 
                            by = list(cokey=cokey)]

#kast and soil depth were calculated 
comp_group<- comp_group[, ksat_cmd := (ksat_w/ch_thick)*8.64][, soildepth_cm := ifelse(is.na(resdept.r),b_depth,resdept.r)]

#grouped data are merged back to mukeys byt maximum component percentages
out_table <- merge(comp_group,component,all=T)
out_table <- out_table[out_table[, .I[comppct.r == max(comppct.r)], by= mukey]$V1][,
  c("mukey","cokey","soildepth_cm","resdept.r","ksat_cmd","comppct.r","compname", "majcompflag")]
getwd()

#make the CSV
fwrite(out_table, "sratchspace/prw_mukey_list.csv")
