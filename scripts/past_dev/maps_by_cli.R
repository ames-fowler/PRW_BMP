library(tictoc)
library (tictoc)
library (raster)

Cli_PRW <- raster("processed_data/Cli_PRW")
Mapunit_r_C <- raster("processed_data/Mapunit_r_7-3_test.tif")

#plot(Cli_PRW)
#plot(Mapunit_r_C)
s<-stack(Mapunit_r_C,Cli_PRW)
head(s)
s_table<-as.data.frame(s, xy = TRUE)%>%
  na.omit()
names(s_table)<-c("x","y","mapunit","cli")

head(s_table)

fwrite(s_table, 'processed_data/s_table.txt')
# 
# #unique(s_table$mapunit,s_table$)
# tic()
# test <- s_table[!duplicated((s_table)[3:4]),]
# toc()#11.19 sec
# 
# tic()
# s_table_U <- unique(s_table[c("mapunit","cli")])  #long time to run 
# toc()#11.31 sec
# 
# 
# ## what does this do?
# maps_by_cli<-mapply(x= unique(s_table_U$cli), function(x)  s_table_U$mapunit[s_table_U$cli==x])
# maps_by_cli_df<- as.data.frame(maps_by_cli) #broken - cannot make a data frame as there are multiple lengths of mukey  per climate zone
# head(maps_by_cli_df)
# ?group_by



# plot(cli_PRW)
# huc12<-shapefile("RAW/extent/HUC12")
# 
# plot(huc12, add=T)
###4,5,6 
# check where the how many climates the Mukeys cross into. 
#v<-s_table_U%>%count(mapunit)
#View(v)
#length(s_table_U$mapunit)
