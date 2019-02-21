


plot(cli_PRW)
plot(Mapunit_r_C
     )
s<-stack(Mapunit_r,cli_PRW)

s_table<-as.data.frame(s, xy = TRUE)%>%
  na.omit()
names(s_table)<-c("x","y","mapunit","cli")

head(s_table)

#unique(s_table$mapunit,s_table$)
s_table_U <- unique(s_table[c("mapunit","cli")])  #long time to run 


maps_by_cli<-mapply(x= unique(s_table_U$cli), function(x)  s_table_U$mapunit[s_table_U$cli==x])
maps_by_cli_df<- as.data.frame(maps_by_cli)
head(maps_by_cli_df)
?group_by



plot(cli_PRW)
huc12<-shapefile("RAW/extent/HUC12")


###4,5,6 
s_table_U%>%count(mapunit)
