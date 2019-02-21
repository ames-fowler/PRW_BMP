
# having run ssurgo_agg.R and maps_by_cli.R now it is time to select the soil types for ecah climate

compr_r()


library(data.table)
#testit <- mapply(x=1:length(maps_by_cli), function(x) setDT(comp_r)[mukey %chin% maps_by_cli[[x]]])


cell_counts<-(s_table[3:4]) %>% group_by(cli) %>% count(mapunit)

comp_r_counts <- merge (cell_counts, comp_r, all.x=T, by.x = "mapunit", by.y = "mukey")
comp_r_counts$weights <-comp_r_counts$n*comp_r_counts$comppct.r/100

Soil_groups <- comp_r_counts %>% 
  group_by(cli, soil_depth)%>%
  summarize(n = max(n), mapunit = mapunit[which.max(n)],
            compname = compname[which.max(n)],
            hz_soil_depth = hz_soil_depth[which.max(n)], 
            comppct.r = comppct.r[which.max(n)],
            cokey = cokey[which.max(n)])%>%
  na.omit()

Soil_groups <- comp_r_counts %>% 
  group_by(cli, soil_depth)%>%
  slice(which.max(weights))%>%
  na.omit()

subset(comp_r_counts, soil_depth=="medium")
subset(comp_r_counts, cli==1 & soil_depth=="medium" & compname == compname[which.max(n)])

  summarize(n = max(n), compname = compname[which.max(n)])

#write.csv(Soil_groups, file = "processed_data/soil_groups")

Soil_groups <- comp_r_counts %>% 
  group_by(cli, soil_depth)%>%
  summarize(n = max(n), mapunit = mapunit[which.max(n)],
            compname = compname[which.max(n)],
            hz_soil_depth = hz_soil_depth[which.max(n)], 
            comppct.r = comppct.r[which.max(n)],
            cokey = cokey[which.max(n)])%>%
  na.omit()


Soil_groups%>%subset.data.frame(comppct.r<50)
head(comp_r_counts)
comp_r_counts%>%subset.data.frame(cli==7)%>%
  sort(n)

sort(comp_r_counts%>%subset.data.frame(cli==7),n)

test5<-chorizon%>%
  filter(cokey==15891253)




one <- as.data.frame (setDT(comp_r)[mukey %chin% maps_by_cli[[1]]])
colnames(one)<-colnames(comp_r)
head(one)

three <- as.data.frame (setDT(comp_r)[mukey %chin% maps_by_cli[[3]]])
colnames(three)<-colnames(comp_r)
head(three)

four <- as.data.frame (setDT(comp_r)[mukey %chin% maps_by_cli[[4]]])
colnames(four)<-colnames(comp_r)
head(four)

five <- as.data.frame (setDT(comp_r)[mukey %chin% maps_by_cli[[5]]])
colnames(five)<-colnames(comp_r)
test<-merge.data.frame(five,cell_counts$n[cell_counts$cli==5]/sum(cell_counts$n[cell_counts$cli==5]))
head(five)
length(five$cokey)

six <- as.data.frame (setDT(comp_r)[mukey %chin% maps_by_cli[[6]]])
colnames(six)<-colnames(comp_r)
test<-merge.data.frame(six,cell_counts$n[cell_counts$cli==6]/sum(cell_counts$n[cell_counts$cli==6]))
head(six)
length(six$cokey)

seven <- as.data.frame (setDT(comp_r)[mukey %chin% maps_by_cli[[7]]])
colnames(seven)<-colnames(comp_r)
test<-merge.data.frame(seven,cell_counts$n[cell_counts$cli==7]/sum(cell_counts$n[cell_counts$cli==7]))
head(seven)
length(seven$cokey)

eight <- as.data.frame (setDT(comp_r)[mukey %chin% maps_by_cli[[8]]])
colnames(eight)<-colnames(comp_r)
test<-merge.data.frame(eight,cell_counts$n[cell_counts$cli==8]/sum(cell_counts$n[cell_counts$cli==8]))
head(eight)
length(eight$cokey)

test$mukey[test$compname=="Endicott"]

#count all duplicates 
cell_counts<-(s_table[3:4]) %>% group_by(cli) %>% count(mapunit)

test2<- cell_counts$n[cell_counts$cli==1]/sum(cell_counts$n[cell_counts$cli==1])
length(cell_counts$cli)

sum(test2)
max(test2)


head(cell_counts)
mukey_vars<- mapply(x=1:10, function (x) length(cell_counts$cli[cell_counts$cli==x]))

cell_counts[5 %in% cell_counts$cli]

count((s_table[3:4]))



table(dummyData)
length(comp_r$mukey)
comp_r$cokey

testit<- comp_r %>% group_by(cokey) %>% count (mukey)
View(testit)  
  
summary(five)
length(five$compname)

hist(four$ksat_min*.864, breaks=20)

one$hz_ksat_min_depth[one$ksat_min==min(one$ksat_min)]


testit<- rbind(comp_r,maps_by_cli[[4]], colnames())
colnames(comp_r)
?cbind



maps_by_cli[[4]]
str(as.vector(
       maps_by_cli[[4]]))

chorizon$ksat.r[chorizon$hzname=="Endicott"]
test<-subset(chorizon, hzname %in% "Endicott")
test$ksat.r

?dplyr::select

head(chorizon)


maps_by_cli[[10]]
length(maps_by_cli)

head(testit)
testit[[3,10]]

str(testit)

