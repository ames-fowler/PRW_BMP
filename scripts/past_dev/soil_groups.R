
# having run ssurgo_agg.R and maps_by_cli.R now it is time to select the soil types for each climate
#compr_r()

library(tidyverse)
library(data.table)
#testit <- mapply(x=1:length(maps_by_cli), function(x) setDT(comp_r)[mukey %chin% maps_by_cli[[x]]])

s_table <- fread ('processed_data/s_table.txt')
comp_r  <- fread ('processed_data/comp_r.txt')
#head(s_table)
cell_counts<-(s_table[,3:4]) %>% group_by(cli) %>% count(mapunit)  #from maps_by_cli

comp_r_counts <- merge (cell_counts, comp_r, all.x=T, by.x = "mapunit", by.y = "mukey")

#Weights column is the number of cells represented by that component. This may not be a dominate component so if  
# more cells are represented by a non-dominate component then said non-dom soil could represent a dominat soil? could check? 
# There are 4 non dominate soil types selected. these need to be repalced manualy if using WEPPcloud to build soil files. 
comp_r_counts$weights <-comp_r_counts$n*comp_r_counts$comppct.r/100

# testit<-comp_r %>%
#   slice(duplicated(comp_r$cokey)==T)
# 
# Soil_groups_test <- comp_r_counts %>%
#   subset(comppct.r>50) %>%
#   group_by(cli, soil_depth)%>%
#   summarize(n = max(n), mapunit = mapunit[which.max(n)],
#             compname = compname[which.max(n)],
#             hz_soil_depth = hz_soil_depth[which.max(n)], 
#             comppct.r = comppct.r[which.max(n)],
#             cokey = cokey[which.max(n)])



soil_groups <- comp_r_counts %>% 
  group_by(cli, soil_depth)%>%
  slice(which.max(weights))%>%
  na.omit()

#write out soil groups -----------
fwrite(soil_groups, 'processed_data/soil_groups.txt') 

#View(Soil_groups)
