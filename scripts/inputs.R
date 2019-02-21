


soil_cli<-Soil_groups[c(1,2,4,11)]

soil<-as.data.frame(list.files(path ="processed_data/soil/", pattern = "*.sl",full.names = F))
names(soil)<- "soil"

soil_df <- soil%>%as.data.frame()%>%
  mutate( ofe = 
            ifelse(grepl("_1",soil),1,3))

soil_df$mapunit <-mapply(x=soil,  function(x)strsplit(as.character(x),'_'))%>%
  lapply('[[',1)%>%
  as.numeric()

soil_cli_df <- merge (soil_cli, soil_df)
soil_cli_df$cli_file<-paste0('cli_zone_', soil_cli_df$cli,'.cli')

slope<-as.data.frame(list.files(path ="processed_data/slope/", pattern = "*.slp",full.names = F))
names(slope)<- "slope"

Slope_df <- slope %>% as.data.frame() %>%
  mutate( ofe = 
            ifelse(grepl("300",slope),3,1))

Man<- as.data.frame(list.files(path ="processed_data/man/", pattern = "*.man",full.names = F))
names(Man)<- "Man"

Man_df <- Man %>% as.data.frame() %>%
  mutate( ofe = 
            ifelse(grepl("_1",Man),1,3))

man_code <- c("f1","f2","g1","g3","h1","h3","s1","s3","wbp_mt1","wbp_mt3","wbf_ct1",
              "wbf_ct3","wbf_mt1","wbf_mt3","wbf_nt1","wbf_nt3", "wbp_ct1", "wbp_ct3", 
              "wbp_nt1", "wbp_nt3", "wf_ct1", "wf_ct3", "wf_mt1", "wf_mt3", "wf_nt1", "wf_nt3")
Man_df$code<-man_code
slope_man<-merge (Slope_df, Man_df, by="ofe")

inputs <- merge(slope_man,soil_cli_df)


write.csv(inputs,"processed_data/inputs")
head(inputs)

#inputs<-read.csv("processed_data/inputs")

