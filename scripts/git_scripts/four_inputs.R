

### turn all combos of HCT into a weep input file table ----

# o Load and clean soils files and soil groups ----  
soil<-as.data.frame(list.files(path ="processed_data/batch/soil/", pattern = "*.sl",full.names = F))
names(soil)<- "soil" 
###soil$soil[grep("68533", soil$soil)]

soil_df <- soil%>%as.data.table()%>%
  separate(soil,
    into= c("mapunit", "ofe"),
    sep = "_",
    remove = FALSE)

soil_df <- soil_df %>% 
  separate(ofe, c('ofe',NA))

soil_df <- soil_df %>% 
  mutate(mapunit = mapunit %>% as.numeric(),
         ofe     = ofe %>% as.numeric())%>%
  as.data.table()

#####soil_cli contains 68559 <- which need to change to #Garfield 68533 as #Garfield  is 35% but most abundant 

soil_groups <- fread('processed_data/soil_groups.txt')

soil_cli<-soil_groups[,c(1,2,4,12)] #row shift to get soil depth 11->12 2019/10/02
soil_cli$mapunit[soil_cli$mapunit==68559] <- c(68533)

# merge soil and cli info 
#soil_cli_df <- merge (soil_cli, soil_df, by = "mapunit")
temp_soil_cli_df <- merge(as.data.frame(soil_cli), as.data.frame(soil_df), by ="mapunit")

temp_soil_cli_df$cli_file<-paste0('cli_zone_', soil_cli$cli,'.cli')


#o load and clean slopes ------------ 

slope<-as.data.frame(list.files(path ="processed_data/batch/slope/s_slope/", pattern = "*.slp",full.names = F))
names(slope)<- "slope"
name = as.list(slope)
slope_df <- slope %>% as.data.table() %>%
  separate(slope,
           into =  c("type","ofe","slope","buffer"),
           sep = "_", 
           remove = FALSE)

slope_df <- slope_df %>% 
  separate(
    col = buffer,
    into = c('buffer',NA),
    sep = "[^[:alnum:]]+",remove = F) %>%
  mutate(ofe = ofe %>% as.numeric(),
         slope = slope %>% as.numeric(),
         buffer = buffer %>% as.numeric(),
         slope_file = unlist(name))%>% as.data.table()

temp_slope_df<- slope_df[buffer==0]


# o load and clean man ###------------

man<- as.data.table(list.files(path ="processed_data/batch/man/", pattern = "*.man",full.names = F))
names(man)<- "man"

man <- man %>% 
  mutate(ofe = lapply(man, function(y) gsub("\\D", "", y)) %>% 
           unlist() %>% 
           as.numeric() %>%
           replace_na(1))%>% as.data.table()



man <- separate(data= man, col = man, into = c("code",NA),sep = '\\.', remove = F) %>%
  mutate( code = substr(code,1,nchar(code)))

index<-c(grep("buff", man$man),grep("Buff", man$man))
temp_man <- man[-index,]

temp_man$practice <- 
  str_replace_all(temp_man$man, c("^.*ww_fallow_l.*$" = "wf","^.*ww_fallow_L.*$" = "wf",
                         "^.*ww_b_p.*$" = "wbp",
                         "^.*ww_brly_p.*$" = "wbp",
                         "^.*hermada.*$" = "h",
                         "^.*shrub.*$" = "s",
                         "^.*grass.*$" = "g",
                         "^.*forest.*$" = "f",
                         "^.*ww_barley_fallow.*$" = "wbf"))
temp_man$till <- 
  str_replace_all(temp_man$man, c("^.*ct.*$" = "ct","^.*mt.*$" = "mt",
                                  "^.*mulch.*$" = "mt",
                                  "^.*nt.*$" = "nt","^.*shr.*$" = "no",
                                  "^.*fal.*$" = "no","^.*her.*$" = "no",
                                  "^.*f.*$" = "no","^.*grass.*$" = "no"))


#merge and output inputs--

# merging by OFE builds replicates  

#slope_man<-merge (slope_df, man, by="ofe")
temp_slope_man <- merge(as.data.frame(temp_slope_df), as.data.frame(temp_man), by="ofe")
#inputs <- merge(slope_man,soil_cli_df)
input_temp <- merge(temp_slope_man,temp_soil_cli_df, by = "ofe")

fwrite(input_temp,"processed_data/batch/inputs_temp_21_07_05") #with hard coded fix of garfield, see note above. 
head(input_temp)

#inputs<-fread("processed_data/batch/inputs_temp_21_04_01")
#View(inputs)

