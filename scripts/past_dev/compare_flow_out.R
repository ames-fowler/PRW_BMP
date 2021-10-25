9_s_25_s_no_19_0 

path<- "processed_data/batch/scratch"#
# get files- name files
getwd()

## setup for no buffer. error on incomplete files. 
loss_files <- as.data.table(list.files(path,pattern = "9_s_25",full.names = F))
loss_files <- loss_files[grepl(("g_no_18_|wbf_ct_18_"), loss_files$V1)]
                                                                                
wat_file <-  loss_files[grepl(("wat"), loss_files$V1)]

wat1 <-  fread(paste0(path,"/",as.character(wat_file[1])), header = F , skip=23,
               na.strings ="", stringsAsFactors= F)
names(wat1) <-  c("OFE","J","Y","P","RM","Q","Ep","Es","Er","Dp","UpStrmQ","SubRIn","latqcc","Total-Soil","frozwt","Snow-Water","QOFE","Tile","Irr","Area") 


wat2 <-  fread(paste0(path,"/",as.character(wat_file[2])), header = F , skip=23,
               na.strings ="", stringsAsFactors= F)
names(wat2) <-  c("OFE","J","Y","P","RM","Q","Ep","Es","Er","Dp","UpStrmQ","SubRIn","latqcc","Total-Soil","frozwt","Snow-Water","QOFE","Tile","Irr","Area") 


plot(wat1$`Snow-Water`[wat1$Y==3&wat1$OFE==17])  # grass
points((wat2$`Snow-Water`[wat2$Y==3&wat2$OFE==17]), col= "blue") # wbf
wat1[,date:= paste0(Y,"_",J)][, name := wat_file[2]]
wat2[,date:= paste0(Y,"_",J)][, name := wat_file[2]]

wats <- merge(wat1, wat2)

plot(wat2[Y==5& J == 46]$, col="blue")
points(wat1[Y==5& J == 46]$UpStrmQ, col="green")

plot(wat2[Y==5& J == 46]$Q, col="blue")
points(wat1[Y==5& J == 46]$Q, col="green")

plot(wat1[Y==5& J == 40]$`Total-Soil`, col="green")
points(wat2[Y==5& J == 40]$`Total-Soil`, col="blue")
plot(wat1[Y==5& J == 41]$`Total-Soil`, col="green")
points(wat2[Y==5& J == 41]$`Total-Soil`, col="blue")
plot(wat1[Y==5& J == 42]$`Total-Soil`, col="green")
points(wat2[Y==5& J == 42]$`Total-Soil`, col="blue")
plot(wat1[Y==5& J == 43]$`Total-Soil`, col="green")
points(wat2[Y==5& J == 43]$`Total-Soil`, col="blue")
plot(wat1[Y==5& J == 44]$`Total-Soil`, col="green")
points(wat2[Y==5& J == 44]$`Total-Soil`, col="blue")
plot(wat1[Y==5& J == 45]$`Total-Soil`, col="green")
points(wat2[Y==5& J == 45]$`Total-Soil`, col="blue")


