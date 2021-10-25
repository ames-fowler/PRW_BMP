
## TURN WEPP OUTOUTS INTO DATAbase of PROFILES FOR EACH HILLSLOPE
## Author Ames Fowler
## contact ames.fowler@wsu.edu
## Date: 1-15-2018
#############################################


rm(list = ls())
graphics.off()


library(data.table)
library(readr)
library(rstudioapi) # set working dir automatically
library(lubridate)  # working with date
library(magrittr)   # %>% & %<>%
library(tidyverse)  # purrr package and nested data frame
library(ggpmisc)    # for labeling linear regression plots
library(scales)     # for date_breaks and date_format
library(gridExtra)  # for combining individual plots
library(tidyr)
library(stringr)


#Functions: 
Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

substrRight <- function(x, n){
  substr(x, n, nchar(x))
}
substrLeft <- function(x, n){
  substr(x, 1, nchar(x)-n+1)
}
#### simulation set by slope shape ---- Concave and convex
#path<- "C:/Users/ames.fowler/Desktop/BMP_batch_workingspace/scratch" #"processed_data/batch/scratch"#
path <- "processed_data/batch/scratch"

# get files- name files
getwd()

## setup for no buffer. error on incomplete files. 
loss_files <- as.data.table(list.files(path,pattern = "*_loss",full.names = F))
#slope_files<-as.data.frame(list.files(path = "slope",pattern = "*.slp",full.names = F))
wat_files <- as.data.table(list.files(path,pattern = "*_wat.txt",full.names = F))

#subset by new files, comment out for new run 20-12-07  ------------------------------------------------------
# 
# loss_files <- loss_files %>%
#   mutate(mtime =  file.info(dir(path,pattern = "*_0_loss", full.names = TRUE), extra_cols = FALSE)[,"mtime"] %>%
#            as.Date())%>% subset(mtime > as.Date("2020-10-25"))
# loss_files <-loss_files[,"V1"]
# 
# wat_files <- wat_files %>%
#   mutate(mtime =  file.info(dir(path,pattern = "*_0_wat", full.names = TRUE), extra_cols = FALSE)[,"mtime"] %>%
#            as.Date())%>%
#   subset(mtime > as.Date("2020-10-25"))
# wat_files <-wat_files[,"V1"]

# get ids --------------------------------------------------
colnames(loss_files)<-"fullname"

#colnames(slope_files)<-"fullname"
colnames(wat_files)<-"fullname"

#isolatte wepp ID number
loss_files$ID <- mapply(x=as.character(loss_files$fullname),function(x) substrLeft(x,10))%>%
  unname()
#slope_files$number <- sapply((strsplit(as.character(substr(slope_files$fullname,2,length(slope_files$fullname))) , "[.]")),"[" , 1)
wat_files$ID <- mapply(x=as.character(wat_files$fullname),function(x) substrLeft(x,9))%>%
  unname() 


### test very high NT values ----
# sum_dat_max$NT.max from man_3_plot.R
nt_pattern <- sum_dat_max$NT.max[order( `Errosion_kg/m2`, decreasing = T)]$nt %>% head(25) %>% unique

loss_files <- loss_files[ID %in% nt_pattern]
wat_files <- wat_files[ID %in% nt_pattern]

####file.names <- dir(path, pattern =".txt") -------------
#path.slope<-'C:/Users/PETBUser/Desktop/TK_WEPP/Twshed_fixed_dem/slope'
# Para_error_list <- fread("processed_data/batch/paradise_error_list_201014.txt", header = F)
# Para_error_list <- as.data.table(Para_error_list$V1[(!grepl("wbf_no", Para_error_list$V1))]) ## remove eronious names
# Para_error_list <- as.data.table(Para_error_list$V1[(!grepl("_loss.txt_loss.txt", Para_error_list$V1))]) ## remove eronious names
# 


# loss_files <- Para_error_list
# loss_files$V2 <- paste0(loss_files[,V1],"_loss.txt")
# names(loss_files) <- c("ID", "fullname")
# 
# wat_files <- as.data.table(loss_files[,ID])
# wat_files$V2 <- paste0(wat_files[,V1],"_wat.txt")
# names(wat_files) <- c("ID", "fullname")

################# make this a function ##########################

ken_shock_files<-list()
#pull files hill slope by type
#p=376
skip<-list()
j<-1
p <- 1670  
loss_files$ID[p]
rm(dataset)

for(p in 1:NROW(loss_files)){
  data <- as.data.frame(read_lines(paste0(path,"/",as.character(loss_files$ID[p]),"_loss.txt")),stringsAsFactors=F)
  #slope <- as.data.frame(read_lines(paste("slope/",as.character(slope_files$fullname[p]),sep="")),stringsAsFactors=F)
  wat <- fread(paste0(path,"/",as.character(loss_files$ID[p]),"_wat.txt"), header = F , skip=23,
                    na.strings ="", stringsAsFactors= F)
  
  #________________________GET_30_yr_hydro_components___________________________________________________________________#
  names(wat) = c("OFE","J","Y","P","RM","Q","Ep","Es","Er","Dp","UpStrmQ","SubRIn","latqcc","Total-Soil","frozwt","Snow-Water","QOFE","Tile","Irr","Area") 
  
  #sum to years then average over 30yrs 
  wat_yr <- wat[, lapply(.SD, sum), by=c("OFE","Y")][
    , lapply(.SD, mean), by=c("OFE")][,ET:=Ep+Es+Er][
      ,c("OFE","RM","Q","ET","Dp","UpStrmQ","SubRIn","latqcc","QOFE")]

  #_______________Get_30_yr_erosion_for_each_OFE________________________________________________________________________#
  c <- which(data[,1]=="  C.  SOIL LOSS/DEPOSITION ALONG SLOPE PROFILE")+10
  n <- which(data[,1]=="note:  (+) soil loss - detachment     (-) soil loss - deposition")-2

  data_ag <- ((data[c:n,1]))
 
  data_ag <- data_ag %>% strsplit(., " ") %>%  unlist() %>% gsub("*********","-1000", ., fixed = T) %>% 
    as.numeric() %>% na.omit() %>% matrix(ncol =3, byrow = T) %>% as.data.table()
  data_ag <- data_ag[order(V1)]
  setnames(data_ag, c("distance","Errosion_kg/m2","OFE"))
  
  #_ fix the kinematic shock 20-10-21-----------------------------------------------------------------------
  temp <- (lead(data_ag$`Errosion_kg/m2`)-(data_ag$`Errosion_kg/m2`)) %>% replace_na(.,0)
  temp <- temp/ifelse(sd(temp)==0,.01,sd(temp))
  temp_flag <- sum(temp>2)
  
  if(temp_flag>0){
    ken_shock_files <- list(ken_shock_files,loss_files$ID[p])
    replace <- ((lag(data_ag$`Errosion_kg/m2`,1)+ lead(data_ag$`Errosion_kg/m2`,1))/2) %>% replace_na(0) # does this need to be wider 
    data_ag$`Errosion_kg/m2` <- ifelse(temp>2 , replace, data_ag$`Errosion_kg/m2`)
    }
  
  data_ag <- data_ag[,.(`Errosion_kg/m2`= mean(`Errosion_kg/m2`)),by=OFE]

  #_Get_Runoff_and_lateral_flow_of_each_point_(OFE+1)___---------------------------------------
  profile <- merge(data_ag, wat_yr)%>%
    mutate(ID = loss_files$ID[p])
  
  #_output _____________________________________________________________________________________________________#
  # path_out<- paste0("./processed_data/batch/profile/dataset.txt")
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- profile
    dataset <- list(dataset)
    i <- 2
    
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <- profile
    dataset[[i]] <- temp_dataset
    i <- i+1
    rm(temp_dataset)
  }
  
}
#View(dataset)
dataset <- rbindlist(dataset)
ken_out <- unlist(ken_shock_files)
test <- mapply(x = ken_out, function(x) strsplit(x, split = "_"))
test2 <- do.call(rbind.data.frame,test)
names(test2) <- c("cli", "soil","slope", "man", "till", "ofe", "buf")



### change the name here ----------------
# fwrite(dataset,"processed_data/batch/para_errorfix2_201014",
#        quote=FALSE, row.names = FALSE, col.names = TRUE)


fwrite(dataset,("processed_data/batch/210714_PRW_convex_ct"),
       quote=FALSE, row.names = FALSE, col.names = TRUE)

fwrite(list(ken_out),("processed_data/batch/210714_Ken_shock_convex_ct"),
       quote=FALSE, row.names = FALSE, col.names = TRUE)
fwrite(list(test2),("processed_data/batch/2107104_Ken_shock_df_convex_ct"),
       quote=FALSE, row.names = FALSE, col.names = TRUE)


rm(dataset)
#dataset <- fread("processed_data/batch/200930_PRW_MASTER_LIST", header=T)
# dataset <- as.data.table(dataset)

### get total hillslope sed 20.12.08 --------------------------------
loss_files <- as.data.table(list.files(path,pattern = "*_0_loss",full.names = T))


x <- loss_files %>% unlist()
wepp_loss_year_avg <- function(x){
  lines <- readLines((as.character(x)))
  spot <- which(lines == "     A.  AVERAGE ANNUAL SEDIMENT LEAVING PROFILE")
  line <- lines[c(spot+3)]
  numbers <- as.numeric(unlist(regmatches(line, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",line))))
  
  if(length(numbers) != 2){print (x)}
  
  return(numbers)
}
# 
# #file_df$.
wepp_dat <- loss_files$V1 %>% map(.x =., .f = ~wepp_loss_year_avg(.x)) %>% 
  unlist() %>% matrix(., ncol=2,byrow = T)


file_df <- as.data.table(loss_files) %>% mutate(Erosion_T_ha1 = unlist(wepp_dat[,1]),
                                                area_ha = unlist(wepp_dat[,2]),
                                                ID = V1 %>% substrLeft(.,10) %>% substrRight(.,30))
file_df <- file_df[,c("Erosion_T_ha1","area_ha", "ID")]
#dataset_test <- merge(dataset, file_df)
fwrite(file_df, "processed_data/batch/210714_sed_ct_convex.txt")




