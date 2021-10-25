
## TURN WEPP OUTOUTS INTO PROFILES FOR EACH HILLSLOPE
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
  substr(x, nchar(x)-n+1, nchar(x))
}
substrLeft <- function(x, n){
  substr(x, 1, nchar(x)-n+1)
}

path<-"processed_data/scratch"
# get files- name files

loss_files<-as.data.table(list.files(path,pattern = "*.loss",full.names = F))
#slope_files<-as.data.frame(list.files(path = "slope",pattern = "*.slp",full.names = F))
wat_files<-as.data.table(list.files(path,pattern = "*wat.txt",full.names = F))

colnames(loss_files)<-"fullname"
#colnames(slope_files)<-"fullname"
colnames(wat_files)<-"fullname"

#isolatte wepp ID number
#files$type <- sapply((strsplit(as.character(files$fullname) , "[.]")),"[" , 2)
file_id<- mapply(x=as.character(loss_files$fullname),function(x) substrLeft(x,10))%>%
  unname() %>% as.data.frame()
colnames(file_id) <- "ID"

loss_files$ID <- as.character(file_id$ID)
#slope_files$number <- sapply((strsplit(as.character(substr(slope_files$fullname,2,length(slope_files$fullname))) , "[.]")),"[" , 1)
wat_files$ID <- file_id

#file.names <- dir(path, pattern =".txt")
#path.slope<-'C:/Users/PETBUser/Desktop/TK_WEPP/Twshed_fixed_dem/slope'

################# make this a function ##########################

dud_list<-list()
#pull files hill slope by type
#p=376
skip<-list()
j<-1
p<-515
for(p in 1:NROW(loss_files)){
  data <- as.data.frame(read_lines(paste(path,"/",as.character(loss_files$fullname[p]),sep="")),stringsAsFactors=F)
  #slope <- as.data.frame(read_lines(paste("slope/",as.character(slope_files$fullname[p]),sep="")),stringsAsFactors=F)
  wat <- fread(paste(path,"/",as.character(wat_files$fullname[p]),sep=""), header = F , skip=23,
                    na.strings ="", stringsAsFactors= F)
  
  #________________________GET_30_yr_hydro_components___________________________________________________________________#
  names(wat) = c("OFE","J","Y","P","RM","Q","Ep","Es","Er","Dp","UpStrmQ","SubRIn","latqcc","Total-Soil","frozwt","Snow-Water","QOFE","Tile","Irr","Area") 
  
  #sum to years then average over 30yrs 
  wat_yr <- wat[, lapply(.SD, sum), by=c("OFE","Y")][
    , lapply(.SD, mean), by=c("OFE")][,ET:=Ep+Es+Er][
      ,c("OFE","RM","Q","ET","Dp","UpStrmQ","SubRIn","latqcc")]
  
  wat_flow <- wat[OFE=="3",c("J","Y","Q", "UpStrmQ","latqcc")]

  #_______________Get_30_yr_erosion_for_each_OFE________________________________________________________________________#
  c<-which(data[,1]=="  C.  SOIL LOSS/DEPOSITION ALONG SLOPE PROFILE")+10
  n<-which(data[,1]=="note:  (+) soil loss - detachment     (-) soil loss - deposition")-2

  data_ag <- ((data[c[i]:n[i],1]))

  
  as.data.table(matrix(unlist(lapply(test, function(x) na.omit(as.numeric(unlist(strsplit(gsub("*********","-1000", x, fixed = T)," ")))))),ncol=3, byrow=T))
 
   data_ag <- as.data.table(matrix(unlist(lapply(data_ag, function(x) na.omit(as.numeric(unlist(strsplit(
     (gsub("*********","-1000", x, fixed = T))," ")))))),ncol=3, byrow=T))[order(V1)][
      ,lapply(.SD,mean),by=V3]
   
  setnames(data_ag, c("OFE","distance","Errosion_kg/m2"))
  
  #_______________Get_Runoff_and_lateral_flow_of_each_point_(OFE+1)_____________________________________________________________________#
  profile <- merge(data_ag, wat_yr)      

  ################print profiles? or just make the big table with some ID??
  
  

    # #_______________output _____________________________________________________________________________________________________#
  path_out<- paste(paste("processed_data/profile",loss_files$ID[p],sep='/'),".txt",sep="")
  
  f <- file(path_out, open="wb")
  write.table((y31_mean),file=f,quote=FALSE, row.names = FALSE, col.names = T, eol="\n",fileEncoding = "UTF-8")
  
  close(f)
  
}





