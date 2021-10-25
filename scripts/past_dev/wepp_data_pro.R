
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
library(readr)
library(dplyr)

#install.packages("stringr", dependencies = TRUE)
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

loss_files<-as.data.frame(list.files(path,pattern = "*.loss",full.names = F))
#slope_files<-as.data.frame(list.files(path = "slope",pattern = "*.slp",full.names = F))
wat_files<-as.data.frame(list.files(path,pattern = "*wat.txt",full.names = F))

colnames(loss_files)<-"fullname"
#colnames(slope_files)<-"fullname"
colnames(wat_files)<-"fullname"
str(loss_files)

#isolatte wepp ID number
#files$type <- sapply((strsplit(as.character(files$fullname) , "[.]")),"[" , 2)
file_id<- mapply(x=as.character(loss_files$fullname),function(x) substrLeft(x,10))%>%
  unname() %>% as.data.frame()
colnames(file_id) <- "ID"

str(file_id)

loss_files$ID <- as.character(file_id$ID)
#slope_files$number <- sapply((strsplit(as.character(substr(slope_files$fullname,2,length(slope_files$fullname))) , "[.]")),"[" , 1)
wat_files$ID <- file_id

#file.names <- dir(path, pattern =".txt")
#path.slope<-'C:/Users/PETBUser/Desktop/TK_WEPP/Twshed_fixed_dem/slope'

dud_list<-list()
#pull files hill slope by type
#p=376
skip<-list()
j<-1
p<-515
for(p in 1:NROW(loss_files)){
  data <- as.data.frame(read_lines(paste(path,"/",as.character(loss_files$fullname[p]),sep="")),stringsAsFactors=F)
  #slope <- as.data.frame(read_lines(paste("slope/",as.character(slope_files$fullname[p]),sep="")),stringsAsFactors=F)
  wat <- read.table(paste(path,"/",as.character(wat_files$fullname[p]),sep=""), sep = "" , header = F , skip=23,
                    na.strings ="", stringsAsFactors= F)
  names(wat) = c("OFE","J","Y","P","RM","Q","Ep","Es","Er","Dp","UpStrmQ","SubRIn","latqcc","Total-Soil","frozwt","Snow-Water","QOFE","Tile","Irr","Area") 
  
  
  #_______________Get_Slopes_for_each_OFE_______________________________________________________________________________#
  # ofe<-as.numeric(slope[6,1])
  # widthtemp<-(strsplit(slope[7,1]," "))
  # width<-as.numeric(widthtemp[[1]][2])
  # OFE_Length_temp<-(strsplit(slope[10,1]," "))
  # OFE_Length<-as.numeric(OFE_Length_temp[[1]][2])
  # f=list()
  # s=1
  # 
  # for (s in 1: ofe){
  #   f[[s]]<-(t(as.numeric(Numextract(slope[7+s*2,1]))))
  # }
  # 
  # f <- do.call(rbind, f)
  # slopes <-((f[,2]+f[,4])/2) 
  # area<-OFE_Length*width
  #_______________Get_30_yr_erosion_for_each_OFE________________________________________________________________________#
  c<-which(data[,1]=="  C.  SOIL LOSS/DEPOSITION ALONG SLOPE PROFILE")+10
  n<-which(data[,1]=="note:  (+) soil loss - detachment     (-) soil loss - deposition")-2
  
  v<-c()
  temp<-data.frame()
  temp2<-data.frame()
  year_1<-data.frame()
  wat_list<-list()
  year1<-c()
  h<-list()
  #for (i in 1:31){
  #  v[i]<-as.data.frame((data[c[i]:n[i]]),stringsAsFactors=F)}
  i=1  
  for (i in 1:length(c)){
    v[i]<-as.data.frame((data[c[i]:n[i],1]),stringsAsFactors=F)
  }
  
  #t<-as.data.frame(v,stringsAsFactors=F,col.names=F)
  #if (length(c)<31){
  #  dud_list<-rbind(dud_list,files$fullname[p])
  #}
  #year_1<-as.data.frame(t(as.numeric(Numextract(t[1,1]))))
  l=1
  for(l in 1:length(c)){
    o<-0
    k=1
    for(k in 1:NROW(v[[length(c)]])){
      input1<-gsub("*********","-1000",v[[l]][[k]], fixed = T)
      temp<-as.data.frame(t(na.omit(as.numeric(unlist(c(strsplit(input1," ")))))))
      #afe 3/17 fortran error - refixed 3/17/18 
      if(NCOL(temp)==6){
        temp$V7=999
        temp$V8=999
        temp$V9=999
        o<-1
      }
      
      temp2<-rbind(temp2,temp)
    }
    #  ?na.omit
    #temp2<- na.omit(temp2,NA="99")  
    year1$Distance<- c(temp2$V1,temp2$V4,temp2$V7)
    year1$Erosion<- c(temp2$V2,temp2$V5,temp2$V8)
    year1$OFE<- c(temp2$V3,temp2$V6,temp2$V9)
    h[[l]]<-year1
    year1<-c()
    temp2<-data.frame()
  }
  
  y31=as.data.frame(h[[1]])
  colnames(y31)<-c("distance_m", "erosion_kgm2","OFE")

  
  y31_mean <- y31 %>% group_by(OFE)%>% summarise_at(vars(erosion_kgm2,distance_m),funs(mean,max))#erosion_WW,erosion_B,erosion_F
  if(o==1){
    y31_mean<-y31_mean[1:(NROW(y31_mean)-1),]
  }
  
  #_______________Get_Runoff_and_lateral_flow_of_each_point_(OFE+1)_____________________________________________________________________#
  wat_mean<- (wat %>% group_by(OFE)%>% summarise_at(vars(RM,Ep,Es,Dp,Q,UpStrmQ,latqcc),sum)/30)
   #?summarise_at
  y31_mean<-cbind(y31_mean,wat_mean[2:8])
  #_______________Get_elevations_of_each_point_(OFE+1)_____________________________________________________________________#
  # y31_mean$slope <- slopes
  # 
  # 
  # top<-data.frame(matrix(nrow = 1, ncol = 13,data=0))
  # names(top)<-names(y31_mean)
  # 
  # y31_mean <- arrange(as.data.frame(rbind(y31_mean, top)),OFE)
  # e<-c(0)
  # #s=2
  # if (ofe>1){
  #   for (s in 2:(ofe+1)){
  #     e[s]<-e[s-1]+y31_mean$slope[s]*(y31_mean$distance_m_max[s]-y31_mean$distance_m_max[s-1])
  #   }
  # }else{ 
  #   e[2]<-y31_mean$slope[2]*y31_mean$distance_m_max[2]
  #   
  # }
  # y31_mean$elevation<-max(e)-e
  #mean(y31_mean$slope)
  #plot(y31_mean$elevation)
  # magnitude ---------------------------------------------------------------
  # magnitude<-as.data.frame(y31_mean$OFE)
  # names(magnitude)<-c("OFE")
  # magnitude$slope<- y31_mean$slope
  # magnitude$elevation<-y31_mean$elevati
  # magnitude$erosion_kg<-y31_mean$erosion_kgm2_mean*area
  # magnitude$RM<-y31_mean$RM*area
  # magnitude$Ep<-y31_mean$Ep*area
  # magnitude$Es<-y31_mean$Es*area
  # magnitude$Dp<-y31_mean$Dp*area 
  # magnitude$Q<-y31_mean$Q*area
  # magnitude$UpQ<-y31_mean$UpStrmQ*area
  # magnitude$lat<-y31_mean$latqcc*area
  # #_______________output _____________________________________________________________________________________________________#
  path_out<- paste(paste("processed_data/profile",loss_files$ID[p],sep='/'),".txt",sep="")
  # path_out_mag<- paste(paste(getwd(),"mag",slope_files$number[p],sep='/'),".txt",sep="")
  #?paste
  f <- file(path_out, open="wb")
  write.table((y31_mean),file=f,quote=FALSE, row.names = FALSE, col.names = T, eol="\n",fileEncoding = "UTF-8")
  # write.table((magnitude),file=f,quote=FALSE, row.names = FALSE, col.names = T, eol="\n",fileEncoding = "UTF-8")
  
  close(f)
  
}





