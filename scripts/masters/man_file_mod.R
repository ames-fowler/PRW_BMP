rm(list = ls())
graphics.off()

library(rstudioapi) # set working dir automatically
library(lubridate)  # working with date
library(magrittr)   # %>% & %<>%
library(tidyverse)  # purrr package and nested data frame
library(ggpmisc)    # for labeling linear regression plots
library(scales)     # for date_breaks and date_format
library(gridExtra)  # for combining individual plots
library(readr)
library(dplyr)
library(magrittr)

setwd("E:/F drive/TK_WEPP/Twshed/man")

lines<-readLines("E:/F drive/TK_WEPP/Twshed/man/ww_fallow_Low_Precip_ct _3.man")
head(lines)
lines
i=19
A <- lines[1:258]
B <- lines[259:261]
C <- lines[262:265]
D <- lines[274]
E <- lines[275:278]

for (i in 1:19){
  substr(A[7],1,2) <- as.character(i)
  substr(A[252],1,2) <- as.character(i)
  
  temp_B2 <- strsplit(B[2],"1",1)
  
  B_list <- list()
  
  OFE_list <- list()
  for(j in seq(from=1,to=30,by=2)){
    
    temp_B <- B
    temp_B[2] <- "# Rotation"
    
    substr(temp_B[2],1,10)<-temp_B[2]
    temp_B[2]<-paste(temp_B[2],(j+1)/2,": year",j, "to",j+1, sep=" ")
    
    C_list<-list()
    E_list<-list()
    for(k in 1:i){
      temp_C<-C
      temp_C[2]<-paste(substr(temp_C[2],1,28),as.character(k),">")
      temp_C[2]
      C_list<-unlist(append(C_list,temp_C))
      
      temp_E<-E
      temp_E[1]<-paste(substr(temp_E[1],1,28),as.character(k),">")
      E_list<-unlist(append(E_list,temp_E))
      
      OFE_list_temp<-c(temp_B,C_list,D,E_list)
      }
    OFE_list<-unlist(append(OFE_list,OFE_list_temp))
    #print(temp_B[2])
    #B_list<-append(B_list,list(temp_B))
    
  }
  
  out<-c(A,OFE_list)
  write.table(out,paste("ww_fallow_low_Precip_ct_",i,'.man',sep=""),
              quote=FALSE, row.names = FALSE, col.names = FALSE)
}

  