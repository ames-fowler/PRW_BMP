#WEPP Soil file Modification 
#
#author: Ames Fowler
#emails: ames.fowler@wdu.edu
#Date: 11/6/17 mod 8/13/20
#

library(tidyverse)

#This file build soil files for multiple ofes and for calibration/sensitivity testing
#it was written in 2017 in base R with modifications with dplyr syntax
# it was written to run within an R project and will need redefinition of paths 
path <- "processed_data/soil/"

## read in all *.sol files from wepp cloud and build names  -----------------
files<-as.data.frame(list.files(path = path, pattern = "*.sol",full.names = F))
colnames(files)<-"fullname"

files$type <- "sl"
files$name <- sapply((strsplit(as.character(files$fullname) , "[.]")),"[" , 1)

## loop over soil files(1 ofe), read, extract, rebuild (x ofes) --------------------------------
i=1
for(i in 1:NROW(files)){
  #read soil file as lines to temp var a
  a<-read.delim2(paste("processed_data/soil/", as.character(files$fullname[i]), sep = ""), 
                 header =FALSE, sep="\n",blank.lines.skip=T)
  colnames(a)<-"lines"
  a <- as.list(a)
  # find the the line before data start; key  used to generate the breaks in text to be modified. 
  x <- which(grepl("Any comments:", a$lines))   
  
  b<-a$lines[1:x]       # heading and comment to be preserved, could improve by checking line 52 
  c<-a$lines[(x+1)]     # number ofe, wepp mod ksat 
  d<-a$lines[(x+2)]     # surface conditions 
                        # name, 	texture, 	horizons, 	albedo, 	intial sat[%], 	
                        # interrill erod [kgs/m4], 	Rill erode []s/m	critcal shear[Pa], 	
                        # eff K 0.000 = modle calc. 
  
  e <- a$lines[(x+3):(NROW(a$lines)-1)] 
  f <- a$lines[(NROW(a$lines))]
  
  e_base <- (strsplit(e, "\t")) %>%  # list of text to dataframe
    lapply(as.numeric) %>% unlist()%>% 
    matrix(nrow= 12) %>% t() %>% as.data.frame() 
  
  colnames(e_base) <- c("na","depth","Pd",	"hyrcond_mm/hr", "anistorphy",	"pwp",
                        "fc",		"sand",	"clay","OM",	"CEC",	"rockfrag")
  
  
  #brute force calibration loop, we can get much smarter about this but... this is a start. 
  for(k in seq(-.5, .5, .1)){
  
    e_base <- e_base %>% mutate(hydrcond_ma = hydrcond_ma*k)
    e  <- table(e_base) 
    g <- unlist(list(d,e,f))
    
    x=1
    # optional ofe loop buils out soil files for 1-x ofes, set x to 1 to avoid adding ofes 
    for (j in 1:x) {
      #j=3
      c<- as.factor(paste(j," 1",sep = ""))
      temp <- unlist(list(b,c,rep(g, j)))
      temp <- unlist(temp)
      write.table(temp,paste0(path,files$name[i],'_',j,'_',k,'.',files$type[i]),quote=FALSE, row.names = FALSE, col.names = FALSE)
     }
}


