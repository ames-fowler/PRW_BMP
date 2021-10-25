## 
## Climate Checks
##
## AMES FOWLER
## 3/20/19
## 
library(dplyr)
library(readr)
library(tidyr)
Climates <- list.files(path = "WEPP_Climate", pattern = ".txt")%>%
  as.vector()

Climates<-sub(".txt","",Climates)%>%
  as
  separate(sep="_")

head(Climates)
names(Climates)<-c('names')

Climates<-sub(".txt","",Climates)
temp <- Climates%>%
  strsplit(Climates,split = "_")%>%
  as_tibble()



sapply(x=temp,unlist())


df<-Climates$names%>%
  transform(y=strsplit(Climates$names,split = "_"))%>%
  unnest(y)
  
names<-unnest(temp)
temp <- do.call(rbind,temp)%>%
  as.data.frame()

temp <- temp$V5%>%
  as.character()%>%
  strsplit(temp$V5,split = ".")

temp <- do.call(rbind,temp)%>%
  as.data.frame()

names<-temp$V2



df <- tibble(
   y = c("a", "d,e,f", "g,h")
)
df %>%
  transform(y = strsplit(y, ",")) %>%
  unnest(y)

for i in length(Climates){
 data <- read.csv(paste("WEPP_Climate/",Climates[[1]],sep=""),sep=T,skip=13)%>%
   as.data.frame()
 
 data<- read_table2(paste("WEPP_Climate/",Climates[[1]],sep=""), 
                                          skip = 13)
 
data$int<- data$prcp/data$dur
 
 
}

Climates[[2]]


as.data.frame(matrix(unlist(names), nrow=length(unlist(names[4]))))


lapply(x=names, as.data.frame())

?strsplit

head(names)
test  <- as.data.frame(names)

strsplit(Climates,split = "_")[[4]][5]


head(names)
as.data.frame(unlist(names))
test<-as.data.frame(do.call(rbind,names))
head(test)
