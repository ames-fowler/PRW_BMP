library(data.table)
library(stringr)
library(dplyr)
library(purrr)
library(ggplot2)

###climate compare


path <- "processed_data/cli"
climate_files <- list.files(path = path, pattern = "cli_zone*")

data <- map(cliamte_files, function(x) fread(paste(path,x,sep="/"),skip=16))
heading <- readLines(paste(path,cliamte_files[1],sep="/"))[14] %>%
  str_split(" ") %>% lapply(function(x){x[!x ==""]})
  

data_merge <- rbindlist(data, use.names=T, idcol=T)
names(data_merge) <- c("id",unlist(heading))
str(data)

data_merge[data_merge$id==2]$id <- 11
data_merge[data_merge$id==1]$id <- 2
data_merge$id <- data_merge$id-1

data_merge$id[]

ggplot(data_merge) + 
  geom_bar(aes(id,y=prcp, fill=id),
           position = "dodge", stat = "summary", fun.y = "max")

p <- ggplot(data_merge) + 
  geom_bar(aes(id,y=prcp, group=id),
          stat = "summary", fun.y = "max")+
  theme_minimal()+
  labs(x="Climate Quantile", y= "Maximum precipitation")
p

#### need to make better plots. 
#### but this shows the mean precip hold the clear thrend but max prcip
#### is more various 
