
#library(read.table)
library('stringr')

#??read_delim2

#wepp_bat <- read.delim2("C:/Users/PETBUser/Dropbox/Masters back up/TK_WEPP/Kwshed/wepp_bat.pl",header=F)



#str(files)
files<-as.data.frame(list.files(path="./run",pattern = "*.run",full.names = F,))
colnames(files)<-"name"
a<-"#!/usr/bin/perl"
b<-"print `./BrLF3.exe <./run/" 
c<-">> error_1_300.err\n`;"
d<-"print `rm ds*.txt\n`;"
e <- "print `WEPP_wtr_sed_no_wsed_output.pl\n`;
print `gully_buffer.pl\n`;
print `yield.pl\n`;
print `rm *_wat.txt`;
print `rm *_pass.txt`;
print `rm *_loss.txt`;
print `rm *_dist.txt`;
"
e
temp<-as.list(1)

for(i in 1:(NROW(files)-3)){
  temp[[i*2-1]]<- str_c(b,files$name[i],c,collapse=',')
  temp[[i*2]]<-d
  }

head(temp)

temp_out<- list(a,temp,e)
head(bat_out)
bat_out <- as.data.frame(unlist(temp_out))
str(bat_out)

f <- file("E:/TK_WEPP/Twshed/wepp_bat2.pl", open="wb")
write.table(print(bat_out),file=f,quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n",fileEncoding = "UTF-8")
close(f)
system("C:/wepp_bat2.pl") 
#?write_delim

