#WEPP Soil file Modification 
#
#author: Ames Fowler
#emails: ames.fowler@wdu.edu
#Date: 11/6/17
#

files<-as.data.frame(list.files(pattern = "*.sol",full.names = F))
colnames(files)<-"fullname"
?strsplit
str(files)
#files$type <- sapply((strsplit(as.character(files$fullname) , "[.]")),"[" , 2)
files$type<- "sl"
files$name <- sapply((strsplit(as.character(files$fullname) , "[.]")),"[" , 1)

#file.names <- dir(path, pattern =".txt")
i=1
for(i in 1:NROW(files)){
  #file <- read.table(file.names[i],header=TRUE, sep=";", stringsAsFactors=FALSE)
  #out.file <- rbind(out.file, file)
  a<-read.delim2(as.character(files$fullname[i]), header =FALSE, sep="\n",blank.lines.skip=T)
  colnames(a)<-"lines"
  a <- as.list(a)
  x <- which(grepl("Any comments:", a$lines))   #This key is used to generate the breaks in text to be modified. 
  b<-a$lines[1:x]
  c<-a$lines[(x+1)]
  d<-a$lines[(x+2):(NROW(a$lines)-1)]
  e<-as.factor(substr(a$lines[(NROW(a$lines))],0,1))
  f<-as.factor(substr(a$lines[(NROW(a$lines))],3,nchar(as.character(a$lines[(NROW(a$lines))]))))
  g<-unlist(list(d,e,f))
  for (j in 1:19) {
     
    c<- as.factor(paste(j," 1",sep = ""))
    temp <- list(b,c,rep(g, j))
    temp <- unlist(temp)
    write.table(temp,paste0(files$name[i],'_',j,'.',files$type[i]),quote=FALSE, row.names = FALSE, col.names = FALSE)
    }
}


