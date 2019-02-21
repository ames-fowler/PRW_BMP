
a <- list("m","y","1","1","y")
c <- list("1","n")
e <- "y"
g <- list("n","n","n","n","n","n","n","n","n")
i <- list(0,30,0)


j=4
for (j in 1:length(inputs$ofe)) {
  file_name<-paste0(inputs$cli[j],"_",str_extract(inputs$soil_depth[j], "^.{1}"),"_",
                    strsplit(as.character(inputs$slope[j]),split="[.]")[[1]][1],
                    "_",inputs$code[j])
  b <- paste0("scratch/",file_name,"_pass.txt")
  d <- paste0("scratch/",file_name,"_loss.txt")
  f <- paste0("scratch/",file_name,"_wat.txt")
  h <- list(paste0("man/",inputs$Man[j]),
            paste0("slope/",inputs$slope[j]),
            paste0("cli/",inputs$cli_file[j]),
            paste0("soil/",inputs$soil[j]))
  temp <- list(a,b,c,d,e,f,g,h,i)
  temp <- unlist(temp, recursive = TRUE)
  
  write.table(temp,paste0("processed_data/run/",file_name,'.run'),quote=FALSE, row.names = FALSE, col.names = FALSE)
}  


#Write a batch run script

run_files<-as.data.frame(list.files(path ="processed_data/run/", pattern = "*.run",full.names = F))
colnames(run_files)<-"fullname"
#files$type <- sapply((strsplit(as.character(files$fullname) , "[.]")),"[" , 2)
run_files$name <- sapply((strsplit(as.character(run_files$fullname) , "[.]")),"[" , 1)


l<-"#!/usr/bin/perl"
n<- c("print `rm *_wat.txt`;",
"print `rm *_pass.txt`;",
"print `rm *_loss.txt`;")

i=3
m<-list()
r<-list()
for (i in 1:NROW(run_files)){
  k<- list(paste0("print `./wepp1000.exe <./run/", run_files$fullname[i],">> error_1_300.err\n`;"), "print `rm ds*.txt\n`;")
  r<-(list(r,k))
}
m <- list(l,r,n)

r2<- unlist(r)

j1 <- length(r2)/4
j2 <- length(r2)/2
j3 <- length(r2)*3/4
j4 <- length(r2)

m1 <- r2[1:j1]
m2 <- r2[(j1+1):j2]
m3 <- r2[(j2+1):j3]
m4 <- r2[(j3+1):j4]
m5 <- r2[grep(pattern = "_300_", x=r2)] 
m6 <- r2[grep(pattern = "_wbf_mt1", x=r2)]

o <- unlist(m)
o1 <- c(l,m1,n)
o2 <- c(l,m2,n)
o3 <- c(l,m3,n)
o4 <- c(l,m4,n)
o5 <- c(l,m5,n)
o6 <- c(l,m6,n)

write.table(unlist(o),"processed_data/PRW.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

#mannual parrellel - 4X the speed.
write.table(o1,"processed_data/PRW1.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o2,"processed_data/PRW2.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o3,"processed_data/PRW3.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o4,"processed_data/PRW4.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

# For all the 3 ofe runs after fixing the slope files mannually
write.table(o5,"processed_data/PRW5.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
# for all wbf_mt1 files that had an error
write.table(o6,"processed_data/PRW6.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

 