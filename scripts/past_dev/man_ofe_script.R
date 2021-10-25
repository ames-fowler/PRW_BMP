#WEPP man file Modification 
#
#author: Ames Fowler
#emails: ames.fowler@edu.edu
#Date: 11/6/19
#

files<-as.data.frame(list.files(path ="processed_data/man/", pattern = "*_1.man",full.names = F))
colnames(files)<-"fullname"
#?strsplit
str(files)
#files$type <- sapply((strsplit(as.character(files$fullname) , "[.]")),"[" , 2)
files$type <- "man"
files$name <- sapply((strsplit(as.character(files$fullname) , "[.]")),"[" , 1)

#file.names <- dir(path, pattern =".txt")
i=1
for(i in 1:NROW(files)){
  #file <- read.table(file.names[i],header=TRUE, sep=";", stringsAsFactors=FALSE)
  #out.file <- rbind(out.file, file)
  a <- readLines(paste("processed_data/man/", as.character(files$fullname[i]), sep = ""))
  x <- which(grepl("number of OFE's", a))   #This key is used to generate the breaks in text to be modified. 
  y <- which(grepl("# initial condition index", a))
  z <- which(grepl("# rotation repeats", a))
  # 
  b <- a[1:x[2]] # first bits 
  c <- a[x[2]+1] # intial contiditions X#ofe
  d <- a[x[2]+2] %>% 
    substr(1,2) %>%
    as.numeric() # number of rotations 
  e <- a[x[2]+3] %>%
    substr(1,1) %>% 
    as.numeric() # years per rotation (d X e = length)
  
  g1 <- a[(x[2]+4):(x[2]+5)] 
  g2 <- a[(x[2]+6)] 
  g3 <- a[(x[2]+7):(x[2]+8)] 
  # Need a loop for number of rotation
  # and for number of ofe - nested loop     
  
  # g<-unlist(list(d,e,f))
  for (i in 1:19){
    substr(b[x[1]], 1, 2) <- as.character(i)   ### consistent
    substr(b[x[2]], 1, 2) <- as.character(i)   ### consistent
    
   j=1
########## worked it up to here. below I am rebuilding the 
   # rotation: rep by number of OFE: Watch the left hand number  
    for (j in 1: d){
      g2 <- paste0("# Rotation ",j,": year ",j*e-(e-1)," to ",j*e)
      test <- c(g1,g2,g3)
    }
      b_list <-c(b,rep(a[y[1]],i,a[z:(z+2)]))
    
    
    ###worked up to here#############################
    
    substr(a[278],1,2) <- as.character(i) ### not consistent
    Alist <- c(A,rep(A1,i),A2)
    temp_B2 <- strsplit(B[2],"1",1)
    
    B_list <- list()
    
    OFE_list <- list()
    for(j in seq(1,30,vv)){
      
      temp_B <- B
      temp_B[2] <- "# Rotation"
      
      substr(temp_B[2],1,10) <- temp_B[2]
      temp_B[2] <- paste(temp_B[2],(j+2)/3,": year",j, "to",j+2, sep=" ")
      
      C_list<-list()
      E_list<-list()
      F_list<-list()
      for(k in 1:i){
        temp_C<-C
        temp_C[2]<-paste(substr(temp_C[2],1,28),as.character(k),">")
        temp_C[2]
        C_list<-unlist(append(C_list,temp_C))
        
        temp_E<-E
        temp_E[1]<-paste(substr(temp_E[1],1,28),as.character(k),">")
        E_list<-unlist(append(E_list,temp_E)) ########################FIX THE EXTRA LINE
        
        temp_F<-F
        temp_F[1]<-paste(substr(temp_F[1],1,28),as.character(k),">")
        F_list<-unlist(append(F_list,temp_F))
        
        OFE_list_temp<-c(temp_B,C_list,D,E_list[1:length(E_list)-1],D,F_list)
      }
      OFE_list<-unlist(append(OFE_list,OFE_list_temp))
      #print(temp_B[2])
      #B_list<-append(B_list,list(temp_B))
      
    }
}


