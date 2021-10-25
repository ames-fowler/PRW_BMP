library(stringr)
library(data.table)
library(dplyr)
library(tidyr)

#Inputs: from inputs.R for HCT now made for all possibilities 1-19 OFE.
##load inputs ------
inputs<-fread("processed_data/batch/inputs_temp_21_07_05") 

# write run files to run folder --------------------
#list the opening calls to the dos program 
a <- list("m","y","1","1","y")
c <- list("1","n")
e <- "y"
g <- list("n","n","n","n","n","n","n","n","n")
i <- list(0,30,0)


j = 6000
# o write striaght slopes no buffer----
for (j in 1:length(inputs$ofe)) {
  file_name <- paste0(inputs$cli[j],"_",str_extract(inputs$soil_depth[j], "^.{1}"),"_",
                      strsplit(as.character(inputs$slope[j]),split="[.]")[[1]][1],
                      "_",inputs$practice [j],"_",inputs$till[j],"_",inputs$ofe[j],"_",
                      inputs$buf[j])
  
  b <- paste0("scratch/s_slope/",file_name,"_pass.txt")
  d <- paste0("scratch/s_slope/",file_name,"_loss.txt")
  f <- paste0("scratch/s_slope/",file_name,"_wat.txt")
  h <- list(paste0("man/",inputs$man[j]),
            paste0("slope/s_slope/",inputs$slope_file[j]),
            paste0("cli/",inputs$cli_file[j]),
            paste0("soil/",inputs$soil[j]))
  temp <- list(a,b,c,d,e,f,g,h,i)
  temp <- unlist(temp, recursive = TRUE)
  
  fwrite(list(temp),paste0("processed_data/batch/run/s_slope/",file_name,'.run'),quote=FALSE, row.names = FALSE, col.names = FALSE)
}  

# o write convex slopes no buffer----

   ##could rewrite thsi as new column calls in a datatable, 
for (j in 1:length(inputs$ofe)) {
  file_name <- paste0(inputs$cli[j],"_",str_extract(inputs$soil_depth[j], "^.{1}"),"_",
                      strsplit(as.character(inputs$slope[j]),split="[.]")[[1]][1],
                      "_",inputs$practice [j],"_",inputs$till[j],"_",inputs$ofe[j],"_",
                      inputs$buf[j])
  
  b <- paste0("scratch/convex/",file_name,"_pass.txt")
  d <- paste0("scratch/convex/",file_name,"_loss.txt")
  f <- paste0("scratch/convex/",file_name,"_wat.txt")
  h <- list(paste0("man/",inputs$man[j]),
            paste0("slope/convex/",gsub(pattern = "scv", replacement = "conx",inputs$slope_file[j])),
            paste0("cli/",inputs$cli_file[j]),
            paste0("soil/",inputs$soil[j]))
  temp <- list(a,b,c,d,e,f,g,h,i)
  temp <- unlist(temp, recursive = TRUE)
  
  fwrite(list(temp),paste0("processed_data/batch/run/convex/",file_name,'.run'),quote=FALSE, row.names = FALSE, col.names = FALSE)
}  


## o write concave slopes run no buffer -----

for (j in 1:length(inputs$ofe)) {
  file_name <- paste0(inputs$cli[j],"_",str_extract(inputs$soil_depth[j], "^.{1}"),"_",
                      strsplit(as.character(inputs$slope[j]),split="[.]")[[1]][1],
                      "_",inputs$practice [j],"_",inputs$till[j],"_",inputs$ofe[j],"_",
                      inputs$buf[j])
  
  b <- paste0("scratch/concave/",file_name,"_pass.txt")
  d <- paste0("scratch/concave/",file_name,"_loss.txt")
  f <- paste0("scratch/concave/",file_name,"_wat.txt")
  h <- list(paste0("man/",inputs$man[j]),
            paste0("slope/concave/",gsub(pattern = "scv", replacement = "conv",inputs$slope_file[j])),
            paste0("cli/",inputs$cli_file[j]),
            paste0("soil/",inputs$soil[j]))
  temp <- list(a,b,c,d,e,f,g,h,i)
  temp <- unlist(temp, recursive = TRUE)
  
  fwrite(list(temp),paste0("processed_data/batch/run/concave/",file_name,'.run'),quote=FALSE, row.names = FALSE, col.names = FALSE)
}  
#toc()

# sub set run files grab currently run files. 
concave_run <- as.data.frame(list.files(path = "processed_data/batch/scratch/concave", 
                                        pattern = "*_loss",full.names = F)) 
concave_run <-  sapply((strsplit(as.character(concave_run %>% unlist) , "_loss")),"[" , 1) 
  
convex_run <- as.data.frame(list.files(path = "processed_data/batch/scratch/convex",
                                       pattern = "*.loss",full.names = F)) 
convex_run <-  sapply((strsplit(as.character(convex_run %>% unlist) , "_loss")),"[" , 1) 

#Write a batch run script ------------------------------
#


#o subset runfiles to fill hole, need to script out each slope shape -----------
#C:\Users\ames.fowler\Desktop\BMP_batch_workingspace\run #
path_scurve ='processed_data/batch/run/' #"processed_data/batch/run/"
path_concave ='processed_data/batch/run/' #"processed_data/batch/run/concave"
path_convex ='processed_data/batch/run/' #"processed_data/batch/run/convex"



run_files<-as.data.frame(list.files(path = path_concave, pattern = "*.run",full.names = F)) #processed_data/batch
colnames(run_files)<-"fullname"
#files$type <- sapply((strsplit(as.character(files$fullname) , "[.]")),"[" , 2)
run_files$name <- sapply((strsplit(as.character(run_files$fullname) , "[.]")),"[" , 1)

name_nums <- str_extract_all(run_files$name, "[[:digit:]]+", simplify = F) %>% 
  unlist  %>% as.numeric %>% matrix(ncol = 4, byrow = T)
run_files <- run_files %>% subset(grepl(pattern = "_g_|_f_|_ct_" , name)&
                                      name_nums[,4]==0&
                                      name_nums[,3]<16&
                                      name_nums[,3]>2&
                                      !(name%in%concave_run) )


# o build perl batch file for list of run files ------------------------
l<-"#!/usr/bin/perl"            #start
n<- c("print `rm *_wat.txt`;",  # end
      "print `rm *_pass.txt`;",
      "print `rm *_loss.txt`;")

i=3
m<-list()
r<-list()
for (i in 1:NROW(run_files)){
  k<- list(paste0("print `./wepp1000.exe <./run/concave/", run_files$fullname[i],">> error_1_300.err\n`;"))
  r<-(list(r,k))     ### each run
}



#o manually build the length of perl file =============

m <- list(l,r,n) # all runs packaged 

r2<- unlist(r) # all runs 


j1 <- length(r2)/4
j2 <- length(r2)/2
j3 <- length(r2)*3/4
j4 <- length(r2)

m1 <- r2[1:j1]
m2 <- r2[(j1+1):j2]
m3 <- r2[(j2+1):j3]
m4 <- r2[(j3+1):j4]
m5 <- r2[grep(pattern = c("/8_s"), x=r2)] 
m6 <- r2[grep(pattern = "/7_s", x=r2)]

M_9 <- r2[grep(pattern = c("/9_"), x=r2)]

M_nobuf <- r2[!grepl(pattern = c("_10.run"), x=r2)]

M_wf <- M_nobuf[grepl(pattern = c("wf"), x = M_nobuf)]
p = "_10_0|_11_0|_12_0|_13_0|_14_0|_15_0|_16_0|_17_0|_18_0|_19_0"

M_wf <- M_wf[grepl(pattern = p , x = M_wf)]

M_1 <- M_nobuf[grep(pattern = c("/1_"), x = M_nobuf)]
M_2 <- M_nobuf[grep(pattern = c("/2_"), x = M_nobuf)]
M_3 <- M_nobuf[grep(pattern = c("/3_"), x = M_nobuf)]
M_4 <- M_nobuf[grep(pattern = c("/4_"), x = M_nobuf)]
M_5 <- M_nobuf[grep(pattern = c("/5_"), x = M_nobuf)]
M_6 <- M_nobuf[grep(pattern = c("/6_"), x = M_nobuf)]
M_7 <- M_nobuf[grep(pattern = c("/7_"), x = M_nobuf)]
M_8 <- M_nobuf[grep(pattern = c("/8_"), x = M_nobuf)]
M_9 <- M_nobuf[grep(pattern = c("/9_"), x = M_nobuf)]
M_10 <- M_nobuf[grep(pattern = c("/10_"), x = M_nobuf)]

M_10ofe <- M_nobuf[grep(pattern = c("_10_"), x = M_nobuf)] #fix 10 ofe
o_10ofe <- c(l,M_10ofe,n)
write.table(o_10ofe,paste0("processed_data/batch/","PRW_fixitofe.pl"),quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

M_buf <- r2[!grepl(pattern = c("_0.run"), x=r2)]

Mb_1 <- M_buf[grep(pattern = c("/1_"), x = M_buf)]
Mb_2 <- M_buf[grep(pattern = c("/2_"), x = M_buf)]
Mb_3 <- M_buf[grep(pattern = c("/3_"), x = M_buf)]
Mb_4 <- M_buf[grep(pattern = c("/4_"), x = M_buf)]
Mb_5 <- M_buf[grep(pattern = c("/5_"), x = M_buf)]
Mb_6 <- M_buf[grep(pattern = c("/6_"), x = M_buf)]
Mb_7 <- M_buf[grep(pattern = c("/7_"), x = M_buf)]
Mb_8 <- M_buf[grep(pattern = c("/8_"), x = M_buf)]
Mb_9 <- M_buf[grep(pattern = c("/9_"), x = M_buf)]
Mb_10 <- M_buf[grep(pattern = c("/10_"), x = M_buf)]

Mb_fix <- M_buf[grep(pattern = c("wbp_mt_12"), x = M_buf)] #M_buf[grep(pattern = c("wbf_ct_18|wbf_nt_2|wbp_nt_1[1-9]|wbp_mt_1[3-6]|wbp_ct_1[3-8]"), x = M_buf)]
ob_fix <- c(l,Mb_fix,n)
write.table(ob_fix,paste0(path,"PRW_fixit.pl"),quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

o <- unlist(m)
o1 <- c(l,m1,n)
o2 <- c(l,m2,n)
o3 <- c(l,m3,n)
o4 <- c(l,m4,n)
o5 <- c(l,m5,n)
o6 <- c(l,m6,n)

o_1 <- c(l,M_1,n)
o_2 <- c(l,M_2,n)
o_3 <- c(l,M_3,n)
o_4 <- c(l,M_4,n)
o_5 <- c(l,M_5,n)
o_6 <- c(l,M_6,n)
o_7 <- c(l,M_7,n)
o_8 <- c(l,M_8,n)
o_9 <- c(l,M_9,n)
o_10 <- c(l,M_10,n)
o_wf <- c(l,M_wf,n)
o_nobuf <- c(l,M_nobuf,n)

obuf <- c(l,M_buf,n)
ob_1 <- c(l,Mb_1,n)
ob_2 <- c(l,Mb_2,n)
ob_3 <- c(l,Mb_3,n)
ob_4 <- c(l,Mb_4,n)
ob_5 <- c(l,Mb_5,n)
ob_6 <- c(l,Mb_6,n)
ob_7 <- c(l,Mb_7,n)
ob_8 <- c(l,Mb_8,n)
ob_9 <- c(l,Mb_9,n)
ob_10 <- c(l,Mb_10,n)

o_concave_path <- unlist(m)
write.table(unlist(o_concave_path),"processed_data/batch/PRW_concave_patch_210705.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

o_convex_path <- unlist(m)
write.table(unlist(o_convex_path),"processed_data/batch/PRW_convex_patch_210705.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")


write.table(unlist(o),"PRW.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

#mannual parrellel - 4X the speed.
write.table(o1,"PRW1.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o2,"PRW2.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o3,"PRW3.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o4,"PRW4.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

# write by climate 
write.table(o_1,"processed_data/batch/PRW_1.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_2,"processed_data/batch/PRW_2.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_3,"processed_data/batch/PRW_3.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_4,"processed_data/batch/PRW_4.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_5,"processed_data/batch/PRW_5.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_6,"processed_data/batch/PRW_6.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_7,"processed_data/batch/PRW_7.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_8,"processed_data/batch/PRW_8.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_9,"processed_data/batch/PRW_9.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_10,"processed_data/batch/PRW_10.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_nobuf,"processed_data/batch/PRW_nobuf.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(o_wf,"processed_data/batch/PRW_wf.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

write.table(ob_1,"processed_data/batch/PRW_b1.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(ob_2,"processed_data/batch/PRW_b2.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(ob_3,"processed_data/batch/PRW_b3.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(ob_4,"processed_data/batch/PRW_b4.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(ob_5,"processed_data/batch/PRW_b5.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(ob_6,"processed_data/batch/PRW_b6.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(ob_7,"processed_data/batch/PRW_b7.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(ob_8,"processed_data/batch/PRW_b8.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(ob_9,"processed_data/batch/PRW_b9.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
write.table(ob_10,"processed_data/batch/PRW_b10.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

write.table(obuf,"processed_data/batch/PRW_fuck.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

# 
# # For all the 3 ofe runs after fixing the slope files mannually
# write.table(o5,"processed_data/PRW5.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")
# # for all wbf_mt1 files that had an error
# write.table(o6,"processed_data/PRW6.pl",quote=FALSE, row.names = FALSE, col.names = FALSE, eol="\n", fileEncoding = "UTF-8")

