# Line 1:     version control number (95.7) - real (datver)
# Line 2:     number of overland flow elements - integer (nelem)
# Line 3:     a) aspect of the profile (degrees from North) - real (azm)
# b) representative profile width (m) - real (fwidth)
# Repeat Lines 4 & 5 for the number of overland flow elements on Line 2
# Line 4:     a) number of slope points on the OFE - integer (nslpts)
# b) length of the overland flow element (m) - real (slplen)
# Repeat 5a) and 5b) for the number of slope points indicated on Line 4a)
# (user may input up to 20 slope point pairs per OFE and can place on
#   multiple lines)
# Line 5:     a) distance from top of OFE to the point (m or m/m) - real (xinput)
# b) slope steepness at the point (m/m) - real (slpinp)
# a) distance from top of OFE to the point (m or m/m) - real (xinput)
# b) slope steepness at the point (m/m) - real (slpinp)
# a) distance from top of OFE to the point (m or m/m) - real (xinput)
# b) slope steepness at the point (m/m) - real (slpinp)
# "       "     "   "    "     "       "      "
# "       "     "   "    "     "       "      "


filenames <- list.files("spatial/watershed", pattern="*.slp", full.names=TRUE)

?list.files

length(filenames)

library(data.table)
library(readr)
fnames<-data.table(filenames)[filenames%like% "flow"]
hnames<-data.table(filenames)[filenames%like% "hill"]
head(hnames)

i=1
flowpath <- list() 
sink("/dev/null")
for(i in 1:nrow(fnames)){
  temp_dat<-readLines(as.character(fnames[i]),n=4)
  path_dat_temp<- data.table(t(temp_dat))
  names(path_dat_temp) <- c("version", "num_OFE", "asp_width", "step_length")
  
  #temp_dat_pro<-fread("spatial/watershed/flow_1001_1.slp", skip=4,)
  temp_dat_pro <- read_table2(as.character(fnames[i]), 
                             col_names = FALSE, skip = 4)
  L<-seq(2,length(temp_dat_pro),2)
  temp_naming <- tstrsplit((fnames[i]), split="_")
  
  temp <- (tstrsplit((path_dat_temp$asp_width), split=" "))
  temp2 <- tstrsplit((path_dat_temp$step_length), split=" ")
  temp3 <- path_dat_temp[, aspect := temp[1]][
      , width := as.integer(temp[2])][
        , cells := temp2[1]][
          , length := temp2[2]][
            , max_slope := max(temp_dat_pro[L])][
              , mean_slope := mean(temp_dat_pro[L])][
                , hill := as.integer(temp_naming[2])][
                  , flowpath := as.integer(tstrsplit(temp_naming[3],split=".", fixed= T)[1])]
  if(i==1){flowpath<- temp3[,-(3:4)] }
  flowpath <- rbind(flowpath,(temp3[,-(3:4)]))
}

hist(as.double(flowpath$length),100, col=flowpath$hill
     )
hist(tan(as.double(flowpath$max_slope)))

?tan

