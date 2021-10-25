library(raster)
library(geoR)
library(ggplot2)
library(latticeExtra)
library(tidyverse)
library(fasterize)
library(sf)

slope_PRW <- raster("processed_data/NED_PF_PRWsd8.tif") 
NED_PRW <- raster("processed_data/NED_PRW.tif")
huc12 <- shapefile("RAW/extent/HUC12")
PRW <- shapefile("RAW/extent/PRW_shape.shp")
streams <- shapefile("EXTRACTIONS/PRW/NHD/PRW_NHD_Flowline") 

streams_utm<- spTransform(streams,
                          crs(huc12))%>%crop(PRW)
streams_utm<- crop(streams_utm, PRW)
#streams <- st_read("EXTRACTIONS/PRW/NHD/PRW_NHD_Flowline.shp")

#plot(streams_utm)

plot <- spplot(huc12, zcol = "OBJECTID")
lables <- layer(sp.text(coordinates(huc12), txt = huc12$OBJECTID, pos=0))
plot + lables


i <- (huc12$OBJECTID)
j=1
g <- subset(huc12, OBJECTID == i[j])

j=19
g <- subset(huc12, OBJECTID == i[j])
Kam_NED <- NED_PRW %>% crop(g) %>% mask(g)
Kam_streams <- streams_utm %>% crop(g)
plot(Kam_streams)

stream_r <- rasterize(Kam_streams,Kam_NED,"FlowDir")
plot(stream_r)

Kam_slope <- terrain(Kam_NED, 'slope', 'degrees')
Kam_d8 <- terrain(Kam_NED, 'flowdir', neighbors=8)
plot(Kam_d8)

??flowPath
??flow_accumulation




#########################################################################################################################
#  R script to run TauDEM

# Packages used
# install rgdal
# install raster
# install shapefiles
install.packages("rgdal")
install.packages("raster")
install.packages("shapefiles")

# This also assumes that MPICH2 is properly installed on your machine and that TauDEM command line executables exist
# MPICH2.  Obtain from http://www.mcs.anl.gov/research/projects/mpich2/
# Install following instructions at http://hydrology.usu.edu/taudem/taudem5.0/downloads.html.  
# It is important that you install this from THE ADMINISTRATOR ACCOUNT.

# TauDEM command line executables.  
# If on a PC download from http://hydrology.usu.edu/taudem/taudem5.0/downloads.html
# The install package will install to c:\program files\taudem or c:\program files (x86)\taudem set a 
# system path.  If you want to do this manually you can download the command line executables and place where you wish.
# If on a different system, download the source code and compile for your system.

library(raster)
library(shapefiles)

# Set working directory to your location
setwd("C:/Users/dtarb/Scratch/Logan")

z=NED_PRW
plot(z)

# Pitremove
system("mpiexec -n 8 pitremove -z -fel processed_data/NED_PRW_fill.tif")  ############ Error here ##########
fel=raster("processed_data/NED_PRW_fill.tif")
plot(fel)
origin(fel) <- origin(z)

# D8 flow directions
system("mpiexec -n 8 D8Flowdir -p processed_data/NED_PRW_p.tif -sd8 processed_data/NED_PRW_sd8.tif -fel processed_data/NED_PRW_fill.tif",show.output.on.console=F,invisible=F)
p=raster("processed_data/NED_PRW_p.tif") # this looks like 1-8 direction 
plot(p)
sd8=raster("processed_data/NED_PRW_sd8.tif") # this looks like slope?
plot(sd8)

# Contributing area
system("mpiexec -n 8 AreaD8 -p processed_data/NED_PRW_p.tif -ad8 processed_data/NED_PRW_ad8.tif")
ad8=raster("processed_data/NED_PRW_ad8.tif")
plot(log(ad8))



# Grid Network 
system("mpiexec -n 8 Gridnet -p processed_data/NED_PRW_p.tif -gord processed_data/NED_PRW_gord.tif -plen processed_data/NED_PRW_plen.tif -tlen processed_data/NED_PRW_tlen.tif")
gord=raster("processed_data/NED_PRW_gord.tif")
plot(gord)
zoom(gord)

# infinity flow direction -------------------------------------------------


# DInf flow directions
system("mpiexec -n 8 DinfFlowdir -ang processed_data/NED_PRW_ang.tif -slp processed_data/NED_PRW_slp.tif -fel processed_data/NED_PRW_fill.tif",show.output.on.console=T,invisible=F)
ang=raster("processed_data/NED_PRW_ang.tif")
plot(ang)
slp=raster("processed_data/NED_PRW_slp.tif")
plot(slp)


# Dinf contributing area
system("mpiexec -n 8 AreaDinf -ang processed_data/NED_PRW_ang.tif -sca processed_data/NED_PRW_sca.tif")
sca=raster("processed_data/NED_PRW_sca.tif")
plot(log(sca))
zoom(log(sca))

# wshed and stream def ----------------------------------------------------

# Threshold
system("mpiexec -n 8 Threshold -ssa processed_data/NED_PRW_ad8.tif -src processed_data/NED_PRW_src.tif -thresh 100")
src=raster("processed_data/NED_PRW_src.tif")
plot(src)
zoom(src)

# a quick R function to write a shapefile
makeshape.r=function(sname="shape",n=1)
{
  xy=locator(n=n)
  points(xy)
  
  #Point
  dd <- data.frame(Id=1:n,X=xy$x,Y=xy$y)
  ddTable <- data.frame(Id=c(1),Name=paste("Outlet",1:n,sep=""))
  ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 1)
  write.shapefile(ddShapefile, sname, arcgis=T)
}

makeshape.r("ApproxOutlets")

# Move Outlets
system("mpiexec -n 8 moveoutletstostreams -p processed_data/NED_PRW_anp.tif -src processed_data/NED_PRW_src.tif -o approxoutlets.shp -om Outlet.shp")
outpt=read.shp("outlet.shp")
approxpt=read.shp("ApproxOutlets.shp")

plot(src)
points(outpt$shp[2],outpt$shp[3],pch=19,col=2)
points(approxpt$shp[2],approxpt$shp[3],pch=19,col=4)

zoom(src)


# Contributing area upstream of outlet
system("mpiexec -n 8 Aread8 -p loganp.tif -o Outlet.shp -ad8 loganssa.tif")
ssa=raster("loganssa.tif")
plot(ssa) 


# Threshold
system("mpiexec -n 8 threshold -ssa loganssa.tif -src logansrc1.tif -thresh 2000")
src1=raster("logansrc1.tif")
plot(src1)
zoom(src1)

# Stream Reach and Watershed
system("mpiexec -n 8 Streamnet -fel loganfel.tif -p loganp.tif -ad8 loganad8.tif -src logansrc1.tif -o outlet.shp -ord loganord.tif -tree logantree.txt -coord logancoord.txt -net logannet.shp -w loganw.tif")
plot(raster("loganord.tif"))
zoom(raster("loganord.tif"))
plot(raster("loganw.tif"))

# Plot streams using stream order as width
snet=read.shapefile("logannet")
ns=length(snet$shp$shp)
for(i in 1:ns)
{
  lines(snet$shp$shp[[i]]$points,lwd=snet$dbf$dbf$Order[i])
}

# Peuker Douglas stream definition
system("mpiexec -n 8 PeukerDouglas -fel loganfel.tif -ss loganss.tif")
ss=raster("loganss.tif")
plot(ss)
zoom(ss)

#  Accumulating candidate stream source cells
system("mpiexec -n 8 Aread8 -p loganp.tif -o outlet.shp -ad8 loganssa.tif -wg loganss.tif")
ssa=raster("loganssa.tif")
plot(ssa)

#  Drop Analysis
system("mpiexec -n 8 Dropanalysis -p loganp.tif -fel loganfel.tif -ad8 loganad8.tif -ssa loganssa.tif -drp logandrp.txt -o outlet.shp -par 5 500 10 0")

# Deduce that the optimal threshold is 300 
# Stream raster by threshold
system("mpiexec -n 8 Threshold -ssa loganssa.tif -src logansrc2.tif -thresh 300")
plot(raster("logansrc2.tif"))

# Stream network
system("mpiexec -n 8 Streamnet -fel loganfel.tif -p loganp.tif -ad8 loganad8.tif -src logansrc2.tif -ord loganord2.tif -tree logantree2.dat -coord logancoord2.dat -net logannet2.shp -w loganw2.tif -o Outlet.shp",show.output.on.console=F,invisible=F)

plot(raster("loganw2.tif"))
snet=read.shapefile("logannet2")
ns=length(snet$shp$shp)
for(i in 1:ns)
{
  lines(snet$shp$shp[[i]]$points,lwd=snet$dbf$dbf$Order[i])
}

# Wetness Index
system("mpiexec -n 8 SlopeAreaRatio -slp loganslp.tif -sca logansca.tif -sar logansar.tif", show.output.on.console=F, invisible=F)
sar=raster("logansar.tif")
wi=sar
wi[,]=-log(sar[,])
plot(wi)

# Distance Down
system("mpiexec -n 8 DinfDistDown -ang loganang.tif -fel loganfel.tif -src logansrc2.tif -m ave v -dd logandd.tif",show.output.on.console=F,invisible=F)
plot(raster("logandd.tif"))



















