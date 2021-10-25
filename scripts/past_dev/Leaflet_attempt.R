install.packages("leaflet")
install.packages("htmlwidets")
library(leaflet)
library(raster)
library(tidyverse)
library(htmlwidgets)

#-------
#making a first map

my_map <- leaflet()%>%
  addTiles()%>%
  addMarkers(lat=39.2980803,lng=-76.5898801,
             popup="Jeff Leek's office")
my_map

###----------
#lots of points
set.seed(2016-04-25)
df <- data.frame(lat = runif(20,min = 39.2, max = 39.3),
                 lng = runif(20,min = -76.6, max = -76.5))

df %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()

#-----
# custom icons

hopkinsIcon <- makeIcon(
  iconUrl = "http://brand.jhu.edu/content/uploads/2014/06/university.shield.small_.blue_.png",
  iconWidth = 31*215/230, iconHeight = 31,
  iconAnchorX = 31*215/230/1, iconAnchorY = 16) 

hopkinsLatLong <- data.frame(
  lat = c(39.2973166, 39.3288851, 39.2906617, 39.2970681, 39.28148006),
  lng = c(-76.5929798, -76.6206598, -76.5469683, -76.6150537, -76.6016766))



hopkinsSites <- c(
  "<a href='http://www.jhsph.edu/'>East Baltimore Campus</a>",
  "<a href='https://apply.jhu.edu/visit/homewood/'>Homewood Campus</a>",
  "<a href='http://www.hopkinsmedicine.org/johns_hopkins_bayview/'>Bayview Medical Center</a>",
  "<a href='http://www.peabody.jhu.edu/'>Peabody Institute</a>",
  "<a href='http://carey.jhu.edu/'>Carey Business School</a>"
)

hopkinsLatLong %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(icon=hopkinsIcon, popup = hopkinsSites)

#------
#clustering research sites
set.seed(2016-04-25)
df <- data.frame(lat = runif(500,min = 39.2, max = 39.3),
                 lng = runif(500,min = -76.6, max = -76.5))


df %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())
# cluster options is pretty slick 


df %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers()

#---- shapes - anotations 
md_cities <- data.frame(name = c("Baltimore", "Frederick", "Rockville", "Gaithersburg", 
                                 "Bowie", "Hagerstown", "Annapolis", "College Park", "Salisbury", "Laurel"),
                        pop = c(619493, 66169, 62334, 61045, 55232,
                                39890, 38880, 30587, 30484, 25346),
                        lat = c(39.2920592, 39.4143921, 39.0840, 39.1434, 39.0068, 39.6418, 38.9784, 38.9897, 38.3607, 39.0993),
                        lng = c(-76.6077852, -77.4204875, -77.1528, -77.2014, -76.7791, -77.7200, -76.4922, -76.9378, -75.5994, -76.8483))
md_cities %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(weight = 1, radius = sqrt(md_cities$pop) * 30)

#------
#add a ledged

df <- data.frame(lat = runif(20, min = 39.25, max = 39.35),
                 lng = runif(20, min = -76.65, max = -76.55),
                 col = sample(c("red", "blue", "green"), 20, replace = TRUE),
                 stringsAsFactors = FALSE)

df %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(color = df$col) %>%
  addLegend(labels = LETTERS[1:3], colors = c("blue", "red", "green"))

# try it for my data ------
#Shape files in leaflet

PRW <- shapefile("F:/Dropbox/active/PCD_work/PRW_BMP/RAW/extent/PRW_shape.shp")
?readOGR
library("rgdal")
shapeData <- readOGR("F:/Dropbox/active/PCD_work/PRW_BMP/RAW/extent/PRW_shape.shp")
shapeData_huc12 <- readOGR("F:/Dropbox/active/PCD_work/PRW_BMP/RAW/extent/HUC12.shp")

shapeData_huc12 <- shapeData_huc12[shapeData_huc12$Shape_Area < 800000000,] 

shapeData_ll <- spTransform(shapeData, CRS("+proj=longlat +ellps=GRS80"))
shapeData_huc12_ll <- spTransform(shapeData_huc12, CRS("+proj=longlat +ellps=GRS80"))
library("leaflet")
extent(shapeData)


tif = system.file("tif/L7_ETMs.tif", package = "stars")
x1 = read_stars("processed_data/landtype.tif")

x1 = x1[, , , 3] # band 3

leaflet() %>%
  addTiles() %>%
  leafem:::addGeoRaster(
    x1
    , opacity = 1
    , colorOptions = colorOptions(
      palette = grey.colors(256)
    )
  )

x1

leaflet()  %>% addTiles() %>%  leafem::addGeoRaster(clip1
                     , colorOptions = colorOptions(
                       palette = grey.colors(256)
                     )
  )


leaflet()  %>% addTiles() %>% 
  addPolygons(data=shapeData,weight=5,col = 'red') %>%
  addPolygons(data=shapeData_huc12, weight = 1)
  

? leafem::addGeoRaster
plot(shapeData)
plot(clip1, add=T)
# %>% addMarkers(lng = -114.4359 ,lat=46.63,popup="Hi there") 
#setView(lng = -118.4359 , lat=47.63,zoom=11) %>% 

#-----
# add raster 
library(gdalUtils)
library(leaflet)
library(leafem)
library(stars)


tif = system.file("processed_data/landtype_1.tif", package = "stars")
x1 = read_stars(tif)

getwd()

tif = system.file("./processed_data/landtype_1.tif", package = "stars")
x1 = read_stars(tif)
x1 = x1[, , , 3] # band 3



raster_ct <- gdalwarp("F:/Dropbox/active/PCD_work/PRW_BMP/processed_data/raster_ct.tif",dstfile="processed_data/gis_scratch/raster_ct.tif",
                      t_srs=crs(shapeData),output_Raster=TRUE,
                      overwrite=TRUE,verbose=TRUE)
test <- raster::extract(raster_ct,shapeData_huc12, fun = max, df= T)

j=shapeData_huc12$OBJECTID
i=19
# kam is 19
# thorn is 28 
test <- subset(shapeData_huc12, OBJECTID == j[i])
plot(clip2)

for(i in seq_along(shapeData_huc12)) {
  cut <- subset(shapeData_huc12, OBJECTID == j[i])
  clip1 <- crop(raster_ct, extent(cut)) 
  clip2 <- mask(clip1, cut)
  extract_clip1 <- extract(na.omit(clip2), cut, fun=mean)  
  shapeData_huc12[[i]]@data["mean"] <- extract_clip1
}

ct_huc12_mean <- extract(raster_ct, shapeData_huc12,fun=mean, df=T, na.omit=T)
ct_huc12_median <- extract(raster_ct, shapeData_huc12,fun=median, d=T, na.omit=T)
ct_huc12_max <- extract(raster_ct, shapeData_huc12,fun=max, df=T, na.omit=T)
ct_huc12_csa <- extract(raster_ct, shapeData_huc12,fun=(function(x) sum(x*4.49>=5)),cellnumbers=T, df=T, na.omit=T)


Test_nlcd <- gdalwarp("F:/Dropbox/active/PCD_work/PRW_BMP/processed_data/raster_nt_Q_19_10_15.tif",dstfile="test_SLP_latlng.tif",
                      t_srs=crs(shapeData),output_Raster=TRUE,
                      overwrite=TRUE,verbose=TRUE)



plot(Test_nlcd)
pal = colorNumeric(c("yellow", "orange", "red"), val,
                   na.color = "transparent")

leaflet()  %>% addTiles() %>% 
  addRasterImage(x=Test_nlcd$test_SLP_latlng) %>%
  addPolygons(data=shapeData_huc12, weight = 1)


#----
# Jan's group research map 



R_sites <-   data.frame(name = c("Palouse Gullies", "Palouse Sediment Connectivity", "Palouse Water Quality", "INFEWS Managed Aquifer Recharge","INFEWS Water Rights", 
                                 "Preist Lake Phosphorus", "Headwaters project"),
                        project_tema = c("Trupti","Ames","Phil","Mengqi","Matt","Galen","Jan"),
                        lat = c(46.763806,  47.187540,  46.718355,  46.604355,  48.451945,  48.497559, -1.465307),
                        lng = c(-117.210144, -117.525171, -117.163889, -120.509646, -119.639029, -116.889519, -77.455199),
                        hyper= c("https://labs.wsu.edu/watersheds/team/","https://labs.wsu.edu/watersheds/team/",
                                 "https://labs.wsu.edu/watersheds/palousewaters/","https://labs.wsu.edu/watersheds/team/",
                                 "https://labs.wsu.edu/watersheds/team/","https://labs.wsu.edu/watersheds/team/",
                                 "https://labs.wsu.edu/watersheds/team/"),
                        html1 = rep("<a href=",7),html2 = rep(">",7),html3 = rep("</a>",7))%>%
  mutate(html_whole = paste0(html1,hyper,html2,name,html3))



R_html <- R_sites %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(),popup = R_sites$html_whole)


library(htmlwidgets)
saveWidget(R_html, file="m.html")

str(R_sites)
