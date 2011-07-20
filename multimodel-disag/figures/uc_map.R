library(maptools)
library(maps)
library(rgdal)
library(tikzDevice)
gpclibPermit()

path <- '../paper/figures/'

# ##read in and transform the original data
# ## Get the data from http://water.usgs.gov/GIS/dsdl/huc250k_shp.zip and unzip
#basins_poly <- readShapeSpatial('gis-data/huc250k_shp/huc250k',
#  proj4string=CRS("+proj=aea +ellps=clrk80 +x_0=0 +y_0=0 +lon_0=-96 +lat_0=23"))
#uc_poly <- basins_poly[basins_poly$REG == '14',]
#uc_poly <- uc_poly[!(uc_poly$HUC250K_==776),]
#uc_poly <- spTransform(uc_poly,CRS('+proj=longlat +ellps=GRS80'))
#coo <- coordinates(uc_poly)
#ID <- cut(coo[,1], range(coo[,1]), include.lowest=TRUE)
#uc_basin <- unionSpatialPolygons(uc_poly,ID)
#uc_basin <- sp:::as.SpatialPolygonsDataFrame.SpatialPolygons(uc_basin)
#save(uc_basin,file='gis-data/uc_outline.rda')
#writeSpatialShape(uc_basin,'gis-data/uc_outline')

#load('gis-data/uc_outline.rda')
uc_basin <- readShapeSpatial('gis-data/uc_outline',proj4string=CRS("+proj=latlong +ellps=GRS80"))

rivers <- readShapeLines('gis-data/MainRivers')
major_res <- readShapeLines('gis-data/MainLakesAndReservoirs')
nat_pts <- read.table('gis-data/OutletPoints.txt',header=T)
snow_pts <- read.csv('../data/timeseries/snow/list.csv')
uc_sites <- scan('../data/timeseries/snow/uc_sites.txt',what=character(),sep=',',strip.white=T)

    # get the file names for all the sites in the upper basin available
snow_pts <- as.vector(snow_pts[snow_pts$Sitename %in% uc_sites,])

#uc_states <- map("state", c("colorado","utah","wyoming","arizona","nevada","new mexico"), 
#  fill=TRUE, col="transparent", plot=FALSE)
#IDs <- uc_states$names #sapply(strsplit(us_states$names, ":"), function(x) x[1])
#uc_states_sp <- map2SpatialPolygons(uc_states, IDs=IDs, 
#  proj4string=CRS("+proj=longlat +datum=wgs84"))

tikz(file.path(path,'uc_map.tex'),width=3.5,height=5)
  plot(uc_basin)#,setParUsrBB=TRUE,ylim=bbox(uc_lines)[2,],xlim=extendrange(bbox(uc_lines)[1,],f=1-r))
  title(xlab='Longitude',ylab='Latitude')
  plot(rivers,add=T,col='lightblue')
  plot(major_res,add=T,col='lightblue')
  map('state',add=T,col='gray')
  points(nat_pts$LONDEC[1:20],nat_pts$LATDEC[1:20],cex=.6,pch=19)
  text(nat_pts$LONDEC[c(-8,-16,-(20:30))],nat_pts$LATDEC[c(-8,-16,-(20:30))],(1:30)[c(-8,-16,-(20:30))],pos=1,cex=.6)
  text(nat_pts$LONDEC[c(8,16,20)],nat_pts$LATDEC[c(8,16,20)],c(8,16,20),pos=3,cex=.6)
  axis(1)
  axis(2)
dev.off()



tikz(file.path(path,'uc_map_snotel.tex'),width=3.5,height=5)
  plot(uc_basin)
  title(xlab='Longitude',ylab='Latitude')
  plot(rivers,add=T,col='lightblue')
  plot(major_res,add=T,col='lightblue')
  map('state',add=T,col='gray')
  points(-snow_pts$Long/100,snow_pts$Lat/100,cex=.6)
  
  axis(1)
  axis(2)
dev.off()