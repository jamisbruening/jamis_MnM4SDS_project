rm(list=ls())
setwd("/Users/jamis/dropbox/geog788p/jamis_MnM4SDS_project")
library(gdata)
library(sp)
library(raster)
library(rgdal)


dat <- read.xls("./data/Nasa_Plot_Fraver_4JB.xlsx", sheet = 1, header = TRUE)
lat <- dat$Y_coord89
lon <- dat$X_coord89
dat <- dat[,-c(18,19)]
pts <- SpatialPointsDataFrame(coords=cbind(lon,lat), data=dat, proj4string=CRS("+init=epsg:32619"))

# make transect variable
r <- raster(nrows=40, ncols=40, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+init=epsg:32619"), vals=1:1600)
ts <- rasterToPolygons(r)
writeOGR(ts,'./data/shapefiles/nasa_plot_tnsct.shp', driver="ESRI Shapefile", layer="nasa_plot_transects", overwrite=T)

# extract transect ID to pts
foo <- over(x=pts, y=ts)
pts@data$tnsct_val <- foo$layer
writeOGR(pts,'./data/shapefiles/nasa_plot_trees.shp', driver="ESRI Shapefile", layer="nasa_plot_trees", overwrite=T)
