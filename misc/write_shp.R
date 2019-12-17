rm(list=ls())
setwd("/Users/jamis/dropbox/geog788p/jamis_MnM4SDS_project")
library(gdata)
library(sp)
library(raster)
library(rgdal)
library(anchors)




dat <- read.xls("./data/Nasa_Plot_Fraver_4JB.xlsx", sheet = 1, header = TRUE)
lat <- dat$Y_coord89
lon <- dat$X_coord89
dat <- dat[,-c(18,19)]
pts <- SpatialPointsDataFrame(coords=cbind(lon,lat), data=dat, proj4string=CRS("+init=epsg:32619"))

# make transect variable
r <- raster(nrows=40, ncols=40, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+init=epsg:32619"), vals=1:1600)
ts <- rasterToPolygons(r)

# extract transect ID to pts
foo <- over(x=pts, y=ts)
pts@data$tnsct_val <- foo$layer
# writeOGR(pts,'./data/shapefiles/nasa_plot_trees.shp', driver="ESRI Shapefile", layer="nasa_plot_trees", overwrite=T)

# aggregate cariables by transect value
Mode = function(x){
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    # mod = NA
    mod=tam
  else
    if(is.numeric(x)){
      foo = as.numeric(names(ta)[ta == tam])
      mod = foo[1]
    }
  else
    mod = names(ta)[ta == tam]
  return(mod)
}


cc <- c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,2,3,4,5,2,3,4,3,4,3,3)
ccc <- c(1,2,3,4,1,2,3,4,1,2,3,4,2,3,2,3)
c <- c(1,2,3,4)
Mode(c)


nSpecies <- function(x) {
  length(unique(x))
}

nTrees <- function(x) {
  length(x)
}

ts_dat <- data.frame(ID = aggregate(pts$Ht_16, by=list(pts$tnsct_val), FUN=mean, na.rm=T)[,1],
                     Ht_16 = aggregate(pts$Ht_16, by=list(pts$tnsct_val), FUN=mean, na.rm=T)[,2],
                     DBH_15 = aggregate(pts$DBH_15, by=list(pts$tnsct_val), FUN=mean, na.rm=T)[,2],
                     BLC_16 = aggregate(pts$BLC_16, by=list(pts$tnsct_val), FUN=mean, na.rm=T)[,2],
                     Cnpy_16 = aggregate(pts$Cnpy_16, by=list(pts$tnsct_val), FUN=Mode)[,2],
                     nSpecies = aggregate(pts$SPP, by=list(pts$tnsct_val), FUN=nSpecies)[,2],
                     nTrees = aggregate(pts$SPP, by=list(pts$tnsct_val), FUN=nTrees)[,2])


# bar <- ts_dat[complete.cases(ts_dat),]

bam <- merge(x=ts_dat, y=ts@data, by.x=c('ID'), by.y=c('layer'), all.y=T)
bam <- replace.value(bam, names=c("Cnpy_16"), from='-Inf')

ts@data <- bam
writeOGR(ts,'./data/shapefiles/nasa_plot_tnsct.shp', driver="ESRI Shapefile", layer="nasa_plot_transects", overwrite=T)
