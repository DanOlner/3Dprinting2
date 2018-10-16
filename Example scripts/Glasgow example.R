
# Test script with sf file of Glasgow -------------------------------------
sflibs <- c('tidyverse', 'sf', 'tmap', 'tmaptools')
lapply(sflibs, require, character.only = TRUE)
source('r2stl/R/r2stl_geo.r')
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

##  Change path to suit
sf.path <- 'C:/Users/Meng Le Zheng/Synced Google Drive/Google Drive/Mini projects/Festival of social science School map/school rank sf in glasgow.RDS'
glas.sf <- sf.path %>% readRDS
glas.sf %>% summary # min is 0.43

##  relief layer
roads.path <- 'C:/Users/Meng Le Zheng/Synced Google Drive/Google Drive/Mini projects/Festival of social science School map/Osm motorways.RDS'
roads.sf <- roads.path %>% readRDS

# Rasterising our file ----------------------------------------------------
glas.shp <- glas.sf %>% as('Spatial')
r <- rasterToFitShapefileExtent(glas.shp,50)

roads.sf <- roads.sf %>% st_buffer(25)
roads.shp <- roads.sf %>% as('Spatial')
roads.shp$relief <- -0.3



r2stl_geo(
  glas.shp,
  'sd.level4',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'Example scripts/glasgow lvl 4 sd.stl',
  interpolate = 0,
  reliefLayer = roads.shp
)
