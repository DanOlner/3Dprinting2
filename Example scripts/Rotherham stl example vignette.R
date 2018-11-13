##  Example of 3D printing vignette 


# Prelude -----------------------------------------------------------------
##  We will create a stl file of Rotherham using LSOA as the smallest areal unit
##  1) The main z axis variable is (standardised) crime rates
##  2) Additional relief layer of primary roads and motorways is taken from osm
##  We need the packages: sf, tidyverse and osmdata. tmap is there to visualise
##  the relief layer in the example

##  Load in packages and source files

sflibs <- c('tidyverse', 'sf', 'tmap', 'osmdata')
lapply(sflibs, require, character.only = TRUE)
source('r2stl/R/r2stl_geo.r')


# 1) Loading in the main spatial data -------------------------------------
##  We will work with easting northing 
ukgrid = "+init=epsg:27700"

##  Data layer as an sf file
rotherham.sf <- st_read(dsn='data/boundarydata',
                        layer = 'rotherham_lsoa_2011_noproj',
                        crs = st_crs(ukgrid))
##  Crime data
rotherham.crime <- 'data/Crimes_SouthYorkshire_byLSOA_allColsArePer1000people.csv' %>% read.csv

##  Merge, replace nas with 0s and subset to relevant variables 
rotherham.sf <- rotherham.sf %>% 
  merge(rotherham.crime, by.x = 'lsoa11cd', by.y = 'LSOAcode') %>%
  replace_na(list(Drugs = 0)) %>%
  select(lsoa11cd, Drugs, geometry)


# 2) Creating the relief layer --------------------------------------------
##  osmdata queries the osm api and returns results as sf files (or shp or xml)
##  Steps are 1) create a bounding box for the extent of search, 
##  2) use add_osm_feature to state your query
##  3) use osmdata_sf to search and return sf objects (other functiosn exist for shp)
##  Afterwards we need to create a raster from this information

##  Get boundary box for Rotherham; we can use place name
bb <- getbb('rotherham, uk') #in long lat and not case sensitive 

##  Get results for a) motorway and b) primary roads to add to relief layer

##  Wrong example: This example only selects roads that are primary AND motorways
# wrong.roads <- opq(bbox = bb) %>%
#   add_osm_feature(key = 'highway', value = 'primary') %>%
#   add_osm_feature(key = 'highway', value = 'motorway') %>%
#   osmdata_sf
# wrong.roads

## Right example: Selecting objects that are motorways and primary roads
##  Basically the same as searching and saving results separately
motorway.roads <- opq(bbox = bb) %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sf
motorway.roads ## Note that there are lots of objects-- we want to keep lines in the end
motorway.roads$osm_lines$type <- 'motorway'

primary.roads <- opq(bbox = bb) %>%
  add_osm_feature(key = 'highway', value = 'primary') %>%
  osmdata_sf
primary.roads$osm_lines$type <- 'primary'

both.roads <- c(motorway.roads, primary.roads) ##combine with one sf file
roads.lines <- both.roads$osm_lines %>% st_transform(crs = st_crs(ukgrid))

##  Creating the relife raster
linesA <- roads.lines %>% filter(type == 'motorway') %>% st_buffer(40) #wider motorways
linesB <- roads.lines %>% filter(type == 'primary') %>% st_buffer(10) #less wide roads
roads.lines <- rbind(linesA, linesB)
roads.lines$relief <- 0

relief <- rasterToFitShapefileExtent(rotherham.sf %>% as('Spatial'), 50)
roadsreliefRaster <- rasterize(roads.lines %>% as('Spatial'), 
                               relief, 
                               roads.lines$relief)
roadsreliefRaster[is.na(roadsreliefRaster)] <- 0.03#
reliefRaster <- roadsreliefRaster

# Rasterising our file ----------------------------------------------------
##  File step is to just use the data in the r2stl_geo routine
r2stl_geo(
  rotherham.sf %>% as('Spatial'),
  'Drugs',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/Rotherham drugs example.stl',
  interpolate = 0,
  reliefLayer = reliefRaster
)

