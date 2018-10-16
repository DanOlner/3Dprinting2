##  Extracting 
library(osmdata)
library(tmap)
library(sf)
ukgrid = "+init=epsg:27700"

pcd.sf2 <- readRDS('school rank sf in glasgow.RDS')

bb = getbb('Glasgow, U.K.')

?add_osm_feature
roads <- opq(bbox = bb) %>%
  add_osm_feature(key = 'highway', value = 'primary') %>%
#  add_osm_feature(key = 'natural', value = 'coastline') %>% #central glasgow has not coastline
  osmdata_sf

roads$osm_lines

roads <- roads$osm_lines %>% st_transform(ukgrid)
roads
pcd.sf2

tmap_mode('view')
pcd.sf2 %>% qtm + roads$osm_lines %>% qtm #seems right

roads %>% saveRDS('Osm motorways.RDS')
