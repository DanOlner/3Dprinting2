library(osmdata)
library(tmap)
bb = getbb('Glasgow, U.K.')
q <- opq(bbox = bb) %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>% 
  osmdata_sf
q$osm_lines %>% qtm