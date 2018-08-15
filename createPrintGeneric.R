#DRY version of 3D print creation
#Refactor to create nine sculptures
#Might only be DRY for Rotherham and these nine, so not perfect
source('r2stl/R/r2stl_geo.r')
#library(RColorBrewer)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Rotherham relief raster----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Make it proportional to c(0,1)
createReliefRaster <- function(data_geo, dimension, base = 3){
  
  r <- rasterToFitShapefileExtent(data_geo,dimension)
  
  ##  Load in the roads
  # roads.path<-'rotherham_mainRoadsBuffer25m'
  # mway.path<-'rotherham_MotorwayBuffer60m'
  
  #Extra wide roads for wood print
  roads.path<-'rotherham_mainRoadsBuffer60m'
  mway.path<-'rotherham_MotorwayBuffer100m'
  # 
  
  
  ##  load in the roads file
  roads <- readOGR('data/boundarydata',roads.path)
  mway <- readOGR('data/boundarydata',mway.path)
  
  ##  Rasterise the roads, the relief variable in roads, and r (the rasterised geo file)
  roads$relief<- -1
  roadsreliefRaster <- rasterize(roads,r,roads$relief)
  roadsreliefRaster[is.na(roadsreliefRaster)] <- 0#
  
  ##  Rasterise the motorways 
  #Currently a single polygon with no attributes. Add one with the value we want to use
  mway$relief <- -2
  mwayreliefRaster <- rasterize(mway,r,mway$relief)
  mwayreliefRaster[is.na(mwayreliefRaster)] <- 0#
  
  ##  >Note: North arrow rasterising routine----
  ##Dan update: <175 to get rid of fuzzy edges. Plot both to see why
  northArrow <- raster('images/northArrow1.tif')
  values(northArrow) <- ifelse(values(northArrow) <175,-1,0)#dan version
  #values(northArrow) <- ifelse(values(northArrow) == 0,-1,0)#orig
  
  sm <- aggregate(northArrow, fact=10)
  proj4string(sm) <- proj4string(roadsreliefRaster)
  
  #Chosen coordinates for corner of image (bottom left)
  # newx <- 439903.630
  # newy <- 379484.665
  newx <- 437815
  newy <- 379451
  #Multiplication of image size
  xmax <- extent(sm)[2] * 8
  ymax <- extent(sm)[4] * 8
  
  extent(sm) <- c(xmin <- newx, xmax <- newx + xmax, ymin <- newy, ymax <- newy + ymax)
  extent(sm)
  #plot(sm)#see difference it makes here to fuzziness
  
  #Getting the north arrow onto a blank layer
  sm2 <- projectRaster(sm,roadsreliefRaster)
  sm2 <- 1-sm2
  
  values(sm2)[is.na(values(sm2))]<-min(values(sm2),na.rm=T)
  values(sm2)<-ifelse(values(sm2)>1.9,-2,0)
  
  reliefRaster <- sm2 + min(roadsreliefRaster,mwayreliefRaster, na.rm = T)
  reliefRaster ##need positve values
  values(reliefRaster)<-values(reliefRaster) + base
  #plot(reliefRaster)
  
  values(reliefRaster)<-values(reliefRaster) / 100
  
  return(reliefRaster)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Data and maps----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We are after:
# Sculpture LATF#1: The spatial distribution of the Pakistani population in Rotherham in 2011 (as a proportion of the total population in each datazone) -- grey plastic
# Sculpture LATF#2: The spatial distribution of the non-UK born population in Rotherham in 2011 (as a proportion of the total population in each datazone) -- grey plastic
# **Sculpture LATF#3: The spatial distribution of Muslims in Rotherham in 2011 (as a proportion of the total population in each datazone) -- Steel or Wood (if not, grey plastic)
# **Sculpture LATF#4: Inversion of LATF#3 --  The spatial distribution of non-Muslims in Rotherham in 2011 (as a proportion of the total population in each datazone) -- Steel or Wood (if not, grey plastic)
# **Sculpture LATF#5: The spatial distribution of multiple deprivation in Rotherham in 2011 -- -- Steel or Wood (if not, white plastic) 
# **Sculpture LATF#6: The spatial distribution of all crime (from https://data.police.uk/) in Rotherham in 2011 (as a proportion of the total population in each datazone) + Social Frontiers -- -- Steel or Wood (if not, grey plastic) 
# Sculpture LATF#7: The spatial distribution of violent crime (from https://data.police.uk/) in Rotherham in 2011 (as a proportion of the total population in each datazone) + Social Frontiers -- white plastic 
# Sculpture LATF#8: The spatial distribution of Burglary  (from https://data.police.uk/) in Rotherham in 2011 (as a proportion of the total population in each datazone) + Social Frontiers -- white plastic 
# Sculpture LATF#9: The spatial distribution of Anti-social behaviour  (from https://data.police.uk/) in Rotherham in 2011 (as a proportion of the total population in each datazone) + Social Frontiers -- white plastic 

#Keep only unique LSOA entries and only the fields we need (see below)
roth.ethnicity <- read_csv('data/eth lsoa msoa.csv') %>%
  .[!duplicated(.[,'LSOA11CD']),] %>% 
  dplyr::select(LSOA11CD,nonwhiteZoneProp.lsoa,pstaniZoneProp.lsoa)
  
roth.cob <- read_csv('data/cob lsoa msoa.csv') %>% .[!duplicated(.[,'LSOA11CD']),] %>% 
  dplyr::select(3,17)

#Already LSOA. All columns are per 1000 people.
#Should probably try and make these proportional to each other.
roth.crime <- read_csv('data/Crimes_SouthYorkshire_byLSOA_allColsArePer1000people.csv') %>% 
  dplyr::select(1:3,12,13)

#Has some missing values. Need to deal with those for the interpolated plots.
#Doesn't mean there's no crime so zero is not ideal. 
#Maybe interpolated not good... could use neighbour average?

#Already LSOA direct from Nomis
#We're after proportion muslim
roth.religion <- read_csv('data/religion2011_LSOA_YorknHumber.csv') %>% 
  mutate(proportionMuslim = (.[[11]]/.[[5]])*100 ) %>% 
  dplyr::select(3,16)

#Add a non-muslim column. Just the inverse.
#Note: voluntary question, so 'proportion non-muslim' incudes people who stated no religion
#Having just looked... using 100% probably not appropriate for printing
#Might as well invert based on the maximum value
#So that the highest muslim proportion zone is zero
#Oh, except the max is 94.83...
roth.religion$proportionNonMuslim <- max(roth.religion$proportionMuslim) - roth.religion$proportionMuslim

#Deprivation
#Not sure why this has 9135 rows. There are only 167 unique rows.
roth.imd <- read_csv('data/rotherham-deprivation-data.csv') %>% 
  .[!duplicated(.[,'LSOA Code']),] %>% 
  dplyr::select(3,5)#use only main IMD rank

#"The Index of Multiple Deprivation ranks every small area in England 
#from 1 (most deprived area) to 32,844 (least deprived area)."
#But we want most deprived to show up higher, so... 
roth.imd$IMD_inverse_rank <- 32844 - roth.imd$`Index of Multiple Deprivation Rank`

#Also think it might be worth making it relative to Rotherham only not the whole of England
roth.imd$IMD_inverse_rank_relative <- rank(roth.imd$IMD_inverse_rank)

#~~~~~~~~~~~~~~
#lsoa shapefile
osgb36 <- ("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
lsoas <- readOGR('data/boundarydata','rotherham_lsoa_2011_noproj',p4s=osgb36)

#Add the nine fields we want to the shapefile.
lsoas2 <- merge(lsoas, roth.ethnicity, by.x = 'lsoa11cd', by.y = 'LSOA11CD')
lsoas2 <- merge(lsoas2, roth.cob, by.x = 'lsoa11cd', by.y = 'LSOA11CD')
lsoas2 <- merge(lsoas2, roth.religion, by.x = 'lsoa11cd', by.y = 'geography code')
lsoas2 <- merge(lsoas2, roth.crime, by.x = 'lsoa11cd', by.y = 'LSOAcode')
lsoas2 <- merge(lsoas2, roth.imd, by.x = 'lsoa11cd', by.y = 'LSOA Code')

#Better names
names(lsoas2)[13] <- 'antiSocialBehaviour'
names(lsoas2)[15] <- 'violentCrime'
names(lsoas2)[17] <- 'IMD_rank'
names(lsoas2)[18] <- 'IMD_rank_inverse'
names(lsoas2)[19] <- 'IMD_rank_inverse_relative'

#Check a couple for NAs... yup, a couple.
#Set those to zero for saving: they don't link in QGIS if NA
table(0 + is.na(lsoas2$violentCrime))
table(0 + is.na(lsoas2$Burglary))

lsoas2$violentCrime[is.na(lsoas2$violentCrime)] <- 0
lsoas2$Burglary[is.na(lsoas2$Burglary)] <- 0

#Save that as single shapefile.
#Gah, abbreviated col names
writeOGR(lsoas2,'data/shapefiles','rotherham_alldatasetsIncolumns', driver = 'ESRI Shapefile')
#And this will lose projection?
#writeSpatialShape('lsoas2,data/shapefiles/rotherham_alldatasetsIncolumns.shp')
#Or maybe having projection is best? What did it abbreviate to?
chk <- readOGR('data/shapefiles','rotherham_alldatasetsIncolumns')

#save for QGIS
write_csv(data.frame(lsoas2),'data/rothLSOAsAllvars4QGIS.csv')

#the data for savinz
# lsoas_df <- data.frame(lsoas2)[,c(1,8:19)]
# 
# #A max and min version
# lsoas_dfMin <- lsoas_df[,c(2:13)] %>%
#   summarise_each(funs(min(.,na.rm=T))) %>% 
#   mutate(minmax = 'min')
# 
# lsoas_dfMax <- lsoas_df[,c(2:13)] %>%
#   summarise_each(funs(max(.,na.rm = T))) %>% 
#   mutate(minmax = 'max')
# 
# rothVarsMinMax <- rbind(lsoas_dfMin,lsoas_dfMax)
#   
# #save both
# write_csv(lsoas_df,'data/rothLSOAsAllvars.csv')
# write_csv(rothVarsMinMax,'data/rothLSOAsAllvarsMinMax.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Run STL creation routine for all----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Save to look at vars in map
#write_csv(data.frame(lsoas2),'local/qgis/rotherhamVars.csv')


#reliefRaster just needs creating once, can be same for all
#Or can add other features after
#Used below in the stl function also
gridRez <- 25
# zRatio <- 0.6
zRatio <- 0.35
reliefRaster <- createReliefRaster(lsoas,gridRez, base = 3)


# for(varName in names(lsoas2)[8:17]) {
# for(varName in names(lsoas2)[8:9]) {
# for(varName in names(lsoas2)[c(8:16,19)]) {
for(varName in names(lsoas2)[c(9,13,15)]) {

resultRaster <- r2stl_geo(
    lsoas2,
    varName,
    gridResolution = gridRez,
    keepXYratio = T,
    zRatio = zRatio,
    show.persp = F,
    # filename = paste0('stl/lifeAtTheFrontier/25m/',varName,'_',gridRez,'m_NoInt.stl'),
    filename = paste0('stl/lifeAtTheFrontier/',varName,'_',gridRez,'m_z',zRatio,'_NoInt.stl'),
    # filename = paste0('stl/lifeAtTheFrontier/interpolated/',varName,'_',gridRez,'m_z',zRatio,'Int6.stl'),
    reliefLayer = reliefRaster,
    # interpolate = 6
    interpolate = 0
  )
  
}

#save specific outputs
#inverse IMD relative to Rotherham, 25m
# saveRDS(resultRaster, 'saves/raster.Rotherham.imd.inverse.relative.25m.rds')
# resultRaster <- readRDS('saves/raster.Rotherham.imd.inverse.relative.25m.rds')
saveRDS(resultRaster, 'saves/raster.Rotherham.allCrimes.25m.rds')
resultRaster <- readRDS('saves/raster.Rotherham.allCrimes.25m.rds')

#Will have to add reliefRaster here as it's separated in the function

plotRaster <- resultRaster + (reliefRaster * 1000)

#https://stackoverflow.com/a/13353264
colfunc <- colorRampPalette(c("white", "blue"))

#Output as image, blue as highest values
# png("images/raster.Rotherham.imd.inverse.relative.25m.png", width=10, height=10, units="in", res=300)
png("images/raster.Rotherham.imd.inverse.relative.25m.png", width=10, height=10, units="in", res=300)
plot(plotRaster, col = colfunc(50), interpolate = T)
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~
#TESTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#Some issue with the relief layer not being added. What's the crack?
shapefile <- lsoas2
variable <- 'IMD_rank'
df <- data.frame(lsoas2)

shapefile_x <- (xmax(shapefile)-xmin(shapefile))/50
shapefile_y <- (ymax(shapefile)-ymin(shapefile))/50


r <- raster(ncols = shapefile_x, nrows = shapefile_y)
proj4string(r) <- proj4string(shapefile)
extent(r) <- extent(shapefile)

useRaster <- rasterize(shapefile,r,df[,variable])

##>update: 3/5/2017, change NA values to 0 to allow relief layer to be added; Meng Le----
values(useRaster)[is.na(values(useRaster))]<-0

useRaster2 <- useRaster + reliefRaster

