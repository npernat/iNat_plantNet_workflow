# Assigning observation to continents ####
# 0. packages ####

install.packages("rworldmap")
install.packages("sf")
library(rworldmap)
library(sf)

# 1. Spatial objects and resulting dataframe ####
imex_points<-data.frame(as.numeric(imex_joined$longitude), 
                   as.numeric(imex_joined$latitude),
                   imex_joined$SampleID) 
which(is.na(imex_points)) # identify NAs
imex_points<-imex_points[-c(351, 1391, 2090,3130),] # two observations (two time NA for longitude and latitude)
colnames(imex_points)<-c("longitude","latitude","SampleID")

# create spatial object as simple feature
sf_points <- st_as_sf(x = imex_points, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# join points simple feature with world map, some countries gave back wrong geometries, so they were
# deleted here as we were only interested in the continents
imex_in_world<-st_join(sf_points, world[-c(12,71,99,184,188,189),], join = st_within)

# create dataframe with columns of interest (continent, SampleID) for merging
imex_continents<-imex_in_world[,c(1,56)] 
imex_region<-left_join(imex_continents, imex_joined, by="SampleID")
imex_region[which(is.na(imex_region$continent)),] # ~ 100 geo-references could not be assigned

# assigning a continent manually by longitude
imex_region$longitude<-as.numeric(imex_region$longitude)
imex_region$latitude<-as.numeric(imex_region$latitude)
imex_region_test<-imex_region %>% 
  mutate(continents = case_when(
  is.na(continent) & between(longitude, -124,-52) ~"North America",
  is.na(continent) & between(longitude, -10, 36) ~"Europe",
  TRUE~continent))
  
imex_region_test[which(is.na(imex_region_test$continents)),]$url # only one observation NA left, from Hawaii, deleted
plot(st_geometry(imex_region_test)) # still one outlier
imex_region_test[which(as.numeric(imex_region$latitude)<11),] # observation from Trinidad and Tobago
imex_region<-imex_region_test[-c(60, 683),] # delete outliers

# check again
plot(st_geometry(imex_region), col=viridis(1, begin=0.4, end=1), pch=1) 
plot(st_geometry(world), add=T)

# 3. beautiful plot for publication ####
     