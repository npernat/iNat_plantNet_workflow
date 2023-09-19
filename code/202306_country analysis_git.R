# Assigning observation to continents ####
# 0. packages ####

install.packages("rworldmap")
install.packages("sf")
library(rworldmap)
library(sf)
library(tidyverse)
library(viridis)
library(cowplot)

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

# observation map
tiff("./output/observation_map.tiff", units="px",width = 3000,height = 1000,res = 360)
ggplot(imex_region, aes(x = longitude,
                           y = latitude)) +
  geom_polygon(data=map_data("world"),
               aes(x = long, y = lat, group = group),
               fill = "grey95",
               color = "gray40",
               size = 0.1) +
  geom_point(aes(color = continents),size = 0.7, alpha = 0.5) + # size = 0.7
  scale_color_viridis_d(begin=0.1, end=0.6) +
  coord_fixed(ylim = c(28,60), xlim = c(-122,15))+
  guides(color="none")+
  theme_void()
dev.off()


# spread map
# Create sf object from imex_Europe dataset
imex_Europe <- st_as_sf(imex_Europe, coords = c("longitude", "latitude"), crs = 4326)

# Generate spatial points for each year
all_points <- data.frame()
for (y in years) {
  points <- imex_Europe[imex_Europe$year <= y, ]
  points$year <- y
  all_points <- rbind(all_points, points)
}

# Convert to sf object
all_points <- st_as_sf(all_points)

# Create base map
base_map <- ggplot() +
  geom_sf(data = st_sf(), fill = "lightgray") +
  coord_sf()

# Plot spatial points for each year

Europe_cropped<-st_crop(Europe, c(xmin= -10, ymin = 35, xmax = 35, ymax = 60))

plots <- lapply(years, function(y) {
  points <- all_points[all_points$year == y, ]
  ggplot() +
    geom_sf(data = Europe_cropped, fill = "grey90")+
    geom_sf(data = points, color = viridis(1, begin=0.2), size=1.5, alpha=0.7) +
    # labs(title = paste(y)) +
    #theme(plot.title = element_text(hjust = 0.5, size=0.1))+
    theme_void()
})

for (i in 1:length(plots)){
  assign(paste0("p", i), plots[[i]])}

# Display the combined plot
tiff("./output/spread_map.tiff", units="px",width = 3000,height = 1000,res = 360)
plot_grid(p6, p7, p8, p9,p10,p11,p12,p13,p14,p15, nrow = 2, labels = c(2012:2021),
          label_size = 8)
dev.off()
# 








