library(dplyr)
library(rworldmap)
library(rworldxtra)
library(ggplot2)
# here::here also called in place

# read in predicts database
predicts_database <- readRDS(here::here("data/predicts_database.rds")) %>%
  filter(Class == "Insecta")

data_coordinates <- predicts_database %>%
  select(Longitude, Latitude) %>%
  unique()

# build base map for fertiliser/climate plot
get_basemap <- function(){
  
  # download full basemap
  base_map <- getMap(resolution = "high")
  
  # convert to correction projection
  proj4string(base_map) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  # return basemap
  return(base_map)
}

# bring in basemap for underlying surface
base_map <- get_basemap() %>%
  fortify()

# build map for distribution of sites
site_distribution <- data_coordinates %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), data = base_map, fill = "lightgrey") +
  geom_point(aes(x = Longitude, y = Latitude), alpha = 0.3) +
  coord_map(projection = "mollweide") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        axis.line = element_blank(),
        text = element_text(size = 13),
        panel.grid = element_blank(), panel.background = element_rect(fill = "grey98"),
        strip.text.x = element_text(size = 14))