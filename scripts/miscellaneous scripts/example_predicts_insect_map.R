library(dplyr)
library(rworldmap)
library(rworldxtra)
library(ggplot2)
# here::here also called in place

# read in predicts database
multi_order_df <- readRDS(here::here(file.choose()))

data_coordinates <- multi_order_df %>%
  dplyr::select(Longitude, Latitude, Order) %>%
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
  geom_point(aes(x = Longitude, y = Latitude, color = Order), size = 4, alpha = 0.2) +
  coord_map(projection = "mollweide") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        axis.line = element_blank(),
        text = element_text(size = 13),
        panel.grid = element_blank(), panel.background = element_rect(fill = "grey98"),
        strip.text.x = element_text(size = 14),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes=list(alpha = 1, shape = 19, size = 4))) +
  scale_color_manual(values= group_colours) +
  theme(legend.key=element_rect(fill=NA), legend.text = element_text(size= 14)) 

ggarrange(site_distribution, common.legend = TRUE, legend = "bottom") 


#ggsave("./figures/C2_latest_figs/multi-group-site_distribution.png", site_distribution, width = 10, height = 6)
