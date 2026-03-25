




# basic ----

#install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "dplyr"))

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)


# Get Canada map at province level
canada <- ne_states(country = "Canada", returnclass = "sf")

# Filter for Newfoundland and Labrador
newfoundland <- canada %>% 
   filter(name_en == "Newfoundland and Labrador")


ggplot(data = newfoundland) +
   geom_sf(fill = "lightblue", color = "black") +
   coord_sf() +
   theme_minimal() +
   ggtitle("Map of Newfoundland and Labrador")

# island ----
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(cowplot)

# get layers
canada_prov <- st_read("C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo", layer = "gadm41_CAN_1")

# Filter to Newfoundland and Labrador
nl <- canada_prov %>% filter(NAME_1 == "Newfoundland and Labrador")

# Cast into individual polygons
nl_polygons <- nl %>% st_cast("POLYGON")


# Compute area in square meters (default unit from st_area)
nl_polygons <- nl %>% st_cast("POLYGON")
areas <- st_area(nl_polygons)

# Set size threshold (e.g., 100 km² = 100,000,000 m²) to eliminate small islands and Labrador
threshold <- units::set_units(100, km^2)
threshold2 <- units::set_units(150000, km^2)

# Filter polygons larger than threshold
large_polygons2 <- nl_polygons[threshold2 > areas & areas > threshold, ]


# Filter polygons: east of -58 and south of 52; removes small islands around Labrador
centroids <- st_centroid(large_polygons2)
coords <- st_coordinates(centroids)
#coords <- st_coordinates(large_polygons2)
large_polygons_island <- large_polygons2[coords[, 1] > -58 & coords[, 2] < 52, ]


# Merge into one geometry (optional)
newfoundland_island <- st_union(large_polygons_island) %>% st_sf()


# Get Canada for inset
canada_outline <- ne_states(country = "Canada", returnclass = "sf")

inset_map <- ggplot(canada_outline) +
   geom_sf(fill = "gray30", color = "gray50") +
   geom_sf(data = nl, fill = "gray90", color = "black") +
   geom_rect(aes(xmin = -60, xmax = -51, ymin = 46.5, ymax = 52, colour = "red"), fill = NA, linewidth = 1.25) +
   theme_void()

inset_map <- inset_map + theme(legend.position = "none")

# Wrap the inset in a plot with a border
framed_inset <- ggplot() +
   theme_void() +
   annotation_custom(ggplotGrob(inset_map), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
   theme(
      plot.background = element_rect(color = "black", fill = NA, linewidth = 1)
   )
library(ggrepel)

coords <- read.csv("river_coordinates-v1.csv")

library(ggspatial)

main_map <- ggplot(newfoundland_island) +
   geom_sf(fill = "grey", color = "black") +
   geom_point(data = coords[-27,], aes(long, lat)) +
   geom_text(data = coords[-27,], 
             aes(long, lat, label = abbreviation),
             position = position_jitter(width = 0.1, height = 0.1)) +
   ylab("Latitude") +
   xlab("Longitude") +
   #xlim(-52, -60) +
   annotation_north_arrow(location = "tl",which_north = "true", 
                          pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                          style = north_arrow_orienteering,width = unit(1, "cm"), 
                          height = unit(1, "cm")) +
   annotation_scale() +
   #  coord_fixed(1.3) +
   theme_minimal()
main_map
ggsave("figs/study_areas.png", main_map, width = 8, height = 6, dpi = 300, units = "in")
ggsave("figs/4study_areas.png", main_map, width = 8, height = 6, dpi = 300, units = "in")




main_map <- ggplot(newfoundland_island) +
   geom_sf(fill = "grey", color = "black") +
   geom_point(data = coords[-27,], aes(long, lat)) +
   geom_text_repel(data = coords[-27,],
             aes(long, lat, label = abbreviation),
             point.padding = 0.35,
             box.padding   = 0.35,
             min.segment.length = 0
   ) +
   ylab("Latitude") +
   xlab("Longitude") +
   annotation_north_arrow(location = "tl",which_north = "true", 
                          pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                          style = north_arrow_orienteering,width = unit(1, "cm"), 
                          height = unit(1, "cm")) +
   annotation_scale() +
   theme_minimal()
main_map
ggsave("figs/study_areas.png", main_map, width = 8, height = 6, dpi = 300, units = "in")
