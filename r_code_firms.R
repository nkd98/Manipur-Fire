## set working directory
main <- "d:/r_projects/forest_fire"
setwd(main)

## install and load the libraries needed.
libs <- c(
  "tidyverse", "data.table",
  "ggmap", "gganimate", "sf", "sp", "rgdal"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == F)){
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(libs, library, character.only = T))


# load the data
fire_data_manipur <- sf::st_read("DL_FIRE_SV-C2_29397/fire_nrt_SV-C2_29397.shp")
fire_data_manipur
summary(fire_data_manipur)
nrow(fire_data_manipur)

# extent of the data
# 92.742920,23.726593,94.934692,25.716554
xmin <- 92.742920
ymin <- 23.726593
xmax <- 94.934692
ymax <- 25.716554

area_coords <- c(
  xmin, ymin, xmax, ymax
)



# map layer from ggmap
bg_layer <- ggmap::get_stamenmap(
  bbox = area_coords,
  zoom = 9,
  maptype = "terrain"
)

ggmap::ggmap(bg_layer) 

  
# Fetch Manipur state boundary
india_states <- rnaturalearth::ne_states(country = "India", returnclass = "sf")
manipur_poly <- subset(india_states, name == "Manipur")



########################## ANALYSIS ##########################



#########
library("osmdata")

# Define a buffer around fire locations to retrieve OSM data
bbox <- area_coords

# Retrieve OSM data within the bounding box
# dat <- opq(bbox) %>%
#   add_osm_feature(key = 'building') |>
#   osmdata_xml(file = 'buildings.osm')

st1 <- sf::st_read('buildings.osm', layer = 'multipolygons', quiet = TRUE)

st1 <- st1 |>
  filter(st_is_valid(geometry))


st1 <- st_transform(st1, st_crs(manipur_poly))

fire_data_manipur <- subset(fire_data_manipur, ACQ_DATE >= as.Date("2023-05-01"))
distance <- st_distance(fire_data_manipur, st1)


# Find the minimum distance for each fire
min_distances <- apply(distance, 1, min)

# Define distance thresholds for classification
near_threshold <- 1000  # Define your own threshold values
medium_threshold <- 5000

# Create a classification column in the fire data based on distance
fire_data_manipur$distance_class <- cut(
  min_distances,
  breaks = c(0, near_threshold, medium_threshold, Inf),  # Adjust thresholds as needed
  labels = c("Near", "Medium", "Far"),
  include.lowest = TRUE
)

#
fire_data_manipur_close <- subset(fire_data_manipur, distance_class == "Near")


# Plot fires and buildings on a map
# ggplot() +
#   geom_sf(data = manipur_poly, aes(geometry = geometry),
#           fill = NA, color = "grey10", inherit.aes = FALSE) +
#   geom_sf(data = fire_data_manipur_manipur, aes(color = ACQ_DATE)) +
#   current_theme() +
#   scale_color_date(labels = scales::date_format("%b %d"),
#                    breaks = scales::date_breaks("5 day"),
#                    low = "red",
#                    high = "yellow") +
#   
#   labs(title = "Fires Classified by Distance to Settlements")
# 
# 
# 
# # # plot the buildings data
# # ggplot() +
# #   geom_sf(data = osm_data$osm_polygons, fill = "gray", alpha = 0.5) +
# #   theme_minimal() +
# #   labs(title = "Buildings Extracted from OSM Data")
# 
# #
# ggplot() +
#   
#   geom_sf(data = manipur_poly, aes(
#     geometry = geometry
#   ),
#   fill = NA, color = "grey10", inherit.aes = FALSE) +
#   geom_sf(data = osm_data$osm_polygons, fill = "gray", alpha = 0.5) +
#   theme_minimal() +
#   labs(title = "Buildings Extracted from OSM Data") 





# p1 <- ggmap::ggmap(bg_layer) +
#   geom_point(
#     data = filter(fire_data_manipur, ACQ_DATE == as.Date("2023-05-01") & inside_polygon ==1) ,
#     aes(
#       x = LONGITUDE,
#       y = LATITUDE,
#       fill = "#FF0000", 
#       shape = 2
#     ),
#     inherit.aes = FALSE
#   ) +
#   geom_sf(data = manipur_poly, aes(
#     geometry = geometry
#   ),
#   fill = NA, color = "grey10", inherit.aes = FALSE)+
#   current_theme() +
#   labs(
#     title = "Fires",
#     caption = "Data: NASA FIRMS"
#   ) +
#   scale_shape_manual() +  # Setting the shape value for "Fire" category
#   scale_color_identity()            # Using the specified color directly
  





##################################################################
######################################################################
# specify a theme
current_theme <- function(){
  theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(
        size = 10, color = "grey10"
      ),
      legend.text = element_text(
        size = 8, color = "grey10"
      ),
      plot.title = element_text(
        size = 16, color = "grey10",
        hjust = .5
      ),
      plot.subtitle = element_text(
        face = "bold",
        size = 24, color = "firebrick",
        hjust = .5
      ),
      plot.caption = element_text(
        size = 10, color = "grey30",
        hjust = .5, vjust = 1
      ),
      plot.margin = unit(
        c(t = -2, r = 0, b = -5, l = .1),
        "lines"
      )
    )
}

fire_data_manipur_close$datum <- as.Date(
  fire_data_manipur_close$ACQ_DATE
)


fire_data_manipur_close <- st_intersection(fire_data_manipur_close, manipur_poly)
df1 <- subset(fire_data_manipur_close, ACQ_DATE <= as.Date("2023-05-31"))
p <- ggmap::ggmap(bg_layer) +
  geom_point(
    data = df1,
    aes(
      x = LONGITUDE,
      y = LATITUDE,
      group = datum
    ),
    color = "red",
    inherit.aes = F
  ) +
  geom_sf(data = manipur_poly, aes(geometry = geometry),
         fill = NA, color = "grey10", inherit.aes = FALSE) +

 theme_void() +
  labs(
    title = "Fire incidents in Manipur (May 1 - 31 May, 2023)",
    caption = "Data: NASA FIRMS\n @niruj",
    subtitle = "{as.Date(frame_time)}"
  ) +
  theme(
    plot.title = element_text(
      size = 14
    ),
    plot.subtitle = element_text(
      face = "bold",
      size = 16, color = "firebrick",
    ),
    
  )


# 6. ANIMATE
#-----------

timelapse_map <- p +
  gganimate::transition_time(
    time = as.Date(datum)
  ) +
  gganimate::shadow_mark() +
  gganimate::enter_fade() +
  gganimate::exit_fade() +
  gganimate::ease_aes(
    "cubic-in-out",
    interval = .25
  )

animated_map <- gganimate::animate(
  timelapse_map,
  duration = 30,
  start_pause = 3,
  end_pause = 10,
  width = 7,
  height = 7,
  units = "in",
  res = 300,
  fps = 15,
  renderer = gifski_renderer(
    loop = T
  )
)

library(ragg)
gganimate::anim_save(
  "manipur2.gif",
  animated_map
)


#####################
p2 <- ggmap::ggmap(bg_layer) +
  geom_point(
    data = df1,
    aes(
      x = LONGITUDE,
      y = LATITUDE,
      color = ACQ_DATE,
    ),
    size = 3,
    shape = 17,
    inherit.aes = F
  ) +
  geom_sf(data = manipur_poly, aes(geometry = geometry),
          fill = NA, color = "grey10", inherit.aes = FALSE) +
  
  theme_void() +
  scale_color_date(labels = scales::date_format("%b %d"),
                   breaks = scales::date_breaks("5 day"),
                   low = "red",
                   high = "yellow") +
  labs(
    title = "Fire incidents in Manipur (May 1 - 31 May, 2023)",
    color = "Date",
    caption = "Data: NASA FIRMS\n @niruj",
  ) +

  theme(
    plot.title = element_text(
      size = 13, face = "bold"
    ),
    plot.subtitle = element_text(
      face = "bold",
      size = 16, color = "firebrick",
    ),
    
  )

