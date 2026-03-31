#RAP Bareground data prep for land steward reports

library(rapr)
library(tigris)

border.c <- st_transform(border, 4269) #this is the CRS needed for tigris
counties_sf <- counties(cb = TRUE, class = "sf") %>% 
  st_transform(st_crs(border.c))  # match CRS
county <- counties_sf %>%
  st_filter(border.c, .predicate = st_intersects)
county_name<-county$NAME

rap_region<-get_rap(county,
                    product = "vegetation-cover",
                    years = as.numeric(tp_lookup$year_label[length(tp_lookup$year_label)]), #takes the most recent year
                    verbose = FALSE
)

rap_region_avg<-mean(terra::values(rap_region$bare_ground))