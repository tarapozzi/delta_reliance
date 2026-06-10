# Initial data exploration
library(tidyverse)

d <- read.csv("data/full_distribution_dataset.csv")
swp_cws <- read.csv("data/swp_systems.csv")
cvp_cws <- read.csv("data/cvp_systems.csv")
delta_cws <- read_csv("data/delta_cws.csv")
swp <- d %>% 
  filter(swp_loop == "Yes") %>%
  select(System_ID, system_lat, system_lon, county, swp_loop)

write.csv(swp, "data/swp_systems_wcounty.csv")

swp_counties <- unique(swp$county)

cvp <- d %>% 
  filter(cvp_loop == "Yes") %>%
  select(System_ID, system_lat, system_lon, county, cvp_loop)
write.csv(cvp, "data/cvp_systems_wcounty.csv")

cvp_counties <- unique(cvp$county)


export_counties <- union(swp_counties, cvp_counties)


### Loading Data and Packages
library(sf)
library(tidyverse)

# CWS shapefile — select WS_downloadable_final_081024.shp
my_data <- st_read(file.choose())

# Legal Delta boundary — select i03_LegalDeltaBoundary.shp
legal_delta <- st_read(file.choose())

#Delta CWS
delta_cws <- read_csv("data/delta_cws.csv")



### Reprojecting and conversion in shape files
# See column names only
names(my_data)

my_data_wgs84 <- st_transform(my_data, crs = 4326)
my_data_wgs84 <- st_make_valid(my_data_wgs84)  # fixes any malformed polygon shapes
legal_delta    <- st_transform(legal_delta, crs = st_crs(my_data_wgs84))



### Centroid Coordinates

centroids <- st_centroid(my_data_wgs84)
my_data_wgs84$longitude <- st_coordinates(centroids)[, 1]
my_data_wgs84$latitude  <- st_coordinates(centroids)[, 2]

# Now try centroids again
centroids <- st_centroid(my_data_wgs84)
my_data_wgs84$longitude <- st_coordinates(centroids)[, 1]
my_data_wgs84$latitude  <- st_coordinates(centroids)[, 2]




### Identify CWS Within Legal Delta
# TRUE (inside) and FALSE (outside)
my_data_wgs84$in_legal_delta <- lengths(st_intersects(my_data_wgs84, legal_delta)) > 0

table(my_data_wgs84$in_legal_delta)  # quick count of how many are in/out



### Join Coordinates and Delta to CSV
# Match rows between the shapefile and CSV using the system ID,
# then fill in coordinates — using shapefile centroids where available,
# and falling back to whatever lat/long was already in the CSV

delta_cws_joined <- delta_cws %>%
  mutate(cws_ID = as.character(cws_ID)) %>%
  left_join(
    st_drop_geometry(my_data_wgs84) %>%
      select(Sys_ID, latitude, longitude, in_legal_delta),
    by = c("cws_ID" = "Sys_ID")
  ) %>%
  mutate(
    final_lat  = ifelse(is.na(latitude), lat, latitude),
    final_long = ifelse(is.na(longitude), long, longitude))




### Export Outputs


write_csv(delta_cws_joined, "delta_cws_final.csv")          # CSV version
st_write(my_data_wgs84, "cws_with_legal_delta_flag.shp")    # shapefile version
getwd()


### Map: CWS Within Legal Delta 
# Blue = legal delta boundary, Red = CWS inside the delta

ggplot() +
  geom_sf(data = legal_delta, fill = "lightblue", alpha = 0.5) +
  geom_sf(data = my_data_wgs84 %>% filter(in_legal_delta == TRUE),
          fill = "red", alpha = 0.7) +
  coord_sf(xlim = c(-122.3, -120.9), ylim = c(37.4, 38.8)) +
  labs(title = "Community Water Systems Within the Legal Delta") +
  theme_classic()

names(delta_cws_joined)
unique(delta_cws_joined$primary_water_source_type)
unique(delta_cws_joined$project_water)


# Filter to only legal delta CWS with coordinates
delta_map <- delta_cws_joined %>%
  filter(in_legal_delta == TRUE, !is.na(final_lat), !is.na(final_long)) %>%
  mutate(
    # Simplify into cleaner categories. Is this correct???
    water_source = case_when(
      primary_water_source_type == "GW"  ~ "Groundwater",
      primary_water_source_type == "GWP" ~ "Groundwater",
      primary_water_source_type == "GU"  ~ "Groundwater",
      primary_water_source_type == "SW"  ~ "Surface Water",
      primary_water_source_type == "SWP" ~ "Surface Water",
      TRUE ~ "Unknown"
    ),
    # Add SWP/CVP label for surface water systems
    project = case_when(
      project_water == "SWP"     ~ "SWP",
      project_water == "CVP"     ~ "CVP",
      project_water == "Neither" ~ "No Project Water",
      water_source == "Groundwater" ~ "Groundwater",
      TRUE ~ "Unknown"
    )
  )

# Convert to spatial object for mapping
delta_map_sf <- st_as_sf(delta_map, coords = c("final_long", "final_lat"), crs = 4326)

# Legal Delta CWS by water source Map
ggplot() +
  geom_sf(data = legal_delta, fill = "lightblue", alpha = 0.3, color = "steelblue") +
  geom_sf(data = delta_map_sf, aes(color = project), size = 3, alpha = 0.8) +
  scale_color_manual(values = c(
    "SWP"              = "darkorange",
    "CVP"              = "purple",
    "No Project Water" = "forestgreen",
    "Groundwater"      = "brown",
    "Unknown"          = "grey50"
  )) +
  coord_sf(xlim = c(-122.3, -120.9), ylim = c(37.4, 38.8)) +
  labs(
    title = "Legal Delta CWS by Primary Water Source",
    subtitle = "Surface water systems labeled by project (SWP/CVP)",
    color = "Water Source / Project"
  ) +
  theme_classic()
