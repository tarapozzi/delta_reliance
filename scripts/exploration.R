# Initial data exploration
library(tidyverse)

d <- read.csv("data/full_distribution_dataset.csv")
swp_cws <- read.csv("data/swp_systems.csv")
cvp_cws <- read.csv("data/cvp_systems.csv")

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
