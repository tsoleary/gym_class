# ------------------------------------------------------------------------------
# Gym Class - NYT Upshot map
# February 23, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

# Load packages
require(tidyverse)
require(urbnmapr)

# Load data
dat <- read_delim("~/Downloads/CountyData.tsv", delim = "\t")


# Plot an empty map of the US
ggplot() + 
  geom_polygon(data = urbnmapr::states, 
               mapping = aes(x = long, y = lat, group = group),
		           fill = “grey”, 
		           color = “white”) +
  coord_map(projection = "albers", 
            lat0 = 39, 
            lat1 = 45)

# Merge state
