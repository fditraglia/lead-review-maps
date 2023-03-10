library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Set ggplot theme
theme_set(theme_bw())

# Get country polygons for all continents excluding Antarctica 
world <- ne_countries(scale = "medium", 
                      returnclass = "sf", 
                      continent = c('North America', 'Africa', 'Europe', 
                                    'Asia', 'South America', 'Oceania'))

# A very simple test map: it seems like we can match on iso3
#ggplot(data = world) +
#  geom_sf(aes(fill = pop_est)) +
#  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Presumably these plots are all going to look really similar. We may want to 
# rethink how we use our remaining figures. Alternatively, we may want do make
# the plots by continent?

# gu_a3 appears to be geounit iso
world |> 
  left_join(select(bllGBD, gu_a3 = iso3c, frac5plus)) |> 
  relocate(frac5plus) |> 
  ggplot() + 
  geom_sf(aes(fill = frac5plus)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

