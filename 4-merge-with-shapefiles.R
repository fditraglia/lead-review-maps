library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# Get country polygons for all continents excluding Antarctica 
world <- ne_countries(scale = "medium", 
                      returnclass = "sf", 
                      continent = c('North America', 'Africa', 'Europe', 
                                    'Asia', 'South America', 'Oceania'))

# gu_a3 is geounit iso. Use this to merge bllGBD
bllWorld <- world |> 
  left_join(rename(bllGBD, gu_a3 = iso3c))

save(bllWorld, file = './data/bllWorld.RData')
save(aerosol, file = './data/aerosol.RData')
save(bllGBD, file = './data/bllGBD.RData')


# Clean up
rm(list = ls())