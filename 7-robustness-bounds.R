# This script carries out a sensitivity analysis to see how changes to the 
# BLL data from the GBD would affect our overall results. In particular, we
# will *scale up* the average BLL by a certain number of percentage points, e.g.
# 5%, 10%, etc. We will then estimate the IQ loss assuming that these BLL values
# are the true ones. This will give us a sense of how sensitive our results are
# to the underlying GBD data.

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Increase average bll by 5% 
multiplier <- 1.05 

#----------------- Same prelim data cleaning as before 
source('1-prep-data.R') 

# LOESS regression of avgbll on frac5plus and frac10plus: needed to predict how
# the fraction with BLLs >=5 and >=10 would differ in a regime where every 
# country had a higher average BLL 
loess_5plus <- loess(frac5plus ~ avgbll, data = bllGBD)
loess_10plus <- loess(frac10plus ~ avgbll, data = bllGBD)


# Predict frac5plus and frac10plus using the LOESS regressions if all countries
# have their BLL scaled by multiplier

bllGBD_scaled <- bllGBD |> 
  mutate(avgbll = avgbll * multiplier)
         
bllGBD_scaled <- bllGBD_scaled |> 
  mutate(frac5plus = predict(loess_5plus, newdata = select(bllGBD_scaled, avgbll)),
         frac10plus = predict(loess_10plus, newdata = select(bllGBD_scaled, avgbll)),
         total5plus = frac5plus * popn0019,
         total10plus = frac10plus * popn0019)

# Clean up and replace overwrite the "true" GBD BLL data with our scaled version  
bllGBD <- bllGBD_scaled
rm(aerosol, loess_5plus, loess_10plus, bllGBD_scaled)

#---------------- Run steps 2 and 3 as before
source('2-calculate-IQ-loss.R')
source('3-returns-to-iq.R')

#---------------- Merge with shapefiles and save results

# Get country polygons for all continents excluding Antarctica 
world <- ne_countries(scale = "medium", 
                      returnclass = "sf", 
                      continent = c('North America', 'Africa', 'Europe', 
                                    'Asia', 'South America', 'Oceania'))

# gu_a3 is geounit iso. Use this to merge bllGBD
bllWorld <- world |> 
  left_join(rename(bllGBD, gu_a3 = iso3c))

save(bllWorld, file = './data/bllWorld-scaled-up.RData')
save(bllGBD, file = './data/bllGBD-scaled-up.RData')

rm(bllGBD, bllWorld, world)

#----------------- Compare headline numbers between "true" and scaled-up BLLs

# scaled up BLLs 
load('./data/bllWorld-scaled-up.RData')
bllWorld_scaled <- bllWorld
rm(bllWorld)

# "true" data
load('./data/bllWorld.RData')


get_headline_stats <- function(in_data) {
  in_data |> 
    mutate(percentage = 100 * relative_iq_cost * beta_IQ_integral,
         dollars = popn0019 * gdpc * relative_iq_cost * beta_IQ_integral) |>
  select(percentage, dollars) |> 
  as_tibble() |> 
  summarize(median_percentage = median(percentage, na.rm = TRUE), 
            total_dollars = sum(dollars, na.rm = TRUE)) 
}

get_headline_stats(bllWorld)

get_headline_stats(bllWorld_scaled)


rm(list = ls())