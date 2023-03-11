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

# Presumably these plots are all going to look really similar. We may want to 
# rethink how we use our remaining figures. Alternatively, we may want do make
# the plots by continent?

# gu_a3 is geounit iso. Use this to merge bllGBD
world <- world |> 
  left_join(rename(bllGBD, gu_a3 = iso3c))

# Fraction with BLL > 5 micrograms / deciliter
world |> 
  ggplot() + 
  geom_sf(aes(fill = frac5plus)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Fraction with BLL > 10 micrograms / deciliter
world |> 
  ggplot() + 
  geom_sf(aes(fill = frac10plus)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Average IQ lost per individual aged 0-19: beta approximation
world |> 
  ggplot() + 
  geom_sf(aes(fill = beta_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# $ value of IQ lost per individual aged 0-19: beta approximation
world |> 
  ggplot() + 
  geom_sf(aes(fill = beta_returns_no_lead)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Average IQ lost per individual aged 0-19: lognormal approximation
world |> 
  ggplot() + 
  geom_sf(aes(fill = lnorm_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# $ value of IQ lost per individual aged 0-19: lognormal approximation
world |> 
  ggplot() + 
  geom_sf(aes(fill = lnorm_returns_no_lead)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Average IQ lost per individual aged 0-19: lower bound 
world |> 
  ggplot() + 
  geom_sf(aes(fill = LB_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# $ value of IQ lost per individual aged 0-19: lower bound 
world |> 
  ggplot() + 
  geom_sf(aes(fill = LB_returns_no_lead)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


# $ value of IQ lost per individual aged 0-19: $1000 per lost IQ point rather 
# than a figure based on returns to education, because these are missing in
# many countries where lead is a big problem

# Beta 
world |> 
  ggplot() + 
  geom_sf(aes(fill = beta_IQ_integral * 1000)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Lognormal 
world |> 
  ggplot() + 
  geom_sf(aes(fill = lnorm_IQ_integral * 1000)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Lower bound
world |> 
  ggplot() + 
  geom_sf(aes(fill = LB_IQ_integral * 1000)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")
