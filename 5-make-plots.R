library(sf)
library(tidyverse)
load('data/bllWorld.RData')

# Set ggplot theme
theme_set(theme_bw())

# Fraction with BLL > 5 micrograms / deciliter
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac5plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", trans = "sqrt") 
ggsave('output/frac5plus.pdf', width = 7, height = 5)

# Fraction with BLL > 10 micrograms / deciliter
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac10plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", trans = "sqrt") 
ggsave('output/frac10plus.pdf', width = 7, height = 5)

# Check which continent Greenland is assigned to
bllWorld |> 
  filter(gu_a3 == 'GRL') |> 
  select(gu_a3, continent)

# relative_iq_cost is highly symmetric so use arithmetic mean to impute missings
bllWorld <- bllWorld |> 
  group_by(continent) |> 
  mutate(relative_iq_cost_continent = mean(relative_iq_cost, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(relative_iq_cost = if_else(!is.na(relative_iq_cost),
                                    relative_iq_cost,
                                    relative_iq_cost_continent)) |> 
  # It doesn't make sense to compute this average for Greenland since
  # its continent is North America
  mutate(relative_iq_cost = if_else(gu_a3 == 'GRL', NA, relative_iq_cost)) 

bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * beta_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '%', trans = 'sqrt')
ggsave('output/relative_iq_cost.pdf', width = 7, height = 5)

#----------------------- Robustness Checks
# Lognormal
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * lnorm_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '%', trans = 'sqrt')

# Lower bound 
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * LB_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '%', trans = 'sqrt')


rm(list = ls())