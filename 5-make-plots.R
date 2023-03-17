library(sf)
library(tidyverse)
load('data/bllWorld.RData')

# Set ggplot theme
theme_set(theme_bw())

# Fraction with BLL > 5 micrograms / deciliter
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = frac5plus)) +
  scale_fill_viridis_c(name="Share >5", option = "plasma", trans = "sqrt") 
ggsave('output/frac5plus.pdf', width = 7, height = 5)

# Fraction with BLL > 10 micrograms / deciliter
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = frac10plus)) +
  scale_fill_viridis_c(name="Share >10",option = "plasma", trans = "sqrt")
ggsave('output/frac10plus.pdf', width = 7, height = 5)

# $ value of IQ lost per individual aged 0-19: beta approximation
#bllWorld |> 
#  ggplot() + 
#  geom_sf(aes(fill = beta_returns_no_lead)) +
#  scale_fill_viridis_c(name="Per Capita Loss $", option = "plasma", trans = "sqrt")
#ggsave('output/beta_returns_no_lead.png', width = 7, height = 5)

 
# Check which continent Greenland is assigned to
bllWorld |> 
  filter(gu_a3 == 'GRL') |> 
  select(gu_a3, continent)

# Replace the missing values with the geometric mean of the continent average
gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

bllWorld <- bllWorld |> 
  group_by(continent) |> 
  mutate(dollars_per_IQ_continent = gm_mean(dollars_per_IQ)) |> 
  ungroup() |> 
  mutate(dollars_per_IQ = if_else(!is.na(dollars_per_IQ),
                                  dollars_per_IQ, 
                                  dollars_per_IQ_continent)) |> 
  # It doesn't make sense to compute this average for Greenland since
  # its continent is North America
  mutate(dollars_per_IQ = if_else(gu_a3 == 'GRL', NA, 
                                  dollars_per_IQ)) 

bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = dollars_per_IQ * beta_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '$US', trans = 'sqrt') +
  labs(title = 'Dollar value of IQ loss')
ggsave('output/IQcost.pdf', width = 7, height = 5)

rm(bllWorld)
load('./data/bllGBD.RData')

# Compute dollar value of IQ loss: per person, lifetime discounted 
bllGBD |> 
  filter(!is.na(dollars_per_IQ)) |>  
  summarize(lower_bound = sum(dollars_per_IQ * popn0019 * LB_IQ_integral) / sum(popn0019),
            beta_approx = sum(dollars_per_IQ * popn0019 * beta_IQ_integral) / sum(popn0019),
            lnorm_approx = sum(dollars_per_IQ * popn0019 * lnorm_IQ_integral) / sum(popn0019))

# Total rather than per person
bllGBD |> 
  filter(!is.na(dollars_per_IQ)) |>  
  summarize(lower_bound = sum(dollars_per_IQ * popn0019 * LB_IQ_integral),
            beta_approx = sum(dollars_per_IQ * popn0019 * beta_IQ_integral),
            lnorm_approx = sum(dollars_per_IQ * popn0019 * lnorm_IQ_integral))


rm(bllGBD)
