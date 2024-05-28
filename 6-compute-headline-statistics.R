library(tidyverse)
load('data/bllWorld.RData')

# Compute summary statistics for the plots
# Across countries for countries with GBD data.
bllWorld |> 
  mutate(percentage = 100 * relative_iq_cost * beta_IQ_integral) |> 
  pull(percentage) |> 
  quantile(na.rm = TRUE) # median of 7.2  (versus 2.4 with Lanphear IQ to BLL)

# Try to calculate an aggregate figure for the world as a whole. 
bllWorld |>  
  mutate(dollars = popn0019 * gdpc * relative_iq_cost * beta_IQ_integral) |> 
  pull(dollars) |> 
  sum(na.rm = TRUE) # 2.9 trillion (versus 1.2 Trillion with Lanphear IQ to BLL)


# Compute summary statistics for the plots
# Across countries for countries with GBD data.
bllWorld |> 
  mutate(percentage = 100 * relative_iq_cost * lnorm_IQ_integral) |> 
  pull(percentage) |> 
  quantile(na.rm = TRUE) # median 3.4 (versus 1.32 with Lanphear IQ to BLL)

# Try to calculate an aggregate figure for the world as a whole. 
bllWorld |>  
  mutate(dollars = popn0019 * gdpc * relative_iq_cost * lnorm_IQ_integral) |> 
  pull(dollars) |> 
  sum(na.rm = TRUE) # 1.5 trillion (versus 812 billion with Lanphear IQ to BLL)


# Compute summary statistics for the plots
# Across countries for countries with GBD data.
bllWorld |> 
  mutate(percentage = 100 * relative_iq_cost * LB_IQ_integral) |> 
  pull(percentage) |> 
  quantile(na.rm = TRUE)

# Try to calculate an aggregate figure for the world as a whole. 
bllWorld |>  
  mutate(dollars = popn0019 * gdpc * relative_iq_cost * LB_IQ_integral) |> 
  pull(dollars) |> 
  sum(na.rm = TRUE) # 656 billion (versus 339 billion with Lanphear IQ to BLL)

rm(list = ls())
