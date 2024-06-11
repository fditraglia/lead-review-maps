# Compute summary statistics for the plots
# Across countries for countries with GBD data.

bllGBD |> 
  mutate(percentage = 100 * relative_iq_cost * beta_IQ_integral) |> 
  pull(percentage) |> 
  quantile(na.rm = TRUE) # median of 7.07  (versus 2.4 with Lanphear IQ to BLL)

# Try to calculate an aggregate figure for the world as a whole. 
bllGBD |>  
  mutate(dollars = popn0019 * gdpc * relative_iq_cost * beta_IQ_integral) |> 
  pull(dollars) |> 
  sum(na.rm = TRUE) # 3.18 trillion (versus 1.2 Trillion with Lanphear IQ to BLL)


# Compute summary statistics for the plots
# Across countries for countries with GBD data.
bllGBD |> 
  mutate(percentage = 100 * relative_iq_cost * lnorm_IQ_integral) |> 
  pull(percentage) |> 
  quantile(na.rm = TRUE) # median 2.59 (versus 1.32 with Lanphear IQ to BLL)

# Try to calculate an aggregate figure for the world as a whole. 
bllGBD |>  
  mutate(dollars = popn0019 * gdpc * relative_iq_cost * lnorm_IQ_integral) |> 
  pull(dollars) |> 
  sum(na.rm = TRUE) # 1.66 trillion (versus 812 billion with Lanphear IQ to BLL)


# Compute summary statistics for the plots
# Across countries for countries with GBD data.
bllGBD |> 
  mutate(percentage = 100 * relative_iq_cost * LB_IQ_integral) |> 
  pull(percentage) |> 
  quantile(na.rm = TRUE) # median 0.85

# Try to calculate an aggregate figure for the world as a whole. 
bllGBD |>  
  mutate(dollars = popn0019 * gdpc * relative_iq_cost * LB_IQ_integral) |> 
  pull(dollars) |> 
  sum(na.rm = TRUE) # 726 billion (versus 339 billion with Lanphear IQ to BLL)

