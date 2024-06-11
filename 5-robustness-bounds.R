# This script should be re-named and re-numbered when I finish updating it. 
#------------------------------------------------------------------------------
# This script produces two tables of results: 
#
#   Panel A - total global lead damage ($) using different methods / assumptions
#
#   Panel B - same as panel A except population-weighted relative IQ cost broken
#             down by continent.
#
# In each panel we will compare the following methods / assumptions:
#
#   (1) Our "baseline" method: point estimate from Crump et al. (2013), beta
#       distributions, and the true GBD data.
#   (2) Same as (1) except with log-normal distributions.
#   (3) Same as (1) except with a lower bound on IQ loss.
#   (4) Same as (1) except using the lower confidence limit from Crump et al. 
#   (5) Same as (1) except using the upper confidence limit from Crump et al. 
#   (6) Same as (1) except increasing the average BLL by 5% (described below)
#   (7) Same as (1) except increasing the average BLL by 10% (described below)
#------------------------------------------------------------------------------
library(scam) # for shape-constrained GAMs

#------------------------------------------------------------------------------
# Helper functions
#------------------------------------------------------------------------------
make_panel_A <- function(in_data, IQ_integral) {
  in_data |>  
    mutate(dollars = popn0019 * gdpc * relative_iq_cost * {{ IQ_integral }}) |> 
    pull(dollars) |> 
    sum(na.rm = TRUE) 
}

make_panel_B <- function(in_data, IQ_integral) {
  in_data |> 
    mutate(percentage = 100 * relative_iq_cost * {{ IQ_integral }}) |> 
    filter(!is.na(percentage) & !is.na(popn0019) & !is.na(continent)) |> 
    group_by(continent) |>
    summarize(popn_weighted_avg = sum(popn0019 * percentage) / sum(popn0019)) 
}

#------------------------------------------------------------------------------
# Approach (1) - Baseline 
#------------------------------------------------------------------------------
A1 <- bllGBD |> 
  make_panel_A(beta_IQ_integral)

B1 <- bllGBD |> 
  make_panel_B(beta_IQ_integral)


#------------------------------------------------------------------------------
# Approach (2) - Log-normal distributions
#------------------------------------------------------------------------------

A2 <- bllGBD |> 
  make_panel_A(lnorm_IQ_integral)

B2 <- bllGBD |> 
  make_panel_B(lnorm_IQ_integral)

#------------------------------------------------------------------------------
# Approach (3) - Lower bound on IQ loss
#------------------------------------------------------------------------------

A3 <- bllGBD |> 
  make_panel_A(LB_IQ_integral)

B3 <- bllGBD |> 
  make_panel_B(LB_IQ_integral)

#------------------------------------------------------------------------------
# Approach (4) - Lower confidence limit from Crump et al. (beta = 1.88)
#------------------------------------------------------------------------------

Crump_lower <- bllGBD |> 
  mutate(beta_IQ_integral = map2_dbl(shape1, shape2, get_beta_IQ_integral,
                                     my_loss = \(x) iq_loss(x, beta = 1.88))) 

A4 <- Crump_lower |>
  make_panel_A(beta_IQ_integral)

B4 <- Crump_lower |>
  make_panel_B(beta_IQ_integral)
  
# Clean up
rm(Crump_lower)

#------------------------------------------------------------------------------
# Approach (5) - Upper confidence limit from Crump et al. (beta = 4.66)
#------------------------------------------------------------------------------

Crump_upper <- bllGBD |> 
  mutate(beta_IQ_integral = map2_dbl(shape1, shape2, get_beta_IQ_integral,
                                     my_loss = \(x) iq_loss(x, beta = 4.66)))

A5 <- Crump_upper |>
  make_panel_A(beta_IQ_integral)

B5 <- Crump_upper |>
  make_panel_B(beta_IQ_integral)

# Clean up
rm(Crump_upper)

#------------------------------------------------------------------------------
# Approach (6) - Increase average BLL by 5%
#------------------------------------------------------------------------------

# Approaches (6) and (7) are sensitivity analyses in which we suppose that the 
# GBD data under-estimates average BLLs in a country by some fraction.

# Shape-constrained GAM (scam) regressions of avgbll on frac5plus and frac10plus: 
# needed to predict how the fraction with BLLs >=5 and >=10 would differ in a regime where every 
# country had a higher average BLL. 

# Unlike LOESS, SCAM can impose monotonicity. We need this to prevent the 5plus
# regression function from decreasing slightly at extremely high BLLs where 
# we have practically no data.
scam_5plus <- scam(frac5plus ~ s(avgbll, bs = 'mpi'), data = bllGBD)
scam_10plus <- scam(frac10plus ~ s(avgbll, bs = 'mpi'), data = bllGBD)

# Plot the regression functions 

plot(bllGBD$avgbll, bllGBD$frac5plus, col = 'red', main = '5 plus')
points(bllGBD$avgbll, predict(scam_5plus), col = 'blue', pch = 20)

plot(bllGBD$avgbll, bllGBD$frac10plus, col = 'red', main = '10 plus')
points(bllGBD$avgbll, predict(scam_10plus), col = 'blue', pch = 20)

#------------------------------------------------------------------------------
# Approach (7) - Increase average BLL by 10%
#------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# OLD STUFF BELOW HERE
#-------------------------------------------------------------------------------

# This script carries out a sensitivity analysis to see how changes to the 
# BLL data from the GBD would affect our overall results. In particular, we
# will *scale up* the average BLL by a certain number of percentage points, e.g.
# 5%, 10%, etc. We will then estimate the IQ loss assuming that these BLL values
# are the true ones. This will give us a sense of how sensitive our results are
# to the underlying GBD data.



                        
  

# Calculate population-weighted relative IQ cost by continent
bllGBD |> 
  filter(!is.na(relative_iq_cost) & !is.na(beta_IQ_integral) & !is.na(popn0019) & !is.na(continent)) |> 
  group_by(continent) |> 
  summarize(relative_iq_cost = 100 * sum(popn0019 * relative_iq_cost * beta_IQ_integral) / sum(popn0019)) 


# Predict frac5plus and frac10plus using the shape-constrained GAM regressions 
# if all countries have their BLL scaled by multiplier

# Increase average bll by 5% 
#multiplier <- 1.05 


#bllGBD_scaled <- bllGBD |> 
#  mutate(avgbll = avgbll * multiplier)
         
#bllGBD_scaled <- bllGBD_scaled |> 
#  mutate(frac5plus = predict(scam_5plus, newdata = select(bllGBD_scaled, avgbll)),
#         frac10plus = predict(scam_10plus, newdata = select(bllGBD_scaled, avgbll)),
#         total5plus = frac5plus * popn0019,
#         total10plus = frac10plus * popn0019)

# Clean up and replace overwrite the "true" GBD BLL data with our scaled version  
#bllGBD <- bllGBD_scaled
#rm(aerosol, loess_5plus, loess_10plus, bllGBD_scaled)


#----------------- Compare headline numbers between "true" and scaled-up BLLs



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


#rm(list = ls())