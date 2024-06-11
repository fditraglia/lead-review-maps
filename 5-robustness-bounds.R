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
    summarize(pw_avg = sum(popn0019 * percentage) / sum(popn0019)) 
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
  select(-ends_with('integral')) |> 
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
  select(-ends_with('integral')) |> 
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
# GBD data under-estimates the true prevalence of elevated BLLs.

# We operationalize this by scaling up the average BLL reported in a given
# country by some percentage, say 5%. Because our baseline calculations
# rely on the mean BLL and fraction above 5 and 10 micrograms / dl, this 
# sensitivity analysis also requires us to adjust these quantities.

# We do this by carrying out shape-constrained GAM (scam) regressions of the 
# GBD-reported average BLLs (avgbll) on the GBD-reported fractions of BLLs
# that are above 5 and 10 micrograms / dl, i.e. frac5plus and frac10plus.

# We then use the fitted scam regressions to predict how the fractions of BLLs
# above 5 and 10 micrograms / dl would change if the average BLL in a given
# country were scaled up by, say, 5%. 

# We then treat the scaled-up average blls and predicted fractions as the "true"
# BLL data and repeat our baseline calculations using these new data. I.e. we
# fit the parameters of a beta distribution and calculate the integrated IQ loss
# using the point estimate from Crump et al. (2013).

# The shape-constrained GAMs prevent the estimated relationship between 
# frac5plus and avgbll from decreasing at extremely high average BLLs, where we 
# have effectively no data.

scam_5plus <- scam(frac5plus ~ s(avgbll, bs = 'mpi'), data = bllGBD)
scam_10plus <- scam(frac10plus ~ s(avgbll, bs = 'mpi'), data = bllGBD)

# Plot the regression functions 

plot(bllGBD$avgbll, bllGBD$frac5plus, col = 'red', main = '5 plus')
points(bllGBD$avgbll, predict(scam_5plus), col = 'blue', pch = 20)

plot(bllGBD$avgbll, bllGBD$frac10plus, col = 'red', main = '10 plus')
points(bllGBD$avgbll, predict(scam_10plus), col = 'blue', pch = 20)

# Function to "scale up" BLL data
scale_up_BLL <- function(multiplier) {
  bllGBD_scaled <- bllGBD |> 
    mutate(avgbll = avgbll * multiplier) 
 
  bllGBD_scaled |> 
    mutate(frac5plus = predict(scam_5plus, newdata = select(bllGBD_scaled, avgbll)),
           frac10plus = predict(scam_10plus, newdata = select(bllGBD_scaled, avgbll)),
           total5plus = frac5plus * popn0019,
           total10plus = frac10plus * popn0019)
}

# Increase average BLL by 5%; adjust fractions of BLLs above 5 and 10 accordingly
bllGBD_scaled5 <- scale_up_BLL(multiplier = 1.05) |> 
  select(-ends_with('integral')) |> 
  mutate(beta_IQ_integral = map2_dbl(shape1, shape2, get_beta_IQ_integral))

A6 <- bllGBD_scaled5 |>
  make_panel_A(beta_IQ_integral)

B6 <- bllGBD_scaled5 |>
  make_panel_B(beta_IQ_integral)

# Clean up
rm(bllGBD_scaled5)

#------------------------------------------------------------------------------
# Approach (7) - Increase average BLL by 10%
#------------------------------------------------------------------------------

# Increase average BLL by 5%; adjust fractions of BLLs above 5 and 10 accordingly
bllGBD_scaled10 <- scale_up_BLL(multiplier = 1.10) |> 
  select(-ends_with('integral')) |> 
  mutate(beta_IQ_integral = map2_dbl(shape1, shape2, get_beta_IQ_integral))

A7 <- bllGBD_scaled10 |>
  make_panel_A(beta_IQ_integral)

B7 <- bllGBD_scaled10 |>
  make_panel_B(beta_IQ_integral)

# Clean up
rm(bllGBD_scaled10)

#------------------------------------------------------------------------------
# Combine results into tables
#------------------------------------------------------------------------------
method_names <- c('Baseline',  # Approach (1)
                  'Log-normal', # Approach (2)
                  'Lower bound', # Approach (3)
                  'Lower CI', # Approach (4)
                  'Upper CI', # Approach (5)
                  '5% increase', # Approach (6)
                  '10% increase') # Approach (7)

panel_A <- tibble(method = method_names,  
                  total_damage = c(A1, A2, A3, A4, A5, A6, A7))


# Rename the column pw_avg from B1, B2, ..., B7 to match method_names
names(B1)[2] <- method_names[1]
names(B2)[2] <- method_names[2]
names(B3)[2] <- method_names[3]
names(B4)[2] <- method_names[4]
names(B5)[2] <- method_names[5]
names(B6)[2] <- method_names[6]
names(B7)[2] <- method_names[7]

# Merge into a single table of results
panel_B <- B1 |> 
  left_join(B2) |> 
  left_join(B3) |>
  left_join(B4) |>
  left_join(B5) |>
  left_join(B6) |>
  left_join(B7) 

