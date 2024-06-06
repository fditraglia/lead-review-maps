# This script calculates IQ losses using the functions from 0-helper-functions.R
# and the GBD BLL data that are loaded in cleaned in 1-load-GDB-data.R

#-------------------------------------------------------------------------------
# To use the above relationship to calculate total (per person) IQ loss we need
# the *distribution* of BLLs, not merely simple summary statistics. In principal
# we could try to access the underlying models used by the GBD to construct this
# information for ourselves, but that would be very time-consuming. Instead we 
# consider three simpler approaches that fit a parametric distribution to the
# observed summary statistics:
#
# (1) A log-normal distribution with a mean equal to the empirical mean and 
#     scale parameter chosen to minimize the sum of squared errors of empirical
#     versus theoretical P(BLL > 5) and P(BLL > 10)
#
# (2) Ditto with a Beta distribution supported on [0, 100]
#
# (3) A three point discrete distribution. Put P(X > 10) probability on 10 and
#     P(X > 5) - P(X > 10) on 5. Ignore any costs from BLLs below 5 to get a 
#     *lower* bound 
#
# The first two of these are implemented using the following functions from 
# 0-helper-functions.R:
#
#   get_lnorm_params()
#   get_beta_params()
#
# The third is implemented below 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Compute beta parameters for all countries
#-------------------------------------------------------------------------------
bllGBD <- bllGBD |> 
  select(avgbll, frac5plus, frac10plus) |> 
  pmap_dfr(get_beta_params) |> 
  bind_cols(bllGBD) 

#-------------------------------------------------------------------------------
# Compute lognormal parameters for all countries
#-------------------------------------------------------------------------------
bllGBD <- bllGBD |> 
  select(avgbll, frac5plus, frac10plus) |> 
  pmap_dfr(get_lnorm_params) |> 
  bind_cols(bllGBD) 

#-------------------------------------------------------------------------------
# Compute integrated IQ loss for all countries: Beta version
#-------------------------------------------------------------------------------

bllGBD <- bllGBD |> 
  mutate(beta_IQ_integral = map2_dbl(shape1, shape2, get_beta_IQ_integral))

#-------------------------------------------------------------------------------
# Compute integrated IQ loss for all countries: Lognormal version
#-------------------------------------------------------------------------------

bllGBD <- bllGBD |> 
  mutate(lnorm_IQ_integral = map2_dbl(meanlog, sdlog, get_lnorm_IQ_integral))

#-------------------------------------------------------------------------------
# Compute lower bound on IQ loss: assign BLL of 10 to everyone above 10, 5 to
# everyone between 5 and 10, and zero to everyone else.
#-------------------------------------------------------------------------------
bllGBD <- bllGBD |>  
  mutate(LB_IQ_integral = (frac5plus - frac10plus) * iq_loss(5) + 
           frac10plus * iq_loss(10))




