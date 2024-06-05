# This script tests the functions in 0-helper-functions.R, comparing the 
# performance of different methods for approximating the BLL distribution. None
# of the other scripts in the project depend on this one.

library(GGally)

# these values are approximately those of China
get_beta_params(3.39, 0.0929, 0.000122)
p <- seq(0, 1, by = 0.001)
plot(100 * p, dbeta(p, 8.20374, 233.49448), type = 'l') 
pbeta(c(5, 10) / 100, 8.20374, 233.49448, lower.tail = FALSE)

# these values are approximately those of Zimbabwe
get_beta_params(9, 0.7, 0.3)
plot(100 * p, dbeta(p, 1.81, 18.32), type = 'l') 
pbeta(c(5, 10) / 100, 1.81, 18.32, lower.tail = FALSE)

# Log-normal version

# Seems like lognormal has trouble approximating dist for places with
# lower means / shares 5+.

# these values are approximately those of China
get_lnorm_params(3.39, 0.0929, 0.000122)
x <- seq(0, 20, by = 0.1)
plot(x, dlnorm(x, -2.25, 2.63), type = 'l') 
plnorm(c(5, 10), -2.25, 2.63, lower.tail = FALSE)

# ditto Zimbabwe
get_lnorm_params(9, 0.7, 0.3)
x <- seq(0, 20, by = 0.1)
plot(x, dlnorm(x, 1.96, 0.68), type = 'l') 
plnorm(c(5, 10), 1.96, 0.68, lower.tail = FALSE)

# Compare results of Beta and Lognormal using IL aggregate data
IL <- read_csv('./data-raw/IL_aggregate_distribution.csv')

# drop_zipsbelowXXX indicates results that exclude kids born in zip codes with
# less than XXX% of children screened by 25 months of age 

#-------------------------------------------------------------------------------
# Compare the beta and lognormal approximations to the *actual* BLL distribution
# for Illinois.
#-------------------------------------------------------------------------------
IL_summary <- IL |> 
  rename(total = num_drop_zipsbelow0) |> 
  summarize(avgbll = sum(maxbll * total / sum(total)),
            # both of these are *strictly* greater to match GBD
            frac5plus = sum((maxbll > 5) * total / sum(total)), 
            frac10plus = sum((maxbll > 10) * total / sum(total))) 

IL_beta <- with(IL_summary, get_beta_params(avgbll, frac5plus, frac10plus))
IL_lnorm <- with(IL_summary, get_lnorm_params(avgbll, frac5plus, frac10plus))

IL_survival <- function(x) {
  maxbll <- IL$maxbll
  total <- IL$num_drop_zipsbelow0
  sum((maxbll > x) * total / sum(total))
}
IL_survival_vec <- Vectorize(IL_survival)

# How do the survival functions differ?
bll_seq <- seq(5, 50, by = 0.1)
plot(bll_seq, IL_survival_vec(bll_seq), type = 's', xlab = 'BLL', ylab = 'P(X>BLL)')
IL_lnorm_survival <- plnorm(bll_seq, meanlog = IL_lnorm[1],  sdlog = IL_lnorm[2], 
                            lower.tail = FALSE)
IL_beta_survival <- pbeta(bll_seq / 100, shape1 = IL_beta[1], shape2 = IL_beta[2],
                          lower.tail = FALSE)
points(bll_seq, IL_beta_survival, type = 'l', col = 'red')
points(bll_seq, IL_lnorm_survival, type = 'l', col = 'blue')

# How to the IQ costs differ? (only look at upper tail)
lnorm_trunc <- function(x) {
# truncated lnorm density 
  dlnorm(x, meanlog = IL_lnorm[1], sdlog = IL_lnorm[2]) / 
    plnorm(5, meanlog = IL_lnorm[1], sdlog = IL_lnorm[2], lower.tail = FALSE)
}

integrate(lnorm_trunc, 5, Inf) # sanity check

beta_trunc <- function(x) {
# truncated beta density on 0 to 100
  dbeta(x / 100, shape1 = IL_beta[1], shape2 = IL_beta[2]) /
    pbeta(5 / 100, shape1 = IL_beta[1], shape2 = IL_beta[2], lower.tail = FALSE) / 
    100 # don't forget Jacobian term!
}

integrate(beta_trunc, 5, Inf) # sanity check

# The beta distribution underestimates by around 25% whereas the lognormal
# overestimates by around 140%. Seems safer to go with the beta.
IL |> 
  rename(total = num_drop_zipsbelow0) |> 
  filter(maxbll > 5) |> 
  summarize(sum(iq_loss(maxbll) * total / sum(total))) |> 
  pull()

integrate(function(x) lnorm_trunc(x) * iq_loss(x), 5, Inf)
integrate(function(x) beta_trunc(x) * iq_loss(x), 5, Inf)

# clean up
rm(beta_trunc, lnorm_trunc, IL_survival, IL_survival_vec, bll_seq, IL_beta,
   IL_beta_survival, IL_lnorm, IL_lnorm_survival, IL_summary, IL, p, x)


#-------------------------------------------------------------------------------
# Compute beta parameters for all countries
#-------------------------------------------------------------------------------
bllGBD <- bllGBD |> 
  select(avgbll, frac5plus, frac10plus) |> 
  pmap_dfr(get_beta_params) |> 
  bind_cols(bllGBD) 
#relocate(shape1, shape2, .after = frac10plus)

#-------------------------------------------------------------------------------
# Compute lognormal parameters for all countries
#-------------------------------------------------------------------------------
bllGBD <- bllGBD |> 
  select(avgbll, frac5plus, frac10plus) |> 
  pmap_dfr(get_lnorm_params) |> 
  bind_cols(bllGBD) |> 
  relocate(meanlog, sdlog, .after = shape2)

#-------------------------------------------------------------------------------
# Compute integrated IQ loss for all countries: Beta version
#-------------------------------------------------------------------------------
get_beta_IQ_integral <- function(shape1, shape2) {
  # Beta density on [0, 100]
  f <- \(x) dbeta(x / 100, shape1, shape2) / 100 # Jacobian!
  integral <- integrate(\(x) f(x) * iq_loss(x), 0, Inf)
  return(integral$value)
}

bllGBD <- bllGBD |> 
  mutate(beta_IQ_integral = map2_dbl(shape1, shape2, get_beta_IQ_integral))

#-------------------------------------------------------------------------------
# Compute integrated IQ loss for all countries: Lognormal version
#-------------------------------------------------------------------------------
get_lnorm_IQ_integral <- function(meanlog, sdlog) {
  # Beta density on [0, 100]
  f <- \(x) dlnorm(x, meanlog, sdlog) 
  integral <- integrate(\(x) f(x) * iq_loss(x), 0, Inf)
  return(integral$value)
}

bllGBD <- bllGBD |> 
  mutate(lnorm_IQ_integral = map2_dbl(meanlog, sdlog, get_lnorm_IQ_integral))


#-------------------------------------------------------------------------------
# Compute lower bound on IQ loss: assign BLL of 10 to everyone above 10, 5 to
# everyone between 5 and 10, and zero to everyone else.
#-------------------------------------------------------------------------------
bllGBD <- bllGBD |>  
  mutate(LB_IQ_integral = (frac5plus - frac10plus) * iq_loss(5) + 
           frac10plus * iq_loss(10))

bllGBD |> 
  select(ends_with('integral')) |> 
  ggpairs() 

# clean up
rm(get_beta_IQ_integral, get_beta_params, get_lnorm_params, get_lnorm_IQ_integral)



