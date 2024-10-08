# This script contains helper functions that are used in the main scripts

#-------------------------------------------------------------------------------
# IQ points lost as a function of BLL (Crump et al 2013)
# 
#   We reverse the sign from Crump et al 2013, working with positive values of
#   IQ points *lost*. We take our values from their Table 5, in particular from
#   the row "Lifetime / Ln(BPb + 1)" which uses "average lifetime BPb (the mean 
#   BPb from 6 months to the time of the IQ test)" as the measure of blood lead
#   levels "BPd" and transforms this measure according to log(x + 1) before 
#   estimating a linear regression on IQ scores "adjusted for site, HOME score,
#   birth weight, maternal IQ, maternal education, maternal alcohol, maternal
#   tobacco usage, and birth order." The point of beta from this source is 3.246 
#   with a 95% confidence interval of [1.833, 4.659]. Note that the value of 
#   the lower confidence limit from the Appendix to Larsen & Sanchez-Triana 2023
#   contains a typo: they quote this value as 1.88. The following function 
#   defaults to the point estimate from Crump et al 2013 but allows this value
#   to be changed for use in our robustness exercises.
#-------------------------------------------------------------------------------
iq_loss <- function(bll, beta = 3.246) {
  beta * log(bll + 1)
}

#-------------------------------------------------------------------------------
# Function to fit lognormal distribution from empirical moments::
#
#   (1) The mean BLL
#   (2) The fraction of BLLs greater than 5 micrograms/decliter
#   (3) The fraction of BLLs greater than 10 micrograms/decliter
#
# E(X) = exp(m + s^2 / 2)
# 
# Solving this for sigma in terms of mu and E(X) gives:
#
# s = 2 * sqrt((log E(X) - m))
#
# We set sigma equal to this value and treat mu as a free parameter subject to
# the constrain m >= log E(X) to ensure that s is positive. This ensures that
# the theoretical mean matches the empirical mean and gives us a problem with
# a single unknown parameter: m.
#-------------------------------------------------------------------------------
get_lnorm_params <- function(avgbll, frac5plus, frac10plus) {
  f <- function(m) {
    theoretical <- plnorm(c(5, 10), 
                         meanlog = m,
                         sdlog = sqrt(2 * (log(avgbll) - m)), 
                         lower.tail = FALSE)
    sum((theoretical - c(frac5plus, frac10plus))^2)
  }
  m_star <- optimize(f, c(-100, log(avgbll)))$minimum
  s_star <- sqrt(2 * (log(avgbll) - m_star))
  return(c(meanlog = m_star, sdlog = s_star))
}

#-------------------------------------------------------------------------------
# Function to fit Beta distribution distribution from empirical moments:
#
#   (1) The mean BLL
#   (2) The fraction of BLLs greater than 5 micrograms/decliter
#   (3) The fraction of BLLs greater than 10 micrograms/decliter
#
# Let a denote `shape1` and b denote `shape2` in `dbeta`. We use the fact that
# E(X) = [a / (a + b)] to *fix* the relationship between a and b by imposing
# (theoretical mean) = (empirical mean). In particular, solving for b gives
#
# b = a (1 - mu) / mu
# 
# where mu is shorthand for E(X). Notice that we divide the empirical mean by
# 100 because the beta distribution is supported on [0, 1]. This means that the
# maximum possible BLL is 100 micrograms/decliter.
#-------------------------------------------------------------------------------
get_beta_params <- function(avgbll, frac5plus, frac10plus) {
  max_bll <- 100 
  mu <- avgbll / max_bll
  f <- function(a) {
    theoretical <- pbeta(c(5, 10) / max_bll, 
                         shape1 = a, 
                         shape2 = a * (1 - mu) / mu,
                         lower.tail = FALSE)
    sum((theoretical - c(frac5plus, frac10plus))^2)
  }
  a_star <- optimize(f, c(0.0001, 10))$minimum
  b_star <- a_star * (1 - mu) / mu
  return(c(shape1 = a_star, shape2 = b_star))
}

#-------------------------------------------------------------------------------
# Compute integrated IQ loss: Beta version
#-------------------------------------------------------------------------------
get_beta_IQ_integral <- function(shape1, shape2, my_loss = iq_loss) {
  # Beta density on [0, 100]
  f <- \(x) dbeta(x / 100, shape1, shape2) / 100 # Jacobian!
  integral <- integrate(\(x) f(x) * my_loss(x), 0, Inf)
  return(integral$value)
}

#-------------------------------------------------------------------------------
# Compute integrated IQ loss: Lognormal version
#-------------------------------------------------------------------------------
get_lnorm_IQ_integral <- function(meanlog, sdlog, my_loss = iq_loss) {
  # Lognormal density on [0, Inf] 
  f <- \(x) dlnorm(x, meanlog, sdlog) 
  integral <- integrate(\(x) f(x) * my_loss(x), 0, Inf)
  return(integral$value)
}