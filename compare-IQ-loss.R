#-------------------------------------------------------------------------------
# 2024-03-24
#
# This is an alternative version of 2-calculate-IQ-loss.R that compares three 
# ways of calculating IQ losses: 
#
#   (1) Our preferred method from 2-calculate-IQ-loss.R based on fitting Beta
#       distributions to the GBD data and the Lanphear (2005) IQ loss function.
#
#   (2) Same as (1) except with beta * log(BLL + 1) from Crump et al.
#
#   (3) The method from Larsen and Sanchez-Triana (2023). This uses the same
#       IQ loss function as in (2) but a completely different way of estimating
#       the distribution of lead exposure levels in the population and a simpler
#       numerical approximation to the final integral. Details follow below.
#
# Below we compute the correlations between these three alternatives and make
# a pairs plot.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# IQ points lost as a function of BLL (Lanphear, 2005)
#-------------------------------------------------------------------------------
lanphear <- function(bll) {
  # In our other paper we multiplied this by 20568 to convert to a dollar cost.
  # Here we leave the result is in units of IQ points.
  pmin(bll, 10) * 0.513 + (bll >= 10) * pmin(bll - 10, 20 - 10) * 0.19 +
             (bll >= 20) * (bll - 20) * 0.11
}

#-------------------------------------------------------------------------------
# IQ points lost as a function of BLL (Crump et al 2013)
#-------------------------------------------------------------------------------
crump <- function(bll) {
  3.246 * log(bll + 1)
}

# Sanity check: should give roughly 5.8 and 7.8, respectively, according to the 
# appendix from Larsen & Sanchez-Triana (2023)
crump(c(5, 10))

# Compare crump() and lanphear() across a range of BLLs
bll_seq <- seq(0, 30, 0.01)
matplot(x = bll_seq,
        y = cbind(lanphear(bll_seq), crump(bll_seq)),
        type = 'l', lwd = 3, 
        xlab = 'BLL', 
        ylab = 'IQ Points Lost',
        main = 'Comparison of Lanphear and Crump IQ Loss Functions')
legend(x = 'topleft', legend = c('Lanphear (2005)', 'Crump et al (2013)'),
       lty = 1:2, lwd = 3, col = c('black', 'red'))

rm(bll_seq)
  
#-------------------------------------------------------------------------------
# To use the above relationship to calculate total (per person) IQ loss we need
# the *distribution* of BLLs, not merely simple summary statistics. In principal
# we could try to access the underlying models used by the GBD to construct this
# information for ourselves, but that would be very time-consuming. In our
# script 2-calculate-IQ-loss.R we considered three approaches that fit 
# a parametric distribution to the observed summary statistics:
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
# Based on comparisons of these methods on microdata from Illinois, we chose to
# use the second option in our main analyes. For the purposes of the comparisons
# in the present script, we only consider results for the Beta distribution
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Function to fit Beta distribution distribution
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
# Compute beta parameters for all countries
#-------------------------------------------------------------------------------
bllGBD <- bllGBD |> 
  select(avgbll, frac5plus, frac10plus) |> 
  pmap_dfr(get_beta_params) |> 
  bind_cols(bllGBD) 
#relocate(shape1, shape2, .after = frac10plus)

#-------------------------------------------------------------------------------
# Compute integrated IQ loss for all countries: Beta version
#-------------------------------------------------------------------------------
get_beta_IQ_integral <- function(shape1, shape2, iq_loss) {
  # Beta density on [0, 100]
  f <- \(x) dbeta(x / 100, shape1, shape2) / 100 # Jacobian!
  integral <- integrate(\(x) f(x) * iq_loss(x), 0, Inf)
  return(integral$value)
}

get_IQ_integral_crump <- function(shape1, shape2) {
  get_beta_IQ_integral(shape1, shape2, iq_loss = crump)
}

get_IQ_integral_lanphear <- function(shape1, shape2) {
  get_beta_IQ_integral(shape1, shape2, iq_loss = lanphear)
}

bllGBD <- bllGBD |> 
  mutate(IQ_integral_lanphear = map2_dbl(shape1, shape2, get_IQ_integral_lanphear),
         IQ_integral_crump = map2_dbl(shape1, shape2, get_IQ_integral_crump))

#-------------------------------------------------------------------------------
# Larsen and Sanchez-Triana (2023) Approach
#
# The authors of this paper use a log-normal approximation to BLLs, but they're
# not totally explicit about how the do it. What follows is my best guess of 
# their procedure based on the explanation in their appendix.
# 
#  Step 1: Estimate standard deviations of BLLs
#   Using data from a paper by Ericson et al, which contains both means and SDs
#   of BLLs, they fit two regressions: one for adults and one for children. The
#   estimated regression functions are:
#
#     Function:   SD/BLL = alpha - beta * log(BLL)
#     Children:   (SD of BLL) = 1.79 - 0.627 * log(Mean of BLL)
#     Adults:     (SD of BLL) = 1.70 - 0.561 * log(Mean of BLL)
#
#  Step 2: Calibrate parameters of a Log-normal distribution
#   The parameters of a Log-normal distribution do *not* equal its mean and
#   standard deviation: they equal the mean and variance on the log scale. To 
#   convert from means and SDs on the "raw" scale to the parameters of the 
#   distribution, we can use the following equalities:
#   
#     mu = log { ([Mean BLL])^2 / sqrt([SD of BLL]^2 + [Mean BLL]^2) }
#     sigma = sqrt{ log[1 + (SD of BLL)^2 / (Mean BLL)^2]}
#
#   These formulas do not appear in the paper or its online appendix, so I'm 
#   only assuming that this is what they must have done.
# 
#  Step 3: Approximate the Integral
#   The authors use a somewhat complicated discrete approximation to in place of
#   our numerical integrals from above. First they define 101 "bins" of BLLs
#
#       i =   0:    BLL from  0.0 -  0.5 micrograms / dl
#       i =   1:    BLL from  0.5 -  1.0 micrograms / dl    
#       i =   2:    BLL from  1.0 -  1.5 micrograms / dl    
#         .
#         .
#         .
#       i =  99:    BLL from 49.5 - 50.0 micrograms / dl    
#       i = 100:    BLL > 50 micrograms / dl
#
#   Next, the assign probabilities to each bin according to the calibrated 
#   log-normal distribution. Let F be the CDF of this distribution and P(i) 
#   be the probability assigned to bin i. They set:
#
#       P(0) = F(0.5)
#       P(1) = F(1.0) - F(0.5)
#       P(2) = F(1.5) - F(1.0)
#            .
#            .
#            .
#       P(99) = F(50.0) - F(49.5)
#       P(100) = 1 - F(50)
#
#   Next they assign IQ point losses to each bin according to the bin *midpoint*
#   except in the last bin, where they evaluate at 50 micrograms / dl
#       
#       IQ(0) = crump(0.25)
#       IQ(1) = crump(0.75)
#       IQ(2) = crump(1.25)
#             .
#             .
#             .
#       IQ(99) = crump(49.75)
#       IQ(100) = crump(50)
#
#   Finally, they approximate lost IQ points from lead exposure as:
#
#       (Lost IQ Points) = Constant * [SUM(i = 0, ..., 100) of IQ(i) * P(i)]
#
#   There are two things I'm not clear about:
#
#       (1) I don't understand the choice of constant. They write: "We define
#           C_k / k [the constant] as the population under age five years in 
#           2019 with k=5 for consistency with the age of BLL measurements and 
#           IQ tests in the studies of the pooled analyses. Alternatively,
#           C_k may be defined as the birth cohort in 2019 adjusted for under-5
#           mortality rates with k=1."
#
#       (2) They have two different regression models for estimating the SD of
#           BLLs from the observed mean of BLLs: one for kids and one for adults.
#           Presumably they combine these to get overall parameters for the 
#           calibrated log-normal distribution but there's no explanation in the
#           paper. I can't think of an obvious way to do it.
#-------------------------------------------------------------------------------

raw_to_log_params <- function(E_X, SD_X) {
  # For X ~ Log-normal(mu, sigma) compute mu and sigma from E(X) and SD(X)
  E_X_squared <- SD_X^2 + E_X^2
  mu <- 2 * log(E_X) - 0.5 * log(E_X_squared)
  sigma <- sqrt(log(E_X_squared) - 2 * log(E_X))
  c(mu = mu, sigma = sigma)
}

# test get_lognormal_params
set.seed(1983)
x <- rlnorm(1e7, meanlog = 0.5, sdlog = 1.3)
raw_to_log_params(mean(x), sd(x))
rm(x)

#-------------------------------------------------------------------------------
# Compute lognormal parameters for all countries
#-------------------------------------------------------------------------------
get_lognormal_params <- function(avgbll) {
  sdbll <- 1.79 - 0.627 * log(avgbll)
  raw_to_log_params(avgbll, sdbll)
}

bllGBD <- bllGBD |> 
  select(avgbll) |> 
  pmap_dfr(get_lognormal_params) |> 
  bind_cols(bllGBD) 


#-------------------------------------------------------------------------------
# Function approximate the IQ loss integral in Larsen & Sanchez-Triana
#-------------------------------------------------------------------------------
get_Larsen_Sanchez_Triana <- function(mu, sigma) {
  
  lnorm_seq <- seq(from = 0, to = 50, by = 0.5)
  p <- c(diff(plnorm(lnorm_seq, mu, sigma)), 1 - plnorm(50, mu, sigma))
  
  bll_seq <- c(seq(from = 0.25, to = 50, by = 0.5), 50)
  iq <- crump(bll_seq)
  
  sum(p * iq)
}


#-------------------------------------------------------------------------------
# Compute the Larsen & Sanchez-Triana integral for all countries 
#-------------------------------------------------------------------------------
bllGBD <- bllGBD |> 
  mutate(IQ_Larsen_Sanchez_Triana = map2_dbl(mu, sigma, get_Larsen_Sanchez_Triana))


#-------------------------------------------------------------------------------
# Compare the three measures of lost IQ points
#-------------------------------------------------------------------------------
library(GGally)
bllGBD |> 
  select(starts_with('IQ_')) |> 
  ggpairs()

# clean up
#rm(get_beta_IQ_integral, get_beta_params, get_lnorm_params, get_lnorm_IQ_integral)



