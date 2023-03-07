# Assumes we've already loaded bll, bll5plus, bll10plus and converted the 
# latter two to shares (divide by popn). This information is contained in 
# bllGBD

# We will fit beta distributions and log-normal distributions

# For the beta, we will take a BLL of 100 as the max
# We will use E(X) = [a / (a + b)] to *fix* the relationship between a and b
# by imposing (theoretical mean) = (empirical mean). We will then fit the 
# remaining parameter by making the tail probabilities (>5, >10) as close as
# possible between empirical and theoretical. We can then check the quality of
# the fit.

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
  return(c(alpha = a_star, beta = b_star))
}

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
  return(c(mu = m_star, sigma = s_star))
}

# Seems like lognormal has trouble approximating dist for places with
# lower means / shares 5+.
# We should compare against "ground truth" for IL.

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
