# This script carries out a sensitivity analysis to see how changes to the 
# BLL data from the GBD would affect our overall results. In particular, we
# will *scale up* the average BLL by a certain number of percentage points, e.g.
# 5%, 10%, etc. We will then estimate the IQ loss assuming that these BLL values
# are the true ones. This will give us a sense of how sensitive our results are
# to the underlying GBD data.

library(scam) # for shape-constrained GAMs

# Shape-constrained GAM (scam) regressions of avgbll on frac5plus and frac10plus: 
# needed to predict how the fraction with BLLs >=5 and >=10 would differ in a regime where every 
# country had a higher average BLL. 

# Unlike LOESS, SCAM can force monotonicity. We need this to prevent the 5plus
# regression function from decreasing slightly at extremely high BLLs
scam_5plus <- scam(frac5plus ~ s(avgbll, bs = 'mpi'), data = bllGBD)
scam_10plus <- scam(frac10plus ~ s(avgbll, bs = 'mpi'), data = bllGBD)

# Plot the regression functions - sanity check

plot(bllGBD$avgbll, bllGBD$frac5plus, col = 'red', main = '5 plus')
points(bllGBD$avgbll, predict(scam_5plus), col = 'blue', pch = 20)

plot(bllGBD$avgbll, bllGBD$frac10plus, col = 'red', main = '10 plus')
points(bllGBD$avgbll, predict(scam_10plus), col = 'blue', pch = 20)

# clean up
rm(scam_5plus, scam_10plus)

                        
  

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