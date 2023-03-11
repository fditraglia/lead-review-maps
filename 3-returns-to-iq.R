# There's a small issue we need to think about when comparing results across
# fixed and variable IQ costs: we have fewer countries for the latter exercise,
# so the figures will necessarily be smaller. Not sure of the best way to handle
# this. Maybe fraction of total world GDP? 

# Also need to make sure we're clear on the units here. It's per year but for
# the entire cohort of 0-19 year olds. This is a little bit tricky to think 
# about. Maybe it's clearer to express this per person?

# Yes: per person resolves both issues at once since we normalize by total popn
# among the group of countries for which we are able to make a comparison.

#------------------------------------------------------------------------------
# Estimate the $ benefit from eliminating lead: Version 1
# 
# In our IL paper we monetized the loss of one IQ point for a 3-year old as 
# as 20,568 in 2019 dollars, following Klemick, Mason, and Sullivan (2020). 
# Their calculation was the present value of total lifetime earnings loss in 
# the US. This value may not apply outside the US, but could serve as a simple
# benchmark. 
# 
# If we assume a discount rate between 3 and 4% per year, along with a working
# lifetime between 40 and 45 years, we need to deflate the total figure by a 
# factor of between 20 and 25. This comes to roughly $1000 per year. We want
# a flow so that the figure is comparable to the one we compute below.
#------------------------------------------------------------------------------

bllGBD |> 
  summarize(lower_bound = sum(1000 * popn0019 * LB_IQ_integral),
            beta_approx = sum(1000 * popn0019 * beta_IQ_integral),
            lnorm_approx = sum(1000 * popn0019 * lnorm_IQ_integral))

# The beta and lognormal both give a figure of around 5-6 trillion. 
# The lower bound approach is about half as big: 2.5 trillion. 
# No matter how you look at it, these are big numbers. Take the lower bound and
# divide it by 2 on the premise that part of the estimated relationship between
# lead and IQ is endogenous family background: you still get more than 1 trillion
# per year. Compare to world GDP of approximately 96 trillion in 2021.

# Try re-expressing this in per-person terms: *divide* by total population 
# between 0 and 19 at the end. This gives a weighted average based on popn.
bllGBD |> 
  summarize(lower_bound = sum(1000 * popn0019 * LB_IQ_integral) / sum(popn0019),
            beta_approx = sum(1000 * popn0019 * beta_IQ_integral) / sum(popn0019),
            lnorm_approx = sum(1000 * popn0019 * lnorm_IQ_integral) / sum(popn0019))

# These figures are much easier to think about: 974 for the lower bound, 2221 
# for the beta and 1947 for the lognormal. Compare to world GDP / capita of 
# around 12,234.

#------------------------------------------------------------------------------
# Estimate the $ benefit from eliminating lead: Version 2
#
# This approach combines estimated returns to education across a large number
# of countries with GDP per capita at PPP. The idea is that GDP / capita at PPP
# is an approximation of income / capita. Multiplying this value by estimated
# returns to one additional year of education gives a result in $/year per
# additional year of education. To convert this to $/year per IQ point, we need
# the relationship between IQ points and years of education. 
#------------------------------------------------------------------------------

# Which and how many years of returns are available in each country?
returns_to_educ |> 
  select(country, Year) |> 
  group_by(country) |> 
  summarize(minYear = min(Year), maxYear = max(Year), nYear = n()) |> 
  arrange(nYear, maxYear) |> 
  print(n = 200)

# If only one year: use this year (no other option!)  
# If more than one year: average all observations after a cutoff date (> 1990)
# (There is no country with more than one observation but all before 1990)
bllGBD <- returns_to_educ |> 
  rename(returns = `β1`) |> 
  mutate(after1990 = Year > 1990) |> 
  group_by(iso3c) |> 
  summarize(n_after_1990 = sum(after1990),
            avgreturns_to_educ = ifelse(n_after_1990 > 1,
                                (after1990 * returns) / n_after_1990,
                                returns[1])) |> 
  select(-n_after_1990) |> 
  ungroup() |> 
  right_join(bllGBD)

# Download gdp per capita at PPP in current international $ 
WBgdpc <- wb_data('NY.GDP.PCAP.PP.CD') |> 
  rename(gdpc = NY.GDP.PCAP.PP.CD)


# For some countries gdpc is only available in selected years.
WBgdpc |> 
  filter(!is.na(gdpc)) |> 
  select(iso3c, date, gdpc) |> 
  group_by(iso3c) |> 
  summarize(minYear = min(date), maxYear = max(date), has2019 = 2019 %in% date) |> 
  filter(!has2019)

# For countries that are missing 2019 gdpc, use the most recent value
bllGBD <- WBgdpc |> 
  filter(!is.na(gdpc)) |> 
  group_by(iso3c) |> 
  mutate(is2019 = (date == 2019), maxyear = max(date)) |> 
  summarize(gdpc = ifelse(2019 %in% date, 
                          gdpc[date == 2019],
                          gdpc[date == maxyear])) |> 
  ungroup() |> 
  right_join(bllGBD)

# clean up
rm(returns_to_educ, WBgdpc)


# Compute our estimate of the returns to eliminating lead
# From Stein et al (2023) an increase of 15 IQ points is associated with an
# increase of 0.53 years of schooling, on average, across Brazil, Guatemala, 
# the Phillipines, and South Africa.
IQ_to_yrschool <- 0.53 / 15
bllGBD <- bllGBD |> 
  mutate(dollar_per_yrschool = avgreturns_to_educ * gdpc,
         dollar_per_IQ = IQ_to_yrschool * dollar_per_yrschool,
         beta_returns_no_lead = beta_IQ_integral * dollar_per_IQ, 
         lnorm_returns_no_lead = lnorm_IQ_integral * dollar_per_IQ, 
         LB_returns_no_lead = LB_IQ_integral * dollar_per_IQ)

bllGBD |> 
  filter(!is.na(beta_returns_no_lead)) |> 
  summarize(lower_bound = sum(LB_returns_no_lead * popn0019), 
            beta_approx = sum(beta_returns_no_lead * popn0019), 
            lnorm_approx = sum(lnorm_returns_no_lead * popn0019))

# These figures are broadly in line with what we found above: now we have 1.6
# trillion for the lower bound, 8 for the beta approximation and 4.5 for the 
# lognormal approximation. Compare to World GDP in 2021 of approximately 96 trillion

# Now put them in per person terms, dividing by total population 0 to 19
bllGBD |> 
  filter(!is.na(beta_returns_no_lead)) |> 
  summarize(lower_bound = sum(LB_returns_no_lead * popn0019) / sum(popn0019), 
            beta_approx = sum(beta_returns_no_lead * popn0019) / sum(popn0019), 
            lnorm_approx = sum(lnorm_returns_no_lead * popn0019) / sum(popn0019))

# 664 for lower bound, 3330 for beta approx and 1868 for lognormal. Compare to
# world GDP per capita of around 12,000.

# Sanity check: histogram of the implied dollars per IQ point per year
bllGBD |> 
  ggplot(aes(x = dollar_per_IQ)) +
  geom_histogram() +
  scale_x_log10() +
  xlab('Implied Dollars per IQ Point Per Year Across Countries')

bllGBD |> 
  filter(!is.na(dollar_per_IQ)) |> 
  summarize(median(dollar_per_IQ), mean(dollar_per_IQ))

# Notice that the median is $787 and the mean is $2000. These bookend the $1000
# figure that we calculated above by discounting the $20,000 lifetime figure.

#------------------------------------------------------------------------------
# The counterfactuals given above assume that we take everyone down to a BLL of
# zero. What if we instead brought everyone down to 5? There are many ways to
# calculate this. A simple lower bound assigns a BLL of 10 to everyone with
# a BLL > 10 and lowers this to 5
#------------------------------------------------------------------------------
iq_gain <- iq_loss(10) - iq_loss(5)
bllGBD |> 
  mutate(total_IQ_gain = iq_gain * total10plus) |> 
  summarize(sum(1000 * total_IQ_gain), sum(1000 * total_IQ_gain) / sum(popn0019))

bllGBD |> 
  mutate(total_IQ_gain = iq_gain * total10plus) |> 
  filter(!is.na(dollar_per_IQ)) |> 
  summarize(sum(dollar_per_IQ * total_IQ_gain), 
            sum(dollar_per_IQ * total_IQ_gain) / sum(popn0019))

# These are comparatively smaller numbers 173 and 59 per person or 451 billion
# and 141 billion total. But this is *really* a lower bound because it assumes
# no one is above 10.

# Clean up
rm(iq_gain, IQ_to_yrschool, iq_loss)
