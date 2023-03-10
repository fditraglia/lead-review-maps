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
# lead and IQ is endognous family background: you still get more than 1 trillion
# per year.

#------------------------------------------------------------------------------
# Estimate the $ benefit from eliminating lead: Version 2
#
# This approach uses the returns to education, GDP per capita, and an estimated 
# relationship between IQ and years of education to replace the $20,000 figure 
# from Version 1. 
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
bllGBD <- bllGBD |> 
  mutate(pc_returns_no_lead = (beta_IQ_integral / 15) * 0.53 * 
           avgreturns_to_educ * gdpc)

bllGBD |> 
  filter(!is.na(pc_returns_no_lead)) |> 
  summarize(sum(pc_returns_no_lead * popn0019))
