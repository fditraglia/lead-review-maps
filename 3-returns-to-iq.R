library(countrycode) #to assign continent

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
  select(iso3c, Year, returns) |> 
  group_by(iso3c) |> 
  summarize(avgreturns_to_educ = ifelse(n() > 1, 
                                        mean(returns[Year > 1990]),
                                        mean(returns))) |> 
  ungroup() |> 
  right_join(bllGBD) |>
  mutate(continent= countrycode(iso3c, origin = "iso3c", destination = "continent")) |>
  group_by(continent) |> 
  mutate(avgreturns_to_educ = replace_na(avgreturns_to_educ, mean(avgreturns_to_educ, na.rm=TRUE))) 

# Download gdp per capita at PPP in current international $ 
WBgdpc <- wb_data('NY.GDP.PCAP.PP.CD', 
                  start_date = 2010,
                  end_date = 2019) |> 
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
  right_join(bllGBD) |>
  select(- continent)

# clean up
rm(returns_to_educ, WBgdpc)

# Compute our estimate of the returns to eliminating lead
# From Stein et al (2023) an increase of 15 IQ points is associated with an
# increase of 0.53 years of schooling, on average, across Brazil, Guatemala, 
# the Philippines, and South Africa.
IQ_to_yrschool <- 0.53 / 15
bllGBD <- bllGBD |> 
  mutate(dollar_per_yrschool = avgreturns_to_educ * gdpc,
         dollar_per_IQ = IQ_to_yrschool * dollar_per_yrschool,
         beta_returns_no_lead = beta_IQ_integral * dollar_per_IQ, 
         lnorm_returns_no_lead = lnorm_IQ_integral * dollar_per_IQ, 
         LB_returns_no_lead = LB_IQ_integral * dollar_per_IQ)

#compute average and sum of returns using beta
bllGBD |>
  mutate(tot_country=beta_returns_no_lead*popn0019,
         tot_population=sum(popn0019, na.rm=TRUE),
         tot_global_damages=sum(tot_country, na.rm=TRUE),
         avg_percapita_global_damages=tot_global_damages/tot_population) |>
  print(n=2)

# Clean up
rm(IQ_to_yrschool, iq_loss)
