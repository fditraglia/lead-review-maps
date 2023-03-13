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
  right_join(bllGBD)

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
  right_join(bllGBD)

# clean up
rm(returns_to_educ, WBgdpc)

#-------------------------------------------------------------------------------
# Approximate $ / IQ point in terms of foregone earnings.
#
# In our IL paper we monetized the loss of one IQ point for a 3-year old as 
# as 20,568 in 2019 dollars, following Klemick, Mason, and Sullivan (2020). This
# is meant to represent the lifetime present value of earnings losses from lower
# cognitive skills. 
# 
# This figure shouldn't be applied to countries with different returns to
# cognitive skills. A back-of-the-envelope approach to generating comparable
# figures for other countries is as follows. 
#
# Treat the returns to one additional year of education as the returns to
# cognitive skills. (Note that "ability bias" actually helps us here since we
# are interested in the returns to ability.) Returns to education are expressed
# as a percentage increase in wage per additional year of education. But wages
# also vary across countries. As a rough approximation, use relative GDP per 
# capita to correct for this.
#
# To convert the $20,568 figure for use in country X, calculate as follows:
#
# ($ per IQ point) = 20568 * (returns(X) / returns(US)) * (GDPc(X) / GDPc(US))
# 
# For some countries this will produce a missing value because we don't have 
# returns to education for all the countries for which we have GBD lead data.
#-------------------------------------------------------------------------------

US_gdpc <- bllGBD |> 
  filter(iso3c == 'USA') |> 
  pull(gdpc)

US_returns <- bllGBD |> 
  filter(iso3c == 'USA') |> 
  pull(avgreturns_to_educ)

bllGBD <- bllGBD |> 
  mutate(dollars_per_IQ = 20568 * avgreturns_to_educ * gdpc / 
           (US_gdpc * US_returns)) 


rm(iq_loss, US_gdpc, US_returns)
