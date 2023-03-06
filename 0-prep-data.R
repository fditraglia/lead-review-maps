library(tidyverse)
library(wbstats) # wb_data() function 
library(readxl)
library(fuzzyjoin)

# IQ points lost as a function of BLL (Lanphear, 2005)
iq_loss <- function(bll) {
  # Previously we multiplied this by 20568 to convert to a dollar cost; here
  # the result is in IQ points.
  pmin(bll, 10) * 0.513 + (bll >= 10) * pmin(bll - 10, 20 - 10) * 0.19 +
             (bll >= 20) * (bll - 20) * 0.11
}

# download 2019 population data from world bank
popn <- wb_data("SP.POP.TOTL") |> 
  filter(date == 2019) |> 
  select(country, popn = SP.POP.TOTL) 

# Load blood lead projections from global burden of disease
bll5plus <- read_csv('data-raw/bll_above_5_ages_0_19.csv')
bll10plus <- read_csv('data-raw/bll_above_10_ages_0_19.csv')
bll <- read_csv('data-raw/bll_0_to_19_both_sexes.csv')

# Load aerosol data from Mengli
aerosol <- read_excel('data-raw/20221110 global aerosol pb.xlsx', sheet = 1)

total5plus <- bll5plus |> 
  select(-c(lower, upper)) |> 
  pivot_wider(names_from = sex, names_prefix = 'mean', values_from = mean) |> 
  mutate(total5plus = meanMale + meanFemale) |> 
  select(-starts_with('mean'), -c(year_id, measure, age_group)) |> 
  rename(country = location_name)

# Try to join with fuzzy matching: where does it fail?
test_join <- total5plus |> 
  stringdist_left_join(popn) 

test_join |> 
  filter(is.na(popn)) |> 
  print(n = 100)

total5plus$country[!(total5plus$country %in%  popn$country)]

setdiff(total5plus$country, popn$country)
setdiff(popn$country, total5plus$country)
