library(tidyverse)
library(wbstats) # wb_data() function 
library(readxl)
library(fuzzyjoin)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# IQ points lost as a function of BLL (Lanphear, 2005)
iq_loss <- function(bll) {
  # Previously we multiplied this by 20568 to convert to a dollar cost; here
  # the result is in IQ points.
  pmin(bll, 10) * 0.513 + (bll >= 10) * pmin(bll - 10, 20 - 10) * 0.19 +
             (bll >= 20) * (bll - 20) * 0.11
}


# Sum the 15-19 values to get a total across M and F, then add to the 0-14
popn <- wb_data(c('SP.POP.0014.TO', 'SP.POP.1519.FE', 'SP.POP.1519.MA')) |>  
  filter(date == 2019) |> 
  mutate(popn0019 = SP.POP.0014.TO + SP.POP.1519.FE + SP.POP.1519.MA) |> 
  select(-starts_with('SP.POP'), -iso2c)


# Load blood lead projections from global burden of disease
bll5plus <- read_csv('data-raw/bll_above_5_ages_0_19.csv')
bll10plus <- read_csv('data-raw/bll_above_10_ages_0_19.csv')
bll <- read_csv('data-raw/bll_0_to_19_both_sexes.csv')

# Load aerosol data from Mengli
aerosol <- read_excel('data-raw/20221110 global aerosol pb.xlsx', sheet = 1)


# Come back and fix the 30-odd mismatched country names. In the meantime just
# drop them! 
total5plus <- bll5plus |> 
  select(-c(lower, upper)) |> 
  pivot_wider(names_from = sex, names_prefix = 'mean', values_from = mean) |> 
  mutate(total5plus = meanMale + meanFemale) |> 
  select(-starts_with('mean'), -c(year_id, measure, age_group)) |> 
  mutate(country = ifelse(location_name %in% popn$country, location_name, NA)) |> 
  relocate(country, location_name) |> 
  left_join(popn) |> 
  relocate(iso3c) |> 
  filter(!is.na(iso3c))

# for 10plus, mean is character data: some values are '<1' so replace with zero
total10plus <- bll10plus |> 
  select(-c(lower, upper)) |> 
  mutate(mean = if_else(mean == '<1', '0', mean)) |> 
  mutate(mean = as.numeric(mean)) |> 
  pivot_wider(names_from = sex, names_prefix = 'mean', values_from = mean) |> 
  mutate(total10plus = meanMale + meanFemale) |> 
  select(-starts_with('mean'), -c(year_id, measure, age_group)) |> 
  mutate(country = ifelse(location_name %in% popn$country, location_name, NA)) |> 
  relocate(country, location_name) |> 
  left_join(popn) |> 
  relocate(iso3c) |> 
  filter(!is.na(iso3c))

# merge the three sets of BLL data for figure 2
# Don't forget to come back and fix this!
bllGBD <- bll |> 
  select(location_name, avgbll = mean) |> 
  left_join(total5plus) |> 
  left_join(total10plus) |> 
  filter(!is.na(iso3c)) |> # remove this later after fixing the mismatches!
  mutate(frac5plus = total5plus / popn0019,
         frac10plus = total10plus / popn0019) |> 
  relocate(iso3c, location_name, country)

#total5plus <- bll5plus |> 
#  select(-c(lower, upper)) |> 
#  pivot_wider(names_from = sex, names_prefix = 'mean', values_from = mean) |> 
#  mutate(total5plus = meanMale + meanFemale) |> 
#  select(-starts_with('mean'), -c(year_id, measure, age_group)) |> 
#  mutate(country = ifelse(location_name %in% popn$country, location_name, NA)) |> 
#  relocate(country, location_name) |> 
#  left_join(popn) |> 
#  relocate(iso3c)

#total5plus |> 
#  filter(is.na(iso3c)) |> 
#  arrange(location_name) |> 
#  print(n = 100)
#
## What are the WB country names and iso3 codes for the mismatches?
#popn |> 
#  filter(country %in% setdiff(country, total5plus$country)) |> 
#  select(country, iso3c) |> 
#  print(n = 100)
#
## Which country names from the GBD results *do not* match the WB data?
#mismatches <- setdiff(total5plus$location_name, popn$country)
#cbind(sort(mismatches))



# Try to join with fuzzy matching: where does it fail?
#test_join <- total5plus |> 
#  stringdist_left_join(popn) 
#
#test_join |> 
#  filter(is.na(popn)) |> 
#  print(n = 100)
#
#total5plus$country[!(total5plus$country %in%  popn$country)]
#
#cbind(sort(setdiff(total5plus$country, popn$country)))
#cbind(sort(setdiff(popn$country, total5plus$country)))

# We want population 19 and under to match our lead data. Search for popn data
# by age category
#as_tibble(wbsearch('population')) |> print(n = 10000)


# A very simple test map: it seems like we can match on iso3
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")
