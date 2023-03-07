library(tidyverse)
library(wbstats) # wb_data() function 
library(readxl)
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


#-------------------------------------------------------------------------------
# Blood lead level (BLL) projections from 2019 Global Burden of Disease (GBD)
#
# We use data for ages 0-19
#
# bll5plus     count with BLL > 5 micrograms / decliter
# bll10plus    count with BLL > 10 micrograms / decliter
# bll          average BLL in micrograms / decliter
#
# See 'data-raw/GBD 2019 Risk Factors Appendix_Lead.pdf' for more details. 
#-------------------------------------------------------------------------------
bll5plus <- read_csv('data-raw/bll_above_5_ages_0_19.csv')
bll10plus <- read_csv('data-raw/bll_above_10_ages_0_19.csv')
bll <- read_csv('data-raw/bll_0_to_19_both_sexes.csv')

#-------------------------------------------------------------------------------
# bll5plus and bll10plus give separate figures for Male and Female. Construct
# total5plus and total10plus by summing across Male and Female in each country
# and dropping columns that we will not use below.
#-------------------------------------------------------------------------------
total5plus <- bll5plus |> 
  select(-c(lower, upper)) |> 
  pivot_wider(names_from = sex, names_prefix = 'mean', values_from = mean) |> 
  mutate(total5plus = meanMale + meanFemale) |> 
  select(-starts_with('mean'), -c(year_id, measure, age_group)) 

#-------------------------------------------------------------------------------
# bll10plus has the added quirk that `mean` is stored as character data, since
# some values are encoded as '<1'. Replace these with zeros and convert the
# result to numeric.
#-------------------------------------------------------------------------------
total10plus <- bll10plus |> 
  select(-c(lower, upper)) |> 
  mutate(mean = if_else(mean == '<1', '0', mean)) |> 
  mutate(mean = as.numeric(mean)) |> 
  pivot_wider(names_from = sex, names_prefix = 'mean', values_from = mean) |> 
  mutate(total10plus = meanMale + meanFemale) |> 
  select(-starts_with('mean'), -c(year_id, measure, age_group))


#-------------------------------------------------------------------------------
# To convert total5plus and total10plus from counts to *proportions* we need  
# the population between the ages of 0 and 19 in each country. We construct this 
# from the following variables in the World Bank database:
#
# SP.POP.0014.TO    total population aged 0-14
# SP.POP.1519.FE    total female population aged 15-19
# SP.POP.1519.MA    total male population aged 15-19
#
# Summing these three gives total population aged 0-19.
#-------------------------------------------------------------------------------
popn <- wb_data(c('SP.POP.0014.TO', 'SP.POP.1519.FE', 'SP.POP.1519.MA')) |>  
  filter(date == 2019) |> 
  mutate(popn0019 = SP.POP.0014.TO + SP.POP.1519.FE + SP.POP.1519.MA) |> 
  select(-starts_with('SP.POP'), -iso2c, -date)

#-------------------------------------------------------------------------------
# Load aerosol data from Mengli
#-------------------------------------------------------------------------------
aerosol <- read_excel('data-raw/20221110 global aerosol pb.xlsx', sheet = 1)



#-------------------------------------------------------------------------------
# Some elements of `location_name` from the GBD lead data do not match the 
# corresponding elements of `country` from `popn`. In some cases, one source
# contains a country that the other does not, e.g. Taiwan. In others, the two
# sources merely use a different naming convention, e.g. 'United States' versus
# 'United States of America.' For later use, we construct a dataframe with all
# lead and population data using `iso3c` as the identifier. 
#-------------------------------------------------------------------------------

# `location_name` contains the same countries for each of the 3 GBD dataframes
all(total5plus$location_name %in% total10plus$location_name)
all(bll$location_name %in% total10plus$location_name)

# Begin by merging the exact matches between `location_name` and `country`
country_names <- total5plus |> 
  mutate(country = ifelse(location_name %in% popn$country, location_name, NA)) |> 
  left_join(popn) |> 
  select(iso3c, country, location_name)

# Which values of `location_name` did not match `country` from `popn`?
country_names |> 
  filter(is.na(iso3c)) |> 
  print(n = 100)

# Which values of `country` from `popn` did not match `location_name`?
popn |>
  filter(!(country %in% total5plus$location_name)) |> 
  print(n = 100)

# Manually fill in missing iso3c codes corresponding to `location_name` 
country_names <- country_names |> 
  mutate(iso3c = case_when(
    str_detect(location_name, "Democratic People's Republic of Korea") ~ 'PRK',
    #str_detect(location_name, "Taiwan") ~ NA,
    str_detect(location_name, "Lao People's Democratic Republic") ~ 'LAO',
    str_detect(location_name, "Viet Nam") ~ 'VNM',
    str_detect(location_name, "Micronesia") ~ 'FSM', 
    str_detect(location_name, "Kyrgyzstan") ~ 'KGZ', 
    str_detect(location_name, "Slovakia") ~ 'SVK', 
    str_detect(location_name, "Republic of Moldova") ~ 'MDA', 
    str_detect(location_name, "Republic of Korea") ~ 'KOR', 
    str_detect(location_name, "United States of America") ~ 'USA', 
    str_detect(location_name, "Bahamas") ~ 'BHS', 
    str_detect(location_name, "Saint Lucia") ~ 'LCA', 
    str_detect(location_name, "Saint Vincent and the Grenadines") ~ 'VCT', 
    str_detect(location_name, "Bolivia") ~ 'BOL', 
    str_detect(location_name, "Venezuela") ~ 'VEN', 
    str_detect(location_name, "Egypt") ~ 'EGY', 
    str_detect(location_name, "Iran") ~ 'IRN', 
    #str_detect(location_name, "Palestine") ~ NA, 
    str_detect(location_name, "Turkey") ~ 'TUR', 
    str_detect(location_name, "Yemen") ~ 'YEM', 
    str_detect(location_name, "Congo") ~ 'COG', 
    str_detect(location_name, "Democratic Republic of the Congo") ~ 'COD', 
    str_detect(location_name, "United Republic of Tanzania") ~ 'TZA', 
    str_detect(location_name, "Côte d'Ivoire") ~ 'CIV', 
    str_detect(location_name, "Gambia") ~ 'GMB', 
    #str_detect(location_name, "Cook Islands") ~ NA, 
    #str_detect(location_name, "Niue") ~ NA, 
    str_detect(location_name, "Saint Kitts and Nevis") ~ 'KNA', 
    #str_detect(location_name, "Tokelau") ~ NA, 
    str_detect(location_name, "United States Virgin Islands") ~ 'VIR', 
    .default = iso3c
  )) 

# Merge popn with GBD data using iso3 to resolve the mis-matched country names
bllGBD <- country_names |> 
  select(-country) |> 
  left_join(popn) |> 
  rename(WBcountry = country) |> 
  left_join(bll) |> 
  select(-c(year_id, sex, age_group, lower, upper, measure)) |> 
  rename(avgbll = mean) |> 
  left_join(total5plus) |> 
  left_join(total10plus) |> 
  mutate(frac5plus = total5plus / popn0019,
         frac10plus = total10plus / popn0019) 
  


##---------------------
## UPDATE BELOW HERE!
##---------------------
#  
#
## Come back and fix the 30-odd mismatched country names. In the meantime just
## drop them! 
#total5plus <- bll5plus |> 
#  select(-c(lower, upper)) |> 
#  pivot_wider(names_from = sex, names_prefix = 'mean', values_from = mean) |> 
#  mutate(total5plus = meanMale + meanFemale) |> 
#  select(-starts_with('mean'), -c(year_id, measure, age_group)) |> 
#  mutate(country = ifelse(location_name %in% popn$country, location_name, NA)) |> 
#  relocate(country, location_name) |> 
#  left_join(popn) |> 
#  relocate(iso3c) |> 
#  filter(!is.na(iso3c))
#
## for 10plus, mean is character data: some values are '<1' so replace with zero
#total10plus <- bll10plus |> 
#  select(-c(lower, upper)) |> 
#  mutate(mean = if_else(mean == '<1', '0', mean)) |> 
#  mutate(mean = as.numeric(mean)) |> 
#  pivot_wider(names_from = sex, names_prefix = 'mean', values_from = mean) |> 
#  mutate(total10plus = meanMale + meanFemale) |> 
#  select(-starts_with('mean'), -c(year_id, measure, age_group)) |> 
#  mutate(country = ifelse(location_name %in% popn$country, location_name, NA)) |> 
#  relocate(country, location_name) |> 
#  left_join(popn) |> 
#  relocate(iso3c) |> 
#  filter(!is.na(iso3c))
#
## merge the three sets of BLL data for figure 2
## Don't forget to come back and fix this!
#bllGBD <- bll |> 
#  select(location_name, avgbll = mean) |> 
#  left_join(total5plus) |> 
#  left_join(total10plus) |> 
#  filter(!is.na(iso3c)) |> # remove this later after fixing the mismatches!
#  mutate(frac5plus = total5plus / popn0019,
#         frac10plus = total10plus / popn0019) |> 
#  relocate(iso3c, location_name, country)
#
##total5plus <- bll5plus |> 
##  select(-c(lower, upper)) |> 
##  pivot_wider(names_from = sex, names_prefix = 'mean', values_from = mean) |> 
##  mutate(total5plus = meanMale + meanFemale) |> 
##  select(-starts_with('mean'), -c(year_id, measure, age_group)) |> 
##  mutate(country = ifelse(location_name %in% popn$country, location_name, NA)) |> 
##  relocate(country, location_name) |> 
##  left_join(popn) |> 
##  relocate(iso3c)
#
##total5plus |> 
##  filter(is.na(iso3c)) |> 
##  arrange(location_name) |> 
##  print(n = 100)
##
### What are the WB country names and iso3 codes for the mismatches?
##popn |> 
##  filter(country %in% setdiff(country, total5plus$country)) |> 
##  select(country, iso3c) |> 
##  print(n = 100)
##
### Which country names from the GBD results *do not* match the WB data?
##mismatches <- setdiff(total5plus$location_name, popn$country)
##cbind(sort(mismatches))
#
#
#
## Try to join with fuzzy matching: where does it fail?
##test_join <- total5plus |> 
##  stringdist_left_join(popn) 
##
##test_join |> 
##  filter(is.na(popn)) |> 
##  print(n = 100)
##
##total5plus$country[!(total5plus$country %in%  popn$country)]
##
##cbind(sort(setdiff(total5plus$country, popn$country)))
##cbind(sort(setdiff(popn$country, total5plus$country)))
#
## We want population 19 and under to match our lead data. Search for popn data
## by age category
##as_tibble(wbsearch('population')) |> print(n = 10000)


# A very simple test map: it seems like we can match on iso3
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")
