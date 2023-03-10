library(tidyverse)
library(wbstats) # wb_data() function 
library(readxl)


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
    str_starts(location_name, "Congo") ~ 'COG', 
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
         frac10plus = total10plus / popn0019) |> 
  filter(!is.na(iso3c))

# Clean up
rm(bll, bll10plus, bll5plus, country_names, popn, total10plus, total5plus)
  

#-------------------------------------------------------------------------------
# Load aerosol data 
#-------------------------------------------------------------------------------
aerosol <- read_excel('data-raw/20221110 global aerosol pb.xlsx', sheet = 1)

#-------------------------------------------------------------------------------
# Merge iso3c codes with aerosol
#-------------------------------------------------------------------------------
iso_lookup <- bllGBD |> 
  filter(!is.na(iso3c)) |> 
  select(iso3c, country = WBcountry) 

aerosol_countries <- sort(unique(aerosol$`Country and Region`))
aerosol_countries[!(aerosol_countries %in% iso_lookup$country)]

aerosol <- aerosol |> 
  rename(country = `Country and Region`) |> 
  filter(!is.na(country)) |> # remove single measurement for "Indian Ocean"
  left_join(iso_lookup) |> 
  relocate(iso3c, country) |> 
  mutate(iso3c = case_when(
    str_detect(country, 'Czech Republic') ~ 'CZE',
    str_detect(country, 'Egypt') ~ '',
    str_detect(country, 'Kazahkstan') ~ 'KAZ',
    str_detect(country, 'Russia') ~ 'RUS',
    str_detect(country, 'South Korea') ~ 'KOR',
    str_detect(country, 'Turkey') ~ 'TUR',
    .default = iso3c
  )) 

# clean up
rm(aerosol_countries)

#-------------------------------------------------------------------------------
# Load returns to education data 
#-------------------------------------------------------------------------------
returns_to_educ <- read_excel('data-raw/Comparable Returns to Education Database.xlsx',
                              sheet = 3)

#-------------------------------------------------------------------------------
# Merge iso3c
#-------------------------------------------------------------------------------

educ_countries <- sort(unique(returns_to_educ$Economy))
cbind(educ_countries[!(educ_countries %in% iso_lookup$country)])

returns_to_educ <- returns_to_educ |> 
  rename(country = Economy) |> 
  left_join(iso_lookup) |> 
  mutate(iso3c = case_when(
    str_detect(country, "Bosnia & Herzegovina") ~ 'BIH',
    str_detect(country, "Côte d'Ivoire") ~ 'CIV',
    str_detect(country, "Czech Republic") ~ 'CZE',     
    str_detect(country, "Macedonia, FYR") ~ 'MKD',     
    str_detect(country, "São Tomé and Principe") ~ 'STP',
    str_detect(country, "Swaziland") ~ NA,            
    str_detect(country, "Turkey") ~ 'TUR',  
    str_detect(country, "West Bank and Gaza") ~ NA,
    .default = iso3c
  )) |> 
  relocate(iso3c)
  
# clean up
rm(educ_countries, iso_lookup)

