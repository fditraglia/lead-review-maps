# This script compares our relative IQ loss figures to declines in GDP / capita
# during the great recession. For the moment this isn't part of the main 
# pipeline because we may or may not choose to include it in the paper. If we do
# decide to include it, we should modify 1-load-and-prep-data.R to load and 
# store the GDP data.

source('0-helper-functions.R')
source('1-load-and-prep-data.R')
source('2-calculate-IQ-loss.R')


if(!file.exists('data-raw/great-recession.csv')) {
  great_recession <- wb_data('NY.GDP.PCAP.PP.KD',
                    start_date = 2007,
                    end_date = 2009) 
  
  write_csv(great_recession, 'data-raw/great-recession.csv')
  rm(great_recession)
}
