library(sf)
library(tidyverse)
library(viridis)
load('data/aerosol.RData')

# Set ggplot theme
theme_set(theme_bw())

#filter to after
#weigh bubbles by number of samples

# Bubbles 
aerosol_filter <- aerosol |> 
  filter(`Study done before/after phasing out Pb (if same year, considered before)`=="After") |> #Before, Same, unknown
  arrange(desc(`Pb (ng ⋅ m−3)`))

ggplot(data = world) + 
    geom_sf() +
    geom_point( data=aerosol_filter, 
                aes(x=Longitude, y=Latitude, size=`number of samples`, color=`Pb (ng ⋅ m−3)`), alpha=0.3) +
  scale_size_continuous(name="N Samples", range=c(1,12)) +
  scale_color_viridis(name="Log Pb (ng ⋅ m−3)", trans="log" ) +
  theme(legend.justification = c(0,0.8)) +
  theme(legend.key.height = unit(0.3, 'cm'))

ggsave('output/aerosol.png', width = 7, height = 5)

rm(aerosol)
