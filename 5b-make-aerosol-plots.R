library(sf)
library(tidyverse)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
load('data/aerosol.RData')

#function
align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}

# Set ggplot theme
theme_set(theme_bw())


# Get country polygons for all continents excluding Antarctica 
world <- ne_countries(scale = "medium", 
                      returnclass = "sf", 
                      continent = c('North America', 'Africa', 'Europe', 
                                    'Asia', 'South America', 'Oceania'))

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
  scale_color_viridis(name="Log Pb (ng ⋅ m−3)", trans="log") +
  theme(legend.key.size = unit(0.3, 'cm'), legend.justification=c(0.8,0.7)) 

