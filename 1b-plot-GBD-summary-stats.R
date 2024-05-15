# This script relies on the data from 1-prep-data.R, so run that first.

bllGBD |> 
  select(avgbll, frac5plus, frac10plus) |> 
  ggpairs()


bllGBD |> 
  select(starts_with('shape')) |> 
  ggplot(aes(shape1, shape2)) +
  geom_point() +
  geom_smooth()
