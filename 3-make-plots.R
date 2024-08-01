# Merge shapefiles with lead data and make plots
bllWorld <- world |> 
  select(-continent) |>  # don't join by continent: we've redefined it in bllGBD (S/C America)
  left_join(bllGBD)

# Set ggplot theme
theme_set(theme_bw())

# Update the theme globally to increase the font size of the legend text and title
theme_update(
  legend.text = element_text(size = 12), # Change legend text size
  legend.title = element_text(size = 14) # Change legend title size
)

# Fraction with BLL > 5 micrograms / deciliter
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac5plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", trans = "sqrt") +
  labs(title = "Fraction of children with BLL > 5 micrograms / deciliter")
ggsave('output/frac5plus.pdf', width = 7, height = 5)
ggsave('output/frac5plus.png', width = 7, height = 5)

# Fraction with BLL > 10 micrograms / deciliter
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac10plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", trans = "sqrt") +
  labs(title = "Fraction of children with BLL > 10 micrograms / deciliter")
ggsave('output/frac10plus.pdf', width = 7, height = 5)
ggsave('output/frac10plus.png', width = 7, height = 5)

bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * beta_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '%', trans = 'sqrt') +
  labs(title = "Relative IQ cost of lead exposure")
ggsave('output/relative_iq_cost.pdf', width = 7, height = 5)
ggsave('output/relative_iq_cost.png', width = 7, height = 5)

#----------------------- Robustness Checks
# Lognormal
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * lnorm_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '%', trans = 'sqrt')

# Lower bound 
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * LB_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '%', trans = 'sqrt')

rm(bllWorld)
