# Merge shapefiles with lead data and make plots
bllWorld <- world |> 
  select(-continent) |>  # don't join by continent: we've redefined it in bllGBD (S/C America)
  left_join(bllGBD)

# Set ggplot theme
theme_set(theme_bw())

# Update the theme globally to increase the font size of the legend text and title
theme_update(
  legend.text = element_text(size = 12), # Change legend text size
  legend.title = element_text(size = 14), # Change legend title size
  panel.grid = element_blank(), # Remove grid lines (lat/long lines)
  axis.text = element_blank(), # Remove axis text (degree labels)
  axis.ticks = element_blank(), # Remove axis ticks
  axis.title = element_blank() # Remove axis titles if any
)


cut_bll <- function(x) {
  cut(x, breaks = c(-Inf, 5, 15, 30, 50, Inf), 
      labels = c("<5%", "5-15%", "15-30%", "30-50%", ">50%"))
}

cut_iq <- function(x) {
  cut(x, breaks = c(-Inf, 2, 5, 10, 15, Inf), 
      labels = c("<2%", "2-5%", "5-10%", "10-15%", ">15%"))
}

bllWorld_binned <- bllWorld |>
  mutate(
    bll5_category = cut_bll(100 * frac5plus),
    bll10_category = cut_bll(100 * frac10plus),
    iq_beta_category = cut_iq(100 * relative_iq_cost * beta_IQ_integral),
    iq_lognorm_category = cut_iq(100 * relative_iq_cost * lnorm_IQ_integral),
    iq_LB_category = cut_iq(100 * relative_iq_cost * LB_IQ_integral)
  )

# Fraction with BLL > 5 micrograms / deciliter
bllWorld_binned |> 
  ggplot() + 
  geom_sf(aes(fill = bll5_category)) +
  scale_fill_viridis_d(option = "plasma", name = "") +
  labs(title = "Fraction of children with BLL > 5 micrograms / deciliter")
ggsave('output/frac5plus.pdf', width = 7, height = 5)
ggsave('output/frac5plus.png', width = 7, height = 5)

# Fraction with BLL > 10 micrograms / deciliter
bllWorld_binned |> 
  ggplot() + 
  geom_sf(aes(fill = bll10_category)) +
  scale_fill_viridis_d(option = "plasma", name = "") +
  labs(title = "Fraction of children with BLL > 10 micrograms / deciliter")
ggsave('output/frac10plus.pdf', width = 7, height = 5)
ggsave('output/frac10plus.png', width = 7, height = 5)

bllWorld_binned |> 
  ggplot() + 
  geom_sf(aes(fill = iq_beta_category)) +
  scale_fill_viridis_d(option = "turbo", name = "") +
  labs(title = "Relative IQ cost of lead exposure")
ggsave('output/relative_iq_cost.pdf', width = 7, height = 5)
ggsave('output/relative_iq_cost.png', width = 7, height = 5)


#----------------------- Robustness Checks
# Lognormal
bllWorld_binned |> 
  ggplot() + 
  geom_sf(aes(fill = iq_lognorm_category)) +
  scale_fill_viridis_d(option = "turbo", name = "") +
  labs(title = "Relative IQ cost of lead exposure: Lognormal approx.")

# Lower bound 
bllWorld_binned |> 
  ggplot() + 
  geom_sf(aes(fill = iq_LB_category)) +
  scale_fill_viridis_d(option = "turbo", name = "") +
  labs(title = "Relative IQ cost of lead exposure: Lower bound approx.")

rm(bllWorld, cut_bll, cut_iq, bllWorld_binned)
