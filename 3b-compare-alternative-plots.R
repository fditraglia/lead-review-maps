# Figure 2 Alternative Approaches
# This script explores different ways to address the referee's concern about scale bars

library(patchwork)

#===============================================================================
# CURRENT APPROACH (from 3-make-plots.R)
#===============================================================================

# Panel a: Fraction with BLL > 5 micrograms / deciliter
p0a <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac5plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", trans = "sqrt") +
  labs(title = "a. Estimated % with BLL > 5 μg/dL")

# Panel b: Fraction with BLL > 10 micrograms / deciliter
p0b <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac10plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", trans = "sqrt") +
  labs(title = "b. Estimated % with BLL > 10 μg/dL")

# Panel c: Economic impact
p0c <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * beta_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '%', trans = 'sqrt') +
  labs(title = "c. Estimated economic damage")

# Combine current approach
p0 <- p0a / p0b / p0c + plot_annotation('Current version')

print(p0)

#===============================================================================
# ALTERNATIVE 1 - Common 0-100% color scales; sqrt transformation
#===============================================================================

p1a <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac5plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", trans = "sqrt",
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(title = "a. Estimated % with BLL > 5 μg/dL")

p1b <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac10plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", trans = "sqrt",
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(title = "b. Estimated % with BLL > 10 μg/dL")

p1c <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * beta_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '%', trans = 'sqrt',
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(title = "c. Estimated economic damage")

p1 <- p1a / p1b / p1c + plot_annotation('Common 0-100% color scales')
print(p1)

#===============================================================================
# ALTERNATIVE 2 - Common 0-100% color scales; uniform linear scale
#===============================================================================

p2a <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac5plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", 
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(title = "a. Estimated % with BLL > 5 μg/dL")

p2b <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac10plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", 
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(title = "b. Estimated % with BLL > 10 μg/dL")

p2c <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * beta_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", name = '%', 
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(title = "c. Estimated economic damage")

p2 <- p2a / p2b / p2c + plot_annotation('Common 0-100% LINEAR color scales')
print(p2)

#===============================================================================
# ALTERNATIVE 3 - Common 0-100% color scales for (a)/(b), (c) gets own scale
#===============================================================================

p3a <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac5plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", 
                       limits = c(0, 100), 
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(title = "a. Estimated % with BLL > 5 μg/dL")

p3b <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * frac10plus)) +
  scale_fill_viridis_c(name = "%", option = "plasma", 
                       limits = c(0, 100), 
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(title = "b. Estimated % with BLL > 10 μg/dL")

p3c <- bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = 100 * relative_iq_cost * beta_IQ_integral)) +
  scale_fill_viridis_c(option = "turbo", name = '%') + 
  labs(title = "c. Estimated economic damage")

p3 <- p3a / p3b / p3c + plot_annotation('Common scales for (a) / (b), (c) gets its own scale')
print(p3)

#===============================================================================
# ALTERNATIVE: Binned categories 
#===============================================================================

# Create meaningful categories
bllWorld_binned <- bllWorld |>
  mutate(
    bll5_category = cut(100 * frac5plus, 
                        breaks = c(-Inf, 5, 15, 30, 50, Inf),
                        labels = c("<5%", "5-15%", "15-30%", "30-50%", ">50%")),
    bll10_category = cut(100 * frac10plus,
                        breaks = c(-Inf, 5, 15, 30, 50, Inf),
                        labels = c("<5%", "5-15%", "15-30%", "30-50%", ">50%")),
    iq_category = cut(100 * relative_iq_cost * beta_IQ_integral,
                      breaks = c(-Inf, 2, 5, 10, 15, Inf),
                      labels = c("<2%", "2-5%", "5-10%", "10-15%", ">15%"))
  )

p_binned_a <- bllWorld_binned |> 
  ggplot() + 
  geom_sf(aes(fill = bll5_category)) +
  scale_fill_viridis_d(option = "plasma", name = "") +
  labs(title = "a. Estimated % with BLL > 5 μg/dL")

p_binned_b <- bllWorld_binned |> 
  ggplot() + 
  geom_sf(aes(fill = bll10_category)) +
  scale_fill_viridis_d(option = "plasma", name = "") +
  labs(title = "b. Estimated % with BLL > 10 μg/dL")

p_binned_c <- bllWorld_binned |> 
  ggplot() + 
  geom_sf(aes(fill = iq_category)) +
  scale_fill_viridis_d(option = "turbo", name = "") +
  labs(title = "c. Estimated economic damage")

binned_fig <- p_binned_a / p_binned_b / p_binned_c + 
  plot_annotation('Binned categories')
print(binned_fig)

