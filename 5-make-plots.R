load('data/bllWorld.RData')

# Set ggplot theme
theme_set(theme_bw())

# Fraction with BLL > 5 micrograms / deciliter
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = frac5plus)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Fraction with BLL > 10 micrograms / deciliter
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = frac10plus)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Average IQ lost per individual aged 0-19: beta approximation
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = beta_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# $ value of IQ lost per individual aged 0-19: beta approximation
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = beta_returns_no_lead)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Average IQ lost per individual aged 0-19: lognormal approximation
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = lnorm_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# $ value of IQ lost per individual aged 0-19: lognormal approximation
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = lnorm_returns_no_lead)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Average IQ lost per individual aged 0-19: lower bound 
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = LB_IQ_integral)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# $ value of IQ lost per individual aged 0-19: lower bound 
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = LB_returns_no_lead)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


# $ value of IQ lost per individual aged 0-19: $1000 per lost IQ point rather 
# than a figure based on returns to education, because these are missing in
# many countries where lead is a big problem

# Beta 
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = beta_IQ_integral * 1000)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Lognormal 
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = lnorm_IQ_integral * 1000)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Lower bound
bllWorld |> 
  ggplot() + 
  geom_sf(aes(fill = LB_IQ_integral * 1000)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

rm(bllWorld)
