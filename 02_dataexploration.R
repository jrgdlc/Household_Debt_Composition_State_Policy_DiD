library(tidyverse)
library(ggplot2)

panel <- read.csv("data/processed/state_debt_demographics.csv") %>%
  select(-1) %>%
  filter(state != "allUS")

summary(panel)

# Missingness check
panel %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "n_na") %>%
  filter(n_na > 0) %>%
  arrange(desc(n_na))

panel %>%
  group_by(state) %>%
  summarise(n_years = n(), .groups = "drop") %>%
  arrange(n_years)

# 2020 demographic statistics excluded due to missing ACS survey data.

# Pay-day lending indicators incorporation
# Two separate constructs, variables:
#
#   rate_cap_36  : state enforces (or enforced, once effective) an
#                  all-in APR cap of roughly 36% or lower on small-dollar/
#                  payday-style loans -- Pew's "restrictive" category.
#                  This is the closest thing to a payday-lending ban.
#
#   comp_reform  : state passed a "comprehensive reform" law that
#                  restructures payday lending (installment structure,
#                  affordability checks, fee caps) WITHOUT pushing the
#                  all-in APR down to ~36%. Payday lending continues to
#                  operate, just under materially different rules.
#                  Colorado, Ohio, and Virginia are the canonical cases.
#
# Both are coded as of-the-year dummies (1 from the effective year
# onward), so a single state can, in principle, carry a reform_year for
# each construct if its law changed more than once (New Mexico did:
# a partial 2018 cap, tightened to a strict 36% all-in cap in 2023).
#
# Sources: 
#

# States that were already in the ~36%-cap / prohibition category for the entire panel (2011-2024)
always_restrictive <- c(
  "AZ", "AR", "CT", "GA", "MD", "MA", "MT",
  "NH", "NJ", "NY", "NC", "PA", "VT", "WV", "DC"
)

# States that moved into the strict ~36% all-in cap category during the panel, with the year the cap took effect:
rate_cap_36_effective_year <- tribble(
  ~state, ~rate_cap_year,
  "SD",   2017,
  "NM",   2018,
  "IL",   2021,
  "NE",   2021
)

# States with a "comprehensive reform" (not a strict 36% cap) that took effect during the panel:
comp_reform_effective_year <- tribble(
  ~state, ~comp_reform_year,
  "CO", 2019,
  "OH", 2019,
  "VA", 2021
)

# New Mexico's 2023 tightening (HB 132) upgraded it from a partial cap to a strict 36% all-in cap on loans up to $10,000:
nm_tighten_year <- 2023

reform <- panel %>%
  distinct(state) %>%
  left_join(rate_cap_36_effective_year, by = "state") %>%
  left_join(comp_reform_effective_year, by = "state") %>%
  mutate(
    always_restrictive = state %in% always_restrictive,
    nm_tighten_year = if_else(state == "NM", nm_tighten_year, NA_real_)
  )

panel <- panel %>%
  left_join(reform, by = "state") %>%
  mutate(
    rate_cap_36 = case_when(
      always_restrictive                                   ~ 1L,
      !is.na(rate_cap_year) & year >= rate_cap_year         ~ 1L,
      TRUE                                                  ~ 0L
    ),
    nm_strict_cap = if_else(state == "NM" & year >= nm_tighten_year, 1L, 0L),
    comp_reform = case_when(
      !is.na(comp_reform_year) & year >= comp_reform_year  ~ 1L,
      TRUE                                                  ~ 0L
    ),
    any_reform = as.integer(rate_cap_36 == 1 | comp_reform == 1),
    reform_year = coalesce(rate_cap_year, comp_reform_year),
    event_time = year - reform_year
  ) %>%
  select(-rate_cap_year, -comp_reform_year, -nm_tighten_year,
         -always_restrictive)


# Data Exploration and visualization
# Plot: Average household debt per capita by rate-cap status over time
panel %>%
  group_by(year, rate_cap_36) %>%
  summarise(mean_debt_pc = mean(total_debt_real, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(year, mean_debt_pc, color = factor(rate_cap_36))) +
  geom_line(linewidth = 1) +
  labs(
    title = "Average household debt per capita by rate-cap status",
    color = "36% rate cap in effect",
    x = NULL, y = "Debt per capita (scale TBD)"
  ) +
  theme_minimal()

# Plot: Debt per capita around reform year (only states with an in-panel policy change)
panel %>%
  filter(!is.na(event_time), abs(event_time) <= 5) %>%
  group_by(event_time) %>%
  summarise(mean_debt_pc = mean(total_debt_real, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(event_time, mean_debt_pc)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Debt per capita around payday lending reform (event time)",
    x = "Years since reform took effect", y = "Debt per capita (scale TBD)"
  ) +
  theme_minimal()
