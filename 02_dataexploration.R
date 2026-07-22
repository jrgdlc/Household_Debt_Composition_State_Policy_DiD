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
# Debt data and composition
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
    x = "Years since reform took effect", y = "Debt per capita"
  ) +
  theme_minimal()

debt_composition_vars <- panel %>%
  select(matches("_debt"), -any_of("total_debt_real")) %>%
  names()
 
debt_composition_vars
 
panel_long_debt <- panel %>%
  select(state, year, all_of(debt_composition_vars)) %>%
  pivot_longer(all_of(debt_composition_vars), names_to = "debt_type", values_to = "amount")
 
# Plot: Aggregate debt composition over time (levels, stacked area)
panel_long_debt %>%
  group_by(year, debt_type) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(year, total_amount, fill = debt_type)) +
  geom_area(position = "stack") +
  labs(
    title = "Aggregate debt composition over time (all states)",
    x = "Year", y = "Total debt (real)", fill = "Debt type"
  ) +
  theme_minimal()
 
# Plot: Aggregate debt composition over time (100% stacked area)
panel_long_debt %>%
  group_by(year, debt_type) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = total_amount / sum(total_amount, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(year, share, fill = debt_type)) +
  geom_area(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Aggregate debt composition (share of total) over time",
    x = "Year", y = "Share of total debt", fill = "Debt type"
  ) +
  theme_minimal()
 
# Plot: Debt composition for states with an in-panel policy change only
reform_states <- panel %>%
  filter(!is.na(reform_year)) %>%
  distinct(state, reform_year)
 
panel_long_debt %>%
  inner_join(reform_states, by = "state") %>%
  ggplot(aes(year, amount, fill = debt_type)) +
  geom_area(position = "stack") +
  geom_vline(aes(xintercept = reform_year), linetype = "dashed", color = "black") +
  facet_wrap(~ state, scales = "free_y") +
  labs(
    title = "Debt composition for in-panel reform states",
    subtitle = "Dashed line = year reform took effect",
    x = "Year", y = "Debt (real)", fill = "Debt type"
  ) +
  theme_minimal()
 
# Plot: Debt composition for in-panel policy states (100% stacked area)
panel_long_debt %>%
  inner_join(reform_states, by = "state") %>%
  group_by(state, year) %>%
  mutate(share = amount / sum(amount, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(year, share, fill = debt_type)) +
  geom_area(position = "fill") +
  geom_vline(aes(xintercept = reform_year), linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ state) +
  labs(
    title = "Debt composition share for in-panel reform states",
    subtitle = "Dashed line = year reform took effect",
    x = "Year", y = "Share of total debt", fill = "Debt type"
  ) +
  theme_minimal()

# Demographic data
panel <- panel %>%
  mutate(across(c(total_pop, white, black, native, asian, pacific, other, multiracial,
                   hispanic, college, masters, professional, doctorate, poverty,
                   median_income, labor_force, unemployed, median_rent, median_home_value,
                   `X18_29`, `X30_44`, `X45_64`, `X65plus`, under18, median_age,
                   debt, auto, credit, mortgage, studentloan),
                as.numeric)) %>%
  mutate(
    poverty_rate     = poverty / total_pop,
    unemployment_rate = unemployed / labor_force,
    college_share    = college / total_pop,
    grad_share       = (masters + professional + doctorate) / total_pop,
    hispanic_share   = hispanic / total_pop,
    black_share      = black / total_pop,
    white_share      = white / total_pop,
    share_18_29 = `X18_29` / total_pop,
    share_30_44 = `X30_44` / total_pop,
    share_45_64 = `X45_64` / total_pop,
    share_65plus = `X65plus` / total_pop
  )

# Plot: Time trends for aggregate demographic data
panel %>%
  group_by(year) %>%
  summarise(across(c(median_age, poverty_rate, unemployment_rate,
                      college_share, grad_share, median_income), 
                    ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-year, names_to = "metric", values_to = "value") %>%
  ggplot(aes(year, value)) +
  geom_line() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "National demographic/economic trends, 2011-2024") +
  theme_minimal()

# Plot: Debt type vs median household income scatter
panel_long_debt %>%
  left_join(panel %>% select(state, year, median_income, poverty_rate,
                              unemployment_rate, college_share, median_age),
            by = c("state","year")) %>%
  ggplot(aes(median_income, amount)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ debt_type, scales = "free_y") +
  labs(title = "Debt by type vs. median household income",
       x = "Median household income", y = "Debt") +
  theme_minimal()

# Plot: Demographic variable correlation plot
cor_vars <- panel %>%
  select(total_debt_real, median_age, poverty_rate, unemployment_rate,
         college_share, grad_share, median_income, median_rent, median_home_value) %>%
  cor(use = "pairwise.complete.obs")

cor_vars %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "corr") %>%
  ggplot(aes(var1, var2, fill = corr)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot: Student loan vs age group shares scatter
panel_long_debt %>%
  filter(debt_type == "student_debt_real") %>%
  left_join(panel %>% select(state, year, share_18_29, share_30_44, share_45_64, share_65plus),
            by = c("state", "year")) %>%
  pivot_longer(starts_with("share_"), names_to = "age_group", values_to = "share") %>%
  ggplot(aes(share, amount)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ age_group, scales = "free_x") +
  labs(
    title = "Student loan debt vs. all age-group shares",
    x = "Age group share of state population", y = "Student loan debt"
  ) +
  theme_minimal()

# Plot: Mortgage loan vs age group shares scatter
panel_long_debt %>%
  filter(debt_type == "mortgage_debt_real") %>%
  left_join(panel %>% select(state, year, share_18_29, share_30_44, share_45_64, share_65plus),
            by = c("state", "year")) %>%
  pivot_longer(starts_with("share_"), names_to = "age_group", values_to = "share") %>%
  ggplot(aes(share, amount)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ age_group, scales = "free_x") +
  labs(
    title = "Mortgage loan debt vs. all age-group shares",
    x = "Age group share of state population", y = "Mortgage loan debt"
  ) +
  theme_minimal()

# Plot: Auto loan vs age group shares scatter
panel_long_debt %>%
  filter(debt_type == "auto_debt_real") %>%
  left_join(panel %>% select(state, year, share_18_29, share_30_44, share_45_64, share_65plus),
            by = c("state", "year")) %>%
  pivot_longer(starts_with("share_"), names_to = "age_group", values_to = "share") %>%
  ggplot(aes(share, amount)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ age_group, scales = "free_x") +
  labs(
    title = "Auto loan debt vs. all age-group shares",
    x = "Age group share of state population", y = "Auto loan debt"
  ) +
  theme_minimal()

# Plot: Credit loan vs age group shares scatter
panel_long_debt %>%
  filter(debt_type == "credit_card_debt_real") %>%
  left_join(panel %>% select(state, year, share_18_29, share_30_44, share_45_64, share_65plus),
            by = c("state", "year")) %>%
  pivot_longer(starts_with("share_"), names_to = "age_group", values_to = "share") %>%
  ggplot(aes(share, amount)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ age_group, scales = "free_x") +
  labs(
    title = "Credit loan debt vs. all age-group shares",
    x = "Age group share of state population", y = "Credit loan debt"
  ) +
  theme_minimal()