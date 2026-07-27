library(tidyverse)
library(fixest)
library(did) 

panel <- read.csv("data/processed/state_debt_reforms.csv") %>%
  select(-1) %>%
  filter(state != "allUS") %>%
  mutate(
    reform_year = as.numeric(reform_year),
    state_id    = as.integer(factor(state))   # did::att_gt wants a numeric id
  )

# Drop always restrictive states (no in-panel changes)
always_restrictive <- c(
  "AZ", "AR", "CT", "GA", "MD", "MA", "MT",
  "NH", "NJ", "NY", "NC", "PA", "VT", "WV", "DC"
)
 
estimation_states <- panel %>%
  filter(!state %in% always_restrictive) %>%
  distinct(state) %>%
  pull(state)

### Initial regressions using total_debt_real as dependent variable
## Pooled regression baseline regressions (bias, not controlled for panel structure)
pooled_lm_td <- lm(total_debt_real ~ comp_reform, data = panel)
summary(pooled_lm_td)

# Biased baseline extended with demographics
pooled_lm_td_plus <- lm(total_debt_real ~ comp_reform + median_age + total_pop + white + black + native + asian + pacific + other + multiracial + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(pooled_lm_td_plus)


## Panel data models controlling for panel structure and fixed effects
# Simple regression total_debt vs comp_reforms
feols_td_pooled <- feols(total_debt_real ~ comp_reform, data = panel)
summary(feols_td_pooled)

feols_td_state_fe <- feols(total_debt_real ~ comp_reform | state, data = panel)
summary(feols_td_state_fe)

feols_td_state_year_fe <- feols(total_debt_real ~ comp_reform | state + year, data = panel)
summary(feols_td_state_year_fe)

# Extensions with demographic variables
feols_td_pooled_demo <- feols(total_debt_real ~ comp_reform + median_age + total_pop + white + black + native + asian + pacific + other + multiracial + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(feols_td_pooled_demo)

feols_td_state_fe_demo <- feols(total_debt_real ~ comp_reform + median_age + total_pop + white + black + native + asian + pacific + other + multiracial + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state, data = panel)
summary(feols_td_state_fe_demo)

feols_td_state_year_fe_demo <- feols(total_debt_real ~ comp_reform + median_age + total_pop + white + black + native + asian + pacific + other + multiracial + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state + year, data = panel)
summary(feols_td_state_year_fe_demo)

### Regressions with credit_card_debt_real as dependent variable (likely more illustrative of potential payday reform effects)
## Pooled regression baseline
pooled_lm_cc <- lm(credit_card_debt_real ~ comp_reform, data = panel)
summary(pooled_lm_cc)

# extended with demographics
pooled_lm_cc_plus <- lm(credit_card_debt_real ~ comp_reform + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(pooled_lm_cc_plus)

## Panel credit card debt models
feols_cc_pooled <- feols(credit_card_debt_real ~ comp_reform, data = panel)
summary(feols_cc_pooled)

feols_cc_state_fe <- feols(credit_card_debt_real ~ comp_reform | state, data = panel)
summary(feols_cc_state_fe)

feols_cc_state_year_fe <- feols(credit_card_debt_real ~ comp_reform | state + year, data = panel)
summary(feols_cc_state_year_fe)

feols_cc_pooled_demo <- feols(credit_card_debt_real ~ comp_reform + median_age + total_pop + white + black + native + asian + pacific + other + multiracial + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(feols_cc_pooled_demo)

feols_cc_state_fe_demo <- feols(credit_card_debt_real ~ comp_reform + median_age + total_pop + white + black + native + asian + pacific + other + multiracial + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state, data = panel)
summary(feols_cc_state_fe_demo)

feols_cc_state_year_fe_demo <- feols(credit_card_debt_real ~ comp_reform + median_age + total_pop + white + black + native + asian + pacific + other + multiracial + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state + year, data = panel)
summary(feols_cc_state_year_fe_demo)

### DiD Estimations
## Simple model: 2019 Colorado reform vs never reformed UT & KS (21 obs)
co_did_data <- panel %>% filter(state %in% c("CO","UT","KS"), year %in% 2016:2022) %>%
  mutate(treated = state == "CO", post = year >= 2019)

feols_co_did <- feols(credit_card_debt_real ~ treated * post, data = co_did_data)
summary(feols_co_did)

# Extend with demographics
feols_co_did_demo <- feols(credit_card_debt_real ~ treated * post + median_income + unemployed, data = co_did_data)
summary(feols_co_did_demo)

# Add fixed effects
feols_co_did_fe <- feols(credit_card_debt_real ~ treated * post | state + year, data = co_did_data)
summary(feols_co_did_fe)

# Plot: Debt per capita in treated vs untreated (control)
co_did_data %>%
  group_by(year, treated) %>%
  summarise(mean_credit = mean(credit_card_debt_real, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(year, mean_credit, color = treated)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_vline(xintercept = 2019, linetype = "dashed") +
  labs(
    title = "Credit card debt per capita: Colorado vs. comparison states",
    x = "Year", y = "Credit card debt per capita (real $)", color = "Treated (CO)"
  ) +
  theme_minimal()

## Multiple treatments DiD: Ohio + Colorado 2019 reforms against unreformed control (440 obs)
ohco_did_data <- panel %>%
  filter(state %in% estimation_states) %>%
  filter(is.na(reform_year) | reform_year == 2019) %>%   # never-treated + 2019 cohort only
  mutate(
    treated_2019 = state %in% c("CO", "OH"),
    post         = year >= 2019
  )

# include state year fixed effects
feols_ohco_did_fe_cc <- feols(credit_card_debt_real ~ treated_2019 * post | state + year, data = ohco_did_data, cluster = ~state)
summary(feols_ohco_did_fe_cc)

feols_ohco_did_fe_auto <- feols(auto_debt_real ~ treated_2019 * post | state + year, data = ohco_did_data, cluster = ~state)
summary(feols_ohco_did_fe_auto)

feols_ohco_did_fe_mg <- feols(mortgage_debt_real ~ treated_2019 * post | state + year, data = ohco_did_data, cluster = ~state)
summary(feols_ohco_did_fe_mg)

feols_ohco_did_fe_edu <- feols(student_debt_real ~ treated_2019 * post | state + year, data = ohco_did_data, cluster = ~state)
summary(feols_ohco_did_fe_edu)

# include select demographics
feols_ohco_did_fe_demo <- feols(credit_card_debt_real ~ treated_2019 * post + median_age + median_rent_real + labor_force | state + year, data = ohco_did_data, cluster = ~state)
summary(feols_ohco_did_fe_demo)

# Economic only: income + unemployment
spec_econ <- feols(credit_card_debt_real ~ treated_2019 * post + median_income + unemployed | state + year, data = ohco_did_data)
summary(spec_econ)

# Economic + poverty
spec_econ_pov <- feols(credit_card_debt_real ~ treated_2019 * post + median_income + unemployed + poverty | state + year, data = ohco_did_data)
summary(spec_econ_pov)

# Economic + housing costs
spec_econ_housing <- feols(credit_card_debt_real ~ treated_2019 * post + median_income + unemployed + median_rent + median_home_value | state + year, data = ohco_did_data)
summary(spec_econ_housing)

# Economic + education
spec_econ_edu <- feols(credit_card_debt_real ~ treated_2019 * post + median_income + unemployed + college + masters | state + year, data = ohco_did_data)
summary(spec_econ_edu)

# Economic + age structure
spec_econ_age <- feols(credit_card_debt_real ~ treated_2019 * post + median_income + unemployed + median_age + `X18_29` + `X30_44` + `X45_64` | state + year, data = ohco_did_data)
summary(spec_econ_age)

# Economic + race/ethnicity (drop one category to avoid collinearity, e.g. white as reference)
spec_econ_race <- feols(credit_card_debt_real ~ treated_2019 * post + median_income + unemployed + black + native + asian + hispanic | state + year, data = ohco_did_data)
summary(spec_econ_race)

# Economic + poverty + housing (no education/race/age)
spec_econ_pov_housing <- feols(credit_card_debt_real ~ treated_2019 * post + median_income + unemployed + poverty + median_rent + median_home_value | state + year, data = ohco_did_data)
summary(spec_econ_pov_housing)

# Full set minus race variables (since they sum to ~1, flagged multicollinearity concern)
spec_full_norace <- feols(credit_card_debt_real ~ treated_2019 * post + median_age + total_pop + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state + year, data = ohco_did_data)
summary(spec_full_norace)

# Full set minus age brackets, minus race (keep median_age only)
spec_lean <- feols(credit_card_debt_real ~ treated_2019 * post + median_age + poverty + median_income + unemployed + college | state + year, data = ohco_did_data)
summary(spec_lean)

## Comparison table: Adjusted R2 across specifications
spec_comparison <- tibble(
  spec = c( "econ", "econ_pov", "econ_housing", "econ_edu", "econ_age",
           "econ_race", "econ_pov_housing", "full_norace", "lean"),
  within_r2 = c(
    r2(spec_econ, "wr2"),
    r2(spec_econ_pov, "wr2"),
    r2(spec_econ_housing, "wr2"),
    r2(spec_econ_edu, "wr2"),
    r2(spec_econ_age, "wr2"),
    r2(spec_econ_race, "wr2"),
    r2(spec_econ_pov_housing, "wr2"),
    r2(spec_full_norace, "wr2"),
    r2(spec_lean, "wr2")
  )
) %>%
  arrange(desc(within_r2))

spec_comparison

## Additional model runs with logged values
ohco_did_data_l <- ohco_did_data %>%
  mutate(log_credit = log(credit_card_debt_real),
         log_income = log(median_income_real), 
         log_age = log(median_age), 
         log_population = log(total_pop),
         log_poverty = log(poverty), 
         log_labor_force = log(labor_force), 
         log_unemployed = log(unemployed))

# Model runs with logged values
model_1 <- feols(log_credit ~ treated_2019 * post + log_income + log_age + log_population + log_poverty + log_labor_force + log_unemployed | state + year, data = ohco_did_data_l)
summary(model_1)
