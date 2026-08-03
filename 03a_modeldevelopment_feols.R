library(tidyverse)
library(fixest)
library(did)

panel <- read.csv("data/processed/state_debt_reforms.csv") %>%
  select(-1) %>%
  filter(state != "allUS") %>%
  mutate(
    reform_year = as.numeric(reform_year),
    state_id    = as.integer(factor(state))
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

# Adding log and share variables
panel <- panel %>%
  mutate(log_credit = log(credit_card_debt_real),
         log_auto = log(auto_debt_real),
         log_mortgage = log(mortgage_debt_real),
         log_student = log(student_debt_real),
         log_income = log(median_income_real), 
         log_age = log(median_age), 
         log_population = log(total_pop),
         log_poverty = log(poverty), 
         log_labor_force = log(labor_force), 
         log_unemployed = log(unemployed),
         credit_share = credit_card_debt_real / total_debt_real,
         auto_share = auto_debt_real / total_debt_real,
         mortgage_share = mortgage_debt_real / total_debt_real,
         student_share = student_debt_real / total_debt_real,
         college_plus = college + professional + masters + doctorate,
         college_share = college_plus / total_pop,
         poverty_share = poverty / total_pop,
         labor_share = labor_force / total_pop,
         unemployment_share = unemployed / total_pop,
         white_share = white / total_pop,
         black_share = black / total_pop,
         native_share = native / total_pop,
         asian_share = asian / total_pop,
         `18_29_share` = X18_29 / total_pop,
         `30_44_share` = X30_44 / total_pop,
         `45_64_share` = X45_64 / total_pop,
         `65_plus_share` = X65plus / total_pop)

### Initial regressions using total_debt_real as dependent variable
## Pooled regression baseline regressions (bias, not controlled for panel structure)
pooled_lm_td <- lm(total_debt_real ~ rate_cap_36, data = panel)
summary(pooled_lm_td)

# Biased baseline extended with demographics
pooled_lm_td_plus <- lm(total_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(pooled_lm_td_plus)


## Panel data models controlling for panel structure and fixed effects
# Simple regression total_debt vs rate_cap_36
feols_td_pooled <- feols(total_debt_real ~ rate_cap_36, data = panel)
summary(feols_td_pooled)

feols_td_state_fe <- feols(total_debt_real ~ rate_cap_36 | state, data = panel)
summary(feols_td_state_fe)

feols_td_state_year_fe <- feols(total_debt_real ~ rate_cap_36 | state + year, data = panel)
summary(feols_td_state_year_fe)

# Extensions with demographic variables
feols_td_pooled_demo <- feols(total_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(feols_td_pooled_demo)

feols_td_state_fe_demo <- feols(total_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state, data = panel)
summary(feols_td_state_fe_demo)

feols_td_state_year_fe_demo <- feols(total_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state + year, data = panel)
summary(feols_td_state_year_fe_demo)

feols_td_state_year_fe_lean  <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` | state + year, data = panel)
summary(feols_td_state_year_fe_lean)

# Build up and iterations, testing different variables with state clustering
feols_td_fe_lean_1 <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real | state + year, data = panel, cluster = ~state)
summary(feols_td_fe_lean_1)

feols_td_fe_lean_2 <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share| state + year, data = panel, cluster = ~state)
summary(feols_td_fe_lean_2)

feols_td_fe_lean_2b <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_plus + unemployed | state + year, data = panel, cluster = ~state)
summary(feols_td_fe_lean_2b)

feols_td_fe_lean_3 <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_td_fe_lean_3)

feols_td_fe_lean_3b <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + white + black + asian + native | state + year, data = panel, cluster = ~state)
summary(feols_td_fe_lean_3b)

feols_td_fe_lean_4 <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_td_fe_lean_4)

feols_td_fe_lean_5 <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` + `65_plus_share` | state + year, data = panel, cluster = ~state)
summary(feols_td_fe_lean_5)

feols_td_fe_lean_5b <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + X18_29 + X30_44 + X45_64 + X65plus | state + year, data = panel, cluster = ~state)
summary(feols_td_fe_lean_5b)

feols_td_fe_lean_6 <- feols(total_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share + X18_29 + X30_44 + X45_64 + X65plus | state + year, data = panel, cluster = ~state)
summary(feols_td_fe_lean_6)

### Regressions with credit_card_debt_real as dependent variable (likely more illustrative of potential payday reform effects)
## Pooled regression baseline
pooled_lm_cc <- lm(credit_card_debt_real ~ rate_cap_36, data = panel)
summary(pooled_lm_cc)

# extended with demographics
pooled_lm_cc_plus <- lm(credit_card_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(pooled_lm_cc_plus)

## Panel credit card debt models
feols_cc_pooled <- feols(credit_card_debt_real ~ rate_cap_36, data = panel)
summary(feols_cc_pooled)

feols_cc_state_fe <- feols(credit_card_debt_real ~ rate_cap_36 | state, data = panel)
summary(feols_cc_state_fe)

feols_cc_state_year_fe <- feols(credit_card_debt_real ~ rate_cap_36 | state + year, data = panel)
summary(feols_cc_state_year_fe)

feols_cc_pooled_demo <- feols(credit_card_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(feols_cc_pooled_demo)

feols_cc_state_fe_demo <- feols(credit_card_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state, data = panel)
summary(feols_cc_state_fe_demo)

feols_cc_state_year_fe_demo <- feols(credit_card_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state + year, data = panel)
summary(feols_cc_state_year_fe_demo)

feols_cc_state_year_fe_lean  <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` | state + year, data = panel)
summary(feols_cc_state_year_fe_lean)

# Build up and iterations, testing different variables with state clustering
feols_cc_fe_lean_1 <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real | state + year, data = panel, cluster = ~state)
summary(feols_cc_fe_lean_1)

feols_cc_fe_lean_2 <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share| state + year, data = panel, cluster = ~state)
summary(feols_cc_fe_lean_2)

feols_cc_fe_lean_2b <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_plus + unemployed | state + year, data = panel, cluster = ~state)
summary(feols_cc_fe_lean_2b)

feols_cc_fe_lean_3 <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_cc_fe_lean_3)

feols_cc_fe_lean_3b <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + white + black + asian + native | state + year, data = panel, cluster = ~state)
summary(feols_cc_fe_lean_3b)

feols_cc_fe_lean_4 <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_cc_fe_lean_4)

feols_cc_fe_lean_5 <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` + `65_plus_share` | state + year, data = panel, cluster = ~state)
summary(feols_cc_fe_lean_5)

feols_cc_fe_lean_5b <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + X18_29 + X30_44 + X45_64 + X65plus | state + year, data = panel, cluster = ~state)
summary(feols_cc_fe_lean_5b)

feols_cc_fe_lean_6 <- feols(credit_card_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share + `18_29_share` + `30_44_share` + `45_64_share` + `65_plus_share` | state + year, data = panel, cluster = ~state)
summary(feols_cc_fe_lean_6)

### Same progression with auto debt
## Pooled regression baseline
pooled_lm_auto <- lm(auto_debt_real ~ rate_cap_36, data = panel)
summary(pooled_lm_auto)

# extended with demographics
pooled_lm_auto_plus <- lm(auto_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(pooled_lm_auto_plus)

## Panel credit card debt models
feols_auto_pooled <- feols(auto_debt_real ~ rate_cap_36, data = panel)
summary(feols_auto_pooled)

feols_auto_state_fe <- feols(auto_debt_real ~ rate_cap_36 | state, data = panel)
summary(feols_auto_state_fe)

feols_auto_state_year_fe <- feols(auto_debt_real ~ rate_cap_36 | state + year, data = panel)
summary(feols_auto_state_year_fe)

feols_auto_pooled_demo <- feols(auto_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(feols_auto_pooled_demo)

feols_auto_state_fe_demo <- feols(auto_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state, data = panel)
summary(feols_auto_state_fe_demo)

feols_auto_state_year_fe_demo <- feols(auto_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state + year, data = panel)
summary(feols_auto_state_year_fe_demo)

feols_auto_state_year_fe_lean  <- feols(auto_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` | state + year, data = panel)
summary(feols_auto_state_year_fe_lean)

# Build up and iterations, testing different variables with state clustering
feols_auto_fe_lean_1 <- feols(auto_debt_real ~ rate_cap_36 + median_income_real + median_rent_real | state + year, data = panel, cluster = ~state)
summary(feols_auto_fe_lean_1)

feols_auto_fe_lean_2 <- feols(auto_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share| state + year, data = panel, cluster = ~state)
summary(feols_auto_fe_lean_2)

feols_auto_fe_lean_2b <- feols(auto_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_plus + unemployed | state + year, data = panel, cluster = ~state)
summary(feols_auto_fe_lean_2b)

feols_auto_fe_lean_3 <- feols(auto_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_auto_fe_lean_3)

feols_auto_fe_lean_4 <- feols(auto_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_auto_fe_lean_4)

feols_auto_fe_lean_5 <- feols(auto_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` + `65_plus_share` | state + year, data = panel, cluster = ~state)
summary(feols_auto_fe_lean_5)

feols_auto_fe_lean_5b <- feols(auto_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + X18_29 + X30_44 + X45_64 + X65plus | state + year, data = panel, cluster = ~state)
summary(feols_auto_fe_lean_5b)

feols_auto_fe_lean_6 <- feols(auto_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share + `18_29_share` + `30_44_share` + `45_64_share` + `65_plus_share` | state + year, data = panel, cluster = ~state)
summary(feols_auto_fe_lean_6)

### Models with mortgage debt
pooled_lm_mg <- lm(mortgage_debt_real ~ rate_cap_36, data = panel)
summary(pooled_lm_mg)

# extended with demographics
pooled_lm_mg_plus <- lm(mortgage_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(pooled_lm_mg_plus)

## Panel credit card debt models
feols_mg_pooled <- feols(mortgage_debt_real ~ rate_cap_36, data = panel)
summary(feols_mg_pooled)

feols_mg_state_fe <- feols(mortgage_debt_real ~ rate_cap_36 | state, data = panel)
summary(feols_mg_state_fe)

feols_mg_state_year_fe <- feols(mortgage_debt_real ~ rate_cap_36 | state + year, data = panel)
summary(feols_mg_state_year_fe)

feols_mg_pooled_demo <- feols(mortgage_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(feols_mg_pooled_demo)

feols_mg_state_fe_demo <- feols(mortgage_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state, data = panel)
summary(feols_mg_state_fe_demo)

feols_mg_state_year_fe_demo <- feols(mortgage_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state + year, data = panel)
summary(feols_mg_state_year_fe_demo)

feols_mg_state_year_fe_lean  <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` | state + year, data = panel)
summary(feols_mg_state_year_fe_lean)

# Build up and iterations, testing different variables with state clustering
feols_mg_fe_lean_1 <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real | state + year, data = panel, cluster = ~state)
summary(feols_mg_fe_lean_1)

feols_mg_fe_lean_2 <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share| state + year, data = panel, cluster = ~state)
summary(feols_mg_fe_lean_2)

feols_mg_fe_lean_2b <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_plus + unemployed | state + year, data = panel, cluster = ~state)
summary(feols_mg_fe_lean_2b)

feols_mg_fe_lean_3 <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_mg_fe_lean_3)

feols_mg_fe_lean_3b <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + white + black + asian + native | state + year, data = panel, cluster = ~state)
summary(feols_mg_fe_lean_3b)

feols_mg_fe_lean_4 <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_mg_fe_lean_4)

feols_mg_fe_lean_5 <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` + `65_plus_share` | state + year, data = panel, cluster = ~state)
summary(feols_mg_fe_lean_5)

feols_mg_fe_lean_5b <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + X18_29 + X30_44 + X45_64 + X65plus | state + year, data = panel, cluster = ~state)
summary(feols_mg_fe_lean_5b)

feols_mg_fe_lean_6 <- feols(mortgage_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share + `18_29_share` + `30_44_share` + `45_64_share` + `65_plus_share` | state + year, data = panel, cluster = ~state)
summary(feols_mg_fe_lean_6)

### Final run with student debt
pooled_lm_edu <- lm(student_debt_real ~ rate_cap_36, data = panel)
summary(pooled_lm_edu)

# extended with demographics
pooled_lm_edu_plus <- lm(student_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(pooled_lm_edu_plus)

## Panel credit card debt models
feols_edu_pooled <- feols(student_debt_real ~ rate_cap_36, data = panel)
summary(feols_edu_pooled)

feols_edu_state_fe <- feols(student_debt_real ~ rate_cap_36 | state, data = panel)
summary(feols_edu_state_fe)

feols_edu_state_year_fe <- feols(student_debt_real ~ rate_cap_36 | state + year, data = panel)
summary(feols_edu_state_year_fe)

feols_edu_pooled_demo <- feols(student_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed, data = panel)
summary(feols_edu_pooled_demo)

feols_edu_state_fe_demo <- feols(student_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state, data = panel)
summary(feols_edu_state_fe_demo)

feols_edu_state_year_fe_demo <- feols(student_debt_real ~ rate_cap_36 + median_age + total_pop + white + black + native + asian + pacific + other + hispanic + college + masters + professional + doctorate + poverty + median_income + labor_force + unemployed | state + year, data = panel)
summary(feols_edu_state_year_fe_demo)

feols_edu_state_year_fe_lean  <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` | state + year, data = panel)
summary(feols_edu_state_year_fe_lean)

# Build up and iterations, testing different variables with state clustering
feols_edu_fe_lean_1 <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real | state + year, data = panel, cluster = ~state)
summary(feols_edu_fe_lean_1)

feols_edu_fe_lean_2 <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share| state + year, data = panel, cluster = ~state)
summary(feols_edu_fe_lean_2)

feols_edu_fe_lean_2b <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_plus + unemployed | state + year, data = panel, cluster = ~state)
summary(feols_edu_fe_lean_2b)

feols_edu_fe_lean_3 <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_edu_fe_lean_3)

feols_edu_fe_lean_3b <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + white + black + asian + native | state + year, data = panel, cluster = ~state)
summary(feols_edu_fe_lean_3b)

feols_edu_fe_lean_4 <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share | state + year, data = panel, cluster = ~state)
summary(feols_edu_fe_lean_4)

feols_edu_fe_lean_5 <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + `18_29_share` + `30_44_share` + `45_64_share` + `65_plus_share` | state + year, data = panel, cluster = ~state)
summary(feols_edu_fe_lean_5)

feols_edu_fe_lean_5b <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + X18_29 + X30_44 + X45_64 + X65plus | state + year, data = panel, cluster = ~state)
summary(feols_edu_fe_lean_5b)

feols_edu_fe_lean_6 <- feols(student_debt_real ~ rate_cap_36 + median_income_real + median_rent_real + college_share + unemployment_share + white_share + asian_share + black_share + native_share + `18_29_share` + `30_44_share` + `45_64_share` + `65_plus_share` | state + year, data = panel, cluster = ~state)
summary(feols_edu_fe_lean_6)

### Model summaries

### Debt shares as predictors