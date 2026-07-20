library(tidycensus)
library(tidyverse)
library(readxl)
library(fredr)

# Annual Debt by State (NYFED)
debt <- read_xlsx("data/raw/area_report_by_year.xlsx", sheet = "total")
debt <- debt[-c(1:6),]
colnames(debt) <- debt[1,]
debt <- debt[-1,]
debt_long <- debt %>% 
  pivot_longer(
    cols = -state,
    names_to = "quarter",    values_to = "debt"
  ) 

auto <- read_xlsx("data/raw/area_report_by_year.xlsx", sheet = "auto")
auto <- auto[-c(1:6),]
colnames(auto) <- auto[1,]
auto <- auto[-1,]
auto_long <- auto %>%
  pivot_longer(
    cols = -state,
    names_to = "quarter",
    values_to = "auto"
  )

credit <- read_xlsx("data/raw/area_report_by_year.xlsx", sheet = "creditcard")
credit <- credit[-c(1:6),]
colnames(credit) <- credit[1,]
credit <- credit[-1,]
credit_long <- credit %>%
  pivot_longer(
    cols = -state,
    names_to = "quarter",
    values_to = "credit"
  )

mortgage <- read_xlsx("data/raw/area_report_by_year.xlsx", sheet = "mortgage")
mortgage <- mortgage[-c(1:6),]
colnames(mortgage) <- mortgage[1,]
mortgage <- mortgage[-1,]
mortgage_long <- mortgage %>%
  pivot_longer(
    cols = -state,
    names_to = "quarter",
    values_to = "mortgage"
  )

studentloan <- read_xlsx("data/raw/area_report_by_year.xlsx", sheet = "studentloan")
studentloan <- studentloan[-c(1:6),]
colnames(studentloan) <- studentloan[1,]
studentloan <- studentloan[-1,]
studentloan_long <- studentloan %>%
  pivot_longer(
    cols = -state,
    names_to = "quarter",
    values_to = "studentloan"
  )

debt_long <- debt_long %>%
  left_join(auto_long, by = c("state","quarter")) %>%
  left_join(credit_long, by = c("state","quarter")) %>%
  left_join(mortgage_long, by = c("state","quarter")) %>%
  left_join(studentloan_long, by = c("state","quarter")) %>%
  mutate(
    year = as.integer(substr(quarter, 4, 7)),
    quarter = substr(quarter,1,2)
  )

# State Demographics by Year (ACS 2011-2024) 
# NB: 2020 excluded due to ACS low responses
acs_demos <- read.csv("data/raw/acs_state_annual_demographics.csv")
acs_demos <- acs_demos %>%
  mutate(
    state = state.abb[match(NAME, state.name)],
    state = case_when(
      NAME == "District of Columbia" ~ "DC",
      NAME == "Puerto Rico" ~ "PR",
      TRUE ~ state,
  ))

# Panel of state debt and demographics 2011-2024
debtpanel <- debt_long %>%
  left_join(acs_demos, by = c("year","state")) %>%
  filter(year %in% 2011:2024) %>%
  select(!c(X))

debtpanel <- debtpanel %>%
  select(state, year, variable, estimate) %>% 
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) %>%
  left_join(debt_long, by = c("state","year")) %>%
  select(!c(`NA`,quarter)) %>%
  mutate(debt = as.numeric(debt))

# Add in CPI adjustment for real dollar values
fredr_set_key("5913cd67e96776fd31cf83ccc1d7b96c")

cpi <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2003-01-01")
)

cpi <- cpi %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(CPI = mean(value))

base_cpi <- cpi %>%
  filter(year == 2024) %>%
  pull("CPI")

cpi <- cpi %>%
  mutate(adj_factor = base_cpi / CPI)

debtpanel <- debtpanel %>%
  left_join(cpi, by = "year") %>%
  mutate(
    total_debt_real = as.numeric(debt) * adj_factor,
    mortgage_debt_real = as.numeric(mortgage) * adj_factor,
    auto_debt_real = as.numeric(auto) * adj_factor,
    credit_card_debt_real = as.numeric(credit) * adj_factor,
    student_debt_real = as.numeric(studentloan) * adj_factor,
    median_income_real = as.numeric(median_income) * adj_factor,
    median_rent_real = as.numeric(median_rent) * adj_factor,
    median_home_value_real = as.numeric(median_home_value) * adj_factor
  )

# Export combined Debt and Demographics panel
write.csv(debtpanel, "state_debt_demographics.csv")
