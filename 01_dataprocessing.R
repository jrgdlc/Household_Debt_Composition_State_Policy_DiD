library(tidycensus)
library(tidyverse)
library(readxl)

# Annual Debt by State (NYFED)
debt <- read_xlsx("data/raw/area_report_by_year.xlsx", sheet = "total")
debt <- debt[-c(1:6),]
colnames(debt) <- debt[1,]
debt <- debt[-1,]
debt_long <- debt %>% 
  pivot_longer(
    cols = -state,            # Keep the "state" column as is, pivot everything else
    names_to = "quarter",     # The new column that will hold your "yyyy-q1" values
    values_to = "debt"        # The new column that will hold the actual debt numbers
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

write.csv(debtpanel, "state_debt_demographics.csv")
