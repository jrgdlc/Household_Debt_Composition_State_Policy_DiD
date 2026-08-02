# Payday Lending Reform and Household Debt

## Background

This project is currently **under development** and is thereby incomplete. 
This README will be duly updated with a more robust descriptions and results 
upon completion.

## Research Question
 
How does state-level payday lending reform affect household debt? The project
distinguishes two separate policy constructs, since they are not the same
treatment:
 
- **`comp_reform`** — "comprehensive reform" laws (installment structure,
  affordability checks, fee caps) that leave payday lending operating under
  new rules. Examples: Colorado and Ohio (2019), Virginia (2021).
- **`rate_cap_36`** — an all-in APR cap near 36%, the closest thing to a
  payday lending ban. States: SD (2017), NM (2018, partial; 2023, strict),
  IL (2021), NE (2021).
Five debt outcomes are examined: total, auto, credit card, mortgage, and
student loan debt (real, per-state).
 
## Data

#### Processed

- `state_debt_demographics.csv` — state-year panel, 2011–2024, debt outcomes
  (NY Fed Consumer Credit Panel) joined to ACS demographic covariates.
- `state_debt_reforms.csv` — the above plus inflation-adjusted debt/income/
  rent/home-value variables and reform indicators (`rate_cap_36`,
  `comp_reform`, `nm_strict_cap`, `any_reform`, `reform_year`, `event_time`).
- **2020 is a gap year** in ACS demographics and is treated as missing, not interpolated.
- Always-restrictive states are excluded from
  the comparison group; they contribute no in-panel treatment timing and
  would otherwise contaminate the "never-treated" pool.
