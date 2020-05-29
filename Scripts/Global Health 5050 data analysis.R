library(tidyverse)


### IMPORT RAW DATA ####

# Import ODW master codes for merging and country groups
odw_master_codes <- read_csv("Input/ODW Country and Region Codes 2020 master sheet.csv") %>% 
  janitor::clean_names() %>% 
  # Clear out extra lines at the bottom that just contain notes
  filter(!is.na(country_name)) %>%
  # Clear out duplicate Faroe Islands
  distinct(iso_3_code, .keep_all = TRUE) %>%
  select(iso3c = iso_3_code, country = country_name, odw_region_name, un_code = un_m49_code,
         incgroup = world_bank_income_group_name, lending_cat = world_bank_lending_code_june_2019,
         wbregion = world_bank_all_income_region_names) %>%
  mutate(un_code = as.numeric(un_code), 
         incgroup = fct_relevel(incgroup, "Low income", "Lower middle income", "Upper middle income", "High income"))

# Import Global health 50/50 Sex-disaggregated data tracker file and clean up variable names
covid_deaths_cases_raw <- read_csv("Input/GH5050 Covid-19 sex-disaggregated data tracker May28.csv") %>%
  janitor::clean_names() 

# Import Our World In Data Coronavirus data and clean, keeping latest value
# Compare to OWID Cases and Deaths
owid <- read_csv("Input/owid-covid-data_May29.csv") %>%
  select(iso3c = iso_code, country = location, date, total_cases, total_deaths) %>%
  mutate(iso3c = case_when(
    country == "Kosovo" ~ "XKX",
    TRUE ~ iso3c
  )) %>%
  # Add country grouping info
  left_join(odw_master_codes, by = c("iso3c")) %>%
  # By country, keep only latest value. 
  # Assumes data is sorted from earliest to latest datapoint, check!
  group_by(iso3c) %>%
  mutate(obs_num = row_number()) %>%
  filter(obs_num == max(obs_num, na.rm = TRUE)) %>%
  ungroup()

# Import countries that have sex AND age disaggregation for cases and deaths
# Also from Global Health 5050
covid_age_sex <- read_csv("Input/gh_sex_age.csv") %>%
  filter(country != "Scotland") %>%
  mutate(
    country = case_when(
      country == "England" ~ "United Kingdom",
      TRUE ~ country
    ),
    iso3c = countrycode::countrycode(country, "country.name", "iso3c"))

### ADDITIONAL PROCESSING ####

# For sex-disaggregated cases and deaths, create aggregates for 
# UK out of weighted average of subcomponents to make country analysis easier
# Will treat UK overall as having full sex disaggregation.
# Check back as updates are made
uk_aggregate <- covid_deaths_cases_raw %>%
  # Keep just UK countries
  filter(str_detect(country, "England|Wales|Scotland|Northern Ireland")) %>%
  # Create weighted average by first creating total cases and deaths
  mutate(num_cases_male = cases_percent_male/100*cases,
         num_cases_female = cases_percent_female/100*cases,
         num_deaths_male = deaths_percent_male/100*deaths,
         num_deaths_female = deaths_percent_female/100*deaths) %>%
  # Sum across all four countries
  summarize(cases = sum(cases, na.rm = TRUE),
            total_cases_male = sum(num_cases_male, na.rm = TRUE),
            total_cases_female = sum(num_cases_female, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            total_deaths_male = sum(num_deaths_male, na.rm = TRUE),
            total_deaths_female = sum(num_deaths_female, na.rm = TRUE)) %>%
  # Create relevant indicators and other variables that we need
  mutate(cases_percent_male = round(total_cases_male/cases*100,0),
         cases_percent_female = round(total_cases_female/cases*100,0),
         deaths_percent_male = round(total_deaths_male/deaths*100,0),
         deaths_percent_female = round(total_deaths_female/deaths*100,0),
         country = "United Kingdom", sex_disaggregated = "Yes",
         date = "12.05.20") %>%
  select(-c(total_cases_male, total_cases_female, total_deaths_male, total_deaths_female))

# Append back to dataset, dropping UK countries first, creating master dataset
covid_deaths_cases <- covid_deaths_cases_raw %>%
  filter(!str_detect(country, "England|Wales|Scotland|Northern Ireland")) %>%
  bind_rows(uk_aggregate) %>%
  # Additinal clean and relevant indicators
  mutate(
    country = case_when(
      country == "Dijbouti" ~ "Djibouti",
      country == "El Salvidor" ~ "El Salvador",
      TRUE ~ country
    ),
  iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
  year = as.numeric(str_extract(date, "[0-9]{4}$")),
  month = as.numeric(str_extract(date, "(?<=\\.)[0-9]{2}(?=\\.)")),
  day = as.numeric(str_extract(date, "^[0-9]{2}(?=\\.)")),
  source_name = "Global Health 50/50") %>%
  # Drop any countries that we didn't get country codes for
  filter(!is.na(iso3c)) %>%
  # Make sure we have no duplicates (older versions had them, so just to be safe)
  distinct(iso3c, .keep_all = TRUE) %>%
  # Import population from WDI, replace with UN WPP for 2020?
  left_join(WDI::WDI(indicator = c("pop" = "SP.POP.TOTL"), start = 2018, end = 2018, extra = TRUE) %>% 
              select(iso3c, pop)) %>%
  # Add country groups
  left_join(odw_master_codes, by = c("iso3c"))

### ANALYSIS ####

# Population of sex-disaggregated countries. World pop in 2018: 7594270360
covid_deaths_cases %>%
  group_by(sex_disaggregated) %>%
  summarize(sum_pop = sum(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(share_pop = sum_pop/7594270360)

# Number of countries with sex-disaggregation
covid_deaths_cases %>%
  count(sex_disaggregated)

# Covid cases male and female shares by groups
covid_deaths_cases %>% 
  # Create absolute number of cases
  mutate(num_cases_male = cases_percent_male/100*cases, 
         num_cases_female = cases_percent_female/100*cases) %>% 
  # Take out places that have total cases, but don't have sex-disaggregated data 
  # so we're not counting cases that don't have sex-disaggregation
  filter(!is.na(num_cases_male)) %>% 
  # Collapse to sum number of cases and sex-disaggregated cases
  group_by(sex_disaggregated) %>% 
  summarize(num_cases = sum(cases, na.rm = TRUE), 
            cases_male = sum(num_cases_male, na.rm = TRUE), 
            cases_female = sum(num_cases_female, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Create shares of sex-disaggregated cases
  mutate(pct_male = cases_male/num_cases, 
         pct_fem = cases_female/num_cases)

# Covid deaths male and female shares by groups
covid_deaths_cases %>% 
  mutate(num_deaths_male = deaths_percent_male/100*deaths, 
         num_deaths_female = deaths_percent_female/100*deaths) %>% 
  # Take out places that have total deaths, but don't have sex-disaggregated data 
  # so we're not counting deaths that don't have sex-disaggregation
  filter(!is.na(num_deaths_male)) %>% 
  # Collapse to sum number of deaths and sex-disaggregated deaths
  group_by(sex_disaggregated) %>% 
  summarize(num_deaths = sum(deaths, na.rm = TRUE), 
            deaths_male = sum(num_deaths_male, na.rm = TRUE), 
            deaths_female = sum(num_deaths_female, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Create shares of sex-disaggregated deaths
  mutate(pct_male = deaths_male/num_deaths, 
         pct_fem = deaths_female/num_deaths)

# Totals By income group
# Cases
covid_deaths_cases %>%
  filter(!is.na(cases_percent_male)) %>%
  group_by(incgroup) %>%
  summarize(sum_cases = sum(cases, na.rm = TRUE))

# Deaths
covid_deaths_cases %>%
  filter(!is.na(deaths_percent_male)) %>%
  group_by(incgroup) %>%
  summarize(sum_deaths = sum(deaths, na.rm = TRUE))

# Total Deaths and Cases from Our World in Data
owid %>%
  filter(country.x != "World") %>%
  group_by(incgroup) %>%
  summarize(sum_cases = sum(total_cases, na.rm = TRUE),
            sum_deaths = sum(total_deaths, na.rm = TRUE))

# Summarize number of people living in countries with sex and age disaggregation
covid_deaths_cases %>%
  left_join(covid_age_sex, by = c("iso3c")) %>%
  group_by(sex_age) %>%
  summarize(sumpop = sum(pop, na.rm = TRUE), countries = n_distinct(iso3c))
# Use world population to get share of people living in countries with sex AND age disaggregation
# 431639358 + 818761165
# 1250400523/7594270360
# 0.1646505

# How many cases and deaths are sex and age disaggregated as a share of total cases/deaths?
# Cases
covid_deaths_cases %>%
  left_join(covid_age_sex, by = c("iso3c")) %>%
  filter(!is.na(cases_percent_male)) %>%
  group_by(incgroup, sex_age) %>%
  summarize(sum_cases = sum(cases, na.rm = TRUE))
# Then divide by income group total cases

# Deaths
covid_deaths_cases %>%
  left_join(covid_age_sex, by = c("iso3c")) %>%
  filter(!is.na(deaths_percent_male)) %>%
  group_by(incgroup, sex_age) %>%
  summarize(sum_deaths = sum(deaths, na.rm = TRUE))
# Then divide by income group total deaths