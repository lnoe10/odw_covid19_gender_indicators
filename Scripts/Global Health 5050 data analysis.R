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

# Import 2019 Population from UN World Population prospects 2019
# This is a filtered CSV of the "Total Population", "All variants" file
# found here https://population.un.org/wpp/Download/Standard/CSV/
# This is filtered for year (2019), as well as Variant (Medium)
pop_2019 <- read_csv("Input/WPP2019_TotalPopulationBySex.csv") %>%
  janitor::clean_names() %>%
  # UN uses iso3 numeric country identifiers. Because we use iso3 character everywhere else,
  # merge in ISO country name list, numeric, and character code from
  # https://www.iso.org/obp/ui/#search click on country codes and search
  # merging in with inner_join also keeps only those entities that are actual
  # countries, since ISO list does not contain aggregates unlike UN WPP
  inner_join(read_csv("Input/iso_name_char_code.csv") %>%
               janitor::clean_names(), by = c("loc_id" = "numeric")) %>%
  # Select only relevant variables
  select(iso_num = loc_id, country = location, pop_male:pop_total, iso3c = alpha_3_code) %>%
  # Because GH 5050 reports constitutent countries from UK separately, create sub-components for
  # England, Wales, Northern Ireland, Scotland, using ONS data from
  # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2019
  # Table 1 has first breakdown of UK populations across countries
  # LOOK FOR UPDATES IN JUNE
  add_row(country = c("England", "Wales", "Scotland", "Northern Ireland"),
          iso3c = c("ENG", "WAL", "SCO", "NIR"),
          pop_total = c(56286.961, 3152.879, 5463.300, 1893.667)) %>%
  # Add male and female population for UK constituent countries
  # using 2018 breakdown from
  # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/articles/ukpopulationpyramidinteractive/2020-01-08
  mutate(pop_male = case_when(
    iso3c == "ENG" ~ 0.494*pop_total,
    iso3c == "WAL" ~ 0.493*pop_total,
    iso3c == "SCO" ~ 0.487*pop_total,
    iso3c == "NIR" ~ 0.492*pop_total,
    TRUE ~ pop_male
  ),
  pop_female = case_when(
    iso3c == "ENG" ~ pop_total - pop_male,
    iso3c == "WAL" ~ pop_total - pop_male,
    iso3c == "SCO" ~ pop_total - pop_male,
    iso3c == "NIR" ~ pop_total - pop_male,
    TRUE ~ pop_female
  ),
  # Convert all population estimates from thousands
  pop_male = pop_male*1000,
  pop_female = pop_female*1000,
  pop_total = pop_total*1000) %>%
  # filter out UK
  filter(iso3c != "GBR")

### ADDITIONAL PROCESSING ####

# Creating master dataset out of GH5050 datasets
covid_deaths_cases <- covid_deaths_cases_raw %>%
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