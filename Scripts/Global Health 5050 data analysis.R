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
covid_deaths_cases_raw <- read_csv("Input/GH5050 Covid-19 sex-disaggregated data tracker Jun04.csv") %>%
  janitor::clean_names() 

# Import Our World In Data Coronavirus data and clean, keeping latest value
# Compare to OWID Cases and Deaths
owid <- read_csv("Input/owid-covid-data_Jun04.csv") %>%
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
covid_age_sex <- read_csv("Input/gh_sex_age_Jun04.csv") %>%
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

# Create variables that hold total world population
world_pop <- read_csv("Input/WPP2019_TotalPopulationBySex.csv") %>%
  janitor::clean_names() %>%
  filter(location == "World") %>%
  mutate(pop_total = pop_total*1000, pop_male = pop_male*1000, pop_female = pop_female*1000)

world_pop_total <- world_pop %>% pull(pop_total)
world_pop_male <- world_pop %>% pull(pop_male)
world_pop_female <- world_pop %>% pull(pop_female)

### ADDITIONAL PROCESSING ####

# Creating master dataset out of GH5050 datasets
covid_deaths_cases <- covid_deaths_cases_raw %>%
  # Additinal clean and relevant indicators
  mutate(
  iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
  iso3c = case_when(
    country == "England" ~ "ENG",
    country == "Wales" ~ "WAL",
    country == "Scotland" ~ "SCO",
    country == "Northern Ireland" ~ "NIR",
    TRUE ~ iso3c
  ),
  year = as.numeric(str_extract(date, "[0-9]{4}$")),
  month = as.numeric(str_extract(date, "(?<=\\.)[0-9]{2}(?=\\.)")),
  day = as.numeric(str_extract(date, "^[0-9]{2}(?=\\.)")),
  # Add source name
  source_name = "Global Health 50/50",
  # Recode sex-disaggregated variable to reflect differences in status
  disaggregated_status = case_when(
    sex_disaggregated == "Yes" ~ "Both",
    sex_disaggregated == "Partial" & !is.na(cases_percent_male) ~ "Cases only",
    sex_disaggregated == "Partial" & !is.na(deaths_percent_male) ~ "Deaths only",
    TRUE ~ "None"
  )) %>%
  # Make sure we have no duplicates (older versions had them, so just to be safe)
  distinct(iso3c, .keep_all = TRUE) %>%
  # Import population from UN WPP, see above
  left_join(pop_2019 %>% select(-c(country))) %>%
  # Add country groups
  left_join(odw_master_codes %>% select(-c(country, un_code))) %>%
  mutate(incgroup = as.character(incgroup),
  incgroup = case_when(
    iso3c == "ENG" ~ "High income",
    iso3c == "WAL" ~ "High income",
    iso3c == "SCO" ~ "High income",
    iso3c == "NIR" ~ "High income",
    TRUE ~ incgroup
  ),
  wbregion = case_when(
    iso3c == "ENG" ~ "Europe & Central Asia",
    iso3c == "WAL" ~ "Europe & Central Asia",
    iso3c == "SCO" ~ "Europe & Central Asia",
    iso3c == "NIR" ~ "Europe & Central Asia",
    TRUE ~ incgroup
  ),
  odw_region_name = case_when(
    iso3c == "ENG" ~ "Northern Europe",
    iso3c == "WAL" ~ "Northern Europe",
    iso3c == "SCO" ~ "Northern Europe",
    iso3c == "NIR" ~ "Northern Europe",
    TRUE ~ odw_region_name
  ),
  lending_cat = case_when(
    iso3c == "ENG" ~ "..",
    iso3c == "WAL" ~ "..",
    iso3c == "SCO" ~ "..",
    iso3c == "NIR" ~ "..",
    TRUE ~ lending_cat
  ))

### ANALYSIS ####

# Create master table of information for 1. Number of countries that
# have sex-disaggreation or only 1 of each, 2. Number of cases
# 3. case split between male and female, 4. Number of deaths, 5. death
# split between male and female, 6. Share of world population by category

# Covid cases male and female shares by groups
covid_table_groups <- covid_deaths_cases %>% 
  # Create absolute number of cases and deaths
  mutate(num_cases_male = cases_percent_male/100*total_cases, 
         num_cases_female = cases_percent_female/100*total_cases,
         num_deaths_male = deaths_percent_male/100*total_deaths, 
         num_deaths_female = deaths_percent_female/100*total_deaths) %>% 
  # Collapse to sum number of cases/deaths and sex-disaggregated cases/deaths
  # By group
  # Also create aggregate for population and count how many countries in each group
  group_by(disaggregated_status) %>% 
  summarize(num_cases = sum(total_cases, na.rm = TRUE), 
            cases_male = sum(num_cases_male, na.rm = TRUE), 
            cases_female = sum(num_cases_female, na.rm = TRUE),
            num_deaths = sum(total_deaths, na.rm = TRUE), 
            deaths_male = sum(num_deaths_male, na.rm = TRUE), 
            deaths_female = sum(num_deaths_female, na.rm = TRUE),
            sum_pop = sum(pop_total, na.rm = TRUE),
            countries = n_distinct(iso3c)) %>% 
  ungroup() %>% 
  # Create shares of sex-disaggregated cases/deaths and share of population
  mutate(pct_male_c = cases_male/num_cases, 
         pct_fem_c = cases_female/num_cases,
         pct_male_d = deaths_male/num_deaths, 
         pct_fem_d = deaths_female/num_deaths,
         share_pop = sum_pop/world_pop_total) %>%
  filter(disaggregated_status != "None") %>%
#  janitor::adorn_totals() %>%
  select(disaggregated_status, countries, num_cases, pct_male_c, pct_fem_c,
         num_deaths, pct_male_d, pct_fem_d, share_pop)

### Create table
# Starting with three rows we already have (Both, Cases only, Deaths only)
(covid_table_groups %>%
      # Append a row made up of sums and weighted averages
      bind_rows(
        # This row is made up of 3 segments
        bind_cols(
          # A segment of sums: Number of countries with any disaggregation
          # Total number of cases, deaths, and share of world population
          covid_table_groups %>%
            summarize(disaggregated_status = "Total", countries = sum(countries, na.rm = TRUE),
                      num_cases = sum(num_cases, na.rm = TRUE),
                      num_deaths = sum(num_deaths, na.rm = TRUE),
                      share_pop = sum(share_pop, na.rm = TRUE)),
          # A segment of weighted average male and female case prevalence
          covid_table_groups %>%
            filter(!is.nan(pct_male_c)) %>%
            summarize(pct_male_c = weighted.mean(pct_male_c, num_cases),
                      pct_fem_c = weighted.mean(pct_fem_c, num_cases)),
          # A segment of weighted average male and female death prevalence
          covid_table_groups %>%
            filter(!is.nan(pct_male_d)) %>%
            summarize(pct_male_d = weighted.mean(pct_male_d, num_deaths),
                      pct_fem_d = weighted.mean(pct_fem_d, num_deaths)))) %>%
    write_csv("Output/Table 1 - Sex-disaggregated data on the COVID-19 pandemic.csv", na = ""))

################# IN PROGRESS BELOW ################################

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
  summarize(sumpop = sum(pop_total, na.rm = TRUE), countries = n_distinct(iso3c))
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