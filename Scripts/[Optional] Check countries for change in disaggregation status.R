# Import Global health 50/50 Sex-disaggregated data tracker file and clean up variable names
old_covid_deaths_cases_raw <- read_csv(str_c("Input/GH5050 Covid-19 sex-disaggregated data tracker ", "Jul", "09",".csv")) %>%
  janitor::clean_names() 

# Creating master dataset out of GH5050 datasets
old_covid_deaths_cases <- old_covid_deaths_cases_raw %>%
  # Additional clean and add relevant indicators
  mutate(
    # Use provided ISO2 country codes to assign country codes to countries. Will trigger warning that
    # Some values were not matched unambiguously.
    # This will be because nations of UK won't be matched, we're
    # cleaning them separately below.
    iso3c = countrycode::countrycode(country_code, "iso2c", "iso3c"),
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
    ),
    # Clean percent male and female for cases and deaths variables by removing
    # percent sign
    cases_percent_male = as.numeric(str_remove(cases_percent_male, "%")),
    cases_percent_female = as.numeric(str_remove(cases_percent_female, "%")),
    deaths_percent_male = as.numeric(str_remove(deaths_percent_male, "%")),
    deaths_percent_female = as.numeric(str_remove(deaths_percent_female, "%"))) %>%
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
         )) %>%
  rename(total_cases = cases_where_sex_disaggregated_data_is_available,
         total_deaths = deaths_where_sex_disaggregated_data_is_available)

# Table of which countries have changed disaggregated status
# looking at CASES to see which changes are likely to make a big impact
covid_deaths_cases %>%
  left_join(old_covid_deaths_cases %>% select(iso3c, disaggregated_status, total_cases, total_deaths), by = c("iso3c")) %>%
  filter(disaggregated_status.x != disaggregated_status.y) %>%
  select(country, disaggregated_status.x, disaggregated_status.y, total_cases.x, total_cases.y, total_deaths.x, total_deaths.y)

# Table of which countries have changed disaggregated status
# looking at DEATHS to see which changes are likely to make a big impact
covid_deaths_cases %>%
  left_join(old_covid_deaths_cases %>% select(iso3c, disaggregated_status, total_cases, total_deaths), by = c("iso3c")) %>%
  filter(disaggregated_status.x != disaggregated_status.y) %>%
  select(country, disaggregated_status.x, disaggregated_status.y, total_deaths.x, total_deaths.y)

# Looking at new countries
covid_deaths_cases %>%
  left_join(old_covid_deaths_cases %>% select(iso3c, disaggregated_status, total_cases, total_deaths), by = c("iso3c")) %>%
  filter(is.na(disaggregated_status.y)) %>%
  select(country, disaggregated_status.x, disaggregated_status.y, total_cases.x, total_cases.y)
