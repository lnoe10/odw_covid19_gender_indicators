library(tidyverse)


### IMPORT RAW DATA ####

# Set date variables to toggle between versions of data to import.
# Using 2 digit month, 2 digit day format
month <- "08"
day <- "07"

# Import ODW master codes for merging and country groups
odw_master_codes <- read_csv("Input/2021 ODW Country and Region Codes.csv") %>%
  # Clean all variable names by converting to snake case
  janitor::clean_names() %>% 
  # Clear out extra lines at the bottom that just contain notes
  filter(!is.na(country_name)) %>%
  # Clear out duplicate Faroe Islands
  distinct(iso_3_code, .keep_all = TRUE) %>%
  # Keep only relevant indicators and rename for clarity 
  select(iso3c = iso_3_code, country = country_name, odw_region_name, un_code = un_m49_code,
         incgroup = world_bank_income_group_name, lending_cat = world_bank_lending_code_july_2020,
         wbregion = world_bank_all_income_region_names) %>%
  mutate(un_code = as.numeric(un_code), 
         incgroup = fct_relevel(incgroup, "Low income", "Lower middle income", "Upper middle income", "High income"))

# Import Global health 50/50 Sex-disaggregated data tracker file and clean up variable names
covid_deaths_cases_raw <- read_csv(str_c("Input/GH5050 Covid-19 sex-disaggregated data tracker ", month, day,".csv"), na = "") %>%
  janitor::clean_names() 

# Import Our World In Data Coronavirus data and clean, keeping date of GH5050 update or
# appending latest date if Gh5050 date isn't available.
# Read in latest data from Our World in Data Github raw
owid_raw <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", guess_max = 10000)
owid <- owid_raw %>% 
  # Keep observations for latest Gh5050 update to line up
  filter(date == str_c("2020-", month, "-", day)) %>% 
  # In cases where Gh5050 date is not available (some countries take
  # longer to report, or stopped reporting), append latest date
  # of all countries
  bind_rows(owid_raw %>% 
              group_by(iso_code) %>% 
              mutate(obs_num = row_number()) %>% 
              filter(obs_num == max(obs_num, na.rm = TRUE)) %>% 
              ungroup() %>%
              select(-obs_num)) %>% 
  # Then keep distinct (in this case first) observation by iso3 code 
  # and country name. Will keep Gh5050 update day for countries that 
  # have it and will keep latest update for countries that don't
  # As of August 8, 212 location available, that should be number of obs
  # in dataset
  distinct(iso_code, location, .keep_all = TRUE) %>%
  # Keep only variables we want
  select(iso3c = iso_code, country = location, date, total_cases, total_deaths) %>%
  # replace iso3 value for Kosovo with version that World Bank uses for merging.
  mutate(iso3c = case_when(
    country == "Kosovo" ~ "XKX",
    TRUE ~ iso3c
  )) %>%
  # Add country grouping info
  left_join(odw_master_codes, by = c("iso3c"))

# Import countries that have sex AND age disaggregation for cases and deaths
# Also from Global Health 5050
covid_age_sex <- read_csv("Input/gh_sex_age_Jul24.csv") %>%
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
  # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2019estimates
  # Figure 3 population pyramid has 2019 population estimates and
  # sex breakdown.
  add_row(country = c("England", "Wales", "Scotland", "Northern Ireland"),
          iso3c = c("ENG", "WAL", "SCO", "NIR"),
          pop_total = c(56286.961, 3152.879, 5463.300, 1893.667),
          pop_male = c(27827.831, 1554.678, 2663.003, 932.717),
          pop_female = c(28459.130, 1598.201, 2800.297, 960.950)) %>%
  mutate(
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
    # Fix Bosnia wrong iso2 code
    str_detect(country, "Bosnia") ~ "BIH",
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


### ANALYSIS ####

### Table 1
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
  select(disaggregated_status, countries, num_cases, pct_male_c, pct_fem_c,
         num_deaths, pct_male_d, pct_fem_d, share_pop)

### Create table 1
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


### Table 2

# Merge OWID data with list of GH5050 countries and their sex-disaggregation status
owid_working <- owid %>%
  # Take out World aggregate and "International", which is no longer used (was for cruise ships, etc.)
  filter(!country.x %in% c("World", "International")) %>%
  # Clean country name variables
  select(-country.y) %>%
  rename(country = country.x) %>%
  # Merge in GH5050 countries and sex-disaggregation status
  left_join(covid_deaths_cases %>%
              filter(!iso3c %in% c("ENG", "WAL", "SCO", "NIR")) %>%
              add_row(iso3c = "GBR", country = "United Kingdom", disaggregated_status = "Both") %>%
              select(iso3c, disaggregated_status)) %>%
  # Clean sex-disaggregated status indicator
  mutate(disaggregated_status = case_when(
    disaggregated_status == "None" ~ NA_character_,
    TRUE ~ disaggregated_status
  ))

# Create interim df that summarizes number of cases and deaths by
# country groups around sex-disaggregated status: Both disaggregations,
# Cases only, deaths only, and neither.
(owid_table <- owid_working %>%
  group_by(disaggregated_status) %>%
    # Note we are keeping cases and deaths for countries that have
    # deaths only/cases only disaggregated instead of dropping them
    # In the blog, we mark them and discuss that these cases/deaths are not
    # actually sex-disaggregated. However, to avoid confusion, we keep them in the same
    # country group line. Need better solution in the future.
  summarize(countries = n_distinct(iso3c), cases_total = sum(total_cases, na.rm = TRUE),
            deaths_total = sum(total_deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(all_cases = sum(cases_total, na.rm = TRUE), all_deaths = sum(deaths_total, na.rm = TRUE),
         share_cases = cases_total/all_cases, share_deaths = deaths_total/all_deaths) %>%
  select(disaggregated_status, countries, cases_total, share_cases, deaths_total, share_deaths))

# Put in sub-totals of Both, Cases only, and deaths only disaggregations
(owid_table_2 <- owid_table %>%
  # Take out the "Neither" line
  filter(!is.na(disaggregated_status)) %>%
  # The function below will put in totals of all the columns, called "Total"
  janitor::adorn_totals(name = "Sub-Total") %>%
  # Append the "Neither" row from the df back
  bind_rows(owid_table %>%
              filter(is.na(disaggregated_status))))

# Add final line that adds sub-total to "Neither" info and creates Global total
(owid_table_2 %>%
    # Append total of sub-total and "Neither" category
    bind_rows(owid_table_2 %>%
              # Keep just sub-total and "Neither" group
              filter(is.na(disaggregated_status) | disaggregated_status == "Sub-Total") %>%
                # Use summarize function to create total line 
                summarize(across(countries:share_deaths, sum)) %>%
                # Then add column to match column headers and name it Global Total
                add_column(disaggregated_status = "Global Total", .before = "countries")) %>%
    # Rename "Neither" category to better label
    mutate(disaggregated_status = case_when(
      is.na(disaggregated_status) ~ "Countries w/o sex disaggregation",
      TRUE ~ disaggregated_status
    )) %>%
    write_csv("Output/Table 2 - Global data on COVID-19 cases and deaths.csv", na = ""))

### Table 3 Cases and Deaths by Income Group

# Create interim df that summarizes number of cases and deaths by
# income groups around source: GH5050 and Other
(owid_table_3 <- owid_working %>%
    group_by(disaggregated_status, incgroup) %>%
    # Note we are keeping cases and deaths for countries that have
    # deaths only/cases only disaggregated instead of dropping them
    # In the blog, we mark them and discuss that these cases/deaths are not
    # actually sex-disaggregated. However, to avoid confusion, we keep them in the same
    # country group line. Need better solution in the future.
    summarize(cases_total = sum(total_cases, na.rm = TRUE),
              deaths_total = sum(total_deaths, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(id_cols = "incgroup", names_from = "disaggregated_status", values_from = cases_total:deaths_total) %>%
    mutate(cases_total_NA = rowSums(select(., cases_total_NA, `cases_total_Deaths only`) , na.rm = TRUE),
          deaths_total_NA = rowSums(select(., deaths_total_NA, `deaths_total_Cases only`) , na.rm = TRUE)) %>%
  select(-c(`cases_total_Deaths only`, `deaths_total_Cases only`)) %>%
  mutate(total_cases = rowSums(select(., matches("cases", ignore.case = FALSE)) , na.rm = TRUE),
           total_ghcases = rowSums(select(., cases_total_Both, `cases_total_Cases only`) , na.rm = TRUE),
           total_deaths = rowSums(select(., matches("deaths", ignore.case = FALSE)) , na.rm = TRUE),
           total_ghdeaths = rowSums(select(., deaths_total_Both, `deaths_total_Deaths only`) , na.rm = TRUE),
           share_cases = total_ghcases/total_cases, share_deaths = total_ghdeaths/total_deaths) %>%
  select(incgroup, total_ghcases, total_cases, share_cases, total_ghdeaths, total_deaths, share_deaths) %>%
  mutate(incgroup = as.character(incgroup),
    incgroup = case_when(
    is.na(incgroup) ~ "Not classified",
    TRUE ~ incgroup
  )) %>%
  janitor::adorn_totals() %>%
  mutate(share_cases = case_when(
    incgroup == "Total" ~ total_ghcases/total_cases,
    TRUE ~ share_cases
  ),
  share_deaths = case_when(
    incgroup == "Total" ~ total_ghdeaths/total_deaths,
    TRUE ~ share_deaths
  )) %>%
  write_csv("Output/Table 3 - Sex-disaggregated COVID-19 cases and deaths by income group.csv", na = ""))


### Table 4 Cases and Deaths by Region

# Create interim df that summarizes number of cases and deaths by
# income groups around source: GH5050 and Other
(owid_table_4 <- owid_working %>%
    group_by(disaggregated_status, wbregion) %>%
    # Note we are keeping cases and deaths for countries that have
    # deaths only/cases only disaggregated instead of dropping them
    # In the blog, we mark them and discuss that these cases/deaths are not
    # actually sex-disaggregated. However, to avoid confusion, we keep them in the same
    # country group line. Need better solution in the future.
    summarize(cases_total = sum(total_cases, na.rm = TRUE),
              deaths_total = sum(total_deaths, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(id_cols = "wbregion", names_from = "disaggregated_status", values_from = cases_total:deaths_total) %>%
    mutate(cases_total_NA = rowSums(select(., cases_total_NA, `cases_total_Deaths only`) , na.rm = TRUE),
           deaths_total_NA = rowSums(select(., deaths_total_NA, `deaths_total_Cases only`) , na.rm = TRUE)) %>%
    select(-c(`cases_total_Deaths only`, `deaths_total_Cases only`)) %>%
    mutate(total_cases = rowSums(select(., matches("cases", ignore.case = FALSE)) , na.rm = TRUE),
           total_ghcases = rowSums(select(., cases_total_Both, `cases_total_Cases only`) , na.rm = TRUE),
           total_deaths = rowSums(select(., matches("deaths", ignore.case = FALSE)) , na.rm = TRUE),
           total_ghdeaths = rowSums(select(., deaths_total_Both, `deaths_total_Deaths only`) , na.rm = TRUE),
           share_cases = total_ghcases/total_cases, share_deaths = total_ghdeaths/total_deaths) %>%
    select(wbregion, total_ghcases, total_cases, share_cases, total_ghdeaths, total_deaths, share_deaths) %>%
    mutate(wbregion = case_when(
             is.na(wbregion) ~ "Not classified",
             TRUE ~ wbregion
           )) %>%
    janitor::adorn_totals() %>%
    mutate(share_cases = case_when(
      wbregion == "Total" ~ total_ghcases/total_cases,
      TRUE ~ share_cases
    ),
    share_deaths = case_when(
      wbregion == "Total" ~ total_ghdeaths/total_deaths,
      TRUE ~ share_deaths
    )) %>%
    write_csv("Output/Table 4 - Sex-disaggregated COVID-19 cases and deaths by region group.csv", na = ""))

################# Scratch space ################################


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