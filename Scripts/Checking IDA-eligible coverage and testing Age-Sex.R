# How many countries in IDA/Blend countries disaggregate?
covid_deaths_cases %>% 
  mutate(lending_cat = case_when(
    lending_cat == "IDA" ~ "IDA/Blend", 
    lending_cat == "Blend" ~ "IDA/Blend", 
    TRUE ~ lending_cat
    )) %>% 
  count(lending_cat, disaggregated_status)

# Merge with OWID cases and deaths info
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
  # Create flag for countries that have been assess by GH5050 but for which there is no info
  # Treating this group differently where we simply don't know if they have
  # any info. This is true for 16 IDA/Blend countries, GH5050 has only assessed 58 countries
  mutate(disaggregated_status = case_when(
    disaggregated_status == "None" ~ "Assessed but no info",
    TRUE ~ disaggregated_status
  ))

# How many cases in IDA/Blend countries can be disaggregated?
owid_working %>%
  filter(!is.na(disaggregated_status)) %>%
  mutate(lending_cat = case_when(
    lending_cat == "IDA" ~ "IDA/Blend", 
    lending_cat == "Blend" ~ "IDA/Blend", 
    TRUE ~ lending_cat
  ),
  disaggregated_cases = case_when(
    disaggregated_status == "Both" ~ "Cases disaggregated",
    disaggregated_status == "Cases only" ~ "Cases disaggregated",
    TRUE ~ "Cases not disaggregated"
  )) %>%
  group_by(lending_cat, disaggregated_cases) %>%
  summarize(countries = n_distinct(iso3c), cases_total = sum(total_cases, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(lending_cat) %>%
  mutate(all_cases = sum(cases_total, na.rm = TRUE), share_cases = cases_total/all_cases) %>%
  ungroup() %>%
  select(lending_cat, disaggregated_cases, countries, cases_total, share_cases)

# How many deaths in IDA/Blend countries can be disaggregated?
owid_working %>%
  filter(!is.na(disaggregated_status)) %>%
  mutate(lending_cat = case_when(
    lending_cat == "IDA" ~ "IDA/Blend", 
    lending_cat == "Blend" ~ "IDA/Blend", 
    TRUE ~ lending_cat
  ),
  disaggregated_deaths = case_when(
    disaggregated_status == "Both" ~ "Deaths disaggregated",
    disaggregated_status == "Deaths only" ~ "Deaths disaggregated",
    TRUE ~ "Deaths not disaggregated"
  )) %>%
  group_by(lending_cat, disaggregated_deaths) %>%
  summarize(countries = n_distinct(iso3c), deaths_total = sum(total_deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(lending_cat) %>%
  mutate(all_deaths = sum(deaths_total, na.rm = TRUE), share_deaths = deaths_total/all_deaths) %>%
  ungroup() %>%
  select(lending_cat, disaggregated_deaths, countries, deaths_total, share_deaths)



# Import historical Global health 50/50 Sex-disaggregated data tracker file and clean up variable names
# For historical analysis
# Get
get_agesex <- GET("https://api.globalhealth5050.org/api/v1/agesex")
get_agesex_text <- content(get_agesex, "text")
get_agesex_json <- fromJSON(get_agesex_text, flatten = TRUE)

# Initialize empty dataframe
gh5050_agesex_raw <- data.frame()
# Run loop through JSON list to extract line of data for every country
# and append to empty dataframe
for (i in 1:length(get_agesex_json$data)){
  country_df <- as.data.frame(get_agesex_json$data[i], col.names = "GH5050")
  gh5050_agesex_raw <- gh5050_agesex_raw %>%
    bind_rows(country_df)
}