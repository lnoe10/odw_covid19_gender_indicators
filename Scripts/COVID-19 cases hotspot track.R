library(tidyverse)

### Importing Data ####
# Import country codes
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

# Import latest COVID-19 data from Our World in Data
# See project page here https://ourworldindata.org/coronavirus
owid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", guess_max = 10000) %>%
  # Create rolling averages ending with date x.
  # For example, if day is Sunday, this will construct 7 day rolling
  # average from previous Monday ending in Sunday.
  group_by(iso_code) %>%
  # Make sure it's sorted correctly with latest date (in date format)
  # at the end
  arrange(iso_code, date) %>%
  # Create averages for new cases as well as new cases per million
  # Also doing deaths, have not used so far.
  mutate(avg_new_7_day = slider::slide_dbl(new_cases, ~mean(.x), .before = 6),
         avg_new_14_day = slider::slide_dbl(new_cases, ~mean(.x), .before = 13),
         avg_new_31_day = slider::slide_dbl(new_cases, ~mean(.x), .before = 30),
         avg_new_7_day_pm = slider::slide_dbl(new_cases_per_million, ~mean(.x), .before = 6),
         avg_new_14_day_pm = slider::slide_dbl(new_cases_per_million, ~mean(.x), .before = 13),
         avg_new_31_day_pm = slider::slide_dbl(new_cases_per_million, ~mean(.x), .before = 30),
         avg_new_61_day_pm = slider::slide_dbl(new_cases_per_million, ~mean(.x), .before = 60),
         avg_newd_7_day_pm = slider::slide_dbl(new_deaths_per_million, ~mean(.x), .before = 6),
         avg_newd_14_day_pm = slider::slide_dbl(new_deaths_per_million, ~mean(.x), .before = 13),
         avg_newd_31_day_pm = slider::slide_dbl(new_deaths_per_million, ~mean(.x), .before = 30),
         avg_newd_61_day_pm = slider::slide_dbl(new_deaths_per_million, ~mean(.x), .before = 60)) %>%
  # filter(row_number() == max(row_number(), na.rm = TRUE)) %>%
  ungroup() %>%
  # Fix iso code for Kosovo ahead of merging
  mutate(iso_code = case_when(
    location == "Kosovo" ~ "XKX",
    TRUE ~ iso_code
  )) %>%
  # Merge in country groups
  left_join(odw_master_codes, by = c("iso_code" = "iso3c")) %>%
  # Drop World and International aggregates
  filter(!location %in% c("World", "International")) %>%
  # Keep only select variables
  select(iso3c = iso_code, date, country = location, new_cases, new_cases_per_million,
         avg_new_7_day, avg_new_14_day, avg_new_31_day,
         avg_new_7_day_pm, avg_new_14_day_pm, avg_new_31_day_pm, avg_new_61_day_pm,
         new_deaths, new_deaths_per_million, avg_newd_7_day_pm, avg_newd_14_day_pm, avg_newd_31_day_pm, avg_newd_61_day_pm,
         incgroup, wbregion, lending_cat) %>%
  # Sort again just in case
  arrange(iso3c, date)

### Analysis of Local Maxima 1 ####
# One way of finding local maxima
# Find local maxima
owid_maxima <- owid %>%
  mutate(local_max_cases = if_else(
    # Condition if value of day's 61 day average is greater than preceding and
    # succeeding 7 days.
    lag(avg_new_61_day_pm, n = 7) < avg_new_61_day_pm &
    lag(avg_new_61_day_pm, n = 6) < avg_new_61_day_pm &
    lag(avg_new_61_day_pm, n = 5) < avg_new_61_day_pm &
    lag(avg_new_61_day_pm, n = 4) < avg_new_61_day_pm & 
    lag(avg_new_61_day_pm, n = 3) < avg_new_61_day_pm & 
    lag(avg_new_61_day_pm, n = 2) < avg_new_61_day_pm & 
      lag(avg_new_61_day_pm) < avg_new_61_day_pm & 
      lead(avg_new_61_day_pm) < avg_new_61_day_pm & 
      lead(avg_new_61_day_pm, n = 2) < avg_new_61_day_pm &
      lead(avg_new_61_day_pm, n = 3) < avg_new_61_day_pm &
      lead(avg_new_61_day_pm, n = 4) < avg_new_61_day_pm &
      lead(avg_new_61_day_pm, n = 5) < avg_new_61_day_pm &
      lead(avg_new_61_day_pm, n = 6) < avg_new_61_day_pm &
      lead(avg_new_61_day_pm, n = 7) < avg_new_61_day_pm, TRUE, FALSE)) %>%
  # Create maxima at last (most recent) date if it's at the end of an increase
  # If value is greater than the average of last seven days
  group_by(iso3c) %>%
  mutate(local_max_cases = case_when(
    date == max(date, na.rm = TRUE) & 
      avg_new_61_day_pm > mean(c(
        lag(avg_new_61_day_pm, n = 7),
        lag(avg_new_61_day_pm, n = 6),
        lag(avg_new_61_day_pm, n = 5),
        lag(avg_new_61_day_pm, n = 4),
        lag(avg_new_61_day_pm, n = 3),
        lag(avg_new_61_day_pm, n = 2),
        lag(avg_new_61_day_pm)), na.rm = TRUE) ~ TRUE,
    TRUE ~ local_max_cases
  )) %>%
  ungroup()

# Chart of trends of 61 day averages with local maxima flagged
owid_maxima %>% 
  filter(iso3c %in% c("BGD", "SWZ", "GMB", "CPV", "UKR")) %>% 
  ggplot(aes(x = date, y = avg_new_61_day_pm, color = country)) + 
  geom_line() + 
  geom_point(data = owid_maxima %>% 
               filter(iso3c %in% c("BGD", "SWZ", "GMB", "CPV", "UKR"), local_max_cases == TRUE)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  labs(x = "", y = "Rolling average of new cases over two months") +
  theme(axis.text.x = element_text(angle = 90))

# Create last point dataframe to extract country label
last_point <- owid_maxima %>%
  filter(date <= lubridate::ymd("2020-10-16")) %>%
  group_by(iso3c) %>%
  slice(which.max(date)) %>%
  ungroup() %>%
  mutate(country = case_when(
    iso3c == "CPV" ~ "Cabo Verde",
    iso3c == "SWZ" ~ "Eswatini",
    TRUE ~ country
  ))

# Chart of trends of 61 day averages with country label on last point
# For, at the time, cluster 3 countries
owid_maxima %>% 
  filter(iso3c %in% c("CPV", "SWZ", "BGD", "UKR"), date <= lubridate::ymd("2020-10-16")) %>% 
  mutate(country = case_when(
    iso3c == "CPV" ~ "Cabo Verde",
    iso3c == "SWZ" ~ "Eswatini",
    TRUE ~ country
  )) %>%
  ggplot(aes(x = date, y = avg_new_61_day_pm, color = country)) + 
  geom_line() + 
  geom_text(data = last_point %>%
                             filter(iso3c %in% c("CPV", "SWZ", "BGD", "UKR"), date <= lubridate::ymd("2020-10-16")),
                           aes(x = date, y = avg_new_61_day_pm, color = country, label = country, hjust = 0)) + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d", limits = c(lubridate::ymd("2020-03-20"), lubridate::ymd("2020-11-10"))) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120), expand = expand_scale(mult = c(0, .1))) +
  labs(x = "", y = "Rolling average of new cases\nper million over two months",
       title = "Highest number of new cases per million among LIC and LMC") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none", panel.background = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size=.1, color="gray" ), axis.line = element_line(colour = "black"))

ggsave("Output/Highest new COVID-19 case countries.png", dpi = 600)

# Chart of trends of 61 day averages with country label on last point
# For, at the time, cluster 2 countries
owid_maxima %>% 
  filter(iso3c %in% c("UGA", "AGO", "ETH", "MOZ", "BGD")) %>% 
  ggplot(aes(x = date, y = avg_new_61_day_pm, color = country)) + 
  geom_line() + 
  geom_text(data = last_point %>%
              filter(iso3c %in% c("UGA", "AGO", "ETH", "MOZ", "BGD")),
            aes(x = date, y = avg_new_61_day_pm, color = country, label = country, hjust = 0)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", limits = c(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-10-31"))) +
  labs(x = "", y = "Rolling average of new cases over two months",
       title = "Cluster 2 countries") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")

ggsave("Output/Cluster 2 countries.png", dpi = 600)

# All low and lower middle income countries
# Chart of trends of 61 day averages with country label on last point
owid_maxima %>% 
  filter(incgroup %in% c("Low income", "Lower middle income")) %>% 
  ggplot(aes(x = date, y = avg_new_61_day_pm, color = iso3c)) + 
  geom_line() + 
  geom_point(data = owid_maxima %>% 
               filter(incgroup %in% c("Low income", "Lower middle income"), local_max_cases == TRUE)) + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d",
               limits = c(Sys.Date() - 252, Sys.Date())) +
  labs(x = "", y = "Rolling average of new cases over two months") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")

ggsave("Output/Low and lower middle cases Sep14.png", dpi = 400)

# Write time series to CSV
# owid_maxima %>%
#   filter(date != "2019-12-31") %>%
#   write_csv("Output/owid_case_deaths_ma.csv", na = "")


### Calculate time trends ####
### Linear ####
owid_trend <- owid %>%
  # Drop NA average new cases. Breaks model otherwise.
  # This should only drop HK, where no info is available
  # Additional filter :, date <= as.Date("2020-09-10")
  filter(iso3c != "HKG", date >= as.Date("2020-05-01"), date <= as.Date("2020-10-16")) %>%
  # Keep last four weeks
  group_by(country, iso3c)# %>%
  # Date format can be modified like numeric!
  # Keeping if date is more recent than four weeks ago, and less than or equal to most recent date
  # filter(date >= max(date, na.rm = TRUE) - 27 & date <= max(date, na.rm = TRUE))


# Run linear model to retrieve coefficients over entire time horizon
# For 61 day average of cases
model_coeffs_lm_61 <- do(owid_trend,
   broom::tidy(
     lm(avg_new_61_day_pm ~ date, data = .)
   )) %>%
  filter(term == "date") %>%
  ungroup()

# For 31 day average of cases
model_coeffs_lm_31 <- do(owid_trend,
                         broom::tidy(
                           lm(avg_new_31_day_pm ~ date, data = .)
                         )) %>%
  filter(term == "date") %>%
  ungroup()

# Run linear model to retrieve fitted values over entire time horizon
# For 61 day average of cases
model_fitted_lm_61 <- do(owid_trend,
                           broom::augment(
                             lm(avg_new_61_day_pm ~ date, data = .)
                           )) %>%
    ungroup()

# For 31 day average of cases
model_fitted_lm_31 <- do(owid_trend,
                         broom::augment(
                           lm(avg_new_31_day_pm ~ date, data = .)
                         )) %>%
  ungroup() 

### LOESS smooth ####
# Smoothing new cases as in 
# COVID-19 in the United States: Trajectories and second surge behavior
# Published in Chaos in August 2020
# They LOESS smooth and then turn values less than 0 into 0
# Prep dataset for LOESS smoothing. Using 7 day rolling average
owid_na <- owid %>% 
  filter(!is.na(avg_new_7_day_pm)) %>% 
  mutate(date = as.numeric(date)) %>%
  group_by(country, iso3c)

# Predicting new values (.fitted) per day per country
model_fitted_loess_7 <- do(owid_na,
                         broom::augment(
                           loess(avg_new_7_day_pm ~ date, data = ., na.action = "na.omit", span = 0.5)
                         )) %>%
  ungroup() %>%
  mutate(date = as.Date(date, origin = "1970-01-01"))

# Using daily new cases per million
owid_daily_na <- owid %>% 
  filter(!is.na(new_cases_per_million)) %>% 
  mutate(date = as.numeric(date)) %>%
  group_by(country, iso3c)

# Predicting new values (.fitted) per day per country
model_fitted_loess_daily <- do(owid_daily_na,
                           broom::augment(
                             loess(new_cases_per_million ~ date, data = ., na.action = "na.omit", span = 0.25)
                           )) %>%
  ungroup() %>%
  # To mirror paper, clean fitted values by turning values less than 0 into 0
  # Turn date back into date format
  mutate(date = as.Date(date, origin = "1970-01-01"),
         .fitted = case_when(
           .fitted <0 ~ 0,
           TRUE ~ .fitted
         )) %>%
  # Calculate maximum and minimum values 17 days ahead and behind (14 day incubation period and lag of reporting on weekend)
  # As used in Chaos paper
  group_by(iso3c) %>% 
  arrange(iso3c, date) %>%
  mutate(rolling_max_back = RcppRoll::roll_max(lag(.fitted), 17, fill = NA, align = "right"),
         rolling_max_forward = RcppRoll::roll_max(lead(.fitted), 17, fill = NA, align = "left"),
         rolling_min_back = RcppRoll::roll_min(lag(.fitted), 17, fill = NA, align = "right"),
         rolling_min_forward = RcppRoll::roll_min(lead(.fitted), 17, fill = NA, align = "left")) %>%
  # Carry forward max and min calculations when beginning and end values 
  # Are within 17 days
  fill(ends_with("back"), .direction = "up") %>%
  fill(ends_with("forward"), .direction = "down") %>%
  ungroup() %>%
  # Calculate peak if a fitted value is greater than both the max behind and ahead
  mutate(peak = case_when(
    .fitted > rolling_max_back & .fitted > rolling_max_forward ~ .fitted,
    .fitted > rolling_max_back & date == max(date, na.rm = TRUE) ~ .fitted,
    TRUE ~ NA_real_
  ),
  # Calculate trough if a fitted value is less than both the min behind and ahead
  trough = case_when(
    .fitted < rolling_min_back & .fitted < rolling_min_forward ~ .fitted,
    TRUE ~ NA_real_
  ))

# Add column that calculates last two months average of smoothed new cases
model_fitted_loess_daily <- model_fitted_loess_daily %>%
  left_join(model_fitted_loess_daily %>%
              filter(date >= max(date, na.rm = TRUE) - 61) %>%
              group_by(iso3c) %>%
              mutate(last_two_avg = mean(.fitted, na.rm = TRUE)) %>%
              ungroup() %>%
              select(iso3c, date, last_two_avg))

# Export
model_fitted_loess_daily %>%
  select(country, iso3c, date, `New cases per million` = new_cases_per_million, `New cases per million LOESS smoothed` = .fitted,
         peak, trough, `Average new cases per million over last 2 months` = last_two_avg) %>%
  write_csv("Output/New cases smoothed.csv", na = "")

# Master graph for daily cases, smoothed new cases, peaks, troughs and
# Last two months average
model_fitted_loess_daily %>%
  filter(iso3c == "AFG") %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = .fitted, color = "blue")) +
  geom_point(aes(y = peak), color = "red") +
  geom_point(aes(y = trough), color = "green4") + 
  geom_line(aes(y = new_cases_per_million, color = "black"), alpha = 0.25) +
  geom_line(aes(y = last_two_avg, color = "orange")) +
  labs(x = "", y = "Cases per million", title = "Afghanistan") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_manual(name = "", 
                      values =c('black'='black','blue'='blue', 'orange' = 'orange'), labels = c("New cases\nper million", "LOESS\nsmoothed", "Last 2\nmo avg."))
ggsave("Output/Sao Tome and Principe trend peaks and troughs.png", dpi = 500)  

# As a robustness check, our LOESS calculations
# replicate loess in geom_smooth perfectly.
owid %>%
  left_join(model_fitted_loess_7 %>% select(iso3c, date, pred_loess7 = .fitted)) %>%
  filter(iso3c == "BGD") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = avg_new_7_day_pm)) +
  geom_smooth(aes(y = avg_new_7_day_pm, color = "blue"), se = FALSE, span = 0.5) +
  geom_line(aes(y = pred_loess7, color = "red"))


### Calculate direction in trend ####
# Idea is to predict based on differences in 31 day and 61 day curve where
# cases are increasing/decreasing

diff_test <- model_fitted_lm_31 %>%
  select(country, iso3c, date, avg_new_31_day_pm, fit_31_day = .fitted, se_31_day = .se.fit) %>%
  left_join(model_fitted_lm_61 %>% select(country, iso3c, date, avg_new_61_day_pm, fit_61_day = .fitted)) %>%
  left_join(owid %>% select(iso3c, date, avg_newd_31_day_pm, avg_newd_61_day_pm), by = c("iso3c", "date")) %>%
  left_join(odw_master_codes %>% select(iso3c, incgroup)) %>%
  #filter(incgroup %in% c("Low income", "Lower middle income")) %>%
  group_by(country, iso3c, incgroup) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  ungroup() %>%
  # We call a country's status increasing if the average 31 day average
  # is greater than the 61 day average (i.e. more cases on average in last month than
  # in previous two months) and if this difference is greater than twice the Standard Error
  # of the 31 day average.
  # The reverse applies for Decreasing trends.
  # Where neither is applicable, we call the trend uncertain.
  mutate(status = case_when(
    (avg_new_31_day_pm - avg_new_61_day_pm > 0) & (abs(avg_new_31_day_pm - avg_new_61_day_pm) >= 2*se_31_day) ~ "Increasing",
    (avg_new_31_day_pm - avg_new_61_day_pm < 0) & (abs(avg_new_31_day_pm - avg_new_61_day_pm) >= 2*se_31_day) ~ "Decreasing",
    TRUE ~ "Uncertain"
  ))

# Export list of countries and trend status
diff_test %>%
  arrange(incgroup, country) %>%
  select(country, iso3c, incgroup, end_date = date, `31-day average of new cases per million on end_date` = avg_new_31_day_pm,
         `61-day average of new cases per million on end_date` = avg_new_61_day_pm, `Standard Error 31-day average series` = se_31_day,
         incgroup, `Status of cases` = status, `31-day average of new deaths per million on end_date` = avg_newd_31_day_pm,
         `61-day average of new deaths per million on end_date` = avg_newd_61_day_pm) %>%
  writexl::write_xlsx("Output/Difference in moving averages16Oct.xlsx")







# Extra example of nice graph with new cases, 31/61 day average, and vertical line.
owid_maxima %>% 
  filter(country == "Gambia") %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = avg_new_61_day_pm, color = "black")) + 
  geom_line(aes(y = avg_new_31_day_pm, color = "red")) + 
  geom_line(aes(y = new_cases_per_million, color = "blue")) + 
  geom_vline(xintercept = as.Date("2020-09-10"), linetype = "dashed") + 
  #geom_smooth(aes(y = avg_new_61_day_pm, color = "grey3"), method = "lm") + 
  #geom_smooth(aes(y = avg_new_31_day_pm, color = "red3"), method = "lm") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(x = "", y = "Rolling avg new cases/mill over 1 or 2 mos",
       title = "Gambia") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_identity(name = "Trends",
                       breaks = c("black", "red", "blue"),
                       labels = c("61 day average", "31 day average", "Daily cases/million"),
                       guide = "legend")

ggsave("Output/Gambia example.png", dpi = 600)

