library(tidyverse)

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

owid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", guess_max = 10000) %>%
  group_by(iso_code) %>%
  arrange(iso_code, date) %>%
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
  # Keep only latest datapoint (under sorting by country and date, latest date)
  # filter(row_number() == max(row_number(), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(iso_code = case_when(
    location == "Kosovo" ~ "XKX",
    TRUE ~ iso_code
  )) %>%
  left_join(odw_master_codes, by = c("iso_code" = "iso3c")) %>%
  filter(!location %in% c("World", "International")) %>%
  select(iso3c = iso_code, date, country = location, new_cases, new_cases_per_million,
         avg_new_7_day, avg_new_14_day, avg_new_31_day,
         avg_new_7_day_pm, avg_new_14_day_pm, avg_new_31_day_pm, avg_new_61_day_pm,
         new_deaths, new_deaths_per_million, avg_newd_7_day_pm, avg_newd_14_day_pm, avg_newd_31_day_pm, avg_newd_61_day_pm,
         incgroup, wbregion, lending_cat) %>%
  arrange(iso3c, date)

# Find local maxima
owid_test <- owid %>%
  mutate(local_max_cases = if_else(
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
      lead(avg_new_61_day_pm, n = 7) < avg_new_61_day_pm, TRUE, FALSE),
    local_max_deaths = if_else(
      lag(avg_newd_61_day_pm, n = 4) < avg_newd_61_day_pm & 
      lag(avg_newd_61_day_pm, n = 3) < avg_newd_61_day_pm & 
        lag(avg_newd_61_day_pm, n = 2) < avg_newd_61_day_pm & 
        lag(avg_newd_61_day_pm) < avg_newd_61_day_pm & 
        lead(avg_newd_61_day_pm) < avg_newd_61_day_pm & 
        lead(avg_newd_61_day_pm, n = 2) < avg_newd_61_day_pm &
        lead(avg_newd_61_day_pm, n = 3) < avg_newd_61_day_pm &
        lead(avg_newd_61_day_pm, n = 4) < avg_newd_61_day_pm, TRUE, FALSE)) %>%
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

# Chart of trends
owid_test %>% 
  filter(iso3c %in% c("BGD", "SWZ", "GMB", "CPV", "UKR")) %>% 
  ggplot(aes(x = date, y = avg_new_61_day_pm, color = country)) + 
  geom_line() + 
  geom_point(data = owid_test %>% 
               filter(iso3c %in% c("BGD", "SWZ", "GMB", "CPV", "UKR"), local_max_cases == TRUE)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  labs(x = "", y = "Rolling average of new cases over two months") +
  theme(axis.text.x = element_text(angle = 90))

last_point <- owid_test %>%
  group_by(iso3c) %>%
  slice(which.max(date)) %>%
  ungroup()

owid_test %>% 
  filter(iso3c %in% c("CPV", "SWZ", "STP", "UKR", "GMB")) %>% 
  ggplot(aes(x = date, y = avg_new_61_day_pm, color = country)) + 
  geom_line() + 
  geom_text(data = last_point %>%
                             filter(iso3c %in% c("CPV", "SWZ", "STP", "UKR", "GMB")),
                           aes(x = date, y = avg_new_61_day_pm, color = country, label = country, hjust = 0)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", limits = c(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-10-31"))) +
  labs(x = "", y = "Rolling average of new cases over two months",
       title = "Cluster 3 countries") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")

ggsave("Output/Cluster 3 countries.png", dpi = 600)

owid_test %>% 
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
owid_test %>% 
  filter(incgroup %in% c("Low income", "Lower middle income")) %>% 
  ggplot(aes(x = date, y = avg_new_61_day_pm, color = iso3c)) + 
  geom_line() + 
  geom_point(data = owid_test %>% 
               filter(incgroup %in% c("Low income", "Lower middle income"), local_max_cases == TRUE)) + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d",
               limits = c(Sys.Date() - 252, Sys.Date())) +
  labs(x = "", y = "Rolling average of new cases over two months") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")

ggsave("Output/Low and lower middle cases Sep14.png", dpi = 400)

owid_test %>%
  filter(date != "2019-12-31") %>%
  write_csv("Data/Output Data/owid_case_deaths_ma.csv", na = "")

owid %>%
  write_csv("Data/Output Data/owid_case_avg.csv", na = "")

owid_trend <- owid %>%
  # Drop NA average new cases. Breaks model otherwise.
  # This should only drop HK, where no info is available
  # Additional filter :, date <= as.Date("2020-09-10")
  filter(iso3c != "HKG") %>%
  # Keep last four weeks
  group_by(country, iso3c)# %>%
  # Date format can be modified like numeric!
  # Keeping if date is more recent than four weeks ago, and less than or equal to most recent date
  # filter(date >= max(date, na.rm = TRUE) - 27 & date <= max(date, na.rm = TRUE))


library(broom)

test <- owid %>%
  select(country, iso3c, avg_new_7_day_pm, date) %>%
  mutate(new_date = as.numeric(date)) %>%
  # filter(!is.na(avg_new_61_day_pm)) %>%
  nest(-c(country, iso3c)) %>%
  mutate(fit = map(data, ~ loess(avg_new_7_day_pm ~ new_date, .))) %>%
  unnest(purr::map2(fit, data, augment))

models <- owid %>%
  tidyr::nest(-c(country, iso3c)) %>%
  mutate(
    # Perform loess calculation on each CpG group
    m = purrr::map(data, loess,
                   formula = avg_new_7_day_pm ~ as.numeric(date)),
    # Retrieve the fitted values from each model
    fitted = purrr::map(m, `[[`, "fitted")
  )

# Run linear model
model_coeffs_lm_61 <- do(owid_trend,
   broom::tidy(
     lm(avg_new_61_day_pm ~ date, data = .)
   )) %>%
  filter(term == "date") %>%
  ungroup()

model_coeffs_lm_31 <- do(owid_trend,
                         broom::tidy(
                           lm(avg_new_31_day_pm ~ date, data = .)
                         )) %>%
  filter(term == "date") %>%
  ungroup()

# Fitted values
model_fitted_lm_61 <- do(owid_trend,
                           broom::augment(
                             lm(avg_new_61_day_pm ~ date, data = .)
                           )) %>%
    ungroup()

model_fitted_lm_31 <- do(owid_trend,
                         broom::augment(
                           lm(avg_new_31_day_pm ~ date, data = .)
                         )) %>%
  ungroup() 

# Prep dataset for LOESS smoothing. Using 7 day rolling average
owid_na <- owid %>% 
  filter(!is.na(avg_new_7_day_pm)) %>% 
  mutate(date = as.numeric(date)) %>%
  group_by(country, iso3c)
# Predicting new values (.fitted) per day per country, setting 
model_fitted_loess_7 <- do(owid_na,
                         broom::augment(
                           loess(avg_new_7_day_pm ~ date, data = ., na.action = "na.omit", span = 0.5)
                         )) %>%
  ungroup() %>%
  mutate(date = as.Date(date, origin = "1970-01-01"))

owid_daily_na <- owid %>% 
  filter(!is.na(new_cases_per_million)) %>% 
  mutate(date = as.numeric(date)) %>%
  group_by(country, iso3c)
model_fitted_loess_daily <- do(owid_daily_na,
                           broom::augment(
                             loess(new_cases_per_million ~ date, data = ., na.action = "na.omit", span = 0.25)
                           )) %>%
  ungroup() %>%
  mutate(date = as.Date(date, origin = "1970-01-01"),
         .fitted = case_when(
           .fitted <0 ~ 0,
           TRUE ~ .fitted
         )) %>%
  group_by(iso3c) %>% 
  arrange(iso3c, date) %>%
  mutate(rolling_max_back = RcppRoll::roll_max(lag(.fitted), 17, fill = NA, align = "right"),
         rolling_max_forward = RcppRoll::roll_max(lead(.fitted), 17, fill = NA, align = "left"),
         rolling_min_back = RcppRoll::roll_min(lag(.fitted), 17, fill = NA, align = "right"),
         rolling_min_forward = RcppRoll::roll_min(lead(.fitted), 17, fill = NA, align = "left")) %>%
  fill(ends_with("back"), .direction = "up") %>%
  fill(ends_with("forward"), .direction = "down") %>%
  ungroup() %>%
  mutate(peak = case_when(
    .fitted > rolling_max_back & .fitted > rolling_max_forward ~ .fitted,
    .fitted > rolling_max_back & date == max(date, na.rm = TRUE) ~ .fitted,
    TRUE ~ NA_real_
  ),
  trough = case_when(
    .fitted < rolling_min_back & .fitted < rolling_min_forward ~ .fitted,
    TRUE ~ NA_real_
  ))

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
  write_csv("Data/Output Data/New cases smoothed.csv", na = "")

# Master graph for daily cases
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

# Replicates loess in geom_smooth perfectly.
owid %>%
  left_join(model_fitted_loess_7 %>% select(iso3c, date, pred_loess7 = .fitted)) %>%
  filter(iso3c == "BGD") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = avg_new_7_day_pm)) +
  geom_smooth(aes(y = avg_new_7_day_pm, color = "blue"), se = FALSE, span = 0.5) +
  geom_line(aes(y = pred_loess7, color = "red"))

owid %>%
  left_join(model_fitted_loess_daily %>% select(iso3c, date, pred_loess_daily = .fitted)) %>%
  filter(iso3c == "BGD") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = new_cases_per_million)) +
  geom_line(aes(y = pred_loess_daily, color = "LOESS\nSmoothed")) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(x = "", y = "New daily cases per million",
       title = "Bangladesh") +
  theme(axis.text.x = element_text(angle = 90), legend.title = element_blank())
ggsave("Output/BGD smoothed per million.png", dpi = 600)



# Calculate direction in trend
diff_test <- model_fitted_lm_31 %>%
  select(country, iso3c, date, avg_new_31_day_pm, fit_31_day = .fitted, se_31_day = .se.fit) %>%
  left_join(model_fitted_lm_61 %>% select(country, iso3c, date, avg_new_61_day_pm, fit_61_day = .fitted)) %>%
  left_join(odw_master_codes %>% select(iso3c, incgroup)) %>%
  filter(incgroup %in% c("Low income", "Lower middle income")) %>%
  group_by(country, iso3c, incgroup) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(status = case_when(
    (avg_new_31_day_pm - avg_new_61_day_pm > 0) & (abs(avg_new_31_day_pm - avg_new_61_day_pm) >= 2*se_31_day) ~ "Increasing",
    (avg_new_31_day_pm - avg_new_61_day_pm < 0) & (abs(avg_new_31_day_pm - avg_new_61_day_pm) >= 2*se_31_day) ~ "Decreasing",
    TRUE ~ "Uncertain"
  ))

diff_test %>%
  count(status)

diff_test %>%
  filter(status == "Increasing") %>%
  count(avg_new_31_day_pm > avg_new_61_day_pm)

diff_test_15Sep %>% 
  left_join(diff_test, by = c("country", "iso3c", "incgroup")) %>% 
  filter(status.x != status.y) %>% 
  select(country, status.x, status.y)

diff_test %>%
  writexl::write_xlsx("Data/Output Data/Difference in moving averages.xlsx")

owid_test %>% 
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

# How to do this with Exponential growth?
# Seems to have to do with NLS


# Checking model outputs correct coefficients pasting linear
# equation on trendline
owid_trend %>%
  ungroup() %>%
  filter(iso3c == "NGA") %>%
  ggplot(aes(x = date, y = avg_new_14_day_pm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(date = 2020-08-03, avg_new_14_day_pm = 14, label = lm_eq(owid_trend %>% ungroup() %>% filter(iso3c == "NGA")), parse = TRUE)

lm_eq <- function(df){
  
  m <- lm(avg_new_14_day_pm ~ date, df);
  
  eq <- substitute((y) == a + b %.% (x)*","~~(r)^2~"="~r2, 
                   
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        
                        b = format(unname(coef(m)[2]), digits = 2),
                        
                        r2 = format(summary(m)$r.squared, digits = 3)))
  
  as.character(as.expression(eq));
  
}
lm_eq <- function(df){
  
  m <- lm(avg_new_14_day_pm ~ date, df);
  
  eq <- substitute((y) == a + b %.% (x)*","~~(r)^2~"="~r2, 
                   
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        
                        b = format(unname(coef(m)[2]), digits = 2),
                        
                        r2 = format(summary(m)$r.squared, digits = 3)))
  
  as.character(as.expression(eq));
  
}

owid %>%
  filter(avg_new_7_day > avg_new_31_day) %>%
  arrange(desc(avg_new_7_day)) %>%
  select(country, avg_new_7_day, avg_new_31_day)

owid %>%
  mutate(delta_cases = (avg_new_7_day - avg_new_31_day)/avg_new_31_day) %>%
  arrange(desc(delta_cases)) %>%
  filter(incgroup != "High income", avg_new_7_day > avg_new_31_day) %>%
  select(country, avg_new_7_day, avg_new_31_day)

owid %>%
  filter(location %in% c("United States", "Germany", "India")) %>%
  ggplot(aes(x = date, y = avg_14_day, color = location)) +
  geom_line()

test <- read_csv("Data/Input Data/owid-covid-data-23Jul.csv", guess_max = 33000) %>%
  group_by(iso_code) %>%
  arrange(iso_code, date) %>%
  filter(!is.na(new_cases_per_million))

# Possible function for logest
logest <- function(y, x, ...){
  if(missing(x) || is.null(x)) x <- seq_along(y)
  result <- lm(log(y + 0.0000000001) ~ x, ...)
  (exp(result$coefficients[2]) - 1)*100
}

# Add columns to dataframe to test logest results from Eric's worksheet
test$avg_7_day <- slider::slide_dbl(test$new_cases_per_million, ~logest(.x), .before = 6)
test$avg_31_day <- slider::slide_dbl(test$new_cases_per_million, ~logest(.x), .before = 30)


# Similar way of doing it via broom package
# From here http://r4stats.com/2017/04/18/group-by-modeling-in-r-made-easy/
do(test,
   tidy(
     lm(log(new_cases_per_million + 1) ~ seq_along(new_cases_per_million), data = .)))
