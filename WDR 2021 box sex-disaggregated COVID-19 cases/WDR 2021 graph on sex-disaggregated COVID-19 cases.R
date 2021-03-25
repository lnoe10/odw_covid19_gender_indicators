library(tidyverse)
# Looking at average monthly new cases per month and use gh5050_historical
# to see if they're sex-disaggregated

# Read in stable OWID file from Dec 15
wdr_graph_owid <- readRDS(file = "WDR 2021 box sex-disaggregated COVID-19 cases/Our World in Data Dec 15.rds")
# Read in stable GH5050 file from Dec 15
wdr_graph_gh5050 <- readRDS(file = "WDR 2021 box sex-disaggregated COVID-19 cases/GH5050 historical Dec 15.rds")
avg_new_cases <- wdr_graph_owid %>%
  # Take out World aggregate and "International", which is no longer used (was for cruise ships, etc.)
  filter(!location %in% c("World", "International")) %>%
  # Clean country name variables
  # Keep only variables we want
  select(iso3c = iso_code, country = location, date, new_cases, new_deaths) %>%
  # replace iso3 value for Kosovo with version that World Bank uses for merging.
  mutate(iso3c = case_when(
    country == "Kosovo" ~ "XKX",
    TRUE ~ iso3c
  )) %>%
  mutate(month = lubridate::month(date)) %>%
  group_by(iso3c, month) %>%
  summarize(mean_new_cases = mean(new_cases, na.rm = TRUE),
            mean_new_deaths = mean(new_deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  # Sex-disaggregated cases yes/no
  left_join(wdr_graph_gh5050 %>%
              as_tibble() %>%
              mutate(month = lubridate::month(date_cases)) %>%
              filter(!is.na(month)) %>%
              distinct(iso3c, month) %>%
              mutate(sex_disaggregated_cases = 1)) %>%
  # Sex-disaggregated deaths yes/no
  left_join(wdr_graph_gh5050 %>%
              as_tibble() %>%
              mutate(month = lubridate::month(date_deaths)) %>%
              filter(!is.na(month)) %>%
              distinct(iso3c, month) %>%
              mutate(sex_disaggregated_deaths = 1))

# Cases over time
avg_new_cases %>%
  group_by(month, sex_disaggregated_cases) %>%
  summarize(sum_cases = sum(mean_new_cases, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = month, names_from = sex_disaggregated_cases, values_from = sum_cases, names_prefix = "SDD_") %>%
  mutate(share_SDD = SDD_1/(SDD_1 + SDD_NA),
         share_NA = SDD_NA/(SDD_1 + SDD_NA)) %>%
  select(-c(SDD_NA, SDD_1)) %>%
  pivot_longer(cols = -c(month), names_to = "sdd", values_to = "share") %>%
  mutate(sdd = case_when(
    sdd == "share_NA" ~ "No",
    TRUE ~ "Yes"
  )) %>%
  rename(`Sex-disaggregated?` = sdd) %>%
  filter(month >= 3) %>%
  ggplot() +
  geom_col(aes(x = month, y = share*100, fill = `Sex-disaggregated?`)) +
  geom_line(data = avg_new_cases %>% 
              group_by(month) %>% 
              summarize(num_countries_deaths = sum(sex_disaggregated_deaths, na.rm = TRUE), 
                        num_countries_cases = sum(sex_disaggregated_cases, na.rm = TRUE)) %>% 
              ungroup() %>%
              filter(!month %in% c(1, 2, 12)),
            aes(x = month, y = num_countries_cases/190*100, color = "Proportion of\ncountries reporting\nsex-disaggregated data")) +
  scale_x_continuous(breaks = c(3, 4, 5, 6, 7, 8, 9, 10, 11),
                     labels = c(3, 4, 5, 6, 7, 8, 9, 10, 11)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
  labs(x = "Month of 2020", y = "Percent",
       caption = "Source: Global Health 50/50 30 Nov 2020 data release; Our World in Data 'Coronavirus' page; Open Data Watch (ODW) calculations") +
  scale_fill_manual(values = c("#d24d57", "#2abb9b")) +
  scale_color_manual(values = c("black")) +
  theme(panel.background = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size=.1, color="gray" ), axis.line = element_line(colour = "black"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(hjust = 0.2, size = 7.5),
        legend.key=element_blank()) +
  guides(
    color = guide_legend(order = 0, title = ""),
    fill = guide_legend(order = 1)
  )

ggsave("Output/Share of cases disaggregated by sex.png", dpi = 600)


# Deaths over time
avg_new_cases %>%
  group_by(month, sex_disaggregated_deaths) %>%
  summarize(sum_deaths = sum(mean_new_deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = month, names_from = sex_disaggregated_deaths, values_from = sum_deaths, names_prefix = "SDD_") %>%
  mutate(share_SDD = SDD_1/(SDD_1 + SDD_NA),
         share_NA = SDD_NA/(SDD_1 + SDD_NA)) %>%
  select(-c(SDD_NA, SDD_1)) %>%
  pivot_longer(cols = -c(month), names_to = "sdd", values_to = "share") %>%
  mutate(sdd = case_when(
    sdd == "share_NA" ~ "No",
    TRUE ~ "Yes"
  )) %>%
  filter(month >= 3) %>%
  ggplot() +
  geom_col(aes(x = month, y = share*100, fill = sdd)) +
  geom_line(data = avg_new_cases %>% 
              group_by(month) %>% 
              summarize(num_countries_deaths = sum(sex_disaggregated_deaths, na.rm = TRUE), 
                        num_countries_cases = sum(sex_disaggregated_cases, na.rm = TRUE)) %>% 
              ungroup() %>%
              filter(!month %in% c(1, 2, 12)),
            aes(x = month, y = num_countries_deaths/191*100)) +
  scale_x_continuous(breaks = c(3, 4, 5, 6, 7, 8, 9, 10, 11),
                     labels = c(3, 4, 5, 6, 7, 8, 9, 10, 11)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  labs(x = "Month of 2020", y = "Share of new global deaths\nShare of countries",
       title = "Sex-disaggregated reporting of COVID-19 deaths", subtitle = "Black line: Share of countries in Our World in Data (OWID) that also report sex-disaggregated data",
       caption = "Source: Global Health 50/50 30 Nov 2020 data release; OWID 'Coronavirus Pandemic (COVID-19)'; Open Data Watch (ODW) calculations",
       fill = "Sex-disaggregated?") +
  scale_fill_manual(values = c("#d24d57", "#2abb9b")) +
  theme(panel.background = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size=.1, color="gray" ), axis.line = element_line(colour = "black"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(hjust = 0.2, size = 7.5))

ggsave("Output/Share of deaths disaggregated by sex.png", dpi = 600)
