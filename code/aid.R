rm(list = ls())



# ---- load packages ----
library(here)
library(kableExtra)
library(tidyverse)
library(webshot2)



# ---- import data ----
level_1a <- read_csv(here("data", "raw", "UgandaAIMS_GeocodedResearchRelease_Level1_v1.4.1", "data", "level_1a.csv"))
# locations <- read_csv('data/raw/UgandaAIMS_GeocodedResearchRelease_Level1_v1.4.1/data/locations.csv')
# projects <- read_csv('data/raw/UgandaAIMS_GeocodedResearchRelease_Level1_v1.4.1/data/projects.csv')
transactions <- read_csv(here("data", "raw", "UgandaAIMS_GeocodedResearchRelease_Level1_v1.4.1", "data", "transactions.csv"))

# ---- clean up data ----
df <- transactions %>%
  filter(transaction_value_code == 'D') 
  # %>% # filter for disbursement
  # select(-c('transaction_isodate', 'transaction_currency','transaction_value_code')) # drop unnecessary columns; note USD currency

df <- merge(df, level_1a, by = 'project_id')
# df <- merge(df, projects, by = 'project_id')
# df <- merge(df, locations, by = 'project_id')

df <- df %>%
  filter(transactions_end_year >= 2013 & transactions_end_year <= 2017) %>% # filter for years 2013-2017
  filter(location_type_code == 'ADM2') %>% # filter for ADM2 locations
  distinct() %>% 
  arrange(project_id) %>% 
  select(c('project_id', 'project_location_id', 'transaction_id', everything()))

df_transact <- df %>%
  arrange(transactions_end_year) %>%
  distinct(transaction_id, .keep_all = TRUE)

all_transact <- df_transact %>% 
  group_by(transactions_end_year) %>%
  summarise(
    mean = mean(transaction_value),
    sd = sd(transaction_value),
    n = n(), 
    min = min(transaction_value), 
    max = max(transaction_value)
  ) %>% 
  mutate(region = 'All', variable = 'transaction') %>% 
  rename(year = transactions_end_year) %>%
  select(c('variable','region'), everything())
  
top_donors <- df_transact %>%
  group_by(donors) %>%
   summarise(
    transact_sum = sum(transaction_value, na.rm = TRUE),
    transact_n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(transact_sum)) %>%
  mutate(
    percent_transact_sum = transact_sum / sum(df_transact$transaction_value, na.rm = TRUE) * 100
  ) %>%
  select(donors, transact_sum, transact_n, percent_transact_sum)

  # make a sensible total row: sum numeric totals, set percent to 100
total_row <- top_donors %>%
  summarise(
    donors = "Total",
    transact_sum = sum(transact_sum, na.rm = TRUE),
    transact_n = sum(transact_n, na.rm = TRUE),
    percent_transact_sum = sum(percent_transact_sum)
  )

top_donors <- bind_rows(top_donors, total_row)

  # create formatted display columns (keep numeric originals for calculations)
top_donors <- top_donors %>%
  mutate(
    transact_sum_f = format(round(transact_sum), big.mark = ",", scientific = FALSE),
    transact_n_f = format(transact_n, big.mark = ",", scientific = FALSE),
    percent_transact_sum_f = paste0(format(round(percent_transact_sum, 1), nsmall = 1, scientific = FALSE), "%")
  ) %>%
  select(donors, transact_n_f, transact_sum_f, percent_transact_sum_f) %>%
  rename(Donor = donors,donors = donors,
         `# Transactions` = transact_n_f,
         `Total (USD)` = transact_sum_f,
         `Share (%)` = percent_transact_sum_f
  )

# all_transaction <- df %>% 
#   group_by(transactions_end_year) %>%
#   summarise(
#     mean = mean(transaction_value),
#     sd = sd(transaction_value),
#     n = n(), 
#     min = min(transaction_value), 
#     max = max(transaction_value)
#   ) %>% 
#   mutate(region = 'All', variable = 'transaction') %>% 
#   rename(year = transactions_end_year) %>%
#   select(c('variable','region'), everything())

all_disburse <- df_transact %>% 
  group_by(transactions_end_year) %>%
  summarise(
    mean = mean(total_disbursements),
    sd = sd(total_disbursements),
    n = n(), 
    min = min(total_disbursements), 
    max = max(total_disbursements)
  ) %>% 
  mutate(region = 'All', variable = 'total_disbursement') %>% 
  rename(year = transactions_end_year) %>%
  select(c('variable','region'), everything())

# all_total_disbursement<- df %>% 
#   group_by(transactions_end_year) %>%
#   summarise(
#     mean = mean(total_disbursements),
#     sd = sd(total_disbursements),
#     n = n(), 
#     min = min(total_disbursements), 
#     max = max(total_disbursements)
#   ) %>% 
#   mutate(region = 'All', variable = 'total_disbursement') %>% 
#   rename(year = transactions_end_year) %>%
#   select(c('variable','region'), everything())

all_even_disburse <- df_transact %>% 
  group_by(transactions_end_year) %>%
  summarise(
    mean = mean(even_split_disbursements),
    sd = sd(even_split_disbursements),
    n = n(), 
    min = min(even_split_disbursements), 
    max = max(even_split_disbursements)
  ) %>% 
  mutate(region = 'All', variable = 'even_split_disbursement') %>% 
  rename(year = transactions_end_year) %>%
  select(c('variable','region'), everything())

# all_even_disbursement<- df %>% 
#   group_by(transactions_end_year) %>%
#   summarise(
#     mean = mean(even_split_disbursements),
#     sd = sd(even_split_disbursements),
#     n = n(), 
#     min = min(even_split_disbursements), 
#     max = max(even_split_disbursements)
#   ) %>% 
#   mutate(region = 'All', variable = 'even_split_disbursement') %>% 
#   rename(year = transactions_end_year) %>%
#   select(c('variable','region'), everything())

# region_transaction <- df %>% 
#   group_by(transactions_end_year, place_name) %>%
#   summarise(
#     mean = mean(transaction_value),
#     sd = sd(transaction_value),
#     n = n(), 
#     min = min(transaction_value), 
#     max = max(transaction_value)
#   ) %>% 
#   mutate(variable = 'transaction') %>% 
#   rename(region = place_name, year = transactions_end_year) %>%
#   select(c('variable','region'), everything()) %>% 
#   arrange(region)

# region_total_disbursement <- df %>% 
#   group_by(transactions_end_year, place_name) %>%
#   summarise(
#     mean = mean(total_disbursements),
#     sd = sd(total_disbursements),
#     n = n(), 
#     min = min(total_disbursements), 
#     max = max(total_disbursements)
#   ) %>% 
#   mutate(variable = 'total_disbursement') %>% 
#   rename(region = place_name, year = transactions_end_year) %>%
#   select(c('variable','region'), everything()) %>% 
#   arrange(region)

# region_even_disbursement <- df %>% 
#   group_by(transactions_end_year, place_name) %>%
#   summarise(
#     mean = mean(even_split_disbursements),
#     sd = sd(even_split_disbursements),
#     n = n(), 
#     min = min(even_split_disbursements), 
#     max = max(even_split_disbursements)
#   ) %>% 
#   mutate(variable = 'even_split_disbursement') %>% 
#   rename(region = place_name, year = transactions_end_year) %>%
#   select(c('variable','region'), everything()) %>% 
#   arrange(region)

df_stats <- bind_rows(all_transact, all_disburse, all_even_disburse)

# df_stats <- bind_rows(all_transaction, all_total_disbursement, all_even_disbursement, region_transaction, region_total_disbursement, region_even_disbursement)

# ---- create csv ----

df <- df %>%
  mutate(
    log_total_commitments = log(if_else(total_commitments > 0, total_commitments, NA_real_)),
    log_transaction_value = log(if_else(transaction_value > 0, transaction_value, NA_real_)),
    log_total_disbursements = log(if_else(total_disbursements > 0, total_disbursements, NA_real_)),
    log_even_split_disbursements = log(if_else(even_split_disbursements > 0, even_split_disbursements, NA_real_))
  )

write_csv(df, here("data", "final", "aid.csv"))



# ---- create summary statistics ----
df_stats <- df_stats %>% 
  arrange(variable)

write_csv(df_stats, here("data", "final", "summary_stats_aid.csv"))

# df_sum_stat <- df_stats %>%
#   mutate(across(where(is.numeric), ~ format(.x, big.mark = ",", scientific = FALSE))) %>%
#   mutate(year = as.numeric(gsub(',', '', year))) %>%
#   filter(region == 'All') %>%
#   kbl() %>%                       # <— kableExtra’s wrapper; detects format
#   kable_styling(full_width = FALSE)

# save_kable(df_sum_stat, 'output/summary_stats_aid.html')
# webshot('output/summary_stats_aid.html', 'output/summary_stats_aid.png',
#         selector = "table",      # crops tightly to the table
#         zoom = 2,                # higher resolution
#         vwidth = 2000,           # width of the browser window
#         vheight = 3000           # height — increase if your table is tall
# )

# df_stat_mean <- df_stats %>% 
#   group_by(variable, region, year) %>%
#   mutate(across(where(is.numeric), ~ format(.x, big.mark = ",", scientific = FALSE))) %>%
#   mutate(year = as.numeric(gsub(',', '', year))) %>% 
#   select(c('variable', 'region', 'year', 'mean', 'sd')) %>%
#   filter(region == 'All') %>%
#   mutate(
#     # format each mean–sd pair as a string
#     mean_sd = paste0(
#       mean,
#       "<br>(",
#       sd,
#       ")"
#     )
#   ) %>%
#   select(variable, region, year, mean_sd) %>%
#   pivot_wider(
#     names_from  = year,
#     values_from = mean_sd
#   ) %>%
#   select(c('variable', 'region', '2013', '2014', '2015', '2016', '2017')) %>%
#   kbl(escape = FALSE, align = "lcccc", caption = "Mean (SD) by Year") %>%
#   kable_styling(full_width = FALSE) 

# save_kable(df_stat_mean, 'output/summary_mean_aid.html')
# webshot('output/summary_mean_aid.html', 'output/summary_mean_aid.png', 
#         selector = "table",      # crops tightly to the table
#         zoom = 2,                # higher resolution
#         vwidth = 2000,           # width of the browser window
#         vheight = 3000           # height — increase if your table is tall
# )

# df_stat_mean_region <- df_stats %>% 
#   group_by(region, year) %>%
#   mutate(across(where(is.numeric), ~ format(.x, big.mark = ",", scientific = FALSE))) %>%
#   mutate(year = as.numeric(gsub(',', '', year))) %>% 
#   filter(variable == 'even_split_disbursement') %>%
#   select(c('region', 'year', 'mean', 'sd')) %>%
#   mutate(
#     # format each mean–sd pair as a string
#     mean_sd = paste0(
#       mean,
#       "<br>(",
#       sd,
#       ")"
#     )
#   ) %>%
#   select(region, year, mean_sd) %>%
#   pivot_wider(
#     names_from  = year,
#     values_from = mean_sd
#   ) %>%
#   select(c('region', '2013', '2014', '2015', '2016', '2017')) %>%
#   kbl(escape = FALSE, align = "lcccc", caption = "Mean (SD) by Year and Region") %>%
#   kable_styling(full_width = FALSE) 

# save_kable(df_stat_mean_region, 'output/summary_mean_aid_region.html')
# webshot('output/summary_mean_aid_region.html', 'output/summary_mean_aid_region.png', 
#         selector = "table",      # crops tightly to the table
#         zoom = 2,                # higher resolution
#         vwidth = 2000,           # width of the browser window
#         vheight = 3000           # height — increase if your table is tall
# )

