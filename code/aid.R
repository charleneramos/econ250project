rm(list = ls())



# ---- load packages ----
library(here)
library(tidyverse)



# ---- import data ----
level_1a <- read_csv(here('data','raw','AidData','UgandaAIMS_GeocodedResearchRelease_Level1_v1.4.1','data','level_1a.csv'))
transactions <- read_csv(here('data','raw','AidData','UgandaAIMS_GeocodedResearchRelease_Level1_v1.4.1','data','transactions.csv'))



# ---- clean up data ----
df <- transactions %>%
  filter(transaction_value_code == 'D') 

df <- merge(df, level_1a, by = 'project_id')

df <- df %>%
  filter(transactions_end_year > 2013 & transactions_end_year <= 2017) %>% # filter for years 2013-2017
  filter(location_type_code == 'ADM2') %>% # filter for ADM2 locations
  distinct() 

df <- df %>%
  mutate(
    log_total_commitments = log(if_else(total_commitments > 0, total_commitments, NA_real_)),
    log_transaction_value = log(if_else(transaction_value > 0, transaction_value, NA_real_)),
    log_total_disbursements = log(if_else(total_disbursements > 0, total_disbursements, NA_real_)),
    log_even_split_disbursements = log(if_else(even_split_disbursements > 0, even_split_disbursements, NA_real_))
  )

write_csv(df, here("data", "final", "aid.csv"))



# ---- panel format ----
aid <- df %>%
  select(place_name, transactions_end_year, total_commitments, transaction_value, total_disbursements, even_split_disbursements) %>%
  group_by(place_name, transactions_end_year) %>%
  summarise(
    total_commitments = sum(total_commitments),
    transaction_value = sum(transaction_value),
    total_disbursements = sum(total_disbursements),
    even_split_disbursements = sum(even_split_disbursements),
  ) %>%
  rename('district' = place_name,
         'year' = transactions_end_year)

aid <- aid %>%
    mutate(district = tolower(district)) %>%
    mutate(district = gsub("district", "", district, ignore.case = TRUE)) %>%
    filter(!grepl('council', district, ignore.case = TRUE)) %>%
    mutate(district = paste0(toupper(substr(district, 1, 1)), 
                             substr(district, 2, nchar(district))))

aid_frame <- aid %>%
    select(district) %>%
    unique()

aid14 <- aid %>%
    arrange(district) %>%
    filter(year == 2014)



# ---- create summary statistics ----
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

df_stats <- bind_rows(all_transact, all_disburse, all_even_disburse)

df_stats <- df_stats %>% 
  arrange(variable)

write_csv(df_stats, here("data", "final", "summary_stats_aid.csv"))



# ---- create summary statistics, top donors ----
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

total_row <- top_donors %>%
  summarise(
    donors = "Total",
    transact_sum = sum(transact_sum, na.rm = TRUE),
    transact_n = sum(transact_n, na.rm = TRUE),
    percent_transact_sum = sum(percent_transact_sum)
  )

top_donors <- bind_rows(top_donors, total_row)

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
