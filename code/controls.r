rm(list = ls())



# ---- load packages ----
library(here)
library(readxl)
library(tidyverse)




# ---- import data ----

aiddata_raw <- read_csv(here('data','raw','AidData','6913605476a3925d993413d2','6913605476a3925d993413d2_results.csv'))

spend14_raw <- read_excel(here('data','raw','Uganda Finance','13_14_ApprovedBudget_LG_GrantDetail.xlsx'))
spend15_raw <- read_excel(here('data','raw','Uganda Finance','14_15_ApprovedBudget_LG_GrantDetail.xlsx'))
spend16_raw <- read_excel(here('data','raw','Uganda Finance','15_16_ApprovedBudget_LG_GrantDetail.xlsx'))
spend17_raw <- read_excel(here('data','raw','Uganda Finance','16_17_ApprovedBudget_LG_GrantDetail.xlsx'))



# ---- clean and format data ----

# light density growth 
ntl <- aiddata_raw %>%
  select(shapeName, viirs_ntl_annual_v21_cf_cvg.2014.sum, viirs_ntl_annual_v21_cf_cvg.2014.count,
                    viirs_ntl_annual_v21_cf_cvg.2015.sum, viirs_ntl_annual_v21_cf_cvg.2015.count,
                    viirs_ntl_annual_v21_cf_cvg.2016.sum, viirs_ntl_annual_v21_cf_cvg.2016.count,
                    viirs_ntl_annual_v21_cf_cvg.2017.sum, viirs_ntl_annual_v21_cf_cvg.2017.count,
                    viirs_ntl_annual_v21_avg_masked.2014.mean,
                    viirs_ntl_annual_v21_avg_masked.2015.mean,
                    viirs_ntl_annual_v21_avg_masked.2016.mean,
                    viirs_ntl_annual_v21_avg_masked.2017.mean) %>%      
   rename('ntl_cf14_sum' = viirs_ntl_annual_v21_cf_cvg.2014.sum, 'ntl_cf14_count' = viirs_ntl_annual_v21_cf_cvg.2014.count,
          'ntl_cf15_sum' = viirs_ntl_annual_v21_cf_cvg.2015.sum, 'ntl_cf15_count' = viirs_ntl_annual_v21_cf_cvg.2015.count,
          'ntl_cf16_sum' = viirs_ntl_annual_v21_cf_cvg.2016.sum, 'ntl_cf16_count' = viirs_ntl_annual_v21_cf_cvg.2016.count,
          'ntl_cf17_sum' = viirs_ntl_annual_v21_cf_cvg.2017.sum, 'ntl_cf17_count' = viirs_ntl_annual_v21_cf_cvg.2017.count,
          'ntl14' = viirs_ntl_annual_v21_avg_masked.2014.mean,
          'ntl15' = viirs_ntl_annual_v21_avg_masked.2015.mean,
          'ntl16' = viirs_ntl_annual_v21_avg_masked.2016.mean,
          'ntl17' = viirs_ntl_annual_v21_avg_masked.2017.mean)

ntl14 <- ntl %>%
  select(shapeName, ntl_cf14_sum, ntl_cf14_count, ntl14) %>%
  mutate(ratio = ntl_cf14_sum/ntl_cf14_count) %>%
  mutate(cf14 = if_else(ratio >= 0.5, 1, 0)) %>%
  select(shapeName, cf14, ntl14)

ntl15 <- ntl %>%
  select(shapeName, ntl_cf15_sum, ntl_cf15_count, ntl15) %>%
  mutate(ratio = ntl_cf15_sum/ntl_cf15_count) %>%
  mutate(cf15 = if_else(ratio >= 0.5, 1, 0)) %>%
  select(shapeName, cf15, ntl15)

ntl16 <- ntl %>%
  select(shapeName, ntl_cf16_sum, ntl_cf16_count, ntl16) %>%
  mutate(ratio = ntl_cf16_sum/ntl_cf16_count) %>%
  mutate(cf16 = if_else(ratio >= 0.5, 1, 0)) %>%
  select(shapeName, cf16, ntl16)

ntl17 <- ntl %>%
  select(shapeName, ntl_cf17_sum, ntl_cf17_count, ntl17) %>%
  mutate(ratio = ntl_cf17_sum/ntl_cf17_count) %>%
  mutate(cf17 = if_else(ratio >= 0.5, 1, 0)) %>%
  select(shapeName, cf17, ntl17)

ntl <- reduce(list(ntl14, ntl15, ntl16, ntl17), left_join, by = 'shapeName')

ntl <- ntl %>%
  select(shapeName, ntl14, ntl15, ntl16, ntl17)

rm(ntl14, ntl15, ntl16, ntl17)



# population density 
pop14 <- aiddata_raw %>%
  select(shapeName, worldpop_pop_count_1km_mosaic.2014.sum) %>%
  rename('pop14' = worldpop_pop_count_1km_mosaic.2014.sum) %>%
  mutate(lpop14 = log(pop14))

pop15 <- aiddata_raw %>%
  select(shapeName, worldpop_pop_count_1km_mosaic.2015.sum) %>%
  rename('pop15' = worldpop_pop_count_1km_mosaic.2015.sum) %>%
  mutate(lpop15 = log(pop15))

pop16 <- aiddata_raw %>%
  select(shapeName, worldpop_pop_count_1km_mosaic.2016.sum) %>%
  rename('pop16' = worldpop_pop_count_1km_mosaic.2016.sum) %>%
  mutate(lpop16 = log(pop16))

pop17 <- aiddata_raw %>%
  select(shapeName, worldpop_pop_count_1km_mosaic.2017.sum) %>%
  rename('pop17' = worldpop_pop_count_1km_mosaic.2017.sum) %>%
  mutate(lpop17 = log(pop17))

pop <- reduce(list(pop14, pop15, pop16, pop17), left_join, by = 'shapeName')

pop <- pop %>%
  select(shapeName, lpop14, lpop15, lpop16, lpop17)

rm(pop14, pop15, pop16, pop17)



# rainfall 
rain14 <- aiddata_raw %>%
  select(shapeName, udel_precip_v501_sum.2014.sum) %>%
  rename('rain14' = udel_precip_v501_sum.2014.sum) %>%
  mutate(lrain14 = log(rain14))

rain15 <- aiddata_raw %>%
  select(shapeName, udel_precip_v501_sum.2015.sum) %>%
  rename('rain15' = udel_precip_v501_sum.2015.sum) %>%
  mutate(lrain15 = log(rain15))

rain16 <- aiddata_raw %>%
  select(shapeName, udel_precip_v501_sum.2016.sum) %>%
  rename('rain16' = udel_precip_v501_sum.2016.sum) %>%
  mutate(lrain16 = log(rain16))

rain17 <- aiddata_raw %>%
  select(shapeName, udel_precip_v501_sum.2017.sum) %>%
  rename('rain17' = udel_precip_v501_sum.2017.sum) %>%
  mutate(lrain17 = log(rain17))

rain <- reduce(list(rain14, rain15, rain16, rain17), left_join, by = 'shapeName')

rain <- rain %>%
  select(shapeName, lrain14, lrain15, lrain16, lrain17)

rm(rain14, rain15, rain16, rain17)



# public expenditure, education
spend14 <- spend14_raw %>%
    filter(Program %in% c('SCHOOL CONSTRUCTION PROGRAMME','Education','SECONDARY SCHOOL CONSTRUCTION')) %>%
    group_by(Vote) %>%
    summarise(value = sum(`Approved Budget` , na.rm = TRUE)) %>%
    mutate(year = 2014,
           log_value = log(value), 
           variable = 'Public expenditure'
    ) %>% 
    # mutate(value = format(value, big.mark = ",", trim = TRUE))%>% 
    select(variable, year, Vote, value, log_value) %>% 
    rename('district' = Vote)

spend15 <- spend15_raw %>%
    filter(Program %in% c('SCHOOL CONSTRUCTION PROGRAMME','Education','SECONDARY SCHOOL CONSTRUCTION')) %>%
    group_by(Vote) %>%
    summarise(value = sum(`Approved Budget` , na.rm = TRUE)) %>%
    mutate(year = 2015,
           log_value = log(value), 
           variable = 'Public expenditure'
    ) %>% 
    select(variable, year, Vote, value, log_value) %>% 
    rename('district' = Vote)

spend16 <- spend16_raw %>%
    filter(Program %in% c('Education Development','Education')) %>%
    group_by(Vote) %>%
    summarise(value = sum(`Approved Budget` , na.rm = TRUE)) %>%
    mutate(year = 2016,
           log_value = log(value), 
           variable = 'Public expenditure'
    ) %>% 
    select(variable, year, Vote, value, log_value) %>% 
    rename('district' = Vote)

spend17 <- spend17_raw %>%
    filter(Program %in% c('Education','Education Development')) %>%
    group_by(Vote) %>%
    summarise(value = sum(`Amount` , na.rm = TRUE)) %>%
    mutate(year = 2017,
           log_value = log(value), 
           variable = 'Public expenditure'
    ) %>% 
    select(variable, year, Vote, value, log_value) %>% 
    rename('district' = Vote)

df <- rbind(spend14, spend15, spend16, spend17)

df <- df %>%
    mutate(district = tolower(district)) %>%
    mutate(district = gsub("district", "", district, ignore.case = TRUE)) %>%
    filter(!grepl('council', district, ignore.case = TRUE)) %>%
    mutate(district = paste0(toupper(substr(district, 1, 1)), 
                             substr(district, 2, nchar(district))))

write.csv(df, here('data', 'final', 'public_spend_edu.csv'), row.names = FALSE)
