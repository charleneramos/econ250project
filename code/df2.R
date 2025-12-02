rm(list = ls())



# ---- load packages ----
library(dplyr)
library(here)
library(OECD)
library(readxl)
library(stringr)
library(tidyverse)
library(WDI)



# ---- import data ----
level_1a <- read_csv(here('data','raw','AidData','UgandaAIMS_GeocodedResearchRelease_Level1_v1.4.1','data','level_1a.csv'))
transactions <- read_csv(here('data','raw','AidData','UgandaAIMS_GeocodedResearchRelease_Level1_v1.4.1','data','transactions.csv'))
aiddata_raw <- read_csv(here('data','raw','AidData','6913605476a3925d993413d2','6913605476a3925d993413d2_results.csv'))



# ---- merge relevant datasets (projects and transactions) ----
df_raw <- transactions %>%
  filter(transaction_value_code == 'D') 

df_raw <- merge(df_raw, level_1a, by = 'project_id')

rm(level_1a, transactions)

write_csv(df_raw, here("data", "final", "aid_merged.csv"))



# ---- format district dataset ----
df_filter <- df_raw %>%
  filter(location_type_code == 'ADM2') # filter for ADM2 locations

clean_district <- function(x) {
  x %>%
    tolower() %>%
    trimws() %>%
    gsub("district", "", ., ignore.case = TRUE) %>%   # remove word
    gsub("council", "", ., ignore.case = TRUE) %>%    # remove council if needed
    gsub("\\s+", " ", .) %>%                          # collapse multiple spaces
    trimws() %>%
    tools::toTitleCase()                              # Kampala, Abim, etc.
}

df_dist <- df_filter %>%
  select(place_name, transactions_end_year, donors, total_disbursements, even_split_disbursements) %>%
  rename('district' = place_name,
         'year' = transactions_end_year,
         'donors' = donors,
         'disburse' = total_disbursements,
         'esplit' = even_split_disbursements) %>%
  mutate(district = clean_district(district))

df_distpre <- df_dist %>%
  filter(year < 2014) %>%
  arrange(district)

df_dist14 <- df_dist %>%
  filter(year == 2014) %>%
  arrange(district)

df_dist15 <- df_dist %>%
  filter(year == 2015) %>%
  arrange(district)

df_dist16 <- df_dist %>%
  filter(year == 2016) %>%
  arrange(district)

df_dist17 <- df_dist %>%
  filter(year == 2017) %>%
  arrange(district)


# ---- prepare controls ----

# population density 
pop14 <- aiddata_raw %>%
  select(shapeName, worldpop_pop_count_1km_mosaic.2014.sum) %>%
  rename('pop' = worldpop_pop_count_1km_mosaic.2014.sum, 'district' = shapeName)

pop15 <- aiddata_raw %>%
  select(shapeName, worldpop_pop_count_1km_mosaic.2015.sum) %>%
  rename('pop' = worldpop_pop_count_1km_mosaic.2015.sum, 'district' = shapeName)

pop16 <- aiddata_raw %>%
  select(shapeName, worldpop_pop_count_1km_mosaic.2016.sum) %>%
  rename('pop' = worldpop_pop_count_1km_mosaic.2016.sum, 'district' = shapeName)

pop17 <- aiddata_raw %>%
  select(shapeName, worldpop_pop_count_1km_mosaic.2017.sum) %>%
  rename('pop' = worldpop_pop_count_1km_mosaic.2017.sum, 'district' = shapeName)

df_dist14 <- left_join(df_dist14, pop14, by = "district")
df_dist15 <- left_join(df_dist15, pop15, by = "district")
df_dist16 <- left_join(df_dist16, pop16, by = "district")
df_dist17 <- left_join(df_dist17, pop17, by = "district")

rm(pop14, pop15, pop16, pop17)



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
  select(shapeName, ntl_cf14_sum) %>%  
  rename('ntl' = ntl_cf14_sum,'district' = shapeName)

ntl15 <- ntl %>%
  select(shapeName, ntl_cf15_sum, ntl_cf15_count, ntl15) %>%
  select(shapeName, ntl_cf15_sum) %>%  
  rename('ntl' = ntl_cf15_sum,'district' = shapeName)

ntl16 <- ntl %>%
  select(shapeName, ntl_cf16_sum, ntl_cf16_count, ntl16) %>%
  select(shapeName, ntl_cf16_sum) %>%  
  rename('ntl' = ntl_cf16_sum,'district' = shapeName)

ntl17 <- ntl %>%
  select(shapeName, ntl_cf17_sum, ntl_cf17_count, ntl17) %>%
  select(shapeName, ntl_cf17_sum) %>%  
  rename('ntl' = ntl_cf17_sum,'district' = shapeName)

df_dist14 <- left_join(df_dist14, ntl14, by = "district")
df_dist15 <- left_join(df_dist15, ntl15, by = "district")
df_dist16 <- left_join(df_dist16, ntl16, by = "district")
df_dist17 <- left_join(df_dist17, ntl17, by = "district")

rm(ntl, ntl14, ntl15, ntl16, ntl17)



# rainfall 
rain14 <- aiddata_raw %>%
  select(shapeName, udel_precip_v501_sum.2014.sum) %>%
  rename('rain' = udel_precip_v501_sum.2014.sum, ,'district' = shapeName)

rain15 <- aiddata_raw %>%
  select(shapeName, udel_precip_v501_sum.2015.sum) %>%
  rename('rain' = udel_precip_v501_sum.2015.sum, ,'district' = shapeName) 

rain16 <- aiddata_raw %>%
  select(shapeName, udel_precip_v501_sum.2016.sum) %>%
  rename('rain' = udel_precip_v501_sum.2016.sum, ,'district' = shapeName)

rain17 <- aiddata_raw %>%
  select(shapeName, udel_precip_v501_sum.2017.sum) %>%
  rename('rain' = udel_precip_v501_sum.2017.sum, ,'district' = shapeName) 

df_dist14 <- left_join(df_dist14, rain14, by = "district")
df_dist15 <- left_join(df_dist15, rain15, by = "district")
df_dist16 <- left_join(df_dist16, rain16, by = "district")
df_dist17 <- left_join(df_dist17, rain17, by = "district")

rm(rain14, rain15, rain16, rain17)



# ---- calculate pr(aid) ----
df_frame <- df_filter %>%
  select(place_name) %>%
  unique() %>%
  arrange(place_name) %>%
  rename('district' = place_name)  

df_frame <- df_frame %>%
    mutate(district = tolower(district)) %>%
    mutate(district = gsub("district", "", district, ignore.case = TRUE)) %>%
    filter(!grepl('council', district, ignore.case = TRUE)) %>%
    mutate(district = paste0(toupper(substr(district, 1, 1)), 
                             substr(district, 2, nchar(district)))) 

df_frame$district <- df_frame$district %>% trimws() %>% gsub(" ", "", .)

prob <- df_filter %>%
  select(place_name, transactions_end_year, transaction_value) %>%
  group_by(place_name, transactions_end_year) %>%
  rename('district' = place_name,
         'year' = transactions_end_year,
         'value' = transaction_value) %>%
  ungroup()

prob <- prob %>%
    mutate(district = tolower(district)) %>%
    mutate(district = gsub("district", "", district, ignore.case = TRUE)) %>%
    filter(!grepl('council', district, ignore.case = TRUE)) %>%
    mutate(district = paste0(toupper(substr(district, 1, 1)), 
                             substr(district, 2, nchar(district)))) 

prob$district <- prob$district %>% trimws() %>% gsub(" ", "", .)

prob <- prob %>% 
  group_by(district, year) %>%
  summarise(value = sum(value))  %>%
  ungroup()

prob02 <- prob %>%
  filter(year == 2002) %>%
  rename('transact02' = value) %>%
  select(district, transact02)

prob03 <- prob %>%
  filter(year == 2003) %>%
  rename('transact03' = value) %>%
  select(district, transact03)

prob04 <- prob %>%
  filter(year == 2004) %>%
  rename('transact04' = value) %>%
  select(district, transact04)

prob05 <- prob %>%
  filter(year == 2005) %>%
  rename('transact05' = value) %>%
  select(district, transact05)  

prob06 <- prob %>%
  filter(year == 2006) %>%
  rename('transact06' = value) %>%
  select(district, transact06)

prob07 <- prob %>%
  filter(year == 2007) %>%
  rename('transact07' = value) %>%
  select(district, transact07)  

prob08 <- prob %>%
  filter(year == 2008) %>%
  rename('transact08' = value) %>%
  select(district, transact08)

prob09 <- prob %>%
  filter(year == 2009) %>%
  rename('transact09' = value) %>%
  select(district, transact09)

prob10 <- prob %>%
  filter(year == 2010) %>%
  rename('transact10' = value) %>%
  select(district, transact10)

prob11 <- prob %>%
  filter(year == 2011) %>%
  rename('transact11' = value) %>%
  select(district, transact11)  

prob12 <- prob %>%  
  filter(year == 2012) %>%
  rename('transact12' = value) %>%
  select(district, transact12) 

prob13 <- prob %>%
  filter(year == 2013) %>%
  rename('transact13' = value) %>%
  select(district, transact13)

prob_aid <- reduce(list(df_frame, 
                        prob02, prob03, prob04, 
                        prob05, prob06, prob07, prob08, prob09, 
                        prob10, prob11, prob12, prob13), 
                   left_join, by = 'district')

prob_aid <- prob_aid %>%
  mutate(count = rowSums(!is.na(select(., -district)))) %>%
  mutate(prob = count / 12) %>%
  select(district, prob)

rm(prob, prob02, prob03, prob04, 
   prob05, prob06, prob07, prob08, prob09, 
   prob10, prob11, prob12, prob13)

df <- prob_aid



# ---- calculate pr(aid_donor) ----
df_frame_donor <- df_filter %>%
  select(donors) %>%
  unique() %>%
  arrange(donors)

df_frame_donor$donors <- df_frame_donor$donors %>% trimws()

df_frame_all <- crossing(df_frame, df_frame_donor)

prob_donor <- df_filter %>%
  select(place_name, transactions_end_year, donors, transaction_value) %>%
  group_by(place_name, transactions_end_year, donors) %>%
  rename('district' = place_name,
         'year' = transactions_end_year,
         'value' = transaction_value) %>%
  ungroup()

prob_donor <- prob_donor %>%
    mutate(district = tolower(district)) %>%
    mutate(district = gsub("district", "", district, ignore.case = TRUE)) %>%
    filter(!grepl('council', district, ignore.case = TRUE)) %>%
    mutate(district = paste0(toupper(substr(district, 1, 1)), 
                             substr(district, 2, nchar(district)))) 

prob_donor$district <- prob_donor$district %>% trimws() %>% gsub(" ", "", .)

prob_donor <- prob_donor %>% 
  group_by(district, year, donors) %>%
  summarise(value = sum(value))  %>%
  ungroup()

prob02_donor <- prob_donor %>%
  filter(year == 2002) %>%
  rename('transact02' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact02)

prob03_donor <- prob_donor %>%
  filter(year == 2003) %>%
  rename('transact03' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact03)

prob04_donor <- prob_donor %>%
  filter(year == 2004) %>%
  rename('transact04' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact04)

prob05_donor <- prob_donor %>%
  filter(year == 2005) %>%
  rename('transact05' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact05) 

prob06_donor <- prob_donor %>%
  filter(year == 2006) %>%
  rename('transact06' = value) %>%
  arrange(donors,year,district) %>% 
  select(district, donors, transact06)

prob07_donor <- prob_donor %>%
  filter(year == 2007) %>%
  rename('transact07' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact07)

prob08_donor <- prob_donor %>%
  filter(year == 2008) %>%
  rename('transact08' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact08)

prob09_donor <- prob_donor %>%
  filter(year == 2009) %>%
  rename('transact09' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact09)

prob10_donor <- prob_donor %>%
  filter(year == 2010) %>%
  rename('transact10' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact10)

prob11_donor <- prob_donor %>%
  filter(year == 2011) %>%
  rename('transact11' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact11)

prob12_donor <- prob_donor %>%  
  filter(year == 2012) %>%
  rename('transact12' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact12)

prob13_donor <- prob_donor %>%
  filter(year == 2013) %>%
  rename('transact13' = value) %>%
  arrange(donors,year,district) %>%
  select(district, donors, transact13)

prob_aid_donor <- reduce(list(df_frame_all, 
                        prob02_donor, prob03_donor, prob04_donor, 
                        prob05_donor, prob06_donor, prob07_donor, prob08_donor, prob09_donor, 
                        prob10_donor, prob11_donor, prob12_donor, prob13_donor), 
                   left_join, by = c('district','donors'))

prob_aid_donor <- prob_aid_donor %>%
  mutate(count = rowSums(!is.na(select(., -district)))) %>%
  mutate(prob = count / 12) %>%
  select(district, donors, prob) %>%
  arrange(donors, district)

rm(prob02_donor, prob03_donor, prob04_donor, 
   prob05_donor, prob06_donor, prob07_donor, prob08_donor, prob09_donor, 
   prob10_donor, prob11_donor, prob12_donor, prob13_donor)



# ---- create district dataset ----
df_distpre$pop <- NA
df_distpre$ntl <- NA
df_distpre$rain <- NA

df_dist$percap <- df_dist$disburse/df_dist$pop


df_distpre <- left_join(df_distpre, prob_aid_donor, by = c("district", "donors"))
df_dist14 <- left_join(df_dist14, prob_aid_donor, by = c("district", "donors"))
df_dist15 <- left_join(df_dist15, prob_aid_donor, by = c("district", "donors")) 
df_dist16 <- left_join(df_dist16, prob_aid_donor, by = c("district", "donors"))
df_dist17 <- left_join(df_dist17, prob_aid_donor, by = c("district", "donors"))

df_dist_all <- bind_rows(df_distpre, df_dist14, df_dist15, df_dist16, df_dist17)

df_dist_all <- df_dist_all %>%
  mutate(percap = as.numeric(disburse)/as.numeric(pop)) %>%
  select(district, year, donors, disburse, esplit, percap, pop, ntl, rain, prob) %>%
  arrange()


rm(df_dist, df_distpre, df_dist14, df_dist15, df_dist16, df_dist17)

write_csv(df_dist_all, here("data", "final", "df_dist_all.csv"))



# ---- create donor dataset ----
df_donor <- df_filter %>%
  select(donors) %>%
  unique() %>%
  arrange(donors) %>%
  filter(!donors %in% c('African Development Fund','China|Sweden|African Development Fund','Denmark/DANIDA',
                        'International Development Association','International Development Association|African Development Fund',
                        'International Development Association|China','International Development Association|Norway',
                        'Islamic Development Bank','Japan|Ireland','United Nations Development Programme'))

year <- data.frame(year = c(2014,2015,2016,2017))

df_donor <- crossing(df_donor, year) %>%
  mutate(donors = recode(donors,
                        'United States of America' = 'United States'))

# donor gdp and total aid
df_donor <- left_join(df_donor, donor_spend_gdp, by = c('donors' = 'country', 'year' = 'year')) %>%
  select(donors, year, NE.CON.GOVT.ZS) %>%
  rename('spend_gdp' = NE.CON.GOVT.ZS)

donors_dac <- c(
  BE = "BEL",  # Belgium
  CN = "CHN",  # China
  EU = "4EU001",   # European Union institutions
  IE = "IRL",  # Ireland
  JP = "JPN",  # Japan
  NO = "NOR",  # Norway
  SE = "SWE",  # Sweden
  GB = "GBR",  # United Kingdom
  US = "USA"   # United States
)

base_url <- "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC1@DF_DAC1,1.5"

donor_part <- paste(donors_dac, collapse = "+")
# DONOR.MEASURE.RECIPIENT.FLOW.CURRENCY.FREQ.UNIT
key <- paste0(donor_part, ".1010..1140.USD.V._Z")

full_url <- paste0(
  base_url, "/",
  key,
  "?dimensionAtObservation=AllDimensions",
  "&format=csvfilewithlabels",
  "&startPeriod=2014&endPeriod=2017"
)

full_url

dac1_raw <- read.csv(full_url, check.names = FALSE)

df_donor <- df_donor %>%
  left_join(
    dac1_raw %>% select(Donor, TIME_PERIOD, OBS_VALUE),
    by = c("donors" = "Donor", "year" = "TIME_PERIOD")
  ) %>%
  mutate(total_aid = OBS_VALUE * 1e6) %>% # Convert from millions to units
    select(-OBS_VALUE)

# filter unique transactions
df_transact <- df_filter %>%
  filter(transactions_end_year > 2013 & transactions_end_year < 2018) %>% # filter for years 2013-2017
  arrange(transactions_end_year) %>%
  distinct(transaction_id, .keep_all = TRUE)


df_transact_donor <- df_transact %>%
  group_by(donors, transactions_end_year) %>%
  mutate(uganda_aid = sum(even_split_disbursements)) %>%
  select(donors, transactions_end_year, uganda_aid) %>%
  distinct() %>%
  arrange(donors, transactions_end_year)

df_transact_donor <- df_transact_donor %>%
  group_by(donors,transactions_end_year) %>%
  select(donors, transactions_end_year, uganda_aid) %>%
  unique() %>%
  arrange(donors) %>%
  mutate(donors = clean_district(donors)) %>%
  filter(!donors %in% c('African Development Fund','China|Sweden|African Development Fund','Denmark/DANIDA',
                        'International Development Association','International Development Association|African Development Fund',
                        'International Development Association|China','International Development Association|Norway',
                        'Islamic Development Bank','Japan|Ireland','United Nations Development Programme', 'International Development Association|china')) %>%
mutate(donors = recode(donors,
                        'United States of America' = 'United States'))
                        

df_donor <- df_donor %>%
  left_join(
    df_transact_donor,
    by = c("donors" = "donors", "year" = "transactions_end_year")
  ) %>%
  arrange(donors, year)


df_donor <- df_donor %>%
  filter(!donors %in% c('Belgium','Sweden','Ireland'))

write_csv(df_donor, here("data", "final", "df_donors.csv"))

# identify top donors
top_donors <- df_transact %>%
  group_by(donors) %>%
   summarise(
    transact_sum = sum(even_split_disbursements, na.rm = TRUE),
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


rm(total_row)


