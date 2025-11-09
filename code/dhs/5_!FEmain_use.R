# /*******************************************************************************************************************************
# Program: 			FEmain.R
# Purpose: 			Main file for the Fertility Chapter. 
#               The main file will call other R files that will produce the FE indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Mahmoud Elkasabi 
# Date last modified:	Sept 29, 2021 by Shireen Assaf
# *******************************************************************************************************************************/

rm(list = ls(all = TRUE))

library(haven)
library(data.table)
library(dplyr)
library(xlsx)
library(survey)
library(sjlabelled)
library(expss)
library(matrixStats)
library(DHS.rates)
library(here)       # to get R project path

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "/Users/charleneramos/Documents/Git/econ250project/data/raw/DHS/DHS-Indicators-R/Chap05_FE"

## select your survey ================= define datasets =============================

# IR Files
IRdatafile <- "data/raw/DHS/UGIR7BFL.DTA"

# PR Files
PRdatafile <- "data/raw/DHS/UGPR7BFL.dta"

# BR Files
BRdatafile <- "data/raw/DHS/UGBR7BFL.DTA"

# KR Files
KRdatafile <- "data/raw/DHS/UGKR7BFL.dta"

# open required datasets
IRdata <-  read_dta(here(IRdatafile))
PRdata <-  read_dta(here(PRdatafile))
BRdata <-  read_dta(here(BRdatafile))
KRdata <-  read_dta(here(KRdatafile))

################################################################################################

## do separate R scripts for each subtopic

# Fertility rates
source(here(paste0(chap,"/FE_TFR.R")))

# Current Fertility indicators
source(here("code/dhs/5_FE_FERT_use.R"))

# Crude Birth Rates
source(here(paste0(chap,"/FE_CBR.R")))

# Birth intervals indicators
source(here("code/dhs/5_FE_INT_use.R"))

# Postpartum amenorrhoea, abstinence, and insusceptibility
DHS_PHASE <- 7
source(here("code/dhs/5_FE_MEDIANS_use.R"))

# move tables to dest folder
file.rename("Tables_FE.xlsx", file.path(here::here("output", "dhs"), "Tables_FE.xlsx"))

