# /*******************************************************************************************************************************
# Program: 			!RCmain.R
# Purpose: 			Main file for the Respondents' Characteristics Chapter. 
#               The main file will call other R files that will produce the RC indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Mahmoud Elkasabi 
# Date last modified:	Sept 29, 2021 by Shireen Assaf
# *******************************************************************************************************************************/
rm(list = ls(all = TRUE))

# install the following library if needed.
library(haven)
library(naniar) #to replace values with NA
library(dplyr)
library(sjlabelled)
library(matrixStats) # for weightedMedian function
library(expss)
library(xlsx)
library(here)       # to get R project path

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "/Users/charleneramos/Documents/Git/econ250project/data/raw/DHS/DHS-Indicators-R/Chap03_RC"

#####################################################################################
## Define datasets

# IR Files
IRdatafile <- "data/raw/DHS/UGIR7BFL.DTA"

# MR Files
MRdatafile <- "data/raw/DHS/UGMR7BFL.dta"

#####################################################################################
## Do separate R scripts for each subtopic

# Respondent characteristic indicators for women
IRdata <-  read_dta(here(IRdatafile))

# create analytic variables
source(here(paste0(chap,"/RC_CHAR_WM.R")))
# run the chapter tables
source(here(paste0(chap,"/RC_tables_WM.R")))
# move table to dest folder 
file.rename("Tables_RC_WM.xlsx", file.path(here::here("output", "dhs"), "Tables_RC_WM.xlsx"))



#####################################################################################

# Respondent characteristic indicators for men
MRdata <-  read_dta(here(MRdatafile))

# create analytic variables
source(here(paste0(chap,"/RC_CHAR_MN.R")))
# run the chapter tables
source(here(paste0(chap,"/RC_tables_MN.R")))
# move table to dest folder 
file.rename("Tables_RC_MN.xlsx", file.path(here::here("output", "dhs"), "Tables_RC_MN.xlsx"))


#####################################################################################
