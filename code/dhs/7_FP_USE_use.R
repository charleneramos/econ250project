# ******************************************************************************
# Program: 			  FP_USE.do
# Purpose: 		    Code contraceptive use indicators (ever and current use). Also source of method, brands, and information given. 
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen
# Date last modified: March 29  2021 by Courtney Allen
# ******************************************************************************

# NOTE: this script is created to run from the FPmain.R file where the following libraries are loaded
# -----------------------------------------------------------------------------#
# Variables created in this file:
# 
# fp_evuse_any		"Ever used any contraceptive method"
# fp_evuse_mod		"Ever used any modern method"
# fp_evuse_fster	"Ever used female sterilization"
# fp_evuse_mster	"Ever used male sterilization"
# fp_evuse_pill		"Ever used pill"
# fp_evuse_iud		"Ever used IUD"
# fp_evuse_inj		"Ever used injectables"
# fp_evuse_imp		"Ever used implants"
# fp_evuse_mcond	"Ever used male condoms"
# fp_evuse_fcond	"Ever used female condom"
# fp_evuse_diaph	"Ever used diaphragm"
# fp_evuse_lam		"Ever used LAM"
# fp_evuse_ec			"Ever used emergency contraception"
# fp_evuse_omod		"Ever used other modern method"
# fp_evuse_trad		"Ever used any traditional method"
# fp_evuse_rhy		"Ever used rhythm"
# fp_evuse_wthd		"Ever used withdrawal"
# fp_evuse_other	"Ever used other"
# 
# fp_cruse_any		"Currently use any contraceptive method"
# fp_cruse_mod		"Currently use any modern method
# fp_cruse_fster	"Currently use female sterilization"
# fp_cruse_mster	"Currently use male sterilization"
# fp_cruse_pill		"Currently use pill"
# fp_cruse_iud		"Currently use IUD"
# fp_cruse_inj		"Currently use injectables"
# fp_cruse_imp		"Currently use implants"
# fp_cruse_mcond	"Currently use male condoms"
# fp_cruse_fcond	"Currently use female condom"
# fp_cruse_diaph	"Currently use diaphragm"
# fp_cruse_lam		"Currently use LAM"
# fp_cruse_ec			"Currently use emergency contraception"
# fp_cruse_omod		"Currently use other modern method"
# fp_cruse_trad		"Currently use any traditional method"
# fp_cruse_rhy		"Currently use rhythm"
# fp_cruse_wthd		"Currently use withdrawal"
# fp_cruse_other	"Currently use other"
# 
# fp_ster_age			"Age at time of sterilization for women"
# fp_ster_median	"Median age at time of sterilization for women"
# 
# fp_source_tot		"Source of contraception - total"
# fp_source_fster	"Source for female sterilization"
# fp_source_pill	"Source for pill"
# fp_source_iud		"Source for IUD"
# fp_source_inj		"Source for injectables"
# fp_source_imp		"Source for implants"
# fp_source_mcond	"Source for male condom"
# 
# fp_brand_pill		"Pill users using a social marketing brand"
# fp_brand_cond		"Male condom users using a social marketing brand"
# 
# fp_info_sideff		  "Informed about side effects or problems among female sterilization, pill, IUD, injectables, and implant users"
# fp_info_what_to_do	"Informed of what to do if experienced side effects among female sterilization, pill, IUD, injectables, and implant users"
# fp_info_other_meth	"Informed of other methods by health or FP worker among female sterilization, pill, IUD, injectables, and implant users"
# fp_info_all 		    "Informed of all three (method information index) among female sterilization, pill, IUD, injectables, and implant users"

#------------------------------------------------------------------------------


## Family planning messages


# Family planning messages by radio 
# mapping new variable -> source v305_XX (string names)
vars_map <- c(
  fp_evuse_pill  = "v305_01",
  fp_evuse_iud   = "v305_02",
  fp_evuse_inj   = "v305_03",
  fp_evuse_diaph = "v305_04",
  fp_evuse_mcond = "v305_05",
  fp_evuse_fster = "v305_06",
  fp_evuse_mster = "v305_07",
  fp_evuse_rhy   = "v305_08",
  fp_evuse_wthd  = "v305_09",
  fp_evuse_other = "v305_10",
  fp_evuse_imp   = "v305_11",
  fp_evuse_lam   = "v305_13",
  fp_evuse_fcond = "v305_14",
  fp_evuse_ec    = "v305_16",
  fp_evuse_omod  = "v305_17",
  fp_evuse_sdm   = "v305_18"
)

# create each indicator safely
for (newname in names(vars_map)) {
  src <- vars_map[[newname]]
  # build 1 if source in 1..7, 0 if source is non-missing and not in 1..7, NA if source is NA
  IRdata[[newname]] <- with(IRdata, ifelse(is.na(get(src)), NA_real_,
                                           ifelse(get(src) > 0 & get(src) < 8, 1, 0)))
  # ensure it's a plain numeric vector (strip labelled/haven attributes)
  IRdata[[newname]] <- as.numeric(unclass(IRdata[[newname]]))

  # attach yes/no labels and a variable label
  val_labels(IRdata[[newname]]) <- c(yes = 1, no = 0)
  var_label(IRdata[[newname]])  <- gsub("_", " ", paste0("Ever used ", gsub("^fp_evuse_", "", newname)))
}

# Create "ever used any contraceptive" from v302 (same logic as you used)
IRdata$fp_evuse_any <- with(IRdata,
  ifelse(is.na(v302), NA_real_, ifelse(v302 > 0 & v302 < 8, 1, 0))
)
IRdata$fp_evuse_any <- as.numeric(unclass(IRdata$fp_evuse_any))
val_labels(IRdata$fp_evuse_any) <- c(yes = 1, no = 0)
var_label(IRdata$fp_evuse_any)  <- "Ever used any contraceptive method"

# Ever used any modern method (derived)
# define modern methods subset (as created above)
modern_vars <- c("fp_evuse_pill","fp_evuse_iud","fp_evuse_inj","fp_evuse_imp",
                 "fp_evuse_mcond","fp_evuse_fcond","fp_evuse_fster","fp_evuse_mster",
                 "fp_evuse_omod","fp_evuse_ec","fp_evuse_lam","fp_evuse_sdm")
IRdata$fp_evuse_mod <- as.numeric(rowSums(do.call(cbind, IRdata[modern_vars]) == 1, na.rm = TRUE) > 0)
val_labels(IRdata$fp_evuse_mod) <- c(yes = 1, no = 0)
var_label(IRdata$fp_evuse_mod)  <- "Ever used any modern method"

# Ever used any traditional method
IRdata$fp_evuse_trad <- as.numeric(
  (IRdata$fp_evuse_rhy == 1) | (IRdata$fp_evuse_wthd == 1) | (IRdata$fp_evuse_other == 1)
)
val_labels(IRdata$fp_evuse_trad) <- c(yes = 1, no = 0)
var_label(IRdata$fp_evuse_trad)  <- "Ever used any traditional method"

# Quick diagnostics / checks
summary_counts <- sapply(c(names(vars_map), "fp_evuse_any", "fp_evuse_mod", "fp_evuse_trad"),
                         function(nm) table(IRdata[[nm]], useNA = "ifany"))
print(summary_counts)


### Current use of contraceptive methods
 
 
# mapping: new_name -> v312 code(s) that indicate that method being currently used
# if a method has multiple numeric v312 codes (e.g. "other" might be 10 or 35), list them
cruse_map <- list(
  fp_cruse_pill  = 1,
  fp_cruse_iud   = 2,
  fp_cruse_inj   = 3,
  fp_cruse_diaph = 4,
  fp_cruse_mcond = 5,
  fp_cruse_fster = 6,
  fp_cruse_mster = 7,
  fp_cruse_rhy   = 8,
  fp_cruse_wthd  = 9,
  fp_cruse_other = c(10,35),
  fp_cruse_imp   = 11,
  fp_cruse_lam   = 13,
  fp_cruse_fcond = 14,
  fp_cruse_ec    = 16,
  fp_cruse_omod  = 17,
  fp_cruse_sdm   = 18
)

# create variables programmatically (safe handling of labelled inputs)
for (newname in names(cruse_map)) {
  codes <- cruse_map[[newname]]
  # create indicator: 1 if v312 in codes, 0 if v312 not in codes and not missing, NA if v312 is NA
  IRdata[[newname]] <- with(IRdata,
    ifelse(is.na(v312), NA_real_,
      ifelse(v312 %in% codes, 1, 0)
    )
  )
  # ensure plain numeric (strip labelled/haven attributes)
  IRdata[[newname]] <- as.numeric(unclass(IRdata[[newname]]))

  # attach value labels and variable label
  val_labels(IRdata[[newname]]) <- c(yes = 1, no = 0)
  var_label(IRdata[[newname]])  <- gsub("_", " ",
    paste0("Currently used ", gsub("^fp_cruse_", "", newname))
  )
}

# Create "currently use any contraceptive" from v313 the same way you did (but make sure NA handling)
# Here: v313 > 0 & v313 < 8 indicates current use in many DHS; keep NA if v313 is NA
IRdata$fp_cruse_any <- with(IRdata, ifelse(is.na(v313), NA_real_,
                                           ifelse(v313 > 0 & v313 < 8, 1, 0)))
IRdata$fp_cruse_any <- as.numeric(unclass(IRdata$fp_cruse_any))
val_labels(IRdata$fp_cruse_any) <- c(yes = 1, no = 0)
var_label(IRdata$fp_cruse_any)  <- "Currently used any contraceptive method"

# Build "currently use any modern" from the indicator variables (robust)
modern_current_vars <- c("fp_cruse_pill","fp_cruse_iud","fp_cruse_inj","fp_cruse_imp",
                         "fp_cruse_mcond","fp_cruse_fcond","fp_cruse_fster","fp_cruse_mster",
                         "fp_cruse_omod","fp_cruse_ec","fp_cruse_lam","fp_cruse_sdm")

# ensure all modern_current_vars exist (safety)
modern_current_vars <- modern_current_vars[modern_current_vars %in% names(IRdata)]

IRdata$fp_cruse_mod <- as.numeric(rowSums(do.call(cbind, IRdata[modern_current_vars]) == 1, na.rm = TRUE) > 0)
val_labels(IRdata$fp_cruse_mod) <- c(yes = 1, no = 0)
var_label(IRdata$fp_cruse_mod)  <- "Currently used any modern method"

# Build "currently use any traditional" from the traditional indicators
trad_current_vars <- c("fp_cruse_rhy","fp_cruse_wthd","fp_cruse_other")
trad_current_vars <- trad_current_vars[trad_current_vars %in% names(IRdata)]

IRdata$fp_cruse_trad <- as.numeric(rowSums(do.call(cbind, IRdata[trad_current_vars]) == 1, na.rm = TRUE) > 0)
val_labels(IRdata$fp_cruse_trad) <- c(yes = 1, no = 0)
var_label(IRdata$fp_cruse_trad)  <- "Currently used any traditional method"

# Quick diagnostics
diagnostics <- sapply(c(names(cruse_map), "fp_cruse_any", "fp_cruse_mod", "fp_cruse_trad"), function(nm) {
  if (nm %in% names(IRdata)) {
    tbl <- table(IRdata[[nm]], useNA = "ifany")
    paste(names(tbl), tbl, collapse = "; ")
  } else {
    "MISSING"
  }
})
print(diagnostics)



#------------------------------------------------------------------------------#



# Age at female sterilization

v320labels <- val_labels(IRdata$v320)

IRdata <- IRdata %>%
  mutate(fp_ster_age = 
           ifelse(v312 == 6, v320, NA)) %>%
  set_value_labels(fp_ster_age = val_labels(IRdata$v320)) %>%
  set_variable_labels(fp_ster_age = "Age at time of sterilization for women")


# !!! need to assess medians
# # Median age at sterilization
#            gen ster_age = int((v317 - v011) / 12)
# replace ster_age = . if v312!=6 
# replace ster_age = . if v320>=5 
# 
# summarize ster_age [fweight=v005], detail
# * 50% percentile
# scalar sp50=r(p50)
# 
# gen dummy=. 
# replace dummy=0 if v312==6 & v320<5
# replace dummy=1 if ster_age<sp50 & v312==6 & v320<5
# summarize dummy [fweight=v005]
# scalar sL=r(mean)
# drop dummy
# 
# gen dummy=. 
# replace dummy=0 if v312==6 & v320<5
# replace dummy=1 if ster_age <=sp50 & v312==6 & v320<5
# summarize dummy [fweight=v005]
# scalar sU=r(mean)
# drop dummy
# 
# gen fp_ster_median=round(sp50+(.5-sL)/(sU-sL),.01)
# label var fp_ster_median "Median age at time of sterilization for women"

#

#------------------------------------------------------------------------------#


### Source of Contraceptive method

# NOTE: only for women that are using a modern method and do not use LAM

# !!! need to move over value labels for all vars below in this section

# Source for all 
IRdata <- IRdata %>%
  mutate(fp_source_tot = v326) %>%
  set_variable_labels(fp_source_tot = "Source of contraception - total")


# Source for female sterilization users
IRdata <- IRdata %>%
  mutate(fp_source_fster = 
           ifelse(v312==6, v326, NA)) %>%
  set_value_labels(fp_source_fster = val_labels(IRdata$v326)) %>%
  set_variable_labels(fp_source_fster = "Source for female sterilization")


# Source for pill users
IRdata <- IRdata %>%
  mutate(fp_source_pill = 
           ifelse(v312==1, v326, NA)) %>%
  set_value_labels(fp_source_pill = val_labels(IRdata$v326)) %>%
  set_variable_labels(fp_source_pill = "Source for pill")


# Source for IUD users
IRdata <- IRdata %>%
  mutate(fp_source_iud = 
         ifelse(v312==2, v326, NA)) %>%
  set_value_labels(fp_source_iud = val_labels(IRdata$v326)) %>%
  set_variable_labels(fp_source_iud = "Source for IUD")


# Source for injectable users
IRdata <- IRdata %>%
  mutate(fp_source_inj = 
           ifelse(v312==3, v326, NA)) %>%
  set_value_labels(fp_source_inj = val_labels(IRdata$v326)) %>%
  set_variable_labels(fp_source_inj = "Source for injectables")


# Source for implant users
IRdata <- IRdata %>%
  mutate(fp_source_imp = 
           ifelse(v312==11, v326, NA)) %>%
  set_value_labels(fp_source_imp = val_labels(IRdata$v326)) %>%
  set_variable_labels(fp_source_imp = "Source for implants")


# Source for male condom users
IRdata <- IRdata %>%
  mutate(fp_source_mcond = 
           ifelse(v312==5, v326, NA)) %>%
  set_value_labels(fp_source_mcond = val_labels(IRdata$v326)) %>%
  set_variable_labels(fp_source_mcond = "Source for male condom")




#------------------------------------------------------------------------------#



### Brands used for pill and condom
  


# Brand used for pill
IRdata <- IRdata %>%
  mutate(fp_brand_pill =
           case_when(
             v312 == 1 ~ v323,
             v323 < 96 ~ v323)
           ) %>%
  set_value_labels(fp_brand_pill = val_labels(IRdata$v323)) %>%
  set_variable_labels(fp_brand_pill = "Pill users using a social marketing brand")


# Brand used for male condom
IRdata <- IRdata %>%
  mutate(fp_brand_cond = 
           case_when(
             v312 == 5 ~ v323a,
             v323a < 96 ~ v323a
           )) %>% 
  set_value_labels(fp_source_mcond = val_labels(IRdata$v323a)) %>%
  set_variable_labels(fp_brand_cond =  "Male condom users using a social marketing brand")



#------------------------------------------------------------------------------#



### Information given


  
# Informed of side effects
IRdata <- IRdata %>%
  mutate(fp_info_sideff = 
           case_when(
             ((v312 %in% c(1,2,3,6,11)) & (v008-v317<60)) ~ 0,
             ((v3a02==1 | v3a03==1) & (v312 %in% c(1,2,3,6,11)) & (v008-v317<60)) ~ 1)) %>%
  set_value_labels(fp_info_sideff = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_info_sideff = "Informed about side effects or problems among female sterilization, pill, IUD, injectables, and implant users")


# Informed of what to do
IRdata <- IRdata %>%
  mutate(fp_info_what_to_do = 
           case_when(
             ((v312 %in% c(1,2,3,6,11)) & (v008-v317<60)) ~ 0,
             (v3a04==1 & (v312 %in% c(1,2,3,6,11)) & (v008-v317<60)) ~ 1)) %>% 
  set_value_labels(fp_info_what_to_do = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_info_what_to_do = "Informed of what to do if experienced side effects among female sterilization, pill, IUD, injectables, and implant users")


# Informed of other methods to use
IRdata <- IRdata %>%
  mutate(fp_info_other_meth = 
           case_when(
             ((v312 %in% c(1,2,3,6,11)) & (v008-v317<60)) ~ 0,
             ((v3a05==1 | v3a06==1) & (v312 %in% c(1,2,3,6,11)) & (v008-v317<60)) ~ 1)) %>%
  set_value_labels(fp_info_other_meth = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_info_other_meth = "Informed of other methods by health or FP worker among female sterilization, pill, IUD, injectables, and implant users")


# Informed of all three (method information index)
IRdata <- IRdata %>%
  mutate(fp_info_all = 
           case_when(
             ((v312 %in% c(1,2,3,6,11)) & (v008-v317<60)) ~ 0,
             ((v3a02==1 | v3a03==1) & v3a04==1 & (v3a05==1 | v3a06==1) & (v312 %in% c(1,2,3,6,11)) & (v008-v317<60)) ~ 1)) %>% 
  set_value_labels(fp_info_all = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_info_all = "Informed of all three (method information index) among female sterilization, pill, IUD, injectables, and implant users")




