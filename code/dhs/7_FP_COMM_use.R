# ******************************************************************************
# Program: 			  FP_COMM.R
# Purpose: 		    Code communication related indicators: exposure to FP messages,
#                 decision on use/nonuse, discussions.  
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen
# Date last modified: March 29 2021 by Courtney Allen
# ******************************************************************************
#   

# -----------------------------------------------------------------------------#
# # Variables created in this file:
# fp_message_radio		"Exposure to family planning message by radio"
# fp_message_tv			  "Exposure to family planning message by TV"
# fp_message_paper		"Exposure to family planning message by newspaper/magazine"
# fp_message_mobile		"Exposure to family planning message by mobile phone"
# fp_message_noneof4	"Not exposed to any of the four media sources"
# fp_message_noneof3 	"Not exposed to TV, radio, or paper media sources"
# 
# fp_decyes_user			"Who makes the decision to use family planning among users"
# fp_decno_nonuser		"Who makes decision not to use family planning among non-users"
# 
# fp_fpvisit_discuss	"Women non-users that were visited by a FP worker who discussed FP"
# fp_hf_discuss			  "Women non-users who visited a health facility in last 12 months and discussed FP"
# fp_hf_notdiscuss		"Women non-users who visited a health facility in last 12 months and did not discuss FP"
# fp_any_notdiscuss		"Women non-users who did not discuss FP neither with FP worker or in a health facility"
# ------------------------------------------------------------------------------
  

## FAMILY PLANNING MESSAGES

# Family planning messages by radio 
IRdata <- IRdata %>%
  mutate(fp_message_radio = 
           ifelse(v384a == 1, 1, 0)) %>%
  set_value_labels(fp_message_radio = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_radio = "Exposure to family planning message by radio")


# Family planning messages by TV 
IRdata <- IRdata %>%
  mutate(fp_message_tv = 
           ifelse(v384b == 1, 1, 0)) %>%
  set_value_labels(fp_message_tv = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_tv = "Exposure to family planning message by TV")


# Family planning messages by newspaper and/or magazine 
IRdata <- IRdata %>%
  mutate(fp_message_paper = 
           ifelse(v384c == 1, 1, 0)) %>%
  set_value_labels(fp_message_paper = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_paper = "Exposure to family planning message by newspaper/magazine")


# Family planning messages by newspaper and/or magazine 
IRdata <- IRdata %>%
  mutate(fp_message_mobile = 
           ifelse(v384d == 1, 1, 0))%>%
  set_value_labels(fp_message_mobile = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_mobile = "Exposure to family planning message by mobile phone")


# Did not hear a family planning message from any of the 4 media sources
IRdata <- IRdata %>%
  mutate(fp_message_noneof4 =
           ifelse(v384a!=1 & v384b!=1 & v384c!=1 & v384d!=1, 1, 0))%>%
  set_value_labels(fp_message_noneof4 = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_noneof4 = "Exposure to family planning message any of four sources (TV, radio, paper, mobile)")


# Did not hear a family planning message from radio, TV or paper
IRdata <- IRdata %>%
  mutate(fp_message_noneof3 =
           ifelse(v384a!=1 & v384b!=1 & v384c!=1, 1, 0)) %>%
  set_value_labels(fp_message_noneof3 = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_noneof3 = "Not exposed to TV, radio, or paper media sources")



## FAMILY PLANNING DECISION MAKING AND DISCUSSION
  
IRdata <- IRdata %>%
  # Decision to use among users: keep v632 only for current users (v312 != 0)
  mutate(fp_decyes_user = ifelse(v312 != 0, v632, NA_real_)) %>%
  # strip any labelled attribute and ensure numeric
  mutate(fp_decyes_user = as.numeric(unclass(fp_decyes_user)))

# attach labels (copying labels from v632 if present)
val_labels(IRdata$fp_decyes_user) <- val_labels(IRdata$v632)
var_label(IRdata$fp_decyes_user) <- "Who makes the decision to use family planning (users)"

IRdata <- IRdata %>%
  # Decision to not use among non-users: keep v632 only for non-users (v312 == 0)
  mutate(fp_decno_nonuser = ifelse(v312 == 0, v632, NA_real_)) %>%
  mutate(fp_decno_nonuser = as.numeric(unclass(fp_decno_nonuser)))

val_labels(IRdata$fp_decno_nonuser) <- val_labels(IRdata$v632)
var_label(IRdata$fp_decno_nonuser) <- "Who makes decision not to use family planning (non-users)"

IRdata <- IRdata %>%
  # Discussed with FP worker (only meaningful for non-users v312 == 0)
  mutate(fp_fpvisit_discuss = case_when(
    v312 == 0 & !is.na(v393a) & v393a == 1 ~ 1,     # non-user and worker discussed
    v312 == 0 & !is.na(v393a) & v393a == 0 ~ 0,     # non-user and worker did not discuss
    TRUE ~ NA_real_
  )) %>%
  mutate(fp_fpvisit_discuss = as.numeric(unclass(fp_fpvisit_discuss)))

val_labels(IRdata$fp_fpvisit_discuss) <- c(yes = 1, no = 0)
var_label(IRdata$fp_fpvisit_discuss) <- "Non-users visited by FP worker who discussed FP"

IRdata <- IRdata %>%
  # Discussed FP in a health facility (non-users who visited a HF (v394==1) and v395 indicates discussed)
  mutate(fp_hf_discuss = case_when(
    v312 == 0 & !is.na(v394) & !is.na(v395) & v394 == 1 & v395 == 1 ~ 1,    # visited HF and discussed
    v312 == 0 & !is.na(v394) & v394 == 1 & !is.na(v395) & v395 != 1     ~ 0, # visited HF and did not discuss
    TRUE ~ NA_real_
  )) %>%
  mutate(fp_hf_discuss = as.numeric(unclass(fp_hf_discuss)))

val_labels(IRdata$fp_hf_discuss) <- c(yes = 1, no = 0)
var_label(IRdata$fp_hf_discuss) <- "Non-users who visited HF in last 12 months and discussed FP"

IRdata <- IRdata %>%
  # Did not discuss FP in health facility (visited HF but did not discuss)
  mutate(fp_hf_notdiscuss = case_when(
    v312 == 0 & !is.na(v394) & v394 == 1 & (is.na(v395) | v395 != 1) ~ 1, # visited HF but not discussed
    v312 == 0 & !is.na(v394) & v394 == 1 & !is.na(v395) & v395 == 1 ~ 0,  # visited HF and did discuss -> 0
    TRUE ~ NA_real_
  )) %>%
  mutate(fp_hf_notdiscuss = as.numeric(unclass(fp_hf_notdiscuss)))

val_labels(IRdata$fp_hf_notdiscuss) <- c(yes = 1, no = 0)
var_label(IRdata$fp_hf_notdiscuss) <- "Non-users who visited HF and did NOT discuss FP"


IRdata <- IRdata %>%
  mutate(
    # make safe numeric copies for logical tests (drop labelled attributes)
    .v393a_num = as.numeric(unclass(v393a)),
    .v395_num  = as.numeric(unclass(v395)),

    # element-wise logic: 1 = non-user AND no discussion in either channel,
    # 0 = non-user AND discussed in >=1 channel, NA otherwise
    fp_any_notdiscuss = case_when(
      v312 == 0 & ((is.na(.v393a_num) | .v393a_num != 1) & (is.na(.v395_num) | .v395_num != 1)) ~ 1,
      v312 == 0 & ((!is.na(.v393a_num) & .v393a_num == 1) | (!is.na(.v395_num) & .v395_num == 1)) ~ 0,
      TRUE ~ NA_real_
    ),

    # ensure plain numeric (strip any leftover attrs)
    fp_any_notdiscuss = as.numeric(unclass(fp_any_notdiscuss))
  ) %>%
  # drop temporary helpers
  select(-.v393a_num, -.v395_num)

# attach yes/no labels and a var label (replace with labelled::val_labels / var_label if you prefer)
# install once if needed
if (!requireNamespace("labelled", quietly = TRUE)) install.packages("labelled")
library(labelled)

# attach value labels and a variable label
labelled::val_labels(IRdata$fp_any_notdiscuss) <- c(yes = 1, no = 0)
labelled::var_label(IRdata$fp_any_notdiscuss)  <- "Non-users who did not discuss FP with worker or in HF"

# quick check
table(IRdata$fp_any_notdiscuss, useNA = "ifany")


val_labels(IRdata$fp_any_notdiscuss) <- c(yes = 1, no = 0)
var_label(IRdata$fp_any_notdiscuss) <- "Non-users who did not discuss FP with worker or in HF"



#-------------------------------------------------------------------------------      


## indicators from MR file


## FAMILY PLANNING MESSAGES



# Family planning messages by radio 
IRdata <- IRdata %>%
  mutate(fp_message_radio = 
           ifelse(v384a == 1, 1, 0)) %>%
  set_value_labels(fp_message_radio = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_radio = "Exposure to family planning message by radio")


# Family planning messages by TV 
IRdata <- IRdata %>%
  mutate(fp_message_tv = 
           ifelse(v384b == 1, 1, 0)) %>%
  set_value_labels(fp_message_tv = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_tv = "Exposure to family planning message by TV")


# Family planning messages by newspaper and/or magazine 
IRdata <- IRdata %>%
  mutate(fp_message_paper = 
           ifelse(v384c == 1, 1, 0)) %>%
  set_value_labels(fp_message_paper = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_paper = "Exposure to family planning message by newspaper/magazine")


# Family planning messages by newspaper and/or magazine 
IRdata <- IRdata %>%
  mutate(fp_message_mobile = 
           ifelse(v384d == 1, 1, 0))%>%
  set_value_labels(fp_message_mobile = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_mobile = "Exposure to family planning message by mobile phone")


# Did not hear a family planning message from any of the 4 media sources
IRdata <- IRdata %>%
  mutate(fp_message_noneof4 =
           ifelse(v384a!=1 & v384b!=1 & v384c!=1 & v384d!=1, 1, 0))%>%
  set_value_labels(fp_message_noneof4 = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_noneof4 = "Exposure to family planning message any of four sources (TV, radio, paper, mobile)")


# Did not hear a family planning message from radio, TV or paper
IRdata <- IRdata %>%
  mutate(fp_message_noneof3 =
           ifelse(v384a!=1 & v384b!=1 & v384c!=1, 1, 0)) %>%
  set_value_labels(fp_message_noneof3 = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_noneof3 = "Not exposed to TV, radio, or paper media sources")
