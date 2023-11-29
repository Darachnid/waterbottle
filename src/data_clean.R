##############################################################################
################################# Data Clean #################################
##############################################################################

##############################################################################
##### This script takes the raw data and outputs a csv file named ############
##### clean_data.csv to the /outputs folder that is more #####################
##### conducive for data analysis. ###########################################
##############################################################################

###################### Set Working directory ##############################
setwd(here::here())

############################### Libraries ##################################
library(tidyverse)

############################### Load Data ##################################
raw_data <- read.csv("data/raw_data.csv")

# Filter out non-consenting respondents

# Age as numeric

# Format the date as a lubridate

# zipcode as a character and adding the '0' at begining back

# educational status as factor

# political as factor

# home_water_source as factor

# primary_drinking_source as factor




##############################################################################
############# Change Boolean-type question responses to numeric #############
##############################################################################
### 1 = issues present
### 0 = issues absent
### NA = blank/prefer not to answer)
##############################################################################

# are_drinking_water_issues_present as numeric 

# consistently_uses_waterbottle as numeric 

# consistently_uses_waterbottle as numeric 



##############################################################################
####################### Convert Likart to numeric ###########################
##############################################################################
##### 1: Strongly Disagree
##### 2: Somewhat Disagree
##### 3: Neither Agree nor Disagree
##### 4: Somewhat Disagree
##### 5: Strongly Agree
##### NA: Blank/Prefer not to answer
##############################################################################


# home_tap_is_safe 

# campus_tap_is_safe 

# bottles_are_safe

# trust_in_local_government

# choices_influenced_by_quality_in_community 









