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
library(tidyverse) # data management
library(stringr) # for handling character strings

############################### Load Data ##################################
education_levels <- c("Freshman student", 
                      "Sophomore student", 
                      "Junior student",
                      "Senior student",
                      "Master's student",
                      "PhD student",
                      "Other")

political_levels <- c("Consistently liberal", 
                      "Mostly liberal",
                      "Mixed",
                      "Mostly conservative",
                      "Consistently conservative",
                      "Other")

home_water_levels <- c("Public Supply",
                       "Well")

drinking_levels <- c("Tap water", 
                     "Plastic bottled water", 
                     "Glass bottled/canned/boxed water",
                     "I don't drink water")

clean_data <- read.csv("data/raw_data.csv") |>

# Replace blank values with NA's
  mutate_if(is.character, ~na_if(.,"")) |>
  
# Replace "Prefer not to answer" values with NA's
  mutate_if(is.character, ~na_if(.,"Prefer not to answer")) |>
  
# Filter out non-consenting respondents
  filter(concent == "Yes, I want to participate") |>
  select(-concent) |> # omit the column as its not needed anymore

# Filter non-numeric ages and convert to numeric
  mutate(age = if_else(str_detect(age, "^[0-9.]+$"), 
                       as.numeric(age), NA_real_)) |> 

# zipcode as a character and adding the '0' at begining back
  mutate(zipcode = if_else(
    str_length(zipcode) == 4, paste0("0", zipcode),
    if_else(str_length(zipcode) == 5, zipcode, NA_character_)
  )) |>

# educational status as factor
  mutate(educational_status = factor(educational_status, 
                                     levels = education_levels)) |>

# political as factor
  mutate(political = factor(political, levels = political_levels)) |>

# home_water_source as factor
  mutate(home_water_source = factor(home_water_source,
                                    levels = home_water_levels)) |>

# primary_drinking_source as factor
  mutate(primary_drinking_source = factor(primary_drinking_source,
                                    levels = drinking_levels)) 



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









