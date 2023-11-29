##############################################################################
################################# Data Clean #################################
##############################################################################

##############################################################################
##### This script takes the raw data and outputs a csv file named ############
##### clean_data.csv/rds to the /outputs folder that is more #################
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

race_levels <- c("White/Caucasian", 
                 "Black or African American", 
                 "Hispanic or Latino",
                 "Asian",
                 "American Indian or Alaska Native",
                 "Native Hawaiian or Other Pacific Islander")

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
                                    levels = drinking_levels)) |> 

##############################################################################
############# Change Boolean-type question responses to numeric #############
##############################################################################
### 1 = issues present
### 0 = issues absent
### NA = blank/prefer not to answer
##############################################################################


  # Converting 'are_drinking_water_issues_present' to numeric
  mutate(are_drinking_water_issues_present = case_when(
    are_drinking_water_issues_present == "Yes" ~ 1,
    are_drinking_water_issues_present == "No" ~ 0,
    TRUE ~ NA_real_)) |>
  
  # Converting 'consistently_uses_waterbottle' to numeric
  mutate(consistently_uses_waterbottle = case_when(
    consistently_uses_waterbottle == "Yes" ~ 1,
    consistently_uses_waterbottle == "No" ~ 0,
    TRUE ~ NA_real_)) |>
  
  # Converting 'is_tap_water_filtered' to numeric
  mutate(is_tap_filtered = case_when(
    is_tap_filtered == "Yes" ~ 1,
    is_tap_filtered == "No" ~ 0,
    TRUE ~ NA_real_)) |>

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


  # Converting 'home_tap_is_safe' Likert scale responses to numeric
  mutate(home_tap_is_safe = case_when(
    home_tap_is_safe == "Strongly disagree" ~ 1,
    home_tap_is_safe == "Somewhat disagree" ~ 2,
    home_tap_is_safe == "Neither agree nor disagree" ~ 3,
    home_tap_is_safe == "Somewhat agree" ~ 4,
    home_tap_is_safe == "Strongly agree" ~ 5,
    TRUE ~ NA_real_)) |>
  
  # Converting 'campus_tap_is_safe' in similar fashion
  mutate(campus_tap_is_safe = case_when(
    campus_tap_is_safe == "Strongly disagree" ~ 1,
    campus_tap_is_safe == "Somewhat disagree" ~ 2,
    campus_tap_is_safe == "Neither agree nor disagree" ~ 3,
    campus_tap_is_safe == "Somewhat agree" ~ 4,
    campus_tap_is_safe == "Strongly agree" ~ 5,
    TRUE ~ NA_real_)) |>
  
  # Repeat for 'bottles_are_safe', 'trust_in_local_government', 'choices_influenced_by_quality_in_community'
  mutate(bottles_are_safe = case_when(
    bottles_are_safe == "Strongly disagree" ~ 1,
    bottles_are_safe == "Somewhat disagree" ~ 2,
    bottles_are_safe == "Neither agree nor disagree" ~ 3,
    bottles_are_safe == "Somewhat agree" ~ 4,
    bottles_are_safe == "Strongly agree" ~ 5,
    TRUE ~ NA_real_)) |>
  
  mutate(trust_in_local_government = case_when(
    trust_in_local_government == "Strongly disagree" ~ 1,
    trust_in_local_government == "Somewhat disagree" ~ 2,
    trust_in_local_government == "Neither agree nor disagree" ~ 3,
    trust_in_local_government == "Somewhat agree" ~ 4,
    trust_in_local_government == "Strongly agree" ~ 5,
    TRUE ~ NA_real_)) |>
  
  mutate(choices_influenced_by_quality_in_community = case_when(
    choices_influenced_by_quality_in_community == "Strongly disagree" ~ 1,
    choices_influenced_by_quality_in_community == "Somewhat disagree" ~ 2,
    choices_influenced_by_quality_in_community == "Neither agree nor disagree" ~ 3,
    choices_influenced_by_quality_in_community == "Somewhat agree" ~ 4,
    choices_influenced_by_quality_in_community == "Strongly agree" ~ 5,
    TRUE ~ NA_real_)) 

##############################################################################
######################## Split Multiple Responses ############################
##############################################################################
# Create new columns and populate with 1 or 0
for(race in race_levels) {
  clean_data[[race]] <- as.integer(grepl(race, clean_data$race))
}

# omit the original race col
clean_data <- clean_data

names <- names(clean_data)

names(clean_data) <- c(names[1:26], c("White", "Black", "Latino",
                                            "Asian", "Native_American",
                                            "Pacific_Native"))

##############################################################################
################################# Write CSV #################################
##############################################################################

# Writing the cleaned data to a CSV file in the outputs folder
write.csv(clean_data, "outputs/clean_data.csv", row.names = FALSE)

# Writing the cleaned data to a RDS file in the outputs folder
# To preserve the "factor" column's levels
saveRDS(clean_data, "outputs/clean_data.rds")










