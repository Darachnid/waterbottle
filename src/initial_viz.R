##############################################################################
############################## Initial Visuals ###############################
##############################################################################

##############################################################################
#### INPUT: ###### data/clean_data.csv #######################################
##############################################################################

##############################################################################
#### OUTPUT: ###### initial plots of the raw data ############################
##############################################################################

# Libraries
library(tidyverse)
library(ggthemes)

# Set the global theme to "minimal" for all plots
theme_set(theme_minimal())

##############################################################################
############################## Initial Visuals ###############################
##############################################################################

##############################################################################
#### INPUT: ###### data/clean_data.csv #######################################
##############################################################################

##############################################################################
#### OUTPUT: ###### initial plots of the raw data ############################
##############################################################################

# Libraries
library(tidyverse)
library(ggthemes)

# Set the global theme to "minimal" for all plots
theme_set(theme_minimal())

data <- readRDS("outputs/clean_data.rds")

############################## Distributions ###############################

##### Plot age and race
data$GenderRace <- paste(data$gender, data$race)

data[which(data$gender != "Man" & data$gender != "Woman"), "gender"] <- "Other"

data[,33] <- NULL

df <- data %>%
  separate_rows(race, sep = ",") %>% # Adjust the separator if needed
  mutate(race = trimws(race)) 

# Reshape the data to long format
long_data <- df |>
  select(gender, 
         race, 
         home_tap_is_safe,
         campus_tap_is_safe,
         bottles_are_safe, 
         trust_in_local_government, 
         choices_influenced_by_quality_in_community) |>
  pivot_longer(!c(race, gender), 
               names_to = "parameter",
               values_to = "rank") |>
  filter(!is.na(race) & !is.na(gender)) |>
  group_by(gender) |>
  arrange(race, gender) |>
  ggplot(aes(x = race, y = rank, fill = gender)) +
  geom_boxplot() +
  facet_wrap(~parameter, nrow = 5) +
  scale_color_colorblind()
long_data