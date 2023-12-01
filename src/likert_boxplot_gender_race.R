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
         choices_influenced_by_quality_in_community) 

names(long_data) <- c("gender",
                      "race", 
                      "Drinking tap water at home is safe",
                      "Drinking tap water on campus is safe",
                      "Drinking plastic bottled water is safe",
                      "Govt. provides reliable information on the safety of drinking water",
                      "My choices between are influenced by water quality in my community")
long_data <- long_data |>
  pivot_longer(!c(race, gender), 
               names_to = "parameter",
               values_to = "rank") |>
  filter(!is.na(race) & !is.na(gender)) |>
  mutate(rank = rank - 3) 

# Create a summary table to count observations
count_data <- long_data |>
  group_by(gender, race, parameter) |>
  summarize(count = n()) |>
  filter(count >= 5)

# Filter original data based on the summary table
filtered_data <- long_data %>%
  inner_join(count_data, by = c("gender", "race", "parameter")) 

# Create the plot
plot <- filtered_data |>
  ggplot(aes(x = race, y = rank, fill = gender)) +
  geom_boxplot() +
  facet_wrap(~parameter, nrow = 5) +
  scale_fill_colorblind() +
  geom_hline(yintercept = 0) +
  labs(title = "Likert Responses by Race and Gender",
       x = "Race",
       fill = "Gender",
       y = "Rank (2 = Strongly Agree)")
plot




