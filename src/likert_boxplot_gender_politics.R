# Libraries
library(tidyverse)
library(ggthemes)
library(ggforce)

# Set the global theme to "minimal" for all plots
theme_set(theme_minimal())


data <- readRDS("outputs/clean_data.rds")

data[which(data$gender != "Man" & data$gender != "Woman"), "gender"] <- "Other"

data[,33] <- NULL

# Reshape the data to long format
long_data <- data |>
  select(gender, 
         political, 
         home_tap_is_safe,
         campus_tap_is_safe,
         bottles_are_safe, 
         trust_in_local_government, 
         choices_influenced_by_quality_in_community) 

names(long_data) <- c("gender",
                      "political", 
                      "Drinking tap water at home is safe",
                      "Drinking tap water on campus is safe",
                      "Drinking plastic bottled water is safe",
                      "Govt. provides reliable information on the safety of drinking water",
                      "My choices between are influenced by water quality in my community")
long_data <- long_data |>
  pivot_longer(!c(political, gender), 
               names_to = "parameter",
               values_to = "rank") |>
  filter(!is.na(political) & !is.na(gender)) |>
  mutate(rank = rank - 3) |>
  filter(political != "Other")

# Create a summary table to count observations
count_data <- long_data |>
  group_by(gender, political, parameter) |>
  summarize(count = n()) |>
  filter(count >= 5)

# Filter original data based on the summary table
filtered_data <- long_data %>%
  inner_join(count_data, by = c("gender", "political", "parameter")) 


long_data_x5 <- bind_rows(long_data, long_data, long_data, long_data, long_data)
long_data_x10 <- bind_rows(long_data, long_data, long_data, long_data, long_data,
                          long_data, long_data, long_data, long_data, long_data)
long_data_x50 <- bind_rows(long_data_x10, long_data_x10, long_data_x10, long_data_x10, long_data_x10)

# Colorless
plot <- long_data_x50 |>
  na.omit() |>
  ggplot(aes(x = political, y = rank, color = political)) +
  geom_jitter(size = 2, width = 0.5, height = 0.5, alpha = 0.5) +
  scale_color_colorblind() +
  facet_wrap(~parameter, nrow = 5) +
  geom_hline(yintercept = 0) +
  labs(title = "Likert Responses by Political Views and Gender",
       x = "Political Views",
       y = "Rank (2 = Strongly Agree)") 
plot

# Color by gender jitter
plot <- long_data_x50 |>
  na.omit() |>
  ggplot(aes(x = political, y = rank, color = gender)) +
  geom_jitter(size = 2, width = 0.5, height = 0.5, alpha = 0.4) +
  scale_color_colorblind() +
  facet_wrap(~parameter, nrow = 5) +
  geom_hline(yintercept = 0) +
  labs(title = "Likert Responses by Political Views and Gender",
       x = "Political Views",
       y = "Rank (2 = Strongly Agree)") 
plot

# Color by gener
plot <- long_data |>
  ggplot(aes(x = political, y = rank)) +
  geom_jitter(size = 4, width = 0.45, height = 00.2,
              aes(color = gender)) +
  scale_color_colorblind() +
  facet_wrap(~parameter, nrow = 5) +
  geom_hline(yintercept = 0) +
  labs(title = "Likert Responses by Race and Gender",
       x = "Political Views",
       y = "Rank (2 = Strongly Agree)") 
plot



# Create the plot
plot <- filtered_data |>
  ggplot(aes(x = political, y = rank, fill = gender)) +
  geom_boxplot() +
  facet_wrap(~parameter, nrow = 5) +
  scale_fill_colorblind() +
  geom_hline(yintercept = 0) +
  labs(title = "Likert Responses by Race and Gender",
       x = "Political Views",
       fill = "Gender",
       y = "Rank (2 = Strongly Agree)") 
plot
 



library(ggplot2)

# Assuming long_data is your prepared data frame
plot <- long_data |>
  ggplot(aes(x = rank, color = gender)) +
  geom_jitter(aes(y = political), size = 4, width = 0.3, height = 0.1, alpha = 0.5) +
  geom_density(aes(y = ..count..), color = "black", size = 1) +  # Single density line for all genders
  scale_color_colorblind() +
  facet_wrap(~parameter, nrow = 5, scales = "free_x") +
  labs(title = "Likert Responses by Race and Gender",
       x = "Rank (2 = Strongly Agree)",
       y = "Political Views") +
  theme_minimal() +
  theme(legend.position = "bottom")
plot

library(ggplot2)

political_levels <- levels(factor(long_data$political))
# Assuming long_data is your prepared data frame
plot <- long_data |>
  ggplot(aes(x = rank, y = political, color = gender)) +
  geom_jitter(size = 4, width = 0.3, height = 0.1, alpha = 0.5) +
  geom_density(aes(y = ..count..), color = "black", size = 1) +
  scale_y_continuous(breaks = 1:length(political_levels), labels = political_levels) 
plot

  geom_density(aes(y = after_stat(density)*5), color = "black", size = 1) +  # Overlay density plot
  scale_color_colorblind() +
  facet_wrap(~parameter, nrow = 5, scales = "free_x") +
  labs(title = "Likert Responses by Race and Gender",
       x = "Rank (2 = Strongly Agree)",
       y = "Political Views") +
  theme_minimal() +
  theme(legend.position = "bottom")
plot


library(ggplot2)

# Assuming long_data is your prepared data frame
plot <- long_data |>
  ggplot(aes(y = rank, x = political, color = gender)) +
  geom_jitter(size = 4, width = 0.3, height = 0.1, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line", aes(group = 1), color = "black", size = 1) +
  scale_color_colorblind() +
  facet_wrap(~parameter, nrow = 5) +
  labs(title = "Likert Responses by Race and Gender",
       x = "Rank (2 = Strongly Agree)",
       y = "Political Views") +
  theme_minimal() +
  theme(legend.position = "bottom")
plot


library(ggplot2)

# Assuming long_data is your prepared data frame
plot <- long_data |>
  ggplot(aes(x = rank, color = gender)) +
  geom_jitter(size = 4, width = 0.3, height = 0.1, alpha = 0.5,
              aes( y = as.numeric(factor(political)))) +
#  stat_summary(fun = "mean", geom = "line", aes(group = 1), color = "black", size = 1) +
#  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, aes(group = 1), color = "blue") +
  geom_histogram(aes(x = rank)) +
  scale_color_colorblind() +
  facet_wrap(~parameter, nrow = 5) +
  scale_y_continuous(breaks = 1:length(political_levels), labels = political_levels) +
  labs(title = "Likert Responses by Race and Gender",
       x = "Rank (2 = Strongly Agree)",
       y = "Political Views") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot
