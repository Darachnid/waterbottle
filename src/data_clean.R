##############################################################################
################################# Data Clean #################################
##############################################################################

##############################################################################
##### This script takes the raw data and outputs a csv file named ############
##### clean_data.csv to the /outputs folder that is more #####################
##### conducive for data analysis. ###########################################
##############################################################################

# Set the Working Directory
setwd(here::here())

# Libraries
library(tidyverse)

# Load Data
raw_data <- read.csv("data/raw_data.csv")
