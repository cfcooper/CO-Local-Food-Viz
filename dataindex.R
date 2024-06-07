

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
 

rm(list=ls()) # Caution: this clears the Environment 

## upload all data ------------------------------------------------------------ 

processor_data <- read.csv("datasets/MPI_directory.csv")
foodhub_data <- read.csv("datasets/foodhub.csv")

foodhub_data$state_zip <- sub(".*, ", "", foodhub_data$location_address)
foodhub_data$zip <- gsub("\\D", "", foodhub_data$state_zip)
foodhub_data$state <- sub(" \\d{5}$", "", foodhub_data$state_zip)

# Extract relevant columns
























