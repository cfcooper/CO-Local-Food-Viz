

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(maps)
library(mapdata)
library(viridis)

rm(list=ls()) # Caution: this clears the Environment



processor_data <- read.csv("MPI_directory.csv")
foodhub_data <- read.csv("foodhub.csv")


processor_data %<>% mutate(t2 = activities) %>% separate_rows(activities, sep = ";")
processor_data$activities <- trimws(processor_data$activities, which = "left")

summary_processor <- processor_data %>% group_by(activities) %>%
  summarise(count = n())

meat_data <- processor_data[processor_data$activities %in% c("Meat Processing", "Meat Slaughter", 
                                                             "Poultry Processing", "Poultry Slaughter"),]
meat_data <- meat_data[meat_data$state %in% c("CO", "WY", "MT", "ID", "WA", "OR"),]



# Seperate city, state, and zipcode
foodhub_data <- foodhub_data %>%
  mutate(street = str_split(address, ",", simplify = TRUE)[, 1],
         city = str_split(address, ",", simplify = TRUE)[, 2],
         state = str_split(address, ",", simplify = TRUE)[, 3])

foodhub_data <- foodhub_data %>%
  mutate(zipcode = str_extract(state, "\\d+"),
         state = str_remove(state, "\\d+\\s*"))
foodhub_data$state <- trimws(foodhub_data$state, which = "right")
foodhub_data$state <- tolower(foodhub_data$state)
foodhub_data <- foodhub_data[foodhub_data$state %in% c("colorado", "wyoming", "montana", "idaho", 
                                                  "washington", "oregon"),]

# create maps

meat_map <- ggplot() +
  geom_polygon(data = base_states, aes(x = long, y = lat, group = group), fill = "#d3d3d3", color = "white") + 
  geom_point(data = meat_data,aes(x = longitude, y = latitude), color = "blue", size = .8) +
  coord_map()+
  theme_void() + labs( 
    title = "Northwest and Rocky Mountain Region: Meat and Poultry Processing Facilities"
  )

meat_map

hub_map <- ggplot() +
  geom_polygon(data = base_states, aes(x = long, y = lat, group = group), fill = "#d3d3d3", color = "white") + 
  geom_point(data = foodhub_data,aes(x = location_x, y = location_y), color = "blue", size = .8) +
  coord_map()+
  theme_void() + labs( 
    title = "Northwest and Rocky Mountain Region: Food Hubs"
  )

hub_map
