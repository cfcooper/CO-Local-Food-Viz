

library(dplyr)
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

geom_point(data = vegharvest,aes(x = long, y = lat, size = vegacres),color = "blue", alpha = .5,
           position = position_jitter(width = 0.2, height = 0.2)) + scale_size(range = c(1, 5))



statemap <- ggplot() +
  geom_polygon(data = base_states, aes(x = long, y = lat, group = group), fill = "#d3d3d3", color = "white") + 
  geom_point(data = meat_data,aes(x = longitude, y = latitude), color = "blue", size = .5) +
  coord_map()+
  theme_void() + labs( 
    title = "Northwest and Rocky Mountain Region"
  )

statemap

color = "blue", alpha = .5,
position = position_jitter(width = 0.2, height = 0.2)) + scale_size(range = c(1, 5)



