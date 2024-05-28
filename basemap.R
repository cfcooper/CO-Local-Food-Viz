
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(mapdata)
library(viridis)


rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## create base region map-------------------------------------------------------

fips_data <- read.csv("mapdata/fips.csv")
fips_data$statelong <- tolower(fips_data$statelong)
fips_data$county <- tolower(fips_data$county)

fips_data <- fips_data[fips_data$statelong %in% c("colorado", "wyoming", "montana", "idaho", 
                                              "washington", "oregon"),]

population <- list.files(path="mapdata/population", pattern="*csv", full.names = T)
population <- lapply(population, read.csv)
population <- do.call("rbind", population)
population <- rename(population, "2020pop" = X2020, "2021pop" = X2021, "2022pop" = X2022, "pop2023" = X2023)
population$county <- tolower(population$county)
population <- separate(population, county, into = c("county", "statelong"), sep = ",")
population$county <- gsub(" county", "", population$county)
population$statelong <- trimws(population$statelong, which = "left")
population$pop2023 <- as.numeric(gsub(",", "", population$pop2023))

population <- merge(fips_data, population, by = c("statelong","county"))


countydat <- map_data("county")
statedat <- map_data("state")

basemap <- countydat[countydat$region %in% c("colorado", "wyoming", "montana", "idaho", 
                                              "washington", "oregon"),]
rm(countydat)


basemap$state <- "x"
basemap$state <- if_else(basemap$region == "colorado", "CO", basemap$state)
basemap$state <- if_else(basemap$region == "wyoming", "WY", basemap$state)
basemap$state <- if_else(basemap$region == "montana", "MT", basemap$state)
basemap$state <- if_else(basemap$region == "idaho", "ID", basemap$state)
basemap$state <- if_else(basemap$region == "washington", "WA", basemap$state)
basemap$state <- if_else(basemap$region == "oregon", "OR", basemap$state)

basemap <- basemap %>% rename("county" = "subregion")


population$code <- paste(population$state,population$county,sep="_")
basemap$code <- paste(basemap$state,basemap$county,sep="_")


statedat$state <- "x"
statedat$state <- if_else(statedat$region == "colorado", "CO", statedat$state)
statedat$state <- if_else(statedat$region == "wyoming", "WY", statedat$state)
statedat$state <- if_else(statedat$region == "montana", "MT", statedat$state)
statedat$state <- if_else(statedat$region == "idaho", "ID", statedat$state)
statedat$state <- if_else(statedat$region == "washington", "WA", statedat$state)
statedat$state <- if_else(statedat$region == "oregon", "OR", statedat$state)
base_states <- statedat[statedat$state %in% c("CO", "WY", "MT", "ID", "WA", "OR"),]

basecountymap <- subset(basemap, code %in% population$code)

lookupcode <- split(population$pop2023, population$code)
basecountymap$pop2023 <- ifelse(basecountymap$code %in% names(lookupcode), unlist(lookupcode[basecountymap$code]), "")




statemap <- ggplot() +
  geom_polygon(data = base_states, aes(x = long, y = lat, group = group), fill = "#d3d3d3", color = "white") +
  coord_map()+
  theme_void() + labs( 
    title = "Northwest and Rocky Mountain Region"
  )

statemap


countymap <- ggplot(data = basecountymap) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = "grey"), color = "white", size = .3) +
  coord_map()+
  theme_void() + labs( 
    title = "Northwest and Rocky Mountain Region"
  )

countymap









