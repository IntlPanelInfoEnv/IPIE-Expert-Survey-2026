##### Import Packages ####
library(rio)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)

#### Cleaning ####
Data <- import("Data.xlsx")
Data <- filter(Data, as.numeric(Progress) > 90) # 470

#### Map of countries of expertise (Figure 1) ####

# 2026: a single country of expertise per respondent (Q9)
data <- as.data.frame(table(Data$Q9))
names(data) <- c("country", "count")
data$country <- as.character(data$country)

# Rename survey labels to match rnaturalearth country names
data$country[data$country == "Ireland {Republic}"] <- "Ireland"
data$country[data$country == "Korea South"] <- "South Korea"
data$country[data$country == "United States"] <- "USA"
data$country[data$country == "Russian Federation"] <- "Russia"
data$country[data$country == "Congo {Democratic Rep}"] <- "Dem. Rep. Congo"
data$country[data$country == "Myanmar, {Burma}"] <- "Myanmar"
data$country[data$country == "Antigua & Deps"] <- "Antigua and Barb."

world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- subset(world_map, name != "Antarctica")
world_map$name[world_map$name == "United States of America"] <- "USA"

unmatched <- setdiff(data$country, world_map$name)
if (length(unmatched) > 0) stop("Countries missing from the map: ",
                                paste(unmatched, collapse = ", "))

merged_data <- merge(world_map, data, by.x = "name", by.y = "country", all.x = TRUE)
merged_data$count[is.na(merged_data$count)] <- 0

merged_data$count <- cut(merged_data$count,
                         breaks = c(-1, 0, 1, 5, 10, 20, 40, 200),
                         labels = c("0", "1", "2-5", "6-10", "11-20", "21-40", "120"))

ggplot(data = merged_data) +
  geom_sf(aes(fill = count, color = count), lwd = 0.2) +
  theme_map() +
  coord_sf(crs = "+proj=eqearth")+
  scale_color_manual(values = rep("black", 7), guide = "none") +
  scale_fill_manual(values = c("white", "lightgoldenrod1", "gold", "goldenrod2", "darkorange2", "firebrick", "black"))+
  guides(fill = "none")

ggsave("../Figures/fig1_map.pdf", width = 15, height = 10)
ggsave("../Figures/fig1_map.png", width = 14, height = 7.5, dpi = 300, bg = "white")
