#setwd("./meta-analysis")

library(stringr)
library(dplyr)
library(tidyr)

studies <- read.csv("data/chapter-1_covariates.csv")

head(studies)

studies$lat
studies$lon

# cleaning lat and lon data

## split double data at ; into max and min

studies$max_lat <- str_split(studies$lat, ",", simplify = TRUE)[,1]

studies$min_lat <- str_split(studies$lat, ",", simplify = TRUE)[,2]

studies$max_lon <- str_split(studies$lon, ",", simplify = TRUE)[,1]

studies$min_lon <- str_split(studies$lon, ",", simplify = TRUE)[,2]

# split max lat in degrees and minutes and seconds

studies$deg_max_lat <- str_split(studies$max_lat, " ", simplify = TRUE)[,1]

studies$min_max_lat <- str_split(studies$max_lat, " ", simplify = TRUE)[,2]

studies$sec_max_lat <- str_split(studies$max_lat, " ", simplify = TRUE)[,3]

# split min lat in degrees and minutes and seconds

studies$deg_min_lat <- str_split(studies$min_lat, " ", simplify = TRUE)[,1]

studies$min_min_lat <- str_split(studies$min_lat, " ", simplify = TRUE)[,2]

studies$sec_min_lat <- str_split(studies$min_lat, " ", simplify = TRUE)[,3]

# split max lon in degrees and minutes and seconds

studies$deg_max_lon <- str_split(studies$max_lon, " ", simplify = TRUE)[,1]

studies$min_max_lon <- str_split(studies$max_lon, " ", simplify = TRUE)[,2]

studies$sec_max_lon <- str_split(studies$max_lon, " ", simplify = TRUE)[,3]

# split min lon in degrees and minutes and seconds

studies$deg_min_lon <- str_split(studies$min_lon, " ", simplify = TRUE)[,1]

studies$min_min_lon <- str_split(studies$min_lon, " ", simplify = TRUE)[,2]

studies$sec_min_lon <- str_split(studies$min_lon, " ", simplify = TRUE)[,3]

# fill in NAs with 0

studies[is.na(studies)] <- 0

# fill blanks with 0

studies$deg_max_lat[studies$deg_max_lat == ""] <- 0
studies$min_max_lat[studies$min_max_lat == ""] <- 0
studies$sec_max_lat[studies$sec_max_lat == ""] <- 0
studies$deg_min_lat[studies$deg_min_lat == ""] <- 0
studies$min_min_lat[studies$min_min_lat == ""] <- 0
studies$sec_min_lat[studies$sec_min_lat == ""] <- 0
studies$deg_max_lon[studies$deg_max_lon == ""] <- 0
studies$min_max_lon[studies$min_max_lon == ""] <- 0
studies$sec_max_lon[studies$sec_max_lon == ""] <- 0
studies$deg_min_lon[studies$deg_min_lon == ""] <- 0
studies$min_min_lon[studies$min_min_lon == ""] <- 0
studies$sec_min_lon[studies$sec_min_lon == ""] <- 0

# conver lat and lon to decimal degrees

studies$lat <- as.numeric(studies$deg_max_lat) + as.numeric(studies$min_max_lat)/60 + as.numeric(studies$sec_max_lat)/3600
studies$lon <- as.numeric(studies$deg_max_lon) + as.numeric(studies$min_max_lon)/60 + as.numeric(studies$sec_max_lon)/3600

# remove old columns

studies <- studies %>% select(-c(max_lat, min_lat, max_lon, min_lon, deg_max_lat, min_max_lat, sec_max_lat, deg_min_lat, min_min_lat, sec_min_lat, deg_max_lon, min_max_lon, sec_max_lon, deg_min_lon, min_min_lon, sec_min_lon))

studies

# plot map

library(ggplot2)

world <- map_data("world")

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = studies, aes(x = lon, y = lat), color = "red", size = 3) +
  theme_bw() +
  theme(axis.title = element_blank()) +
  coord_fixed(ratio = 1.5) +
  labs(title = "Study locations") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("map.png", width = 10, height = 6, dpi = 300)
