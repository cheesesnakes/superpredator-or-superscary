# all studies

library(ggplot2, help, pos = 2, lib.loc = NULL)
library(tidyr, help, pos = 2, lib.loc = NULL)
library(dplyr, help, pos = 2, lib.loc = NULL)

# Load the data

fts_all <- read.csv("data/chapter-1_full-text-screening.csv")

## histogram of years

ggplot(fts_all, aes(x = year)) +
  geom_histogram(binwidth = 1, col = "black") +
  labs(x = "Year",
       y = "Number of studies") +
  theme_bw()+
  theme(text = element_text(size = 16))

ggsave("histogram_years.png", width = 8, height = 6)

# number of studies included for review

nrow(fts_all[fts_all$status == "include" | fts_all$status == "included",])

# number of studies excluded from review

nrow(fts_all[fts_all$status == "exclude",])

# inclusion rate

nrow(fts_all[fts_all$status == "include" | fts_all$status == "included",]) / nrow(fts_all)

# number of studies where labels contains "data"

fts_included <- fts_all[fts_all$status == "included",]

nrow(fts_included[grepl("data", fts_included$labels, ignore.case = TRUE),])

# number included in meta-analysis

data <- read.csv("data.csv")

length(unique(data$cite.key))

# number species

length(unique(data$pop_sn))

# number of studies across exposures

data <- read.csv("data_tc_smd.csv")

data_comp %>%
  group_by(exposure) %>%
  summarise(n = length(unique(cite.key)))

# number of studies across outcomes

data_comp %>%
  group_by(outcome) %>%
  summarise(n = length(unique(cite.key)))

# number of studies across trophic level

data_comp %>%
  group_by(trophic_level) %>%
  summarise(n = length(unique(cite.key)))
  