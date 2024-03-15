#setwd("./meta-analysis")

# required libraries

library(dplyr)
library(tidyr)


# import data

meta <- read.csv("./data/chapter-1_meta.csv")
effect <- read.csv("./data/chapter-1_effects.csv")

# view data

head(meta)
summary(meta)
colnames(meta)

head(effect)
summary(effect)
colnames(effect)

# clean data

## rename columns
meta <- meta %>% rename(cite.key = File52) 

## join meta and effect

data <- meta %>% left_join(effect, by = "cite.key")

## set colnames to lower case

colnames(data) <- tolower(colnames(data))

# merge pop_cn.x and pop_cn.y, where pop_cn.y is NA

data <- data%>%
    mutate(pop_cn = ifelse(pop_cn.y == "", pop_cn.x, pop_cn.y),
    pop_cn =  ifelse(pop_cn.x == "Multispecies", pop_cn.y, pop_cn.x)) %>%
    select(-pop_cn.x, -pop_cn.y)

# select outcome from effect onluy

data <- data %>% select(-outcome.x)%>%
    rename(outcome = outcome.y)

head(data)
summary(data)
colnames(data)

data <-
    data %>%
    select(cite.key, study_type, sampling, sampling_time, pop_cn, pop_sn, exposure, control, outcome, treatment,
    mean, mean.unit, var, var.unit, n, remarks)

# rename outcome feeding to foragin

data <- data %>%
    mutate(outcome = ifelse(outcome == "feeding", "foraging", outcome))

# print and then remove NA outcome

(data %>%
    filter(is.na(outcome))
)$cite.key #verify and fix <----------------------------------------------

data <- data %>%
    filter(!is.na(outcome))

# fixing bite-rate entries

data[data$outcome == "bite-rate",]$mean.unit = "bites"
data[data$outcome == "bite-rate",]$outcome = "foraging"