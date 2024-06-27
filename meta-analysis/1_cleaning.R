#setwd("./meta-analysis")

# required libraries

pacman::p_load(dplyr, tidyr, stringr)

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
    mutate(pop_cn.y = as.character(pop_cn.y))%>%
    mutate(pop_cn = ifelse(pop_cn.y != "" | !is.na(pop_cn.y), pop_cn.y, pop_cn.x),
    pop_cn =  ifelse(pop_cn.x == "Multispecies", pop_cn.y, pop_cn.x)) %>%
    select(-pop_cn.x, -pop_cn.y)%>%
    # str_trim
    mutate(cite.key = str_trim(cite.key),
            group = str_trim(group),
            pop_cn = str_trim(pop_cn),
            mean.unit = str_trim(mean.unit))

# select outcome from effects data

data <- data %>% select(-outcome.x)%>%
    rename(outcome = outcome.y)%>%
    mutate(outcome = str_trim(outcome))

head(data)
summary(data)
colnames(data)

# select relevant columns

data <-
    data %>%
    select(cite.key, study_type, sampling, sampling_time, pop_cn, pop_sn, exposure, control, outcome, treatment,
    group, mean, scale, mean.unit, var, lower, upper, var.unit, multiplier, n, remarks)

# rename outcome: feeding to foraging

data <- data %>%
    mutate(outcome = ifelse(outcome == "feeding", "foraging", outcome))

## total number of studies in meta analysis

n_studies <- data %>% 
    select(cite.key) %>% 
    distinct() %>% 
    nrow()

n_studies

# print and then remove NA outcome

(data %>%
    filter(is.na(outcome))
)$cite.key #reviews and studies without data or excluded from meta analysis


data <- data %>%
    filter(!is.na(outcome))

# fixing bite-rate entries

data[data$outcome == "bite-rate",]$mean.unit = "bites"
data[data$outcome == "bite-rate",]$outcome = "foraging"

# remove leading \ from var

data <- data %>%
    mutate(var = gsub("\\\\", "", var))

# format pop_cn as a sentence

data <- data %>%
    mutate(pop_cn = str_to_title(pop_cn))