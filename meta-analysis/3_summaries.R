#!/home/cheesesnakes/.local/bin/radian

#setwd("./meta-analysis")

# required libraries

pacman::p_load(dplyr, tidyr, stringr, ggplot2)

# import clean data

source("2_conversions.R", echo = FALSE)

# view data

colnames(data)
head(data)
summary(data)

# add information about species

pop <- read.csv("./data/populations.csv")

data <- data %>%
    left_join(pop, by = "pop_cn")%>%
    rename(pop_sn = pop_sn.y)%>%
    mutate(pop_sn = ifelse(is.na(pop_sn), pop_sn.x, pop_sn))%>%
    select(-pop_sn.x)%>%
    mutate(trophic_level = ifelse(pop_sn == "Pyrrhocorax graculus", 1, trophic_level),
    pop_sn = str_to_sentence(pop_sn))

# make trophic level as factor

data <- data %>%
    mutate(trophic_level = as.factor(trophic_level))

# summaries --------------------------------------------

## total number of studies in meta analysis

n_studies <- data %>% 
    select(cite.key) %>% 
    distinct() %>% 
    nrow()

print(paste("Number of studies included in meta-analysis:",n_studies))

## number of datapoints per outcome

outcomes <- data$outcomes

## split outcome by ' + '

outcomes <- str_split(outcomes, " \\+ ")

outcomes <- str_split(outcomes, "/")

outcomes <- str_split(outcomes, ",")

## trim

outcomes <- lapply(outcomes, str_trim)

## unlist

outcomes <- unlist(outcomes)

## rename

## change AD to alert distance

outcomes <- lapply(outcomes, function(x) {
    x <- lapply(x, function(y) {
        if(y == "AD") {
            y <- "Alert Distance"
        }
        return(y)
    })
    return(x)
})

## Change ED to escape distance

outcomes <- lapply(outcomes, function(x) {
    x <- lapply(x, function(y) {
        if(y == "ED") {
            y <- "Escape Distance"
        }
        return(y)
    })
    return(x)
})

## change feeding rate and feeding time to foraging rate and foraging time

outcomes <- lapply(outcomes, function(x) {
    x <- lapply(x, function(y) {
        if(y == "feeding rate") {
            y <- "foraging rate"
        }
        return(y)
    })
    return(x)
})

## change foraging frequency to foraging rate

outcomes <- lapply(outcomes, function(x) {
    x <- lapply(x, function(y) {
        if(y == "foraging frequency") {
            y <- "foraging rate"
        }
        return(y)
    })
    return(x)
})

## Change GUD to giving up density

outcomes <- lapply(outcomes, function(x) {
    x <- lapply(x, function(y) {
        if(y == "GUD") {
            y <- "Giving Up Density"
        }
        return(y)
    })
    return(x)
})

## change prob. Flight to probability of flight

outcomes <- lapply(outcomes, function(x) {
    x <- lapply(x, function(y) {
        if(y == "prob. Flight") {
            y <- "Probability of Flight"
        }
        return(y)
    })
    return(x)
})

## change foraging to foraging rate

outcomes <- lapply(outcomes, function(x) {
    x <- lapply(x, function(y) {
        if(y == "foraging") {
            y <- "foraging rate"
        }
        return(y)
    })
    return(x)
})

## change vigilance to vigilance rate

outcomes <- lapply(outcomes, function(x) {
    x <- lapply(x, function(y) {
        if(y == "vigilance") {
            y <- "vigilance rate"
        }
        return(y)
    })
    return(x)
})

# count

n_datapoints <- table(outcomes)

print("Number of datapoints per outcome")

print(n_datapoints)

## removing FID and GUD studies due lack of data

data <- data %>% 
    filter(outcome != "FID" & outcome != "GUD")
#    filter(pop_cn !="")

# studies by type - table

print("Number of studies by type")

print(table(data$study_type))

## studies by contrast type

print("Number of studies by contrast type")

print(table(data$contrast))

## number of datapoints per population, sorted
data%>%
    group_by(pop_sn)%>%
    summarise(n = length(unique(cite.key)),
            trophic_level = last(trophic_level))%>%
    arrange(desc(n))%>%
    ggplot(aes(x = reorder(pop_sn, n), y = n, fill = trophic_level)) +
    geom_col(col = "black") +
    labs(title = "Number of datapoints per population")+
    xlab("Population")+
    ylab("Number of datapoints")+
    coord_flip()+
    theme_bw()+
    theme(axis.text.y = element_text(face = "italic"),
    text = element_text(size = 20),
    legend.position = "top")+
    scale_fill_brewer(name = "Trophic Level", palette = "Set1")

ggsave("figures/populations.png", width = 10, height = 10, dpi = 300)

# Load the data

fts_all <- read.csv("data/chapter-1_full-text-screening.csv")

## histogram of years

ggplot(fts_all, aes(x = year)) +
  geom_histogram(binwidth = 1, col = "black") +
  labs(x = "Year",
       y = "Number of studies") +
  theme_bw()+
  theme(text = element_text(size = 16))

ggsave("figures/histogram_years.png", width = 8, height = 6)

# number of studies included for review

print("Number of studies included for review")

print(nrow(fts_all[fts_all$status == "include" | fts_all$status == "included",]))

# number of studies excluded from review

print("Number of studies excluded from review")

print(nrow(fts_all[fts_all$status == "exclude",]))

# inclusion rate

print("Inclusion rate")

print(nrow(fts_all[fts_all$status == "include" | fts_all$status == "included",]) / nrow(fts_all))

# number of studies where labels contains "data"

fts_included <- fts_all[fts_all$status == "included",]

print("Number of studies where labels contains 'data'")

print(nrow(fts_included[grepl("data", fts_included$labels, ignore.case = TRUE),]))

# number included in meta-analysis

data <- read.csv("data/effect-size.csv")

print("Number of studies included in meta-analysis")

print(length(unique(data$cite.key)))

# number species

print("Number of species")

print(length(unique(data$pop_sn)))


print("Number of datapoints")

# number of studies across exposures

data %>%
  group_by(exposure) %>%
  summarise(n = length(unique(cite.key)))%>%
  print(.)

# number of studies across outcomes

data %>%
  group_by(outcome) %>%
  summarise(n = length(unique(cite.key)))%>%
  print(.)

# number of studies across trophic level

data%>%
  group_by(trophic_level) %>%
  summarise(n = length(unique(cite.key)))%>%
  print(.)
  

