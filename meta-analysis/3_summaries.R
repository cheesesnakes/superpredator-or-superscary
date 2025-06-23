# required libraries

pacman::p_load(dplyr, tidyr, stringr, ggplot2, here, flextable)

# import clean data

here::i_am("meta-analysis/3_summaries.R")
source(here::here("meta-analysis/2_conversions.R"), echo = FALSE)

# view data

colnames(data)
head(data)
summary(data)

# add information about species

pop <- read.csv(here::here("meta-analysis/data/populations.csv"))

data <- data %>%
    left_join(pop, by = "pop_cn") %>%
    rename(pop_sn = pop_sn.y) %>%
    mutate(pop_sn = ifelse(is.na(pop_sn), pop_sn.x, pop_sn)) %>%
    select(-pop_sn.x) %>%
    mutate(
        trophic_level = ifelse(pop_sn == "Pyrrhocorax graculus", 1, trophic_level),
        pop_sn = str_to_sentence(pop_sn)
    )

# make trophic level as factor

data <- data %>%
    mutate(trophic_level = as.factor(trophic_level))

# expanding studies with multiple exposures

data <- data %>%
    mutate(exposure_category = str_replace_all(exposure_category, " \\+ ", ", ")) %>%
    separate_rows(exposure_category, sep = ", ")

# make interacitons interaction

data <- data %>%
    mutate(exposure_category = ifelse(str_trim(exposure_category) == "lethal interactions", "lethal interaction", exposure_category))

# summaries --------------------------------------------

## total number of studies in meta analysis

n_studies <- data %>%
    select(cite.key) %>%
    distinct() %>%
    nrow()

print(paste("Number of studies included in meta-analysis:", n_studies))

## removing FID and GUD studies due lack of data

data <- data %>%
    filter(outcome != "FID" & outcome != "GUD")
#    filter(pop_cn !="")

# studies by type - table

print("Number of studies by type")

data %>%
    distinct(cite.key, exposure_category, study_type) %>%
    group_by(exposure_category, study_type) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = study_type, values_from = n, values_fill = list(n = 0)) %>%
    print(.)

## studies by contrast type

print("Number of studies by contrast type")

data %>%
    distinct(cite.key, exposure_category, contrast_type) %>%
    group_by(exposure_category, contrast_type) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = contrast_type, values_from = n, values_fill = list(n = 0)) %>%
    print(.)

# number of datapoints per outcome

outcomes <- data %>%
    distinct(cite.key, exposure_category, outcomes) %>%
    # split outcomes at ' + '
    mutate(outcomes = str_split(outcomes, " \\+ ")) %>%
    unnest(outcomes) %>%
    # trim whitespace
    mutate(outcomes = str_trim(outcomes)) %>%
    # rename lethal interacitons to lethal interaciton
    group_by(exposure_category, outcomes) %>%
    summarise(n = n())

# plot

print("Number of datapoints per outcome")

ggplot(outcomes, aes(x = reorder(outcomes, n), y = n, fill = exposure_category)) +
    geom_col(col = "black") +
    labs(title = "Number of datapoints per outcome") +
    xlab("Outcome") +
    ylab("Number of datapoints") +
    coord_flip() +
    theme_bw() +
    theme(
        axis.text.y = element_text(face = "italic"),
        text = element_text(size = 20),
        legend.position = "top"
    ) +
    scale_fill_brewer(name = "Exposure Category", palette = "Set1")

ggsave(here("meta-analysis/figures/outcomes.png"), width = 10, height = 10, dpi = 300)


## number of datapoints per population, sorted
data %>%
    group_by(pop_sn) %>%
    summarise(
        n = length(unique(cite.key)),
        trophic_level = last(trophic_level)
    ) %>%
    arrange(desc(n)) %>%
    ggplot(aes(x = reorder(pop_sn, n), y = n, fill = trophic_level)) +
    geom_col(col = "black") +
    labs(title = "Number of datapoints per population") +
    xlab("Population") +
    ylab("Number of datapoints") +
    coord_flip() +
    theme_bw() +
    theme(
        axis.text.y = element_text(face = "italic"),
        text = element_text(size = 20),
        legend.position = "top"
    ) +
    scale_fill_brewer(name = "Trophic Level", palette = "Set1")

ggsave("meta-analysis/figures/populations.png", width = 10, height = 10, dpi = 300)

# Load the data

fts_all <- read.csv(here::here("meta-analysis/data/chapter-1_full-text-screening.csv"))

## histogram of years

ggplot(fts_all, aes(x = year)) +
    geom_histogram(binwidth = 1, col = "black") +
    labs(
        x = "Year",
        y = "Number of studies"
    ) +
    theme_bw() +
    theme(text = element_text(size = 16))

ggsave("meta-analysis/figures/histogram_years.png", width = 8, height = 6)

# number of studies included for review

print("Number of studies included for review")

print(nrow(fts_all[fts_all$status == "include" | fts_all$status == "included", ]))

# number of studies excluded from review

print("Number of studies excluded from review")

print(nrow(fts_all[fts_all$status == "exclude", ]))

# inclusion rate

print("Inclusion rate")

print(nrow(fts_all[fts_all$status == "include" | fts_all$status == "included", ]) / nrow(fts_all))

# number of studies where labels contains "data"

fts_included <- fts_all[fts_all$status == "included", ]

print("Number of studies where labels contains 'data'")

print(nrow(fts_included[grepl("data", fts_included$labels, ignore.case = TRUE), ]))

# number included in meta-analysis

data <- read.csv(here::here("meta-analysis/data/effect-size.csv"))

print("Number of studies included in meta-analysis")

print(length(unique(data$cite.key)))

# number species

print("Number of species")

print(length(unique(data$pop_sn)))


print("Number of datapoints")

# number of studies across exposures

data %>%
    group_by(exposure_category) %>%
    summarise(n = length(unique(cite.key))) %>%
    print(.)

# number of studies across outcomes

data %>%
    group_by(outcome) %>%
    summarise(n = length(unique(cite.key))) %>%
    print(.)

print(paste(
    "Number of studies with more than one type of outcome:",
    (data %>%
        group_by(cite.key) %>%
        summarise(n = length(unique(outcome))) %>%
        filter(n > 1) %>%
        nrow())
))

print("List of studies with more than one type of outcome")

data %>%
    group_by(cite.key) %>%
    summarise(n = length(unique(outcome))) %>%
    filter(n > 1) %>%
    select(cite.key)

# number of studies across trophic level

data %>%
    group_by(trophic_level) %>%
    summarise(n = length(unique(cite.key))) %>%
    print(.)

# table with pop_cn, pop_sn, trophic_level, functional_group and citations

source("meta-analysis/1-2_authors.R", chdir = TRUE)

table_a1 <- data %>%
    select(pop_cn, pop_sn, trophic_level, functional_group, cite.key) %>%
    distinct() %>%
    left_join(authors, by = "cite.key") %>%
    select(pop_cn, pop_sn, trophic_level, functional_group, cite) %>%
    mutate(trophic_level = ifelse(trophic_level == 1, "Consumer",
        ifelse(trophic_level == 2, "Primary Predator",
            ifelse(trophic_level == 3, "Secondary Predator", "Unknown")
        )
    )) %>%
    group_by(pop_cn) %>%
    summarise(
        pop_sn = first(pop_sn),
        trophic_level = first(trophic_level),
        functional_group = first(functional_group),
        cite = paste(cite, collapse = ", ")
    )



write.csv(table_a1, file = here::here("meta-analysis/output/table_a1.csv"), quote = TRUE)

# make flextable

table_a1_ft <- table_a1 %>%
    mutate(across(everything(), str_to_sentence)) %>%
    group_by(pop_sn) %>%
    summarise(
        pop_cn = paste(unique(pop_cn), collapse = ", "),
        trophic_level = first(trophic_level),
        functional_group = first(functional_group),
        cite = paste(unique(cite), collapse = ", ")
    ) %>%
    ungroup() %>%
    arrange(pop_sn) %>%
    flextable() %>%
    set_header_labels(
        pop_cn = "Common Name",
        pop_sn = "Specific Epithet",
        trophic_level = "Trophic Level",
        functional_group = "Functional Group",
        cite = "Citations"
    ) %>%
    # make Specific Epithet italic
    italic(j = "pop_sn") %>%
    set_table_properties(layout = "fixed", width = 0.95)

print(table_a1_ft)

save_as_docx(table_a1_ft, path = here::here("meta-analysis/output/table_a1.docx"))
