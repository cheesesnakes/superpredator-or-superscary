#setwd("./meta-analysis")

# required libraries

library(ggplot2)
library(stringr)

# import clean data

source("clearning.R")


# view data

colnames(data)
head(data)
summary(data)

# summaries --------------------------------------------

## number of studies

n_studies <- data %>% 
    select(cite.key) %>% 
    distinct() %>% 
    nrow()

n_studies

## number of datapoints per outcome

n_datapoints <- data %>% 
    group_by(outcome) %>% 
    summarise(n = n())

print(n_datapoints)

## removing FID and GUD studies due lack of data

data <- data %>% 
    filter(outcome != "FID" & outcome != "GUD")

# studies by type - table

table(data$study_type)

## number of datapoints per population, sorted

ggplot(data, aes(x = reorder(pop_cn, pop_cn, FUN = length))) +
    geom_bar() +
    labs(title = "Number of datapoints per population")+
    xlab("Population")+
    ylab("Number of datapoints")+
    coord_flip()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size = 20))

# Corelational studies --------------------------------------------

data_cor <- data %>%
    filter(study_type == "correlational")

## format var data for ci

data_cor <- data_cor %>%
    mutate(
        se = as.numeric(ifelse(var.unit == "se", var, NA)),
        var = str_split(var, "/"),
        upper = as.numeric(sapply(var, function(x) x[1])),
        lower = as.numeric(sapply(var, function(x) x[2])))%>%
    # upper and lower ci for se 
    mutate(
        upper = (ifelse(var.unit == "se", mean + se*1.96, upper)),
        lower = (ifelse(var.unit == "se", mean - se*1.96, lower)))%>%
    # replace NA multiplier with 1
    mutate(multiplier = ifelse(is.na(multiplier), 1, multiplier))%>%
    # apply multiplier to mean, upper and lower 
    mutate(
        mean = mean*multiplier,
        upper = upper*multiplier,
        lower = lower*multiplier)

# plot effect size and confidence intervals

ggplot(data = data_cor, aes(x = mean, y = reorder(pop_cn, mean), col = cite.key))+
    geom_point(size = 2)+
    geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    labs(title = "Effect size and confidence intervals")+
    xlab("Effect size")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    legend.position = "top")+
    facet_wrap(~outcome)

ggsave("effect_size_ci.png", width = 16, height = 10, dpi = 300)


# Treatment - control studies --------------------------------------------

data_tc <- data %>%
    filter(study_type == "treatment-control")%>%
    select(cite.key, pop_cn, treatment, outcome, mean, mean.unit, var, var.unit, n, remarks)%>%
    mutate(treatment = ifelse(treatment == "control", "control", "treatment"))

# spread mean and var for treatment and control

data_tc <- data_tc%>%
    group_by(cite.key, pop_cn, outcome, remarks)%>%
    pivot_wider(names_from = treatment, values_from = c(mean, var, n))%>%
    ungroup()

#  calculate standarised mean difference

data_tc <- data_tc%>%
    # correct types
    mutate(
        mean_treatment = as.numeric(as.character(mean_treatment)),
        mean_control = as.numeric(as.character(mean_control)),
        var_treatment = as.numeric(as.character(var_treatment)),
        var_control = as.numeric(as.character(var_control)),
        n_treatment = as.numeric(as.character(n_treatment)),
        n_control = as.numeric(as.character(n_control)))%>%
    mutate(
        smd = (mean_treatment - mean_control)/sqrt(((n_treatment - 1)*var_treatment*var_treatment + (n_control - 1)*var_control*var_control)/(n_treatment + n_control - 2)),
        se = sqrt((n_treatment + n_control)/(n_treatment*n_control) + smd^2/(2*(n_treatment + n_control - 2))),
        upper = smd + se*1.96,
        lower = smd - se*1.96)

# plot effect size and confidence intervals

ggplot(data = data_tc, aes(x = smd, y = reorder(pop_cn, smd), col = cite.key))+
    geom_point(size = 2)+
    geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    labs(title = "Standardised mean difference and confidence intervals")+
    xlab("Standardised mean difference")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    legend.position = "top")+
    facet_wrap(~outcome)

ggsave("smd_ci.png", width = 16, height = 10, dpi = 300)

# Playback studies --------------------------------------------

data_pb <- data %>%
    filter(study_type == "playback")%>%
    select(cite.key, pop_cn, treatment, outcome, mean, mean.unit, var, var.unit, n, remarks)
+
# format var data for ci

data_pb <- data_pb %>%
    mutate(
        se = as.numeric(ifelse(var.unit == "se", var, NA)),
        var = str_split(var, "/"),
        upper = as.numeric(sapply(var, function(x) x[1])),
        lower = as.numeric(sapply(var, function(x) x[2])))%>%
    # convert ci to se 
    mutate(
        upper = (ifelse(var.unit == "se", se, (upper - mean)/1.96)),
        lower = (ifelse(var.unit == "se", se, (lower-mean)/1.96)))%>%
    # take mean of lower and upper
    mutate(se = abs(upper + lower)/2)%>%
    select(cite.key, pop_cn, treatment, outcome, mean, se, n, remarks)


# pivot data

data_pb <- data_pb%>%
    group_by(cite.key, pop_cn, outcome, remarks)%>%
    pivot_wider(names_from = treatment, values_from = c(mean, se, n))%>%
    ungroup()

# BACI studies --------------------------------------------

data_baci <- data %>%
    filter(study_type == "BACI")%>%
    select(cite.key, pop_cn, treatment, outcome, mean, mean.unit, var, var.unit, n, remarks)
