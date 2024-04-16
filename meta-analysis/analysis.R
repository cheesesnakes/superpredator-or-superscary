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

# add information about species

pop <- read.csv("./data/populations.csv")

data <- data %>%
    left_join(pop, by = "pop_cn")%>%
    rename(pop_sn = pop_sn.y)%>%
    mutate(pop_sn = ifelse(is.na(pop_sn), pop_sn.x, pop_sn))%>%
    select(-pop_sn.x)%>%
    mutate(trophic_level = ifelse(pop_sn == "Pyrrhocorax graculus", 1, trophic_level))

# make trophic level as factor

data <- data %>%
    mutate(trophic_level = as.factor(trophic_level))
# summaries --------------------------------------------

## total number of studies in meta analysis

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
#    filter(pop_cn !="")

# studies by type - table

table(data$study_type)

## number of datapoints per population, sorted

ggplot(data, aes(x = reorder(pop_sn, pop_sn, FUN = length))) +
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

ggplot(data = data_cor, aes(x = mean, y = reorder(pop_sn, mean), col = cite.key))+
    geom_point(size = 2)+
    geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    xlab("Effect size")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    legend.position = "none")+
    facet_grid(trophic_level~outcome, scales = "free")+
    # italicise x axis
    theme(axis.text.y = element_text(face = "italic"))

ggsave("es_cor.png", width = 12, height = 6, dpi = 300)

# Playback studies --------------------------------------------

data_pb <- data %>%
    filter(study_type == "playback")%>%
    select(cite.key, pop_cn, treatment, group, outcome, mean, mean.unit, var, var.unit, n, remarks)%>%
    #remove negative control
    filter(treatment != "negative control")%>%
    # rename postive control to control
    mutate(treatment = ifelse(treatment == "positive control", "control", treatment))%>%
    # rename treatment to treatment if not control
    mutate(treatment = ifelse(treatment != "control", "treatment", treatment))
 
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
    select(cite.key, pop_cn, group, treatment, outcome, mean, se, n, remarks)

# pivot data

data_pb <- data_pb%>%
    group_by(cite.key, pop_cn, outcome, remarks)%>%
    pivot_wider(names_from = treatment, values_from = c(mean, se, n))%>%
    ungroup()
# calculate standarised mean difference

data_pb <- data_pb%>%
    # correct types
    mutate(
        mean_treatment = as.numeric(as.character(mean_treatment)),
        mean_control = as.numeric(as.character(mean_control)),
        se_treatment = as.numeric(as.character(se_treatment)),
        se_control = as.numeric(as.character(se_control)),
        n_treatment = as.numeric(as.character(n_treatment)),
        n_control = as.numeric(as.character(n_control)))%>%
    mutate(
        smd = (mean_treatment - mean_control)/sqrt(se_treatment^2 + se_control^2),
        upper = smd + 1.96*sqrt(se_treatment^2 + se_control^2),
        lower = smd - 1.96*sqrt(se_treatment^2 + se_control^2))

# plot effect size and confidence intervals

ggplot(data = data_pb, aes(y = smd, x = reorder(group, smd), col = pop_cn))+
    geom_point(size = 2, position = position_dodge(preserve = "single"))+
    geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.05)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    labs(title = "Standardised mean difference and confidence intervals")+
    xlab("Standardised mean difference")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    legend.position = "top")+
    coord_flip()+
    facet_wrap(~outcome, scales = "free_x")+
    # italicise x axis
    theme(axis.text.y = element_text(face = "italic"))


ggsave("es_pb.png", width = 24, height = 10, dpi = 300)

# BACI studies --------------------------------------------

data_baci <- data %>%
    filter(study_type == "BACI")%>%
    select(cite.key, pop_cn, group, treatment, outcome, mean, mean.unit, var, var.unit, n, remarks)%>%
    # set mean, var, and n as numeric
    mutate(
        mean = as.numeric(mean),
        var = as.numeric(var),
        n = as.numeric(n))%>%
    # rename before treatment to control and after treatmen to treatment
    mutate(treatment = ifelse(treatment == "before treatment", "control", treatment),
    treatment = ifelse(treatment == "after treatment", "treatment", treatment))%>%
    # calculate standard error from var where var.unit = sd
    mutate(var = ifelse(var.unit == "sd", var/sqrt(n), var))

# spread mean and var, n for treatment and control

data_baci <- data_baci%>%
    group_by(cite.key, group, outcome)%>%
    pivot_wider(names_from = treatment, values_from = c(mean, var, n))%>%
    ungroup()

# calculate standarised mean difference

data_baci <- data_baci%>%
    # correct types
    mutate(
        mean_treatment = as.numeric(as.character(mean_treatment)),
        mean_control = as.numeric(as.character(mean_control)),
        var_treatment = as.numeric(as.character(var_treatment)),
        var_control = as.numeric(as.character(var_control)),
        n_treatment = as.numeric(as.character(n_treatment)),
        n_control = as.numeric(as.character(n_control)))%>%
    mutate(
        smd = (mean_treatment - mean_control)/sqrt(var_treatment^2 + var_control^2),
        upper = smd + 1.96*sqrt(var_treatment^2 + var_control^2),
        lower = smd - 1.96*sqrt(var_treatment^2 + var_control^2))

# Treatment - control studies --------------------------------------------

data_tc <- data %>%
    filter(study_type == "treatment-control")%>%
    select(cite.key, group, pop_cn, treatment, outcome, multiplier, mean, mean.unit, var, var.unit, n, remarks)%>%
    # make n numeric
    mutate(n = as.numeric(n))%>%
    mutate(treatment = ifelse(treatment == "control", "control", "treatment"))%>%
    # convert standard deviation to standard error
    mutate(var = ifelse(var.unit == "sd", as.numeric(var)/sqrt(n), var))

# filter studies that report confidence intervals

data_tc_ci <- data_tc%>%
    filter(var.unit == "ci")

# spread mean and var for treatment and control

data_tc <- data_tc%>%
    group_by(cite.key, group, pop_cn, outcome)%>%
    pivot_wider(names_from = treatment, values_from = c(mean, var, n))%>%
    ungroup()

#  calculate standarised mean difference

data_tc <- data_tc%>%
    # remove non - standard units (not se)
    filter(var.unit == "se" | var.unit == "sd")%>%
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
        lower = smd - se*1.96)%>%
    # correction for foraging rates
    mutate(smd = ifelse(!is.na(multiplier), smd*multiplier, smd),
    upper = ifelse(!is.na(multiplier), upper*multiplier, upper),
    lower = ifelse(!is.na(multiplier), lower*multiplier, lower))
    

# add studies from play back group = human

data_tc <- data_tc%>%
    bind_rows(data_pb%>%
    filter(group == "human" & outcome != "latency" )%>%
    select(cite.key, group, pop_cn, outcome, smd, upper, lower))

# add studies from baci

data_tc <- data_tc%>%
    bind_rows(data_baci%>%
    select(cite.key, group, pop_cn, outcome, smd, upper, lower))

# add species information

data_tc <-  data_tc %>%
    left_join(pop, by = "pop_cn")%>%
    mutate(trophic_level = ifelse(pop_sn == "Pyrrhocorax graculus", 1, trophic_level))

# plot effect size and confidence intervals

ggplot(data = data_tc, aes(x = smd, y = reorder(pop_sn, smd), col = cite.key))+
    geom_point(size = 2)+
    geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    xlab("Standardised mean difference")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    #remove legend
    legend.position = "none"
    )+
    facet_grid(trophic_level~outcome, scales = "free")+
    # italicise x axis
    theme(axis.text.y = element_text(face = "italic"))

ggsave("es_tc.png", width = 16, height = 9, dpi = 300)

# treatment - control studies that report confidence intervals

data_tc_ci <- data_tc_ci%>%
    # split var into upper and lower
    mutate(var = str_split(var, "/"),
    lower = as.numeric(sapply(var, function(x) x[1])),
    upper = as.numeric(sapply(var, function(x) x[2])))%>%
    # calculate uppper and lower when upper is NA
    mutate(lower = ifelse(is.na(upper), mean - lower, lower),
    upper = ifelse(is.na(upper), mean + (mean - lower), upper))

# add species information

data_tc_ci <-  data_tc_ci %>%
    left_join(pop, by = "pop_cn")%>%
    mutate(trophic_level = ifelse(pop_sn == "Pyrrhocorax graculus", 1, trophic_level))

# plot 

ggplot(data = data_tc_ci, aes(y = mean, x = treatment, col = cite.key))+
    geom_point(size = 2)+
    geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    xlab("Effect size")+
    ylab("Population")+
    theme_bw()+
    coord_flip()+
    theme(text = element_text(size = 20),
    legend.position = "none")+
    facet_grid(outcome~pop_sn, scales = "free_x")+
    # italicise facet column
    theme(strip.text.x = element_text(face = "italic"))
    

ggsave("es_tc_ci.png", width = 12, height = 4.5, dpi = 300)
