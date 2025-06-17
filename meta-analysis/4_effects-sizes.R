#!/home/cheesesnakes/.local/bin/radian

#setwd("./meta-analysis")

# required libraries

pacman::p_load(dplyr, tidyr, stringr, ggplot2, esc, here)

# import clean data

source(here::here("meta-analysis/2_conversions.R"), echo = FALSE)
source(here::here("meta-analysis/1-2_authors.R"), chdir = TRUE)

# add information about species

pop <- read.csv(here::here("meta-analysis/data/populations.csv"))

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
    
# Corelational studies --------------------------------------------

data_cor <- data %>%
    filter(study_type == "correlational" & mean.unit == "coefficient" | mean.unit == "odds")

## format var data for ci

data_cor <- data_cor %>%
    mutate(sd = var,
        upper = sd*1.96 + mean,
        lower = mean - sd*1.96)%>%
    # replace NA multiplier with 1
    mutate(multiplier = ifelse(is.na(multiplier), 1, multiplier))%>%
    # apply multiplier to mean, upper and lower 
    mutate(
        mean = mean*multiplier,
        upper = upper*multiplier,
        lower = lower*multiplier)

# convert regression coefficient to smd

data_cor_smd <- data.frame(esc_B(study = data_cor$cite.key, b = data_cor$mean, sdy = data_cor$sd, grp1n = data_cor$n, grp2n = data_cor$n))

data_cor_smd <- cbind(data_cor, select(data_cor_smd, -c(study)))%>%
    select(cite.key, es, se, ci.lo, ci.hi, pop_cn, pop_sn, exposure_category, treatment, group, controls, outcome, n, functional_group, trophic_level)%>%
    distinct(cite.key, es, .keep_all = TRUE)%>%
    rename(smd = es,
            upper = ci.hi,
            lower= ci.lo)

# plot effect size and confidence intervals

ggplot(data = data_cor, aes(x = mean, y = reorder(pop_sn, mean), col = trophic_level))+
    geom_point(size = 2)+
    geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    xlab("Effect size")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    legend.position = "none")+
    facet_wrap(~outcome, scales = "free_x")+
    # italicise x axis
    theme(axis.text.y = element_text(face = "italic"))

ggsave(here::here("meta-analysis/figures/es_cor.png"), width = 12, height = 6, dpi = 300)

# Playback studies --------------------------------------------

data_pb <- data %>%
    filter(study_type == "playback")%>%
    select(cite.key, pop_cn, treatment, group, outcome, mean, var, n, remarks)%>%
    # rename postive control to control
    mutate(treatment = ifelse(treatment == "positive control" | treatment == "negative control" , "control", treatment))%>%
    # rename treatment to treatment if not control
    mutate(treatment = ifelse(treatment != "control", "treatment", treatment))
 
# format var data for ci

data_pb <- data_pb %>%
    mutate(se = var/sqrt(as.numeric(n)))%>%
    select(cite.key, pop_cn, group, treatment, outcome, mean, se, n, remarks)

# pivot data

data_pb <- data_pb%>%
    group_by(cite.key, pop_cn, outcome, remarks)%>%
    pivot_wider(names_from = treatment, values_from = c(mean, se, n))%>%
    ungroup()

# calculate standarised mean difference control:treatment

data_pb_smd <- data.frame(
esc_mean_se(
  grp1m = data_pb$mean_treatment,
  grp1se = data_pb$se_treatment,
  grp1n = data_pb$n_treatment,
  grp2m = data_pb$mean_control,
  grp2se = data_pb$se_control,
  grp2n  = data_pb$n_control,
  es.type = "d",
  study = data_pb$cite.key
)
)

# add covariates from data_pb and rename vars for join

data_pb_smd <- cbind(data_pb, select(data_pb_smd, -c(study)))%>%
    select(cite.key, es, se, ci.lo, ci.hi, pop_cn, group, outcome)%>%
    distinct(cite.key, es, .keep_all = TRUE)%>%
    rename(smd = es,
            upper = ci.hi,
            lower= ci.lo)

data_pb_smd$outcome <- as.factor(data_pb$outcome)

data_pb_smd$group <- as.factor(data_pb$group)

# plot effect size and confidence intervals

ggplot(data = data_pb_smd, aes(y = smd, x = reorder(group, smd), col = pop_cn))+
    geom_point(size = 2)+
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


ggsave(here::here("meta-analysis/figures/es_pb.png"), width = 24, height = 10, dpi = 300)

# BACI studies --------------------------------------------

data_baci <- data %>%
    filter(study_type == "BACI")%>%
    select(cite.key, pop_cn, group, treatment, outcome, mean, var,  n, remarks)%>%
    # set mean, var, and n as numeric
    mutate(
        mean = as.numeric(mean),
        var = as.numeric(var)^2,
        n = as.numeric(n))%>%
    # rename before treatment to control and after treatmen to treatment
    mutate(treatment = ifelse(treatment == "before treatment", "control", treatment),
    treatment = ifelse(treatment == "after treatment" | treatment == "active disturbance" , "treatment", treatment))%>%
    # calculate standard error from var where var.unit = sd
    mutate(se = sqrt(var/n))%>%
    select(-c(var))

# spread mean and var, n for treatment and control

data_baci <- data_baci%>%
    group_by(cite.key, group, outcome)%>%
    mutate(treatment = ifelse(treatment == "control", "control", "treatment"))%>%
    pivot_wider(names_from = treatment, values_from = c(mean, se, n))%>%
    ungroup()

# calculate standarised mean difference

data_baci_smd <- data.frame(
esc_mean_se(
  grp1m = data_baci$mean_treatment,
  grp1se = data_baci$se_treatment,
  grp1n = data_baci$n_treatment,
  grp2m = data_baci$mean_control,
  grp2se = data_baci$se_control,
  grp2n  = data_baci$n_control,
  es.type = "d",
  study = data_baci$cite.key
)
)

# add covariates from data_baci and rename vars for join

data_baci_smd <- cbind(data_baci, select(data_baci_smd, -c(study)))%>%
    select(cite.key, es, se, ci.lo, ci.hi, pop_cn, group, outcome)%>%
    distinct(cite.key, es, .keep_all = TRUE)%>%
    rename(smd = es,
            upper = ci.hi,
            lower= ci.lo)

# Treatment - control studies --------------------------------------------

data_tc <- data %>%
    filter(study_type == "treatment-control"| study_type == "correlational")%>%
    filter(mean.unit != "odds" & mean.unit != "coefficient")%>%
    select(cite.key, group, pop_cn, exposure_category, treatment, outcome, multiplier, mean, mean.unit, var,  n, remarks)%>%
    # make n numeric
    mutate(n = as.numeric(n))%>%
    mutate(treatment = ifelse(treatment == "control", "control", "treatment"))%>%
    # convert standard deviation to standard error
    mutate(se = var/sqrt(n))

data_tc <- data_tc%>%
    select(cite.key, group, pop_cn, outcome, exposure_category, treatment, mean, mean.unit, multiplier, se, n)%>%
    distinct()%>%
    # set types
    mutate(mean =  as.numeric(mean),
            se = as.numeric(se),
            n = as.numeric(n))

# spread mean and var for treatment and control

data_tc <- data_tc%>%
    group_by(cite.key, pop_cn, group, exposure_category, outcome, mean.unit)%>%
    pivot_wider(names_from = treatment, values_from = c(mean, se, n))%>%
    ungroup()%>%
    #correction for foraging rates
    mutate(
        mean_treatment = ifelse(!is.na(multiplier), mean_treatment*multiplier, mean_treatment),
        mean_control = ifelse(!is.na(multiplier), mean_control*multiplier, mean_control),
        se_treatment = ifelse(!is.na(multiplier), se_treatment*multiplier, se_treatment),
        se_control = ifelse(!is.na(multiplier), se_control*multiplier, se_control))

#  calculate standarised mean difference

data_tc_smd <- data.frame(
esc_mean_se(
  grp1m = data_tc$mean_treatment,
  grp1se = data_tc$se_treatment,
  grp1n = data_tc$n_treatment,
  grp2m = data_tc$mean_control,
  grp2se = data_tc$se_control,
  grp2n  = data_tc$n_control,
  es.type = "d",
  study = data_tc$cite.key
)
)

# add covariates from data_tc and rename vars for join

data_tc_smd <- cbind(data_tc, select(data_tc_smd, -c(study)))%>%
    select(cite.key, es, se, ci.lo, ci.hi, pop_cn, group, exposure_category, outcome)%>%
    distinct(cite.key, es, .keep_all = TRUE)%>%
    rename(smd = es,
            upper = ci.hi,
            lower= ci.lo)

length(unique(data_tc_smd$cite.key))


# add studies from play back group = human

data_tc_smd <- data_tc_smd%>%
    bind_rows(data_pb_smd%>%
    filter(group == "human" & outcome != "latency" )%>%
    select(cite.key, group, pop_cn, outcome, smd, se, upper, lower))

length(unique(data_tc_smd$cite.key))

# add studies from baci

data_tc_smd <- data_tc_smd%>%
    bind_rows(data_baci_smd%>%
    select(cite.key, group, pop_cn, outcome, smd, se, upper, lower))

length(unique(data_tc_smd$cite.key))

# add species information

data_tc_smd <-  data_tc_smd %>%
    left_join(pop, by = "pop_cn")%>%
    mutate(trophic_level = ifelse(pop_sn == "Pyrrhocorax graculus", 1, trophic_level))

data_tc_smd <- data_tc_smd%>%
    select(cite.key, smd, se, upper, lower, pop_cn, pop_sn, group, exposure_category, outcome, functional_group, trophic_level)

data_tc_smd$trophic_level <- as.factor(data_tc_smd$trophic_level)

# add observational studies

data_tc_smd <- full_join(data_tc_smd, data_cor_smd)

length(unique(data_tc_smd$cite.key))


# fix giordano mouse

data_tc_smd <- data_tc_smd%>%
    mutate(smd = ifelse(pop_cn == "Mouse" & is.na(smd), 0, smd),
            upper = ifelse(pop_cn == "Mouse" & is.na(upper), 0, upper),
            lower = ifelse(pop_cn == "Mouse" & is.na(lower), 0, lower))


# make pop_sn capitalised

data_tc_smd <- data_tc_smd%>%
    mutate(pop_sn = str_to_title(pop_sn))

# add citations from authors

data_tc_smd <- data_tc_smd%>%
    left_join(authors, by = "cite.key")%>%
    mutate(cite = ifelse(is.na(cite), cite.key, cite))

# add meta - data

meta <- meta %>%
    mutate(cite.key = as.character(cite.key)) 

data_comp <- data_tc_smd%>%
    left_join(meta, by = c("cite.key"))%>%
    rename(pop_cn = pop_cn.x,
           exposure_category = exposure_category.y)%>%           
    select(cite.key, group, exposure_category, pop_cn, outcome, smd, se, upper, lower)

length(unique(data_comp$cite.key))

# add species data

data_comp <- data_comp%>%
    left_join(pop, by = c("pop_cn"))%>%
    mutate(trophic_level = as.factor(trophic_level))


# set order as hunting, active disturbance, passive disturbance

data_comp <- data_comp%>%
    mutate(exposure_category = factor(str_to_lower(exposure_category), levels = c("lethal interaction", "active interaction", "passive interaction")))%>%
    # capilatise first letter
    mutate(exposure_category = str_to_title(exposure_category))

length(unique(data_comp$cite.key))

# add citation from authors

data_comp <- data_comp%>%
    left_join(authors, by = "cite.key")%>%
    mutate(cite = ifelse(is.na(cite), cite.key, cite))%>%
    # capitalise first letter of outcome
    mutate(outcome = str_to_title(outcome))

length(unique(data_comp$cite.key))

# add treament

treatment <- data%>%
    select(cite.key, treatment)%>%
    distinct()%>%
    # remove entries containing control
    filter(!grepl("control", treatment))%>%
    # join with data_comp where exposure_category is NA
    left_join(data_comp%>%
    filter(is.na(exposure_category)), by = "cite.key")%>%
    filter(!is.na(smd))%>%
    mutate(treatment = ifelse(group == "Treatment_1", "active interaction", ifelse(group == "Treatment_2", "lethal interaction", treatment)))%>%
    distinct()%>%
    mutate(exposure_category = str_to_title(treatment))%>%
    select(-treatment)

treatment

# drop na and bind

data_comp <- data_comp%>%
    filter(!is.na(exposure_category))%>%
    bind_rows(treatment)

# add movement

data_comp <- data_comp%>%
    mutate(outcome = ifelse(outcome == "Foraging" | outcome == "Vigilance", outcome, "Movement"))

# fix hunting exposure category

data_comp <- data_comp%>%
    mutate(exposure_category = ifelse(exposure_category == "Hunting", "Lethal Interaction", exposure_category))%>%
    mutate(exposure_category = ifelse(exposure_category == "Passive Disturbance", "Passive Interaction", exposure_category))

write.csv(data_comp, here::here("meta-analysis/data/effect-size.csv"), row.names = F)

# plotting with type of interactions

data_comp %>%
    mutate(exposure_category = factor(exposure_category, levels = c("Lethal Interaction", "Active Interaction", "Passive Interaction")))%>%
    mutate(outcome = factor(outcome, levels = c("Foraging", "Vigilance", "Movement")))%>%
    # rename trophic levels
    mutate(trophic_level = ifelse(trophic_level == 1, "Consumer", ifelse(trophic_level == 2, "Primary Predator", "Secondary Predator")),
    trophic_level = factor(trophic_level, levels = c("Secondary Predator", "Primary Predator", "Consumer")))%>%
    filter(outcome != "latency" & !is.na(pop_cn) & !is.na(trophic_level))%>%
    ggplot(aes(x = reorder(pop_cn, smd), y = smd, col = trophic_level))+
    geom_point(size = 2)+
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    labs(x = "Species", y = "Standardised mean difference")+
    facet_grid(exposure_category~outcome, scales = "free")+
    # order strip text
    theme_bw()+
    theme(legend.position = "top")+
    coord_flip()+
    # remove shape legend
    guides(shape = "none")+
    # set legend title
    scale_color_brewer(name = "Trophic level", palette = "Set1")+
    # italicise x-axis labels
    theme(axis.text.y = element_text(face = "italic"),
        text = element_text(size = 20))

ggsave(here::here("meta-analysis/figures/fig-2_1.png"), width = 12, height = 10, dpi = 300)

# plot effect size and confidence intervals

ggplot(data = data_tc_smd, aes(x = smd, y = reorder(pop_sn, smd), col = trophic_level))+
    geom_point(size = 2, position = position_dodge(width = 0.1))+
    geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
#    geom_point(data = stat_man, aes(y = 0.5, x = E), col = "red", size = 2, shape = 5)+
 #   geom_errorbarh(data = stat_man, aes(xmax = upper, xmin = lower, x = E, y = 0.5), col = "red", height = 0.1, size = 1)+
    xlab("Standardised mean difference (± 95% CI)")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    legend.position = "top"
    )+
    facet_wrap(~outcome, scales = "free")+
    # italicise x axis
    theme(axis.text.y = element_text(face = "italic"))+
    scale_color_brewer(name = "Trophic Level", palette = "Set1")

ggsave(here::here("meta-analysis/figures/fig-2.png"), width = 24, height = 8, dpi = 300)

# count positive and negative studies which conf int don't overlap with zero

data_comp%>%
select(exposure_category, outcome, smd, upper, lower)%>%
mutate(sign = ifelse(smd > 0, "positive", "negative"))%>%
group_by(exposure_category, outcome, sign)%>%
summarise(n = sum(ifelse(sign == "positive", lower > 0, upper < 0)))%>%
pivot_wider(names_from = exposure_category, values_from = n)%>%
print()

# count number of insig interactions

data_comp%>%
select(exposure_category, outcome, smd, upper, lower)%>%
mutate(sign = ifelse(smd > 0, "positive", "negative"))%>%
group_by(exposure_category, outcome)%>%
summarise(n = sum(ifelse(sign == "positive", lower < 0, upper > 0)))%>%
pivot_wider(names_from = exposure_category, values_from = n)%>%
print()

