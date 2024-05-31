#!/home/cheesesnakes/.local/bin/radian

#setwd("./meta-analysis")

# required libraries

library(ggplot2)
library(stringr)

# import clean data

source("conversions.R", echo = FALSE)


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

ggsave("pop_sn.png", width = 10, height = 10, dpi = 300)

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

library(esc)

data_cor_smd <- data.frame(esc_B(study = data_cor$cite.key, b = data_cor$mean, sdy = data_cor$sd, grp1n = data_cor$n, grp2n = data_cor$n))

data_cor_smd <- cbind(data_cor, select(data_cor_smd, -c(study)))%>%
    select(cite.key, es, se, ci.lo, ci.hi, pop_cn, pop_sn, exposure, treatment, group, control, outcome, n, functional_group, trophic_level)%>%
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

ggsave("es_cor.png", width = 12, height = 6, dpi = 300)

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


ggsave("es_pb.png", width = 24, height = 10, dpi = 300)

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
    select(cite.key, group, pop_cn, exposure, treatment, outcome, multiplier, mean, mean.unit, var,  n, remarks)%>%
    # make n numeric
    mutate(n = as.numeric(n))%>%
    mutate(treatment = ifelse(treatment == "control", "control", "treatment"))%>%
    # convert standard deviation to standard error
    mutate(se = var/sqrt(n))

data_tc <- data_tc%>%
    select(cite.key, group, pop_cn, outcome, exposure, treatment, mean, mean.unit, multiplier, se, n)%>%
    distinct()%>%
    # set types
    mutate(mean =  as.numeric(mean),
            se = as.numeric(se),
            n = as.numeric(n))

# spread mean and var for treatment and control

data_tc <- data_tc%>%
    group_by(cite.key, pop_cn, group, exposure, outcome, mean.unit)%>%
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
    select(cite.key, es, se, ci.lo, ci.hi, pop_cn, group, exposure, outcome)%>%
    distinct(cite.key, es, .keep_all = TRUE)%>%
    rename(smd = es,
            upper = ci.hi,
            lower= ci.lo)

# add studies from play back group = human

data_tc_smd <- data_tc_smd%>%
    bind_rows(data_pb_smd%>%
    filter(group == "human" & outcome != "latency" )%>%
    select(cite.key, group, pop_cn, outcome, smd, se, upper, lower))

# add studies from baci

data_tc_smd <- data_tc_smd%>%
    bind_rows(data_baci_smd%>%
    select(cite.key, group, pop_cn, outcome, smd, se, upper, lower))

# add species information

data_tc_smd <-  data_tc_smd %>%
    left_join(pop, by = "pop_cn")%>%
    mutate(trophic_level = ifelse(pop_sn == "Pyrrhocorax graculus", 1, trophic_level))

data_tc_smd <- data_tc_smd%>%
    select(cite.key, smd, se, upper, lower, pop_cn, pop_sn, group, exposure, outcome, functional_group, trophic_level)

data_tc_smd$trophic_level <- as.factor(data_tc_smd$trophic_level)

# add observational studies

data_tc_smd <- full_join(data_tc_smd, data_cor_smd)

# rename movement metrics

data_tc_smd <- data_tc_smd%>%
    mutate(outcome = ifelse(outcome == "displacement" | outcome == "movement rate" | outcome == "home range",
                            "movement", outcome))

# fix giordano mouse

data_tc_smd <- data_tc_smd%>%
    mutate(smd = ifelse(pop_cn == "Mouse" & is.na(smd), 0, smd),
            upper = ifelse(pop_cn == "Mouse" & is.na(upper), 0, upper),
            lower = ifelse(pop_cn == "Mouse" & is.na(lower), 0, lower))

# make pop_sn capitalised

data_tc_smd <- data_tc_smd%>%
    mutate(pop_sn = str_to_title(pop_sn))

write.csv(data_tc_smd, "data_tc_smd.csv", row.names = F)


# Is the effect significantly different from 0 across outcomes?

library(meta)
library(purrr)


# map function metagen to data_tc_smd for each outcome

for (i in unique(data_tc_smd$outcome)) {
 
    data <- data_tc_smd %>%
        filter(outcome == i)

    stat <- metagen(
            TE = smd,
            seTE = se,
            data = data,
            studlab = cite.key,
            comb.fixed = FALSE,
            comb.random = TRUE,
            hakn = TRUE,
            method.tau = "DL",
            prediction = TRUE,
            sm = "SMD",
            title = i
        )

    print(stat)

    # save forest plot

    forest(stat, file = paste0("meta_", i, ".png"), width = 1000)
}

## using functions fro function.R

source("funcs.R", echo = FALSE)

stat_man <- data_tc_smd%>%
    mutate(weight = 1/se^2)%>%
    group_by(outcome)%>%
    filter(pop_cn != "Mouse")%>%
    mutate(Q = Q(weight, smd),
           df = df(n()),
           C = C(weight),
            T = T(Q, df, C),
            I_sq = I_sq(Q, df),
            corrected_wight = 1/((se^2)+ T) )%>%
    summarise(Q = last(Q),
              df = last(df),
              C = last(C),
              T = last(T),
              I_sq = I_sq(Q, df),
              var_E = 1/sum(corrected_wight),
              E = summary_effect(corrected_wight, smd),
              Z = Z(E, var_E),
              p = 2*pnorm(-abs(Z)),
              # prediction interval
                lower = E - 1.96*sqrt(var_E),
                upper = E + 1.96*sqrt(var_E))


print(stat_man)

# save comparison data

write.csv(stat_man, "stat_man.csv")

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

ggsave("es_tc.png", width = 24, height = 8, dpi = 300)