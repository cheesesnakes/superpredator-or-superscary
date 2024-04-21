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
    mutate(trophic_level = ifelse(pop_sn == "Pyrrhocorax graculus", 1, trophic_level))%>%
    filter(var.unit != "credible interval" )%>%
    filter(var.unit != "interquantile")
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
    filter(study_type == "correlational" & mean.unit == "coefficient" | mean.unit == "odds")

## format var data for ci

data_cor <- data_cor %>%
    mutate(var = str_split(var, "/"),
        upper = as.numeric(sapply(var, function(x) x[1])),
        lower = as.numeric(sapply(var, function(x) x[2])))%>%
    # calulate lower for se and sd
    mutate(lower = ifelse(var.unit == "se", -(1.96*upper)+mean, 
                            ifelse(var.unit == "sd", -(1.96*upper/sqrt(n))+mean, lower)),
            upper = ifelse(var.unit == "se", (1.96*upper)+mean, 
                            ifelse(var.unit == "sd", (1.96*upper/sqrt(n))+mean, upper)))%>%
    # convert vars to standard dev
    mutate(sd = ifelse(var.unit == "se", as.numeric(upper)*sqrt(n), 
                    ifelse(var.unit == "sd", as.numeric(upper), 
                            ifelse(var.unit == "ci", upper*sqrt(n)/1.96, NA))))%>%
    # replace NA multiplier with 1
    mutate(multiplier = ifelse(is.na(multiplier), 1, multiplier))%>%
    # apply multiplier to mean, upper and lower 
    mutate(
        mean = mean*multiplier,
        upper = upper*multiplier,
        lower = lower*multiplier)

# convert regression coefficient to smd

library(esc)

data_cor_smd <- data.frame(esc_B(study = data_cor$cite.key, b = data_cor$mean, sdy = data_cor$sd, grp1n = data_cor$n, grp2n = data_cor$n)
)

data_cor_smd <- data_cor_smd%>%
    rename(cite.key = study)%>%
    left_join(data_cor, by = c("cite.key"))%>%
    select(cite.key, es, se, ci.lo, ci.hi, pop_cn, pop_sn, exposure, treatment, group, control, outcome, n, functional_group, trophic_level)%>%
    distinct(cite.key, es, group, outcome, .keep_all = TRUE)%>%
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
        lower = smd - 1.96*sqrt(se_treatment^2 + se_control^2),
        se = sqrt(se_treatment^2 + se_control^2))

# plot effect size and confidence intervals

ggplot(data = data_pb, aes(y = smd, x = reorder(group, smd), col = pop_cn))+
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
        lower = smd - 1.96*sqrt(var_treatment^2 + var_control^2),
        se = sqrt(var_treatment^2 + var_control^2))

# Treatment - control studies --------------------------------------------

data_tc <- data %>%
    filter(study_type == "treatment-control" | study_type == "correlational" & mean.unit != "coefficient")%>%
    select(cite.key, group, pop_cn, exposure, treatment, outcome, multiplier, mean, mean.unit, var, var.unit, n, remarks)%>%
    # make n numeric
    mutate(n = as.numeric(n))%>%
    mutate(treatment = ifelse(treatment == "control", "control", "treatment"))%>%
    # split var into upper and lower
    mutate(var = str_split(var, "/"))%>%
    # convert standard deviation to standard error
    mutate(se = ifelse(var.unit == "sd", 
                        as.numeric(sapply(var, function(x) x[1]))/sqrt(n), 
                            ifelse(var.unit == "ci", 
                                    abs(as.numeric(sapply(var, function(x) x[1])) - mean )/1.96,  
                                        as.numeric(sapply(var, function(x) x[1])))))

data_tc <- data_tc%>%
    filter(var.unit != "range")%>%
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
    ungroup()

#  calculate standarised mean difference

data_tc <- data_tc%>%
    # remove non - standard units (not se)
    # correct types
    mutate(
        mean_treatment = as.numeric(as.character(mean_treatment)),
        mean_control = as.numeric(as.character(mean_control)),
        var_treatment = se_treatment,
        var_control = se_control,
        n_treatment = as.numeric(as.character(n_treatment)),
        n_control = as.numeric(as.character(n_control)))

data_tc <- data_tc%>%
    mutate(
        smd = (mean_treatment - mean_control)/sqrt(((n_treatment - 1)*var_treatment*var_treatment + (n_control - 1)*var_control*var_control)/(n_treatment + n_control - 2)),
        se = sqrt((n_treatment + n_control)/(n_treatment*n_control) + smd^2/(2*(n_treatment + n_control - 2))),
        upper = smd + se*1.96,
        lower = smd - se*1.96,
        var = se^2)

# correction for foraging rates
data_tc <- data_tc%>%
    mutate(smd = ifelse(!is.na(multiplier), smd*multiplier, smd),
    upper = ifelse(!is.na(multiplier), upper*multiplier, upper),
    lower = ifelse(!is.na(multiplier), lower*multiplier, lower))
    

# add studies from play back group = human

data_tc <- data_tc%>%
    bind_rows(data_pb%>%
    filter(group == "human" & outcome != "latency" )%>%
    select(cite.key, group, pop_cn, outcome, smd, se, upper, lower))

# add studies from baci

data_tc <- data_tc%>%
    bind_rows(data_baci%>%
    select(cite.key, group, pop_cn, outcome, smd, se, upper, lower))

# add species information

data_tc <-  data_tc %>%
    left_join(pop, by = "pop_cn")%>%
    mutate(trophic_level = ifelse(pop_sn == "Pyrrhocorax graculus", 1, trophic_level))

data_tc <- data_tc%>%
    select(cite.key, smd, se, upper, lower, pop_cn, pop_sn, group, exposure, outcome, functional_group, trophic_level)

data_tc$trophic_level <- as.factor(data_tc$trophic_level)

# add observational studies

data_tc <- full_join(data_tc, data_cor_smd)

# rename movement metrics

data_tc <- data_tc%>%
    mutate(outcome = ifelse(outcome == "displacement" | outcome == "movement rate" | outcome == "home range",
                            "movement", outcome))

# fix giordano mouse

data_tc <- data_tc%>%
    mutate(smd = ifelse(pop_cn == "Mouse" & is.na(smd), 0, smd),
            upper = ifelse(pop_cn == "Mouse" & is.na(upper), 0, upper),
            lower = ifelse(pop_cn == "Mouse" & is.na(lower), 0, lower))

# make pop_sn capitalised

data_tc <- data_tc%>%
    mutate(pop_sn = str_to_title(pop_sn))

write.csv(data_tc, "data_tc.csv", row.names = F)


# Is the effect significantly different from 0 across outcomes?

source('funcs.R')

stat_all <- data_tc%>%
    mutate(weight = 1/se^2)%>%
    drop_na(smd)%>%
    ungroup()%>%
    group_by(outcome)%>%
    filter(upper != 0)%>%
    mutate(Q = Q(weight, smd),
              df = df(n()),
              C = C(weight),
              T_sq = T(Q, df, C),
              I_sq = I_sq(Q, df),
            weight_corrected = 1/(se^2 + T_sq)
)%>%
    summarise(Q = last(Q),
                df = last(df),
                T_sq = last(T_sq),
                I_sq = last(I_sq),
              E = summary_effect(weight_corrected, smd),
              var_E = variance_summary_effect(weight_corrected),
              R_sq = R_sq(Q, df, weight_corrected),
              Z = Z(E, var_E), 
              p_0 = p(Z),
              p_q = pchisq(Q, df, lower.tail = F))


# make and save table

stat_all%>%
    mutate(p_0 = ifelse(p_0 < 0.001, "<0.001", round(p_0, 3)),
            p_q = ifelse(p_q < 0.001, "<0.001", round(p_q, 3)))


# plot effect size and confidence intervals

ggplot(data = data_tc, aes(x = smd, y = reorder(pop_sn, smd), col = trophic_level))+
    geom_point(size = 2)+
    geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    xlab("Standardised mean difference")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    legend.position = "top"
    )+
    facet_wrap(~outcome, scales = "free_x")+
    # italicise x axis
    theme(axis.text.y = element_text(face = "italic"))+
    scale_color_brewer(name = "Trophic Level", palette = "Set1")

ggsave("es_tc.png", width = 16, height = 8, dpi = 300)