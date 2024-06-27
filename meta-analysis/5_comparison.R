library(viridis, help, pos = 2, lib.loc = NULL)

source("4_effects-sizes.R", echo = FALSE)
source('5-1_funcs.R', chdir = TRUE)

# Is the effect significantly different from 0 across outcomes?

pacman::p_load(meta, ggplot2, purrr)

# map function metagen to data_tc_smd for each outcome


for (i in unique(data_tc_smd$outcome)) {
 
    data <- data_tc_smd %>%
        filter(outcome == i)

    stat <- metagen(
            TE = smd,
            seTE = se,
            data = data,
            studlab = cite,
            comb.fixed = FALSE,
            comb.random = TRUE,
            hakn = TRUE,
            method.tau = "REML",
            prediction = TRUE,
            sm = "SMD",
            title = i
        )

    print(stat)

    # save forest plot

    forest(stat, file = paste0("figures/meta_", i, ".png"), width = 1000)

    write.csv(stat, paste0("output/summary_metagen_", i, ".csv"))
}

## using functions from funcs.R

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

write.csv(stat_man, "output/summary_manual.csv")

# Comparison across hunting, non-hunting disturbance and natural predators --------------------------------------------

# is hunting different from non-hunting?

# sub-group analysis manually

comp_stat <- data_comp%>%
  mutate(se = abs((upper - smd)/1.96),
          var = se^2,
          weight = 1/var)%>%
    group_by(exposure, outcome)%>%
    filter(upper != 0)%>%
    filter(!is.na(smd))%>%
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

print(comp_stat)

# save comparison data

write.csv(comp_stat, "output/subgroup_manual.csv")

# subgroup analysis

meta_stat <- data.frame()

for (i in unique(data_tc_smd$outcome)) {
 
    data <- data_comp %>%
        filter(outcome == i)

    stat <- metagen(
            TE = smd,
            seTE = se,
            data = data,
            studlab = cite,
            method.tau = "REML",
            prediction = TRUE,
            sm = "SMD",
            title = paste("Effect of type of human activity on effect size for", i),
            subgroup = exposure,
            method.ci = "z",
            method.random.ci = "KR"
        )

    stat_subg <- cbind(stat$TE.random.w, stat$upper.random.w, stat$lower.random.w)

    stat_subg <- as.data.frame(stat_subg)%>%tibble::rownames_to_column("exposure")

    stat_subg <- stat_subg%>%
        mutate(outcome = i)

    meta_stat <- rbind(meta_stat, stat_subg)

    print(stat)

    # save forest plot

    forest(stat, file = paste0("figures/subgroup_", i, ".png"), width = 1000)

}

colnames(meta_stat) <- c("exposure", "E", "upper", "lower", "outcome")

write.csv(meta_stat, "output/subgroup_metagen.csv")

meta_stat%>%
    filter(exposure != "Active Disturbance" | outcome != "movement")%>%
    mutate(exposure = factor(exposure, levels = c("Hunting", "Active Disturbance", "Passive Disturbance")))%>%
    ggplot(aes(x = outcome, y = E, ymin = lower, ymax = upper, col = exposure, shape = exposure))+
    geom_pointrange(position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    guides(shape = "none", col = guide_legend(title = "Type of Human Activity"))+
    labs(x = "Measured Behaviours", y = "Summary Effect (± 95% confidence interval)")+
    scale_color_brewer(name = "Type of Human Activity", palette = "Dark2")+
    theme_bw()+
    coord_flip()+
    theme(legend.position = "top")

ggsave("figures/fig-6.png", width = 8, height = 8, dpi = 300)

# difference between hunting and non-hunting

comp_stat%>%
    mutate(exposure = factor(exposure, levels = c("Hunting", "Active Disturbance", "Passive Disturbance")))%>%
    ggplot(aes(x = outcome, y = E, ymin = lower, ymax = upper, col = exposure, shape = outcome))+
    geom_pointrange(position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    guides(shape = "none", col = guide_legend(title = "Type of Human Activity"))+
    labs(x = "Measured Behaviours", y = "Summary Effect (± 95% confidence interval)")+
    scale_color_brewer(name = "Type of Human Activity", palette = "Dark2")+
    theme_bw()+
    coord_flip()+
    theme(legend.position = "top")

ggsave("figures/subgroup_manual.png", width = 8, height = 8, dpi = 300)

# funnel plot

## add citation from authors

ggplot(data_comp, aes(x = smd, y = 1/se, col = outcome, shape = exposure))+
    geom_point(size = 2)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    # label studies
    geom_text(aes(label = cite), nudge_x = 0.1, nudge_y = 0.1, check_overlap = TRUE)+
    labs(x = "Effect size", y = "1/√variance")+
    theme_bw()+
    theme(legend.position = "top")+
    scale_color_brewer(name = "Outcome", palette = "Set1", guide = "none")+
    scale_shape_manual(name = "Type of Interaction",values = c(1, 2, 3))+
    facet_wrap(~outcome, scales = "free")

ggsave("figures/funnel.png", width = 12, height = 6, dpi = 300)
