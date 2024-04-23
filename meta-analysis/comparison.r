library(viridis, help, pos = 2, lib.loc = NULL)

source("analysis.R", echo = FALSE)

# Comparison across hunting, non-hunting disturbance and natural predators --------------------------------------------

meta <- meta %>%
    mutate(cite.key = as.character(cite.key)) 

data_comp <- data_tc_smd%>%
    left_join(meta, by = c("cite.key"))%>%
    rename(pop_cn = pop_cn.x,
           outcome = outcome.x,
           exposure = exposure.x)%>%
    mutate(exposure = ifelse(is.na(exposure), exposure.y, exposure))%>%
    select(cite.key, group, exposure, treatment, pop_cn, outcome, smd, se, upper, lower)

# clearning exposure data


data_comp <- data_comp%>%
    mutate(exposure = ifelse(group == "human", "hunting", exposure),
           exposure = ifelse(group == "Treatment_1", "active disturbance", exposure),
           exposure = ifelse(group == "Treatment_2", "hunting", exposure),
           exposure = ifelse(exposure == "human playback", "hunting", exposure),
           exposure = ifelse(exposure == "nonhunting disturbance/hunting?", "active disturbance", exposure),
           exposure = ifelse(exposure == "hunting/nonhunting disturbance/natural predator playback", "hunting", exposure),
           exposure = ifelse(exposure == "nonhunting disturbance", "non-hunting disturbance", exposure))

# add data_pb where group is not human

# add natural predator to exposure

data_comp <- data_comp%>%
    mutate(exposure = ifelse(exposure != "hunting" & exposure != "active disturbance" & exposure != "passive disturbance", "active disturbance", exposure))

# filter out natural predator

data_comp <- data_comp%>%
    filter(exposure != "natural predator")

# add species data

data_comp <- data_comp%>%
    left_join(pop, by = c("pop_cn"))%>%
    mutate(trophic_level = as.factor(trophic_level))

# set order as hunting, active disturbance, passive disturbance

data_comp <- data_comp%>%
    mutate(exposure = factor(exposure, levels = c("hunting", "active disturbance", "passive disturbance")))%>%
    # capilatise first letter
    mutate(exposure = str_to_title(exposure))

#plotting

data_comp %>%
    filter(outcome != "latency" & !is.na(pop_sn))%>%
    ggplot(aes(x = reorder(pop_sn, smd), y = smd, col = trophic_level))+
    geom_point()+
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    labs(x = "Exposure", y = "Standardised mean difference")+
    facet_grid(outcome~exposure, scales = "free")+
    # order strip text
    theme(strip.text.x = element_text(size = 12, face = "bold"))+
    theme_bw()+
    theme(legend.position = "top")+
    coord_flip()+
    # remove shape legend
    guides(shape = "none")+
    # set legend title
    scale_color_brewer(name = "Trophic level", palette = "Set1")+
    # italicise x-axis labels
    theme(axis.text.y = element_text(face = "italic"))
    
ggsave("es_comp.png", width = 8, height = 8, dpi = 300)

# is hunting different from non-hunting?

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

write.csv(comp_stat, "comp_stat.csv")

# subgroup analysis

meta_stat <- data.frame()

for (i in unique(data_tc_smd$outcome)) {
 
    data <- data_comp %>%
        filter(outcome == i)

    stat <- metagen(
            TE = smd,
            seTE = se,
            data = data,
            studlab = cite.key,
            method.tau = "DL",
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

    forest(stat, file = paste0("forest_", i, ".png"), width = 1000)

}

colnames(meta_stat) <- c("exposure", "E", "upper", "lower", "outcome")

meta_stat%>%
    mutate(exposure = factor(exposure, levels = c("Hunting", "Active Disturbance", "Passive Disturbance")))%>%
    ggplot(aes(x = outcome, y = E, ymin = lower, ymax = upper, col = exposure, shape = exposure))+
    geom_pointrange(position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    guides(shape = "none", col = guide_legend(title = "Type of Human Activity"))+
    labs(x = "Measured Behaviours", y = "Summary Effect (± 95% confidence interval)")+
    theme_bw()+
    theme(legend.position = "top")

ggsave("es_meta_subgroup.png", width = 8, height = 8, dpi = 300)

# difference between hunting and non-hunting

comp_stat%>%
    mutate(exposure = factor(exposure, levels = c("Hunting", "Active Disturbance", "Passive Disturbance")))%>%
    ggplot(aes(x = outcome, y = E, ymin = lower, ymax = upper, col = exposure, shape = outcome))+
    geom_pointrange(position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    labs(x = "Type of Human Activity", y = "Summary Effect (± 95% confidence interval)")+
    theme_bw()+
    theme(legend.position = "top")+
    scale_color_brewer(name = "Outcome", palette = "Set1")+
    # remove shape legend
    guides(shape = "none")

ggsave("es_diff.png", width = 8, height = 8, dpi = 300)

# funnel plot

ggplot(data_comp, aes(x = smd, y = 1/se, col = outcome, shape = exposure))+
    geom_point(size = 2)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    labs(x = "Effect size", y = "1/√variance")+
    theme_bw()+
    theme(legend.position = "top")+
    scale_color_brewer(name = "Outcome", palette = "Set1")+
    scale_shape_manual(values = c(1, 2, 3))

ggsave("funnel.png", width = 8, height = 8, dpi = 300)