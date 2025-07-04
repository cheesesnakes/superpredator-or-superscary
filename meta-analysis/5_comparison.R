library(viridis, help, pos = 2, lib.loc = NULL)

here::i_am("meta-analysis/5_comparison.R")
source(here::here("meta-analysis/4_effects-sizes.R"), echo = FALSE)
source(here::here("meta-analysis/5-1_funcs.R"), chdir = TRUE)

# Is the effect significantly different from 0 across outcomes?

pacman::p_load(meta, ggplot2, purrr, flextable)

# map function metagen to data_tc_smd for each outcome


for (i in unique(data_comp$outcome)) {
    data <- data_comp %>%
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

    forest(stat, file = here::here(paste0("meta-analysis/figures/meta_", i, ".png")), width = 1000)

    # save summary data

    data.frame(stat) %>%
        mutate(pval = formatC(pval, format = "e", digits = 3)) %>%
        mutate_if(is.numeric, round, 3) %>%
        flextable() %>%
        save_as_docx(path = here::here(paste0("meta-analysis/output/summary_metagen_", i, ".docx")))
}

## using functions from funcs.R

stat_man <- data_comp %>%
    mutate(weight = 1 / se^2) %>%
    group_by(outcome) %>%
    filter(pop_cn != "Mouse") %>%
    mutate(
        Q = Q(weight, smd),
        df = df(n()),
        C = C(weight),
        T = T(Q, df, C),
        I_sq = I_sq(Q, df),
        corrected_wight = 1 / ((se^2) + T)
    ) %>%
    summarise(
        Q = last(Q),
        df = last(df),
        C = last(C),
        T = last(T),
        I_sq = I_sq(Q, df),
        var_E = 1 / sum(corrected_wight),
        E = summary_effect(corrected_wight, smd),
        Z = Z(E, var_E),
        p = 2 * pnorm(-abs(Z)),
        # prediction interval
        lower = E - 1.96 * sqrt(var_E),
        upper = E + 1.96 * sqrt(var_E)
    )


# save comparison data

stat_man %>%
    flextable() %>%
    set_header_labels(
        Q = "Q",
        df = "df",
        C = "C",
        T = "T",
        I_sq = "I²",
        var_E = "Variance of E",
        E = "E (Effect Size)",
        Z = "Z",
        p = "p-value",
        lower = "Lower CI",
        upper = "Upper CI"
    ) %>%
    save_as_docx(path = here::here("meta-analysis/output/summary_manual.docx"))

# Comparison across hunting, non-hunting disturbance and natural predators --------------------------------------------

# is hunting different from non-hunting?

# sub-group analysis manually

comp_stat <- data_comp %>%
    mutate(
        se = abs((upper - smd) / 1.96),
        var = se^2,
        weight = 1 / var
    ) %>%
    group_by(exposure_category, outcome) %>%
    filter(upper != 0) %>%
    filter(!is.na(smd)) %>%
    mutate(
        Q = Q(weight, smd),
        df = df(n()),
        C = C(weight),
        T = T(Q, df, C),
        I_sq = I_sq(Q, df),
        corrected_wight = 1 / ((se^2) + T)
    ) %>%
    summarise(
        Q = last(Q),
        df = last(df),
        C = last(C),
        T = last(T),
        I_sq = I_sq(Q, df),
        var_E = 1 / sum(corrected_wight),
        E = summary_effect(corrected_wight, smd),
        Z = Z(E, var_E),
        p = 2 * pnorm(-abs(Z)),
        # prediction interval
        lower = E - 1.96 * sqrt(var_E),
        upper = E + 1.96 * sqrt(var_E)
    )


# save comparison data as flextable

comp_stat %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        Q = "Q",
        df = "df",
        C = "C",
        T = "T",
        I_sq = "I²",
        var_E = "Variance of E",
        E = "E (Effect Size)",
        Z = "Z",
        p = "p-value",
        lower = "Lower CI",
        upper = "Upper CI"
    ) %>%
    save_as_docx(path = here::here("meta-analysis/output/subgroup_manual.docx"))

# subgroup analysis

meta_stat <- data.frame()

for (i in unique(data_comp$outcome)) {
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
        subgroup = exposure_category,
        method.ci = "z",
        method.random.ci = "KR"
    )

    stat_subg <- cbind(stat$TE.random.w, stat$upper.random.w, stat$lower.random.w)

    stat_subg <- as.data.frame(stat_subg) %>% tibble::rownames_to_column("exposure_category")

    stat_subg <- stat_subg %>%
        mutate(outcome = i)

    meta_stat <- rbind(meta_stat, stat_subg)

    print(stat)

    # save forest plot

    forest(stat, file = here::here(paste0("meta-analysis/figures/subgroup_", i, ".png")), width = 1000)
}

colnames(meta_stat) <- c("exposure_category", "E", "upper", "lower", "outcome")

write.csv(meta_stat, here::here("meta-analysis/output/subgroup_metagen.csv"))

# save as flextable

meta_stat %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        exposure_category = "Type of Human Activity",
        E = "E (Effect Size)",
        upper = "Upper CI",
        lower = "Lower CI",
        outcome = "Outcome"
    ) %>%
    save_as_docx(path = here::here("meta-analysis/output/subgroup_metagen.docx"))

meta_stat %>%
    mutate(exposure_category = factor(exposure_category, levels = c("Passive Interaction", "Active Interaction", "Lethal Interaction"))) %>%
    mutate(outcome = stringr::str_to_title(outcome)) %>%
    mutate(outcome = factor(outcome, levels = c("Foraging", "Vigilance", "Movement"))) %>%
    # capitalise first letter of outcome
    filter(exposure_category != "Active non-lethal interactions" | outcome != "Movement") %>%
    ggplot(aes(x = exposure_category, y = E, ymin = lower, ymax = upper, col = exposure_category, shape = exposure_category)) +
    geom_pointrange(position = position_dodge(width = 0.5), size = 1, linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(shape = "none", color = "none") +
    labs(x = "Type of interaction", y = "Summary Effect (± 95% confidence interval)") +
    scale_color_brewer(name = "Type of Human Activity", palette = "Dark2") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    theme_bw() +
    coord_flip() +
    theme(
        legend.position = "top",
        text = element_text(size = 20),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5)
    ) +
    facet_grid(~outcome)

ggsave(here::here("meta-analysis/figures/fig-6.png"), width = 10, height = 5, dpi = 300)

# difference between hunting and non-hunting

comp_stat %>%
    ggplot(aes(x = outcome, y = E, ymin = lower, ymax = upper, col = exposure_category, shape = outcome)) +
    geom_pointrange(position = position_dodge(width = 0.5), size = 1, linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(shape = "none", col = guide_legend(title = "Type of Human Activity")) +
    labs(x = "Measured Behaviours", y = "Summary Effect (± 95% confidence interval)") +
    scale_color_brewer(name = "Type of Human Activity", palette = "Dark2") +
    theme_bw() +
    coord_flip() +
    theme(legend.position = "top")

ggsave(here::here("meta-analysis/figures/subgroup_manual.png"), width = 8, height = 8, dpi = 300)

# funnel plot

## add citation from authors

ggplot(data_comp, aes(x = smd, y = 1 / se, col = outcome, shape = exposure_category)) +
    geom_point(size = 2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    # label studies
    geom_text(aes(label = cite), nudge_x = 0.1, nudge_y = 0.1, check_overlap = TRUE) +
    labs(x = "Effect size", y = "1/√variance") +
    theme_bw() +
    theme(legend.position = "top") +
    scale_color_brewer(name = "Outcome", palette = "Set1", guide = "none") +
    scale_shape_manual(name = "Type of Interaction", values = c(1, 2, 3)) +
    facet_wrap(~outcome, scales = "free")

ggsave(here::here("meta-analysis/figures/funnel.png"), width = 12, height = 6, dpi = 300)
