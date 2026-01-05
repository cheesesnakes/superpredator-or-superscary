# meta-regressions

pacman::p_load(meta, broom, metafor, flextable)

# effect of size of animal
here::i_am("meta-analysis/6_meta.R")
source(here::here("meta-analysis/4_effects-sizes.R"), echo = FALSE, print.eval = FALSE)

size <- read.csv(here::here("meta-analysis/data/size.csv"))

size <- size %>%
    mutate(body_mass = ifelse(is.na(body_mass), (body_mass_minimum + body_mass_maximum) / 2, body_mass)) %>%
    mutate(
        body_mass = ifelse(body_mass_units != "kg" & body_mass_units != "tonne", body_mass / 1000, body_mass),
        body_mass = ifelse(body_mass_units == "tonne", body_mass * 1000, body_mass)
    ) %>%
    group_by(pop_sn) %>%
    summarise(size = mean(body_mass, na.rm = TRUE))

data_size <- data_comp %>%
    left_join(size, by = c("pop_sn")) %>%
    mutate(size = as.numeric(size)) %>%
    select(cite.key, pop_sn, size, smd, se, lower, upper, exposure_category, outcome, trophic_level) %>%
    filter(!size > 1000)

# map metareg over each outcome in data_size

coeff <- data.frame(
    intercpt = numeric(), b = numeric(), ci.lo.min = numeric(), ci.lo.max = numeric(),
    ci.hi.min = numeric(), ci.hi.max = numeric(), outcome = character()
)

j <- 1

reg_tab <- data.frame(
    intercept = numeric(), se_intercept = numeric(), slope = numeric(), se_slope = numeric(), tau2 = numeric(), se_tau2 = numeric(),
    I2 = numeric(), H2 = numeric(), R2 = numeric(), outcome = character(), predictor = character()
)

k <- 1

for (i in unique(data_size$outcome)) {
    data <- data_size %>%
        filter(outcome == i)

    size <- data$size

    stat <- metagen(
        TE = smd,
        seTE = se,
        data = data,
        studlab = cite.key,
        comb.fixed = FALSE,
        comb.random = TRUE,
        hakn = TRUE,
        method.tau = "REML",
        prediction = TRUE,
        sm = "SMD",
        title = i
    )

    reg <- metareg(stat, ~size)

    print(paste("Size:", i, sep = " "))

    print(summary(reg))

    coeff[j, ] <- c(reg$beta[1], reg$beta[2], reg$ci.lb[1], reg$ci.lb[2], reg$ci.ub[1], reg$ci.ub[2], i)

    reg_tab[k, ] <- c(reg$beta[1], reg$se[1], reg$beta[2], reg$se[2], reg$tau2, reg$se.tau2, reg$I2, reg$H2, 0, i, "size")

    k <- k + 1

    j <- j + 1
}

# set type coeff

coeff <- coeff %>%
    mutate(
        b = as.numeric(b),
        intercpt = as.numeric(intercpt),
        ci.lo.min = as.numeric(ci.lo.min),
        ci.lo.max = as.numeric(ci.lo.max),
        ci.hi.min = as.numeric(ci.hi.min),
        ci.hi.max = as.numeric(ci.hi.max)
    )

print(reg_tab)

write.csv(reg_tab, here::here("meta-analysis/output/metareg_size.csv"), row.names = FALSE)

# save as flextable

reg_tab %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        intercept = "Intercept",
        se_intercept = "SE Intercept",
        slope = "Slope",
        se_slope = "SE Slope",
        tau2 = "Tau^2",
        se_tau2 = "SE Tau^2",
        I2 = "I^2",
        H2 = "H^2",
        R2 = "R^2",
        outcome = "Outcome",
        predictor = "Predictor"
    ) %>%
    save_as_docx(path = here::here("meta-analysis/output/metareg_size.docx"))

data_size <- data_size %>%
    mutate(trophic_level = as.factor(trophic_level))

ggplot(data_size, aes(x = size, y = smd, size = se, col = trophic_level)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_abline(data = coeff, aes(intercept = intercpt, slope = b), color = "#c23b22") +
    geom_abline(data = coeff, aes(intercept = ci.lo.min, slope = b), color = "#c23b22", linetype = "dotted") +
    geom_abline(data = coeff, aes(intercept = ci.hi.min, slope = b), color = "#c23b22", linetype = "dotted") +
    facet_wrap(~outcome, ncol = 1, scales = "free") +
    # remove size scale
    scale_size_continuous(guide = "none") +
    scale_color_brewer(palette = "Set1", name = "Trophic Level") +
    theme_bw() +
    labs(x = "Size (kg)", y = "Standardized mean difference") +
    theme(
        legend.position = "top",
        text = element_text(size = 16)
    )

ggsave(here::here("meta-analysis/figures/fig-4.png"), width = 8, height = 12)

# effect of absolute latitude

source(here::here("meta-analysis/4-1_map.R"), echo = FALSE)

data_lat <- data_comp %>%
    left_join(studies, by = c("cite.key" = "File52")) %>%
    mutate(abs_lat = abs(lat)) %>%
    select(cite.key, pop_sn, abs_lat, smd, se, lower, upper, exposure_category, outcome, trophic_level)


# map metareg over each outcome in data_lat

coeff <- data.frame(
    intercpt = numeric(), b = numeric(), ci.lo.min = numeric(), ci.lo.max = numeric(),
    ci.hi.min = numeric(), ci.hi.max = numeric(), outcome = character()
)

j <- 1

for (i in unique(data_lat$outcome)) {
    data <- data_lat %>%
        filter(outcome == i)

    lat <- data$abs_lat

    stat <- metagen(
        TE = smd,
        seTE = se,
        data = data,
        studlab = cite.key,
        comb.fixed = FALSE,
        comb.random = TRUE,
        hakn = TRUE,
        method.tau = "REML",
        prediction = TRUE,
        sm = "SMD",
        title = i
    )

    reg <- metareg(stat, ~abs_lat)

    print(paste("Absolute Latitude:", i, sep = " "))

    print(summary(reg))

    coeff[j, ] <- c(reg$beta[1], reg$beta[2], reg$ci.lb[1], reg$ci.lb[2], reg$ci.ub[1], reg$ci.ub[2], i)

    reg_tab[k, ] <- c(reg$beta[1], reg$se[1], reg$beta[2], reg$se[2], reg$tau2, reg$se.tau2, reg$I2, reg$H2, reg$R2, i, "latitude")

    k <- k + 1

    j <- j + 1

    # bubble(reg, studylab = TRUE, file = here::here(paste0("lat_reg_", i, ".png")), main = i)
}

# set type coeff

coeff <- coeff %>%
    mutate(
        b = as.numeric(b),
        intercpt = as.numeric(intercpt),
        ci.lo.min = as.numeric(ci.lo.min),
        ci.lo.max = as.numeric(ci.lo.max),
        ci.hi.min = as.numeric(ci.hi.min),
        ci.hi.max = as.numeric(ci.hi.max)
    )

print(coeff)

print(reg_tab)

write.csv(reg_tab, here::here("meta-analysis/output/metareg_lat.csv"), row.names = FALSE)

# save as flextable

reg_tab %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        intercept = "Intercept",
        se_intercept = "SE Intercept",
        slope = "Slope",
        se_slope = "SE Slope",
        tau2 = "Tau^2",
        se_tau2 = "SE Tau^2",
        I2 = "I^2",
        H2 = "H^2",
        R2 = "R^2",
        outcome = "Outcome",
        predictor = "Predictor"
    ) %>%
    save_as_docx(path = here::here("meta-analysis/output/metareg_lat.docx"))

# plot

data_lat <- data_lat %>%
    mutate(trophic_level = as.factor(trophic_level))

ggplot(data_lat, aes(x = abs_lat, y = smd, size = se, col = trophic_level)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_abline(data = coeff, aes(intercept = intercpt, slope = b), color = "#c23b22") +
    geom_abline(data = coeff, aes(intercept = ci.lo.min, slope = b), color = "#c23b22", linetype = "dotted") +
    geom_abline(data = coeff, aes(intercept = ci.hi.min, slope = b), color = "#c23b22", linetype = "dotted") +
    facet_wrap(~outcome, ncol = 1, scales = "free") +
    theme_bw() +
    scale_size_continuous(guide = "none") +
    scale_color_brewer(palette = "Set1", name = "Trophic Level") +
    labs(x = "Absolute latitude", y = "Standardized mean difference") +
    theme(
        legend.position = "top",
        text = element_text(size = 16)
    )

ggsave(here::here("meta-analysis/figures/reg_lat.png"), width = 8, height = 12)

# effect of type of human interaction

# map metareg over each outcome in data_comp

for (i in unique(data_comp$outcome)) {
    data <- data_comp %>%
        filter(outcome == i)

    stat <- metagen(
        TE = smd,
        seTE = se,
        data = data,
        studlab = cite.key,
        comb.fixed = FALSE,
        comb.random = TRUE,
        hakn = TRUE,
        method.tau = "REML",
        prediction = TRUE,
        sm = "SMD",
        title = i
    )

    reg <- metareg(stat, ~exposure_category)

    print(paste("Type_int", i, sep = " ", " "))

    print(summary(reg))

    # bubble(reg, studylab = TRUE, file = here::here(paste0("treatment_reg_", i, ".png")), main = i)

    reg_tab[k, ] <- c(reg$beta[1], reg$se[1], reg$beta[2], reg$se[2], reg$tau2, reg$se.tau2, reg$I2, reg$H2, reg$R2, i, "interaction_type")

    k <- k + 1
}

print(reg_tab)

# multiple regression

size <- read.csv(here::here("meta-analysis/data/size.csv"))

size <- size %>%
    mutate(body_mass = ifelse(is.na(body_mass), (body_mass_minimum + body_mass_maximum) / 2, body_mass)) %>%
    mutate(
        body_mass = ifelse(body_mass_units != "kg" & body_mass_units != "tonne", body_mass / 1000, body_mass),
        body_mass = ifelse(body_mass_units == "tonne", body_mass * 1000, body_mass)
    ) %>%
    group_by(pop_sn) %>%
    summarise(size = mean(body_mass, na.rm = TRUE))

data_comp <- data_comp %>%
    left_join(size, by = c("pop_sn")) %>%
    left_join(studies, by = c("cite.key" = "File52")) %>%
    mutate(abs_lat = abs(lat)) %>%
    mutate(size = as.numeric(size)) %>%
    mutate(abs_lat = abs(lat)) %>%
    select(cite.key, pop_sn, size, abs_lat, smd, se, lower, upper, exposure_category, outcome, exposure_category) %>%
    drop_na(se, pop_sn) %>%
    mutate(data_id = row_number())

estimates <- data.frame()
stats <- data.frame()

for (i in unique(data_comp$outcome)) {
    data <- data_comp %>%
        filter(outcome == i)

    smd <- data$smd

    se <- data$se

    reg <- rma.mv(
        yi = smd, V = se^2,
        mod = ~ size + exposure_category,
        random = list(~ 1 | cite.key, ~ 1 | data_id),
        data = data, method = "REML", dfs = "contain", test = "t"
    )

    png(here::here(paste0("meta-analysis/figures//multi_reg_size_", i, ".png")))

    regplot(x = reg, mod = "size", pi = TRUE, xlab = "Size (kg)", ylab = "Standardized mean difference", xlim = c(0, 600))

    dev.off()

    if (i != "Movement") {
        png(here::here(paste0("meta-analysis/figures//multi_reg_hunting-active_", i, ".png")))

        regplot(x = reg, mod = "exposure_categoryLethal Interaction", pi = TRUE, xlab = "Type of human interaction", ylab = "Standardized mean difference")

        dev.off()

        png(here::here(paste0("meta-analysis/figures//multi_reg_passive-active_", i, ".png")))

        regplot(x = reg, mod = "exposure_categoryPassive Interaction", pi = TRUE, xlab = "Type of human interaction", ylab = "Standardized mean difference")

        dev.off()
    } else {
        png(here::here(paste0("meta-analysis/figures//multi_reg_passive-lethal_", i, ".png")))

        regplot(x = reg, mod = "exposure_categoryPassive Interaction", pi = TRUE, xlab = "Type of human interaction", ylab = "Standardized mean difference")

        dev.off()
    }



    print(i)

    print(summary(reg))

    estimates <- rbind(estimates, cbind(broom::tidy(reg), outcome = i))

    stats <- rbind(stats, broom::glance(reg))
}

print(estimates)
print(stats)

# save as flextable

estimates %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        term = "Term",
        estimate = "Estimate",
        std.error = "Standard Error",
        statistic = "Statistic",
        p.value = "P-value",
        outcome = "Outcome"
    ) %>%
    save_as_docx(path = here::here("meta-analysis/output/metarma_full_estimates.docx"))

stats %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        r.squared = "R-squared",
        sigma2 = "Sigma^2",
        tau2 = "Tau^2",
        I2 = "I-squared",
        H2 = "H-squared",
        R2 = "R-squared"
    ) %>%
    save_as_docx(path = here::here("meta-analysis/output/metarma_full.docx"))

write.csv(estimates, here::here("meta-analysis/output/metarma_full.csv"), row.names = FALSE)
