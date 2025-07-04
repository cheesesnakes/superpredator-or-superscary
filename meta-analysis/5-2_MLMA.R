pacman::p_load(dplyr, metafor, flextable)

# Get computed effect size data ---> data_comp
here::i_am("meta-analysis/5-2_MLMA.R")
source(here::here("meta-analysis/4_effects-sizes.R"), chdir = TRUE)

head(data_comp)

nrow(data_comp)

# add data id

data_comp <- data_comp %>%
    mutate(data_id = row_number())

# remove nas from data_comp

data_comp <- drop_na(data_comp, pop_sn)

nrow(data_comp)

# fitting a non-phylogenetic multilevel metaanalysis model for each outcome

# dataframe to store the results

results <- data.frame(
    outcome = character(),
    intercept = numeric(),
    se_intercept = numeric(),
    lower = numeric(),
    upper = numeric(),
    sigma2 = numeric(),
    tau2 = numeric(),
    T = numeric(),
    n = numeric(),
    Q = numeric(),
    Qp = numeric(),
    df = numeric(),
    p = numeric()
)

factors <- c("Species", "Study ID", "Data ID")

results_random <- data.frame(
    outcome = character(),
    factor = character(),
    sigma2 = numeric(),
    n_levels = numeric()
)

# loop over each outcome

for (i in unique(data_comp$outcome)) {
    data <- data_comp %>%
        filter(outcome == i)

    smd <- data$smd
    se <- data$se

    mlma <- rma.mv(yi = smd, V = se^2, mod = ~1, random = list(~ 1 | pop_sn, ~ 1 | cite.key, ~ 1 | data_id), data = data, method = "REML", dfs = "contain", test = "t")

    print(paste("Outcome:", i, sep = " "))

    print(summary(mlma))

    png(here::here(paste("meta-analysis/figures/forest-metafor_", i, ".png", sep = "")), width = 800, height = 800)

    forest(mlma, slab = data$cite, digits = 3, refline = 0, xlab = "Standardised Mean Difference", main = paste("Forest plot of", i))

    dev.off()

    results <- rbind(results, data.frame(
        outcome = i,
        intercept = mlma$beta[1],
        se_intercept = mlma$se[1],
        lower = mlma$ci.lb[1],
        upper = mlma$ci.ub[1],
        sigma2 = mlma$sigma2[1],
        tau2 = mlma$tau2[1],
        T = mlma$zval[1],
        n = mlma$k[1],
        Q = mlma$QE[1],
        Qp = mlma$QEp[1],
        df = mlma$k[1],
        p = mlma$p[1]
    ))

    results_random <- rbind(results_random, data.frame(
        outcome = rep(i, 3),
        factor = factors,
        sigma2 = mlma$sigma2,
        n_levels = mlma$s.nlevels
    ))
}

# save the results

write.csv(results, here::here("meta-analysis/output/mlma_results.csv"))
write.csv(results_random, here::here("meta-analysis/output/mlma_results_random.csv"))

# flextable for results

results_ft <- results %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        outcome = "Outcome",
        intercept = "Intercept",
        se_intercept = "SE Intercept",
        lower = "Lower CI",
        upper = "Upper CI",
        sigma2 = "Sigma^2",
        tau2 = "Tau^2",
        T = "T-value",
        n = "N",
        Q = "Q-statistic",
        Qp = "Q p-value",
        df = "Degrees of Freedom",
        p = "p-value"
    ) %>%
    set_caption("Multilevel Meta-Analysis Results")

save_as_docx(results_ft, path = here::here("meta-analysis/output/mlma_results.docx"))

results_random_ft <- results_random %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        outcome = "Outcome",
        factor = "Factor",
        sigma2 = "Sigma^2",
        n_levels = "N Levels"
    ) %>%
    set_caption("Multilevel Meta-Analysis Random Effects Results")

save_as_docx(results_random_ft, path = here::here("meta-analysis/output/mlma_results_random.docx"))

# fitting phylogenetic multilevel metaanalysis model for each outcome

source(here::here("meta-analysis/5-3_phylo.R"))

# Create correlation matrix for analysis

phylo_cor <- vcv(tree, cor = TRUE)

# filter out unmatched species

data_comp <- data_comp %>%
    mutate(tips = str_replace_all(pop_sn, " ", "_")) %>%
    filter(tips %in% tip_names)

# dataframe to store the results

results_phylo <- data.frame(
    outcome = character(),
    intercept = numeric(),
    se_intercept = numeric(),
    lower = numeric(),
    upper = numeric(),
    sigma2 = numeric(),
    tau2 = numeric(),
    T = numeric(),
    n = numeric(),
    Q = numeric(),
    Qp = numeric(),
    df = numeric(),
    p = numeric()
)

results_phylo_random <- data.frame(
    outcome = character(),
    factor = character(),
    sigma2 = numeric(),
    n_levels = numeric()
)

# loop over each outcome

for (i in unique(data_comp$outcome)) {
    data <- data_comp %>%
        filter(outcome == i)

    smd <- data$smd

    se <- data$se

    phylo <- phylo_cor[match(unique(data$tips), tip_names), match(unique(data$tips), tip_names)]

    mlma <- rma.mv(yi = smd, V = se^2, mod = ~1, random = list(~ 1 | pop_sn, ~ 1 | cite.key, ~ 1 | data_id, ~ 1 | tips), R = list(tips = phylo), data = data, method = "REML", dfs = "contain", test = "t")

    png(here::here(paste("meta-analysis/figures/forest-metafor_phylo_", i, ".png", sep = "")), width = 800, height = 800)

    forest(mlma, slab = data$cite, digits = 3, refline = 0, xlab = "Standardised Mean Difference", main = paste("Forest plot of", i))

    dev.off()

    print(paste("Outcome:", i, sep = " "))

    print(summary(mlma))

    results_phylo <- rbind(results_phylo, data.frame(
        outcome = i,
        intercept = mlma$beta[1],
        se_intercept = mlma$se[1],
        lower = mlma$ci.lb[1],
        upper = mlma$ci.ub[1],
        sigma2 = mlma$sigma2[1],
        tau2 = mlma$tau2[1],
        T = mlma$zval[1],
        n = mlma$k[1],
        Q = mlma$QE[1],
        Qp = mlma$QEp[1],
        df = mlma$k[1],
        p = mlma$p[1]
    ))

    results_phylo_random <- rbind(results_phylo_random, data.frame(
        outcome = rep(i, 4),
        factor = c(factors, "Phylogeny"),
        sigma2 = mlma$sigma2,
        n_levels = mlma$s.nlevels
    ))
}

# save the results

write.csv(results_phylo, here::here("meta-analysis/output/mlma_phylo_results.csv"))
write.csv(results_phylo_random, here::here("meta-analysis/output/mlma_phylo_results_random.csv"))

# flextable for results

results_phylo_ft <- results_phylo %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        outcome = "Outcome",
        intercept = "Intercept",
        se_intercept = "SE Intercept",
        lower = "Lower CI",
        upper = "Upper CI",
        sigma2 = "Sigma^2",
        tau2 = "Tau^2",
        T = "T-value",
        n = "N",
        Q = "Q-statistic",
        Qp = "Q p-value",
        df = "Degrees of Freedom",
        p = "p-value"
    ) %>%
    set_caption("Phylogenetic Multilevel Meta-Analysis Results")

save_as_docx(results_phylo_ft, path = here::here("meta-analysis/output/mlma_phylo_results.docx"))

results_phylo_random_ft <- results_phylo_random %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable() %>%
    set_header_labels(
        outcome = "Outcome",
        factor = "Factor",
        sigma2 = "Sigma^2",
        n_levels = "N Levels"
    ) %>%
    set_caption("Phylogenetic Multilevel Meta-Analysis Random Effects Results")

save_as_docx(results_phylo_random_ft, path = here::here("meta-analysis/output/mlma_phylo_results_random.docx"))
