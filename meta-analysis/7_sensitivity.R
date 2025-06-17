pacman::p_load(meta, broom, metafor)

here::i_am("meta-analysis/7_sensitivity.R")
source(here::here("meta-analysis/4_effects-sizes.R"), echo = FALSE, print.eval = FALSE)

# publication bias funnel plot assymetry

## seperate for each outcome

funnel_sym <- data.frame()

for (i in unique(data_comp$outcome)) {
  
  data <- data_comp %>%
    filter(outcome == i)
  
  smd <- data$smd
  se <- data$se
  
  mod <- rma(yi = smd, sei = se, data = data, method = "REML", test = "t", slab = data$outcome, digits = 3)

  png(here::here(paste("output/funnel_sym_", i, ".png", sep = "")), width = 800, height = 800)

  funnel(mod, refline = 0, digits = 3, xlab = "Standardized mean difference",
   ylab = "Standard error of the mean difference", main = paste("Funnel plot of publication bias for", i))
  
  dev.off()

  regtest <- regtest(mod, model = "rma")

    funnel_sym <- rbind(funnel_sym, data.frame(outcome = i,
                                                 intercept = regtest$est,
                                                 lower = regtest$ci.lb,
                                                 upper = regtest$ci.ub,
                                                 Z = regtest$zval,
                                                 p = regtest$pval))

  
}

print(funnel_sym)

write.csv(funnel_sym, here::here("meta-analysis/output/funnel_sym.csv"), row.names = FALSE)

# multi level meta-regression on variance to test for publication bias

## seperate for each outcome

vartest <- data.frame()

data_comp <- data_comp %>%
  mutate(data_id = row_number())

for (i in unique(data_comp$outcome)) {
  
  data <- data_comp %>%
    filter(outcome == i)
  
  smd <- data$smd
  se <- data$se
  
  mod <- rma.mv(yi = smd, V = se^2, random = list(~1 | data_id, ~1 | cite.key), mod = se^2, data = data, method = "REML", test = "t", slab = data$cite, digits = 3)

  vartest <- rbind(vartest, cbind(broom::tidy(mod), outcome = i))
  
}

print(vartest)

write.csv(vartest, here::here("meta-analysis/output/vartest.csv"), row.names = FALSE)

# time - lag effect 

ggplot(data_comp, aes(x = year, y = smd)) +
  geom_point(aes(size = 1/(se^2), col = outcome)) + # pastel red = "#FF6666"
  geom_smooth(method = "lm", linetype = "dashed", size = 1, col = "dark grey") +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_size_continuous(guide = "none") +
  labs(x = "Year",
       y = "Standardized mean difference") +
  facet_wrap(~outcome, ncol = 1, scales = "free") +
  theme_bw()

ggsave(here::here("meta-analysis/figures/time_lag.png"), width = 6, height = 12)

# leave one out --- robustness

loo_test <- data.frame()

for (i in unique(data_comp$outcome)){
  
  data_i <- data_comp %>%
    filter(outcome == i)
  
  for (j in unique(data_i$cite.key)){

    cite = data_i %>%
      filter(cite.key == j) %>%
      pull(cite)

    data_j <- data_i %>%
      filter(cite.key != j)
    
    smd <- data_j$smd

    se <- data_j$se

    mod <- rma.mv(yi = smd, V = se^2, mod = ~1, random = list(~1 | data_id, ~1 | cite.key), data = data_j, method = "REML", test = "t", slab = cite, digits = 3)

    loo_test <- rbind(loo_test, cbind(broom::tidy(mod, conf.int=TRUE), outcome = i, cite = cite))

  } 

}

write.csv(loo_test, here::here("meta-analysis/output/loo_test.csv"), row.names = FALSE)

## plot  estimate with confidence intervals

ggplot(loo_test, aes(x = estimate, y = cite, xmin = conf.low, xmax = conf.high)) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  facet_wrap(~outcome, nrow = 1, scales = "free") +
  scale_x_continuous(limits = c(-3, 3)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw()

ggsave(here::here("meta-analysis/figures/loo_test.png"), width = 24, height = 6)
