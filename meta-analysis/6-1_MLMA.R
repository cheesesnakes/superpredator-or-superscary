pacman::p_load(dplyr, metafor)

# Get computed effect size data ---> data_comp

source('4_effects-sizes.R', chdir = TRUE)

head(data_comp)

nrow(data_comp)

# remove nas from data_comp

data_comp <- drop_na(data_comp, pop_sn)

nrow(data_comp)

# fitting a non-phylogenetic multilevel metaanalysis model for each outcome

# dataframe to store the results

results <- data.frame(outcome = character(), 
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
p = numeric())

# loop over each outcome

for (i in unique(data_comp$outcome)) {
  
    data <- data_comp %>%
        filter(outcome == i)
    
    smd <- data$smd
    se <- data$se
    
    mlma <- rma.mv(yi = smd, V = se^2, random = list(~ 1 | pop_sn, ~1 | cite.key), data = data, method = "REML", dfs = "contain", test = "t")
    
    print(paste("Outcome:", i, sep = " "))

    print(summary(mlma))

    results <- rbind(results, data.frame(outcome = i,
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
                                        )
                    )    

    # save the results

    write.csv(results, "output/mlma_results.csv")
}
