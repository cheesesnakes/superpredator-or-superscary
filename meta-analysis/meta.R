# meta-regressions

library(meta, help, pos = 2, lib.loc = NULL)

# effect of size of animal

source("comparison.r", echo = FALSE)

size <- read.csv('data/size.csv')

size <- size%>%
mutate(body_mass = ifelse(is.na(body_mass), (body_mass_minimum + body_mass_maximum)/2, body_mass))%>%
mutate(body_mass = ifelse(body_mass_units != "kg" & body_mass_units != "tonne", body_mass/1000, body_mass),
body_mass = ifelse(body_mass_units == "tonne", body_mass*1000, body_mass)
)%>%
group_by(pop_sn)%>%
summarise(size = mean(body_mass, na.rm = TRUE))

data_size <- data_comp%>%
left_join(size, by = c('pop_sn'))%>%
mutate(size = as.numeric(size))%>%
select(cite.key, pop_sn, size, smd, se, lower, upper, treatment, outcome)%>%
filter(!size > 1000)

# map metareg over each outcome in data_size

coeff <- data.frame(intercpt = numeric(), b = numeric(), ci.lo.min = numeric(), ci.lo.max = numeric(), 
ci.hi.min = numeric(), ci.hi.max = numeric(), outcome = character())

j = 1

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
            method.tau = "DL",
            prediction = TRUE,
            sm = "SMD",
            title = i
        )
    
    reg <- metareg(stat, ~size)

    print(reg)

    coeff[j,] <- c(reg$beta[1], reg$beta[2], reg$ci.lb[1], reg$ci.lb[2], reg$ci.ub[1], reg$ci.ub[2], i)

    bubble(reg, studylab = TRUE, file = paste0("size_reg_", i, ".png"), main = i)

    j = j + 1
}

# set type coeff

coeff <- coeff%>%
mutate(b = as.numeric(b),
intercpt = as.numeric(intercpt),
ci.lo.min = as.numeric(ci.lo.min),
ci.lo.max = as.numeric(ci.lo.max),
ci.hi.min = as.numeric(ci.hi.min),
ci.hi.max = as.numeric(ci.hi.max))

ggplot(data_size, aes(x = size, y = smd, size = se)) +
    geom_point(col = "#3D426B") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_abline(data = coeff, aes(intercept = intercpt, slope = b), color = "#c23b22") +
    geom_abline(data = coeff, aes(intercept = ci.lo.min, slope = b), color = "#c23b22", linetype = "dotted") +
    geom_abline(data = coeff, aes(intercept = ci.hi.min, slope = b), color = "#c23b22", linetype = "dotted") +
    facet_wrap(~outcome, ncol = 1, scales = "free")+
    theme_bw()+
    labs(x = "Size (kg)", y = "Standardized mean difference")+
    theme(legend.position = "none",
    text = element_text(size = 16))

ggsave("reg_size.png", width = 8, height = 12)

# effect of absolute latitude

source("map.r", echo = FALSE)

data_lat <- data_comp%>%
    left_join(studies, by = c('cite.key' = "File52"))%>%
    mutate(abs_lat = abs(lat))%>%
    select(cite.key, pop_sn, abs_lat, smd, se, lower, upper, treatment, outcome)


# map metareg over each outcome in data_lat

coeff <- data.frame(intercpt = numeric(), b = numeric(), ci.lo.min = numeric(), ci.lo.max = numeric(),
ci.hi.min = numeric(), ci.hi.max = numeric(), outcome = character())

j = 1

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
            method.tau = "DL",
            prediction = TRUE,
            sm = "SMD",
            title = i
        )
    
    reg <- metareg(stat, ~lat)

    print(reg)

    coeff[j,] <- c(reg$beta[1], reg$beta[2], reg$ci.lb[1], reg$ci.lb[2], reg$ci.ub[1], reg$ci.ub[2], i)

    j = j + 1

    bubble(reg, studylab = TRUE, file = paste0("lat_reg_", i, ".png"), main = i)

}

# set type coeff

coeff <- coeff%>%
mutate(b = as.numeric(b),
intercpt = as.numeric(intercpt),
ci.lo.min = as.numeric(ci.lo.min),
ci.lo.max = as.numeric(ci.lo.max),
ci.hi.min = as.numeric(ci.hi.min),
ci.hi.max = as.numeric(ci.hi.max))

coeff
# plot

ggplot(data_lat, aes(x = abs_lat, y = smd, size = se)) +
    geom_point(col = "#3D426B") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_abline(data = coeff, aes(intercept = intercpt, slope = b), color = "#c23b22") +
    geom_abline(data = coeff, aes(intercept = ci.lo.min, slope = b), color = "#c23b22", linetype = "dotted") +
    geom_abline(data = coeff, aes(intercept = ci.hi.min, slope = b), color = "#c23b22", linetype = "dotted") +
    facet_wrap(~outcome, ncol = 1, scales = "free")+
    theme_bw()+
    labs(x = "Absolute latitude", y = "Standardized mean difference")+
    theme(legend.position = "none",
    text = element_text(size = 16))

ggsave("reg_lat.png", width = 8, height = 12)

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
            method.tau = "DL",
            prediction = TRUE,
            sm = "SMD",
            title = i
        )
    
    reg <- metareg(stat, ~exposure)

    print(reg)

    bubble(reg, studylab = TRUE, file = paste0("treatment_reg_", i, ".png"), main = i)

}
