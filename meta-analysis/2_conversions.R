# converting mean and variablity to workable units

source(here::here("meta-analysis/1_cleaning.R"), echo = FALSE)

# set type for all

data <- data %>%
    mutate(
        mean = as.numeric(mean),
        var = as.numeric(var),
        lower = as.numeric(lower),
        upper = as.numeric(upper),
        sampling_time = as.numeric(sampling_time)
    )

# convert percentage to proportion

data <- data %>%
    mutate(
        mean = ifelse(str_detect(scale, "percentage"), mean / 100, mean),
        var = ifelse(str_detect(scale, "percentage"), var / 100, var),
        lower = ifelse(str_detect(scale, "percentage"), lower / 100, lower),
        upper = ifelse(str_detect(scale, "percentage"), upper / 100, upper),
        scale = ifelse(str_detect(scale, "percentage"), "proportion", scale)
    )

# convert  log-proportion to proportion

data <- data %>%
    mutate(
        mean = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", exp(mean), mean),
        var = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", exp(var), var),
        lower = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", exp(lower), lower),
        upper = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", exp(upper), upper),
        scale = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", "proportion", scale)
    )

# conver credible intervals to standard deviation

data <- data %>%
    mutate(
        var = ifelse(var.unit == "credible interval", abs((upper - lower) / (2 * 1.96)), var),
        var.unit = ifelse(var.unit == "credible interval", "se", var.unit)
    )

# convert interquartile range to standard deviation

data <- data %>%
    mutate(
        var = ifelse(var.unit == "interquantile", abs((lower - upper) / (2 * 1.35)), var),
        var.unit = ifelse(var.unit == "interquantile", "sd", var.unit)
    )

# convert range to standard deviation

data <- data %>%
    mutate(
        var = ifelse(var.unit == "range", abs((upper - lower) / (2 * 3.29)), var),
        var.unit = ifelse(var.unit == "range", "sd", var.unit)
    )

# convert meadian and range to mean

data <- data %>%
    mutate(
        mean = ifelse(scale == "median", (lower + upper) / 2, mean),
        scale = ifelse(scale == "median", "", scale)
    )


# convert se to standard deviation

data <- data %>%
    mutate(
        var = ifelse(var.unit == "se" & mean.unit != "coefficient", var * sqrt(n), var),
        var.unit = ifelse(var.unit == "se" & mean.unit != "coefficient", "sd", var.unit)
    )

# convert ci to standard deviation

data <- data %>%
    mutate(
        var = ifelse(var.unit == "ci" & !is.na(lower), (abs(upper - mean) + abs(mean - lower) / 2) / 1.96, var),
        var = ifelse(var.unit == "ci" & is.na(lower), abs(var - mean / 1.96), var),
        var.unit = ifelse(var.unit == "ci", "sd", var.unit)
    )

# conver log to exponential
data <- data %>%
    mutate(
        mean = ifelse(str_detect(scale, "log"), exp(mean), mean),
        var = ifelse(str_detect(scale, "log"), exp(var), var),
        lower = ifelse(str_detect(scale, "log"), exp(lower), lower),
        upper = ifelse(str_detect(scale, "log"), exp(upper), upper),
        scale = ifelse(str_detect(scale, "log"), str_replace(scale, "log", ""), scale)
    )

# reverse arcsine square root transformation
data <- data %>%
    mutate(
        mean = ifelse(str_detect(scale, "arcsine"), sin(mean), mean),
        var = ifelse(str_detect(scale, "arcsine"), sin(var), var),
        lower = ifelse(str_detect(scale, "arcsine"), sin(lower), lower),
        upper = ifelse(str_detect(scale, "arcsine"), sin(upper), upper),
        scale = ifelse(str_detect(scale, "arcsine"), str_replace(scale, "arcsine", ""), scale)
    )

# reverse square root transformation
data <- data %>%
    mutate(
        mean = ifelse(str_detect(scale, "squareroot"), mean^2, mean),
        var = ifelse(str_detect(scale, "squareroot"), (var^2) / 4, var),
        lower = ifelse(str_detect(scale, "squareroot"), lower^2, lower),
        upper = ifelse(str_detect(scale, "squareroot"), upper^2, upper),
        scale = ifelse(str_detect(scale, "squareroot"), str_replace(scale, "squareroot", ""), scale)
    )

# conver logit to proportion
data <- data %>%
    mutate(
        mean = ifelse(str_detect(scale, "logit"), exp(mean) / (1 + exp(mean)), mean),
        var = ifelse(str_detect(scale, "logit"), (exp(var) / (1 + exp(var))^2), var),
        lower = ifelse(str_detect(scale, "logit"), exp(lower) / (1 + exp(lower)), lower),
        upper = ifelse(str_detect(scale, "logit"), exp(upper) / (1 + exp(upper)), upper),
        scale = ifelse(str_detect(scale, "logit"), str_replace(scale, "logit", ""), scale)
    )

# check mean:var ratio

data <- data %>%
    mutate(ratio = abs(mean / var))

# rescale control:treatment to treatment:control
data <- data %>%
    mutate(
        multiplier = ifelse(is.na(multiplier), -1, 1)
    )

# data <- data%>%
#   select(-c(var.unit, scale, sampling_time, sampling))

write.csv(data, here::here("meta-analysis/data/clean_data.csv"), row.names = FALSE)
