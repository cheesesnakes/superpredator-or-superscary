# converting mean and variablity to workable units

source("clearning.R", echo = FALSE)

# set type for all

data <- data%>%
    mutate(mean = as.numeric(mean),
           var = as.numeric(var),
           lower = as.numeric(lower),
           upper = as.numeric(upper),
           sampling_time = as.numeric(sampling_time))

# convert proportions to absolute numbers

# time
data <- data%>%
    mutate(mean = ifelse(scale == "proportion" & mean.unit == "time", mean*sampling_time, mean),
           var = ifelse(scale == "proportion" & mean.unit == "time", var*sampling_time, var),
           lower = ifelse(scale == "proportion" & mean.unit == "time", lower*sampling_time, lower),
           upper = ifelse(scale == "proportion" & mean.unit == "time", upper*sampling_time, upper))

# n

data <- data%>%
    mutate(mean = ifelse(scale == "proportion" & mean.unit == "n", mean*n, mean),
           var = ifelse(scale == "proportion" & mean.unit == "n", var*n, var),
           lower = ifelse(scale == "proportion" & mean.unit == "n", lower*n, lower),
           upper = ifelse(scale == "proportion" & mean.unit == "n", upper*n, upper))

# convert percentages to absolute numbers

# time

data <- data%>%
    mutate(mean = ifelse(scale == "percentage" & mean.unit == "time", mean*sampling_time/100, mean),
           var = ifelse(scale == "percentage" & mean.unit == "time", var*sampling_time/100, var),
           lower = ifelse(scale == "percentage" & mean.unit == "time", lower*sampling_time/100, lower),
           upper = ifelse(scale == "percentage" & mean.unit == "time", upper*sampling_time/100, upper))

# n

data <- data%>%
    mutate(mean = ifelse(scale == "percentage" & mean.unit == "n", mean*n/100, mean),
           var = ifelse(scale == "percentage" & mean.unit == "n", var*n/100, var),
           lower = ifelse(scale == "percentage" & mean.unit == "n", lower*n/100, lower),
           upper = ifelse(scale == "percentage" & mean.unit == "n", upper*n/100, upper))

# convert  log-proportion to time

data <- data%>%
    mutate(mean = ifelse(scale == "log-proportion" & mean.unit == "time", exp(mean)*sampling_time, mean),
           var = ifelse(scale == "log-proportion" & mean.unit == "time", exp(var)*sampling_time, var),
           lower = ifelse(scale == "log-proportion" & mean.unit == "time", exp(lower)*sampling_time, lower),
           upper = ifelse(scale == "log-proportion" & mean.unit == "time", exp(upper)*sampling_time, upper))

# conver arcsine square root proportions to time

data <- data%>%
    mutate(mean = ifelse(scale == "arcsine square root proportion" & mean.unit == "time", sin(mean)^2*sampling_time, mean),
           var = ifelse(scale == "arcsine square root proportion" & mean.unit == "time", sin(var)^2*sampling_time, var),
           lower = ifelse(scale == "arcsine square root proportion" & mean.unit == "time", sin(lower)^2*sampling_time, lower),
           upper = ifelse(scale == "arcsine square root proportion" & mean.unit == "time", sin(upper)^2*sampling_time, upper))

# conver credible intervals to variance

data <- data%>%
    mutate(var = ifelse(var.unit == "credible interval", abs((upper-lower)/(2*1.96)), var))
    
# convert interquartile range to variance

data <- data%>%
    mutate(var = ifelse(var.unit == "interquantile", abs((lower-upper)/(2*1.35)), var))

# convert range to variance

data <- data%>%
    mutate(var = ifelse(var.unit == "range", abs((upper-lower)/(2*3.29)), var))

# convert meadian and range to mean

data <- data%>%
    mutate(mean = ifelse(mean.unit == "median", (lower+upper)/2, mean))

# conver sd to variance

data <- data%>%
    mutate(var = ifelse(var.unit == "sd", var, var))

# convert se to variance

data <- data%>%
    mutate(var = ifelse(var.unit == "se", (var), var))

# convert ci to variance

data <- data%>%
    mutate(var = ifelse(var.unit == "ci" & !is.na(lower), (abs(upper-lower)/(2*1.96)), var),
           var = ifelse(var.unit == "ci" & is.na(lower), abs(var/1.96), var))

# check mean:var ratio

data <- data%>%
    mutate(ratio = abs(mean/var))

#data <- data%>%
 #   select(-c(var.unit, scale, sampling_time, sampling))

write.csv(data, "data.csv", row.names = FALSE)
