# converting mean and variablity to workable units

source("clearning.R", echo = FALSE)

# set type for all

data <- data%>%
    mutate(mean = as.numeric(mean),
           var = as.numeric(var),
           lower = as.numeric(lower),
           upper = as.numeric(upper),
           sampling_time = as.numeric(sampling_time))

# convert percentage to proportion

data <- data%>%
    mutate(mean = ifelse(scale == "percentage" & mean.unit != "coefficient", mean/100, mean),
           var = ifelse(scale == "percentage" & mean.unit != "coefficient", var/100, var),
           lower = ifelse(scale == "percentage" & mean.unit != "coefficient", lower/100, lower),
           upper = ifelse(scale == "percentage" & mean.unit != "coefficient", upper/100, upper),
           scale = ifelse(scale == "percentage" & mean.unit != "coefficient", "proportion", scale))

# convert  log-proportion to proportion

data <- data%>%
    mutate(mean = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", exp(mean), mean),
           var = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", exp(var), var),
           lower = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", exp(lower), lower),
           upper = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", exp(upper), upper),
           scale = ifelse(scale == "Log-proportion" & mean.unit != "coefficient", "proportion", scale))

# conver credible intervals to standard deviation

data <- data%>%
    mutate(var = ifelse(var.unit == "credible interval" , abs((upper-lower)/(2*1.96)), var),
    var.unit = ifelse(var.unit == "credible interval", "se", var.unit))
    
# convert interquartile range to standard deviation

data <- data%>%
    mutate(var = ifelse(var.unit == "interquantile", abs((lower-upper)/(2*1.35)), var),
    var.unit = ifelse(var.unit == "interquantile", "sd", var.unit))

# convert range to standard deviation

data <- data%>%
    mutate(var = ifelse(var.unit == "range", abs((upper-lower)/(2*3.29)), var),
    var.unit = ifelse(var.unit == "range", "sd", var.unit))

# convert meadian and range to mean

data <- data%>%
    mutate(mean = ifelse(scale == "median", (lower+upper)/2, mean),
    scale = ifelse(scale == "median", "", scale))

# conver sd to standard deviation

data <- data%>%
    mutate(var = ifelse(var.unit == "sd", var, var))

# convert se to standard deviation

data <- data%>%
    mutate(var = ifelse(var.unit == "se" & mean.unit != "coefficient", var*sqrt(n), var),
    var.unit = ifelse(var.unit == "se" & mean.unit != "coefficient", "sd", var.unit))

# convert ci to standard deviation

data <- data%>%
    mutate(var = ifelse(var.unit == "ci" & !is.na(lower), (abs(upper-mean) + abs(mean-lower)/2)/1.96, var),
           var = ifelse(var.unit == "ci" & is.na(lower), abs(var-mean/1.96), var),
           var.unit = ifelse(var.unit == "ci", "sd", var.unit))

# check mean:var ratio

data <- data%>%
    mutate(ratio = abs(mean/var))

#data <- data%>%
 #   select(-c(var.unit, scale, sampling_time, sampling))

write.csv(data, "data.csv", row.names = FALSE)
