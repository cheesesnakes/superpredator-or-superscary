# summary stats for clinchy et al. 2016

setwd("C:\\Users\\shony\\Nextcloud\\Work\\PhD\\Thesis\\review\\Analysis\\scripts-lit")

data <- read.csv("../data-lit/clinchy_et_al_2016_data.csv")

# libraries

library(tidyr)
library(dplyr)

# data cleaning

data <- data%>%
  gather(c(latency:individuals), key = "vars", value = "value")%>%
  mutate(value = as.numeric(value))

# transforming data as per methods

library(MASS)

## function for box cox transformation

bc_trans <- function(data) {
  
  bc <- boxcox(data$value ~ data$treatment, plotit = F)
  
  lambda <- bc$x[which.max(bc$y)]
  
  bc_value <- (data$value^lambda - 1)/lambda
  
  return(bc_value)
}

## function to calculate lambda

lam <- function(data) {
  
  bc <- boxcox(data$value ~ data$treatment, plotit = F)
  
  lambda <- bc$x[which.max(bc$y)]
  
  return(lambda)
}

## tranforming data

library(purrr)

data <- data%>%
  group_by(vars)%>%
  nest()%>%
  mutate(bc_value = map(data, ~bc_trans(.)),
         lambda = map(data, ~lam(.)))%>%
  unnest()%>%
  ungroup()

# data summary - transformed

## sample size

data%>%
  group_by(vars, treatment)%>%
  summarise(n = n())

## means

data%>%
  group_by(vars, treatment)%>%
  summarise(mean = mean(bc_value, na.rm = T),
            sd = sd(bc_value, na.rm = T), 
            n = n(),
            lambda = last(lambda),
            .groups = "drop")%>%
  # inverse box cox trasnform
  mutate(mean = exp(log(1 + lambda*mean)/lambda),
         sd = exp(log(1 + lambda*sd)/lambda),
         se = sd/sqrt(n))%>%
  dplyr::select(treatment, vars, mean)%>%
  spread(vars, mean)

## se 

data%>%
  group_by(vars, treatment)%>%
  summarise(mean = mean(bc_value, na.rm = T),
            sd = sd(bc_value, na.rm = T), 
            n = n(),
            lambda = last(lambda),
            .groups = "drop")%>%
  # inverse box cox trasnform
  mutate(mean = exp(log(1 + lambda*mean)/lambda),
         sd = exp(log(1 + lambda*sd)/lambda),
         se = sd/sqrt(n))%>%
  dplyr::select(treatment, vars, se)%>%
  spread(vars, se)

## Data summary - raw

## mean

data%>%
  group_by(vars, treatment)%>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T), 
            n = n(),
            se = sd/sqrt(n))%>%
  dplyr::select(treatment, vars, mean)%>%
  spread(vars, mean)

## se

data%>%
  group_by(vars, treatment)%>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T), 
            n = n(),
            se = sd/sqrt(n))%>%
  dplyr::select(treatment, vars, se)%>%
  spread(vars, se)


# plot

library(ggplot2)

data%>%
  ggplot(aes(treatment, value))+
  geom_boxplot()+
  facet_wrap(~vars, scales = "free_y")



  
         



