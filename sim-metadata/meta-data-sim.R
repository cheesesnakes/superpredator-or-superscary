# Effect of fisheries on herbivore behavior

setwd("C:\\Users\\shony\\Nextcloud\\Work\\PhD\\Thesis\\review\\Analysis")

# simulating data for meta analysis

## variables

## function to create simulated studies

authors <- c("foo",  "bar", "baz") # list of authors

year <- seq(1995, 2021, 1) # list of years

N <- 100 # number of studies found

refs <- vector(length = N) # vec of study refs

for (i in 1:N) {
  
  a <- sample(authors, 1)
  
  yr <- sample(year, 1)
  
  refs[i] <- paste0(a , yr)
}

## assigning treatments to studies

treatment <- c("Human", "Natural") # presence of fishing boats

treats <- sample(treatment, N, replace = T)

## assign response variable to studies

effect <- c("Foraging area", "Bite rate", "Giving up density", "Latency to feed") # effect on herbivore behavior

responses <- sample(effect, N, replace = T)
  
## function for state based simulation of data


## combine as data frame

data <- data.frame(refs = sample(refs, 50),
                   treatment = treats,
                   response = responses,
                   treat.effect = rnorm(N),
                   treat.var = rnorm(N),
                   cnt.effect =  rnorm(N),
                   cnt.var = rnorm(N),
                   treat.n = rpois(N, 10),
                   cnt.n = rpois(N, 10))

## Simulating hypothesis

hyps <- function(treat, resp){
  
  if(treat == "Human"){
    
    if(resp == "Bite rate"){
      
      treat.effect = rnorm(1, 2,1)
      
    }else if(resp == "Foraging area"){
      
      treat.effect = rnorm(1, -2,0.5)
      
    }else if(resp == "Latency to feed"){
      
      treat.effect = rnorm(1, 1,0.5)
      
    }else{
      
      treat.effect = rnorm(1, 2,1)
    }
    
  }else{
    
    if(resp == "Bite rate"){
      
      treat.effect = rnorm(1, 1,0.5)
    
      }else if(resp == "Foraging area"){
      
        treat.effect = rnorm(1, -1,0.5)
        
    }else if(resp == "Latency to feed"){
      
      treat.effect = rnorm(1, 0.5,0.5)
      
    }else{
      
      treat.effect = rnorm(1, 0.5,0.5)
    }
    
  }
  
  return(treat.effect)
  
}

## adding hypothesis to data frame

library(purrr)
library(tidyr)
library(dplyr)

data <- data%>%
  mutate(treat.effect = map2_dbl(treatment, response, ~hyps(.x, .y)))

## calculating cohen's d


data <- data%>%
  mutate(d = (treat.effect - cnt.effect)/sqrt((cnt.var^2 + treat.var^2)/2))
  
## bootstrap CI for effect size

boots <- function(e) {
  
  means <- numeric(1000)
  
  for (i in 1:1000) {
    
    means[i] <- mean(sample(e, 1000, replace = T))
    
  }
  
  lower <- quantile(means, 0.025)
  upper <- quantile(means, 0.975)
  
  return(data.frame(lower, upper))
  
}

library(purrr)
library(dplyr)

analysed <- data%>%
  select(treatment, response, d)%>%
  group_by(treatment, response)%>%
  nest()%>%
  mutate(mean.effect =  map(data, ~mean(.x$d)),
            CI = map(data, ~boots(.x$d)))%>%
  unnest(c(mean.effect, CI))%>%
  ungroup()%>%
  select(-data)

## plot

library(ggplot2)

ggplot(data = analysed, aes(response, mean.effect, col = treatment, shape = treatment))+
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), size = 1, col = "white")+
  geom_hline(yintercept = 0, size = 1, lty = 2)+
  scale_color_discrete(name = "Treatment")+
  scale_shape(name = "Treatment")+
  labs(x = "Response", y = "Mean effect ± 95% CI")+
  theme_bw()+
  theme(text = element_text(size = 20),
        legend.position = "top")

ggsave("fig3.png", height = 8, width = 8)

## Across trophic levels and ecosystems

trophic <- c("Top predator", "Mesopredator", "Consumer")

system <- c("Marine",  "Terrestrial")

## combine as data frame

data_2 <- data.frame(refs = refs,
                   trophic = sample(trophic, 50, replace = T),
                   system = system,
                   response = responses,
                   treat.effect = rnorm(N),
                   treat.var = rnorm(N),
                   cnt.effect =  rnorm(N),
                   cnt.var = rnorm(N),
                   treat.n = rpois(N, 10),
                   cnt.n = rpois(N, 10))

## Simulating hypothesis

hyps_2 <- function(treat, resp){
  
  if(trophic == "Top Predator"){
    
    if(resp == "Bite rate"){
      
      treat.effect = rnorm(1, 2,1)
      
    }else if(resp == "Foraging area"){
      
      treat.effect = rnorm(1, -2,1)
      
    }else if(resp == "Latency to feed"){
      
      treat.effect = rnorm(1, 1,1)
      
    }else{
      
      treat.effect = rnorm(1, 2,1)
    }
    
  }else if(trophic == "Mesopredator"){
    
    if(resp == "Bite rate"){
      
      treat.effect = rnorm(1, 1,1)
      
    }else if(resp == "Foraging area"){
      
      treat.effect = rnorm(1, -1,1)
      
    }else if(resp == "Latency to feed"){
      
      treat.effect = rnorm(1, 0.5,1)
      
    }else{
      
      treat.effect = rnorm(1, 1,1)
    }
    
  } else{
    
    if(resp == "Bite rate"){
      
      treat.effect = rnorm(1, 0.5,1)
      
    }else if(resp == "Foraging area"){
      
      treat.effect = rnorm(1, -0.5,1)
      
    }else if(resp == "Latency to feed"){
      
      treat.effect = rnorm(1, 0.25,1)
      
    }else{
      
      treat.effect = rnorm(1, 0.5,1)
    }
  
  }
  return(treat.effect)
  
}

## adding hypothesis to data frame

library(purrr)
library(tidyr)
library(dplyr)

data_2 <- data_2%>%
  mutate(treat.effect = map2_dbl(trophic, response, ~hyps_2(.x, .y)))

## calculating cohen's d


data_2 <- data_2%>%
  mutate(d = (treat.effect - cnt.effect)/sqrt((cnt.var^2 + treat.var^2)/2))

## bootstrap CI for effect size

library(purrr)
library(dplyr)

analysed_2 <- data_2%>%
  select(trophic, system, response, d)%>%
  group_by(trophic, system, response)%>%
  nest()%>%
  mutate(mean.effect =  map(data, ~mean(.x$d)),
         CI = map(data, ~boots(.x$d)))%>%
  unnest(c(mean.effect, CI))%>%
  ungroup()%>%
  select(-data)

## plot

library(ggplot2)

ggplot(data = analysed_2, aes(trophic, mean.effect, col = system, shape = system))+
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), size = 1, col = "white")+
  geom_hline(yintercept = 0, size = 1, lty = 2)+
  scale_color_discrete(name = "Ecosystem")+
  scale_shape(name = "Ecosystem")+
  labs(x = "Trophic Level", y = "Mean effect ± 95% CI")+
  theme_bw()+
  theme(text = element_text(size = 20),
        legend.position = "top")+
  facet_wrap(~response)  

ggsave("fig4.png", height = 10, width = 10)
