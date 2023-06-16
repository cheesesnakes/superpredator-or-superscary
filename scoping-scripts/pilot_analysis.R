# analysing pilot data for intitial screening

setwd("C:/Users/shony/Nextcloud/Work/PhD/Thesis/review/Analysis/scoping-lit")

pilot <- read.csv("pilot_1_rev.csv")

## cleaning --------

summary(pilot)

head(pilot)

library(dplyr)
library(tidyr)
library(stringr)

## function to split notes from rayyan

note_split <- function(notes) {
  
  test <- str_split(notes, "    | \\|")
  
  test <- unlist(test, use.names = T)
  
  test <- str_trim(test)
  
  test <- str_split(test, ":", simplify = T)
  
  test <- data.frame(test)
  
  test <- test%>%
    unite("value",X2:colnames(test)[ncol(test)], sep = ":")%>%
    rename(var = X1)%>%
    mutate(id = 1)
  
  test <- pivot_wider(test, id_cols = id,  names_from = var, values_from = value)
  
  return(test[2:ncol(test)])
  
}

## Splitting notes column

library(purrr)

pilot <- pilot%>%
  mutate(data = map(notes, ~note_split(.)))%>%
  select(-notes)%>%
  unnest(data)

## filtering pilotted papers

pilot <- pilot[!is.na(pilot$`RAYYAN-INCLUSION`),]

summary(pilot)
head(pilot)

## function to split reviewer and inclusion status

inc <- function(stat) {
  
  in_stat <- stat
  
  in_stat <- str_split(in_stat, "=>",simplify = T)
  
  in_stat <- str_remove_all(string = in_stat, pattern = "[:punct:]")
  
  in_stat <- str_trim(in_stat)
  
  reviewer = in_stat[1]
  
  status = in_stat[2]
  
  return(data.frame(reviewer, status))
  
}

## cleaning review status

pilot <- pilot%>%
  mutate(data = map(`RAYYAN-INCLUSION`, ~inc(.)))%>%
  select(-`RAYYAN-INCLUSION`)%>%
  unnest(data)

## function to split user notes

info_split <- function(info_str) {
  
  if(is.na(info_str)){
    
    return(NA)
    
  }else{
    
    info <- info_str
    
    info <- str_split(info, "=>")
    
    info <- unlist(info)
    
    info <- info[2]
    
    info <- str_split(info, ";")
    
    info <- unlist(info)
    
    info <- str_split(info,  ":", simplify = T)
    
    colnames(info) <- c("var", "value")
    
    info <- data.frame(info)
    
    info$value <- str_remove_all(info$value, "\\}|\\]|\\\"")
    
    info$var <- str_remove_all(info$var, "[:punct:]")
    
    info <- info%>%
      mutate(id = 1,
             var = str_trim(var),
             value = str_trim(value))%>%
      pivot_wider(id_cols = id, names_from = var, values_from = value)%>%
      select(-id)
    
    return(info)
    
  }
  
}

## splitting user notes

pilot <- pilot%>%
  mutate(data = map(`USER-NOTES`, ~info_split(.)))%>%
  select(-`USER-NOTES`)%>%
  unnest(data)

## cleaning labels

pilot$`RAYYAN-LABELS` <- str_remove_all(pilot$`RAYYAN-LABELS`, ":")

## selecting relavant vars

pilot <- pilot%>%
  select(-c(month, day, issn, volume, issue, pages, url, publisher, pubmed_id, pmc_id, `Times Cited in Web of Science Core Collection`, `Total Times Cited`, `Cited Reference Count`))


## Analysis and plotting --------

library(ggplot2)

theme_set(theme_bw()+
            theme(text = element_text(size = 20)))

## hit rate

pilot%>%
  count(status)%>%
  ggplot(aes(status, n))+
  geom_col(col = "black")

pilot%>%
  count(status)%>%
  spread(status, n)%>%
  summarise(hit.rate = Included/(Excluded + Included + Maybe))

## population studied

### Function to split data 

data_split <- function(data){
  
  pops <- data
  
  pops <- str_split(pops, ",")
  
  pops <- unlist(pops)
  
  pops <- str_trim(pops)
  
  pops <- data.frame(table(pops))
  
  return(pops)
  
}

pops <- data_split(pilot$population)

pops%>%
  ggplot(aes(reorder(pops, -Freq), Freq))+
  geom_col()+
  coord_flip()+
  labs(x = "Population Studied", y = "No. of studies")
  
ggsave(last_plot(), filename = "fig_4.png", height = 6, width = 8)

## interventions

### merging exposure and interventions

pilot <- pilot%>%
  mutate(exposure = ifelse(is.na(exposure), intervention, exposure),
         # cleaning exposure data
         exposure = ifelse(grepl("development*", exposure), "development gradient", exposure),
         exposure = ifelse(grepl("call*|playback*", exposure), "human playback", exposure),
         exposure = ifelse(grepl("activit*", exposure), "human activity", exposure),
         exposure = ifelse(grepl("hunt*", exposure), "hunting", exposure))



exposure <- data_split(pilot$exposure)

exposure%>%
  ggplot(aes(reorder(pops, Freq), Freq))+
  geom_col()+
  coord_flip()+
  labs(x = "Exposure treatment", y = "No. of studies")

ggsave(last_plot(), filename = "fig_1.png", height = 6, width = 8)

## controls

pilot <- pilot%>%
  mutate(control = ifelse(grepl("none", control), "none", control),
         control = ifelse(grepl("lynx|puma", control), "natural predator",control))

controls <- data_split(pilot$control)

controls%>%
  ggplot(aes(reorder(pops, Freq), Freq))+
  geom_col()+
  coord_flip()+
  labs(x = "Control", y = "No. of studies")

ggsave(last_plot(), filename = "fig_2.png", height = 6, width = 8)

## outcomes

pilot <- pilot%>%
  mutate(outcome = ifelse(grepl("activity times", outcome), "activity time", outcome),
         outcome = ifelse(grepl("foraging\n", outcome), "foraging behavior", outcome))

outcomes <- data_split(pilot$outcome)

outcomes%>%
  ggplot(aes(reorder(pops, Freq), Freq))+
  geom_col()+
  coord_flip()+
  labs(x = "Outcomes", y = "No. of studies")

ggsave(last_plot(), filename = "fig_3.png", height = 6, width = 8)

## export for full text screening

full_text <- filter(pilot, status == "Included" | status == "Maybe")

write.csv(full_text, "pilot_full-text.csv", row.names = F, na = "")
