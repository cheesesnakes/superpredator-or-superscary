# sankey plot exposure_category -> treatment -> outcome

pacman::p_load(networkD3, dplyr, tidyr, stringr)

meta <- read.csv('data/chapter-1_study-info.csv')

colnames(meta)

studies <- meta%>%
    rename(cite.key = File52)%>%
    select(cite.key, exposure_category, treatments, outcomes)

# expand exposure_category

studies <- studies%>%
    mutate(exposure_category = str_split(exposure_category, " \\+ "))%>%
    unnest(exposure_category)%>%
    # rename lethal interacitons to lethal interaciton
    mutate(exposure_category = ifelse(exposure_category == "lethal interactions", "Lethal Interaction", exposure_category))

# expand treatments

studies <- studies%>%
    mutate(treatments = str_split(treatments, " \\+ "))%>%
    unnest(treatments)

# expand outcomes

studies <- studies%>%
    mutate(outcomes = str_split(outcomes, " \\+ "))%>%
    unnest(outcomes)

# clean strings make lower then title

studies <- studies%>%
    mutate(exposure_category = str_to_lower(exposure_category),
           treatments = str_to_lower(treatments),
           outcomes = str_to_lower(outcomes))%>%
    mutate(exposure_category = str_to_title(exposure_category),
              treatments = str_to_title(treatments),
              outcomes = str_to_title(outcomes))

# rename and format outcomes

## AD to Alert Distance

studies <- studies%>%
    mutate(outcomes = ifelse(outcomes == "Ad", "Alert Distance", outcomes))

## FID to Flight Initiation Distance

studies <- studies%>%
    mutate(outcomes = ifelse(outcomes == "Fid", "Flight Initiation Distance", outcomes))

## GUD to Giving Up Density

studies <- studies%>%
    mutate(outcomes = ifelse(outcomes == "Gud", "Giving Up Density", outcomes))

## ED to Escape Distance

studies <- studies%>%
    mutate(outcomes = ifelse(outcomes == "Ed", "Escape Distance", outcomes))

## Prob. Flight to Flight Probability

studies <- studies%>%
    mutate(outcomes = ifelse(outcomes == "Prob. Flight", "Flight Probability", outcomes))

## Prob. Feeding to Foraging Probability

studies <- studies%>%
    mutate(outcomes = ifelse(outcomes == "Prob. Feeding", "Foraging Probability", outcomes))

## replace Feeding with Foraging in string

studies <- studies%>%
    mutate(outcomes = str_replace_all(outcomes, "Feeding", "Foraging"))

## remove Vigilance and Foraging from outcomes

studies <- studies%>%
    filter(outcomes != "Vigilance" & outcomes != "Foraging")

## Vigilance Rabe pop to Vigilance Rate pop

studies <- studies%>%
    mutate(outcomes = str_replace_all(outcomes, "Viglance Rate Pop.", "Vigilance Rate Pop."))

## Vigilsnce Rate pop. to Population Vigilance Rate

studies <- studies%>%
    mutate(outcomes = str_replace_all(outcomes, "Vigilance Rate Pop.", "Population Vigilance Rate"))

## Density to Population Density

studies <- studies%>%
    mutate(outcomes = str_replace_all(outcomes, "Density pop.", "Population Density"))
    
## classify outcome_type as Foraging, Vigilance, Movement or Other

## if Foraging or Feed in string then Foraging

studies <- studies%>%
    mutate(outcome_type = ifelse(str_detect(outcomes, "Foraging|Feed"), "Foraging", "Other"))

## if Vigilance in string then Vigilance

studies <- studies%>%
    mutate(outcome_type = ifelse(str_detect(outcomes, "Vig"), "Vigilance", outcome_type))

## if Movement in string then Movement

studies <- studies%>%
    mutate(outcome_type = ifelse(str_detect(outcomes, "Movement"), "Movement", outcome_type))

## Add other exposure_category

## remove nas and blanks in all columns

studies <- studies%>%
    filter(!is.na(exposure_category) & exposure_category != "",
           !is.na(treatments) & treatments != "",
           !is.na(outcomes) & outcomes != "",
           !is.na(outcome_type) & outcome_type != "")%>%
    filter(treatments != "Natural Predator" & treatments != "Dog")

## make displacement, range size Movement

studies <- studies%>%
    mutate(outcome_type = ifelse(outcomes == "Displacement" | outcomes == "Range Size", "Movement", outcome_type))

# make data frame of nodes

nodes <- data.frame(name = unique(c(studies$exposure_category, studies$treatments, studies$outcomes, studies$outcome_type)))

# make data frame of links

links <- data.frame(source = match(studies$exposure_category, nodes$name) - 1,
                    target = match(studies$treatments, nodes$name) - 1,
                    value = 1)

links <- rbind(links,
                data.frame(source = match(studies$treatments, nodes$name) - 1,
                              target = match(studies$outcome_type, nodes$name) - 1,
                              value = 1))

links <- rbind(links,
                data.frame(source = match(studies$outcome_type, nodes$name) - 1,
                              target = match(studies$outcomes, nodes$name) - 1,
                              value = 1))
# plot

sankeyNetwork(Links = links, Nodes = nodes, 
Source = "source", Target = "target", Value = "value", NodeID = "name", 
units = "Number of datapoints", fontSize = 12, nodeWidth = 10)
