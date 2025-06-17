# sankey plot exposure_category -> treatment -> outcome

pacman::p_load(networkD3, dplyr, tidyr, stringr, here, webshot2)

here::i_am("meta-analysis/3-1_sankey.R")
meta <- read.csv(here("meta-analysis/data", "chapter-1_study-info.csv"))

colnames(meta)

studies <- meta %>%
    rename(cite.key = File52) %>%
    select(cite.key, exposure_category, treatments, outcomes)

# expand variables with multiple values

studies <- studies %>%
    mutate(
        exposure_category = str_split(exposure_category, " \\+ "),
        treatments = str_split(treatments, " \\+ "),
        outcomes = str_split(outcomes, " \\+ ")
    ) %>%
    unnest(exposure_category) %>%
    unnest(treatments) %>%
    unnest(outcomes) %>%
    # rename lethal interacitons to lethal interaciton
    mutate(exposure_category = ifelse(exposure_category == "lethal interactions", "Lethal Interaction", exposure_category)) %>%
    mutate_at(
        c("exposure_category", "treatments", "outcomes"),
        ~ str_trim(str_to_title(.x)) # trim whitespace
    )

# rename and format outcomes

studies <- studies %>%
    mutate(
        treatments = case_when(
            treatments == "Pedestrian" ~ "Pedestrians",
            treatments == "Pedestrians" ~ "Pedestrians",
            treatments == "Road" ~ "Roads",
            treatments == "Conflict" ~ "Human - Animal Conflict",
            treatments == "Generic Disturbance" ~ "Unspecified Disturbance",
            TRUE ~ treatments
        ),
        outcomes = case_when(
            outcomes == "Ad" ~ "Alert Distance",
            outcomes == "Fid" ~ "Flight Initiation Distance",
            outcomes == "Gud" ~ "Giving Up Density",
            outcomes == "Ed" ~ "Escape Distance",
            outcomes == "Prob. Flight" ~ "Flight Probability",
            outcomes == "Prob. Feeding" ~ "Foraging Probability",
            TRUE ~ outcomes
        ),
        # fix spelling errors
        outcomes = str_replace_all(outcomes, "Feeding", "Foraging"),
        outcomes = str_replace_all(outcomes, "Viglance Rate Pop.", "Vigilance Rate Pop."),
        outcomes = str_replace_all(outcomes, "Vigilance Rate Pop.", "Vigilance Rate"),
        outcomes = str_replace_all(outcomes, "Collective Vigilance", "Vigilance Rate"),
        outcomes = str_replace_all(outcomes, "Density pop.", "Population Density")
    ) %>%
    filter(
        !treatments %in% c("Human Cue", "Natural Predator", "Natural Perdator", "Dog"),
        !outcomes %in% c("Vigilance", "Foraging")
    )


## classify outcome_type as Foraging, Vigilance, Movement or Other

studies <- studies %>%
    mutate(
        outcome_type = case_when(
            str_detect(outcomes, "Foraging|Feed") ~ "Foraging",
            str_detect(outcomes, "Vig") ~ "Vigilance",
            str_detect(outcomes, "Movement") ~ "Movement",
            outcomes %in% c("Displacement", "Range Size") ~ "Movement",
            TRUE ~ "Other"
        )
    )

## remove nas and empty strings

studies <- studies %>%
    filter(if_all(everything(), ~ !is.na(.) & str_trim(.) != ""))

## remove erroneous connections due to multiple treatments

studies <- studies %>%
    filter(
        !(
            (exposure_category == "Active Interaction" & treatments %in% c("Hunting", "Roads")) |
                (exposure_category == "Lethal Interaction" & treatments == "Hiking") |
                (exposure_category == "Passive Interaction" & treatments %in% c("Hiking", "Sonar"))
        )
    )

## Remove unneded outcome types

studies <- studies %>%
    filter(!outcomes %in% c(
        "Flight Probability", "Density", "Escape Distance",
        "Giving Up Density", "Flight Initiation Distance",
        "Alert Distance", "Fecal Cortisol", "Group Size",
        "Abundance", "Return Time"
    ))

# Rename active and passive interactions to Active non-lethal interactions and passive non-lethal interactions

studies <- studies %>%
    mutate(
        exposure_category = case_when(
            exposure_category == "Active Interaction" ~ "Active Non-lethal Interaction",
            exposure_category == "Passive Interaction" ~ "Passive Non-lethal Interaction",
            TRUE ~ exposure_category
        )
    )

# make data frame of nodes

nodes <- c(studies$exposure_category, studies$treatments, studies$outcomes, studies$outcome_type)

nodes <- data.frame(table(nodes))

nodes$name <- paste0(nodes$nodes, " (", nodes$Freq, ")")

# make data frame of links

links <- data.frame(
    source = match(studies$exposure_category, nodes$nodes) - 1,
    target = match(studies$treatments, nodes$nodes) - 1,
    value = 1
)

links <- rbind(
    links,
    data.frame(
        source = match(studies$treatments, nodes$nodes) - 1,
        target = match(studies$outcome_type, nodes$nodes) - 1,
        value = 1
    )
)

links <- rbind(
    links,
    data.frame(
        source = match(studies$outcome_type, nodes$nodes) - 1,
        target = match(studies$outcomes, nodes$nodes) - 1,
        value = 1
    )
)

nodes <- data.frame(name = nodes$name)

# plot

sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "source", Target = "target", Value = "value", NodeID = "name",
    units = "Number of datapoints", fontSize = 12, nodeWidth = 10
)

sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "source", Target = "target", Value = "value", NodeID = "name",
    units = "Number of datapoints", fontSize = 20, nodeWidth = 20
) %>%
    saveNetwork(file = here("meta-analysis/figures", "sankey.html"), selfcontained = TRUE)

webshot(here("meta-analysis/figures", "sankey.html"),
    file = here("meta-analysis/figures", "sankey.png"),
    vwidth = 1000, vheight = 800, zoom = 2, delay = 0.2
)
