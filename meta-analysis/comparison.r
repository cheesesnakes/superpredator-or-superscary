library(viridis, help, pos = 2, lib.loc = NULL)

# Comparison across hunting, non-hunting disturbance and natural predators --------------------------------------------

meta <- meta %>%
    mutate(cite.key = as.character(cite.key)) %>%
    # choose the first five letters and the last four 
    mutate(cite.key = str_sub(cite.key, 1, 5) %>% paste(str_sub(cite.key, -4, -1), sep = "_"))

data_comp <- data_tc%>%
    left_join(meta, by = c("cite.key"))%>%
    rename(pop_cn = pop_cn.x,
           outcome = outcome.x)%>%
    select(cite.key, group, exposure, pop_cn, outcome, smd, upper, lower)

# clearning exposure data

data_comp <- data_comp%>%
    mutate(exposure = ifelse(group == "human", "hunting", exposure),
           exposure = ifelse(group == "Treatment_1", "non-hunting disturbance", exposure),
           exposure = ifelse(group == "Treatment_2", "hunting", exposure),
           exposure = ifelse(exposure == "human playback", "hunting", exposure),
           exposure = ifelse(exposure == "nonhunting disturbance/hunting?", "non-hunting disturbance", exposure),
           exposure = ifelse(exposure == "hunting/nonhunting disturbance/natural predator playback", "hunting", exposure),
           exposure = ifelse(exposure == "nonhunting disturbance", "non-hunting disturbance", exposure))

# add data_pb where group is not human

data_comp <- data_comp%>%
    bind_rows(data_pb%>%
        filter(group != "human")%>%
        left_join(meta, by = c("cite.key"))%>%
        rename(pop_cn = pop_cn.x,
               outcome = outcome.x)%>%
        select(cite.key, group, exposure, pop_cn, outcome, smd, upper, lower))

# add natural predator to exposure

data_comp <- data_comp%>%
    mutate(exposure = ifelse(exposure != "hunting" & exposure != "non-hunting disturbance", "natural predator", exposure))

# filter out natural predator

data_comp <- data_comp%>%
    filter(exposure != "natural predator")

# add species data

data_comp <- data_comp%>%
    left_join(pop, by = c("pop_cn"))%>%
    mutate(trophic_level = as.factor(trophic_level))

#plotting

data_comp %>%
    filter(outcome != "latency")%>%
    ggplot(aes(x = reorder(pop_sn, smd), y = smd, col = trophic_level))+
    geom_point()+
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    labs(x = "Exposure", y = "Standardised mean difference")+
    facet_grid(outcome~exposure, scales = "free")+
    theme_bw()+
    theme(legend.position = "top")+
    coord_flip()+
    # set legend title
    scale_color_brewer(name = "Trophic level", palette = "Set1")+
    # italicise x-axis labels
    theme(axis.text.y = element_text(face = "italic"))


ggsave("es_comp.png", width = 6, height = 7, dpi = 300)
    