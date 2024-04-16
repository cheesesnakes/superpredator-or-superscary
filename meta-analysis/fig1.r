# making a grid of plots

source("analysis.R")

## adding plots

plot_a <- ggplot(data = data_cor, aes(x = mean, y = reorder(pop_sn, mean), col = cite.key))+
    geom_point(size = 2)+
    geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    xlab("Effect size")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    legend.position = "none")+
    facet_grid(trophic_level~outcome, scales = "free")+
    # italicise x axis
    theme(axis.text.y = element_text(face = "italic"))+
    scale_color_brewer(palette = "Set1")+
    # add title A
    labs(title = "A")

plot_b <- ggplot(data = data_tc, aes(x = smd, y = reorder(pop_sn, smd), col = cite.key))+
    geom_point(size = 2)+
    geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    xlab("Standardised mean difference")+
    ylab("Population")+
    theme_bw()+
    theme(text = element_text(size = 20),
    #remove legend
    legend.position = "none"
    )+
    facet_grid(trophic_level~outcome, scales = "free")+
    # italicise x axis
    theme(axis.text.y = element_text(face = "italic"))+
    scale_color_brewer(palette = "Set1")+
    # add label B
    labs(title = "B")

plot_c <- ggplot(data = data_tc_ci, aes(y = mean, x = treatment, col = cite.key))+
    geom_point(size = 2)+
    geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.05)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    xlab("Effect size")+
    ylab("Population")+
    theme_bw()+
    coord_flip()+
    theme(text = element_text(size = 20),
    legend.position = "none")+
    facet_grid(outcome~pop_sn, scales = "free_x")+
    # italicise facet column
    theme(strip.text.x = element_text(face = "italic"))+
    scale_color_brewer(palette = "Set1")+
    # add label C
    labs(title = "C")

# make grid

library(gridExtra)

fig <- grid.arrange(plot_a, plot_b, plot_c, ncol = 1)

# save fig 

ggsave("fig1.png", fig, width = 12, height = 20, dpi = 300)
