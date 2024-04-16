# export a list of pop_cn and pop_sn together

pop <- data %>%
    select(pop_cn, pop_sn) %>%
    distinct() %>%
    arrange(pop_cn)

write.csv(pop, file = "populations.csv", quote = TRUE)
