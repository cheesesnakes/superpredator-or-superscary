# export a list of pop_cn and pop_sn together

pop <- data %>%
    select(pop_cn, pop_sn) %>%
    distinct() %>%
    arrange(pop_cn)

write.csv(pop, file = "populations.csv", quote = TRUE)

# read saved populations.csv

pop <- read.csv("data/populations.csv")

# read animal_traits.csv

traits <- read.csv("data/animal_traits.csv")

# join pop and traits

size <- pop %>%
    left_join(traits, by = c("pop_sn" = "species"))%>%
    select(pop_cn, pop_sn, body.mass, body.mass...units, body.mass...maximum, inTextReference)%>%
    distinct()%>%
    rename(body_mass = body.mass,
              body_mass_units = body.mass...units,
              body_mass_maximum = body.mass...maximum,
              in_text_reference = inTextReference)

write.csv(size, file = "size.csv", quote = TRUE)
