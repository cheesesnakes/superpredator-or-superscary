source('1_cleaning.R', chdir = TRUE)

# export a list of pop_cn and pop_sn together

pop <- data %>%
    select(pop_cn, pop_sn) %>%
    distinct() %>%
    arrange(pop_cn)

#write.csv(pop, file = "populations.csv", quote = TRUE) # run once

# read saved populations.csv

pop <- read.csv(here::here("meta-analysis/data/populations.csv"))

# read animal_traits.csv

traits <- read.csv(here::here("meta-analysis/data/animal_traits.csv"))

# join pop and traits

size <- pop %>%
    left_join(traits, by = c("pop_sn" = "species"))%>%
    select(pop_cn, pop_sn, body.mass, body.mass...units, body.mass...maximum, body.mass...minimum,inTextReference)%>%
    distinct()%>%
    rename(body_mass = body.mass,
              body_mass_units = body.mass...units,
              body_mass_maximum = body.mass...maximum,
              body_mass_minimum = body.mass...minimum,
              in_text_reference = inTextReference)

#write.csv(size, file = "data/size.csv", quote = TRUE)

