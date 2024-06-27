# libraries
pacman::p_load(rotl, ape, phytools)

# load species names

species <- read.csv("data/populations.csv", header = TRUE, stringsAsFactors = FALSE)


# list of species names

species <- unique(species$pop_sn)

# format species names as sentence case

species <- tolower(species)

species <- stringr::str_trim(species) #trim

species <- stringr::str_to_sentence(species)

length(species)

species

# get phylogenetic tree

taxa <- tnrs_match_names(species)

nrow(taxa)

head(taxa)

# retrieve tree

tree <- tol_induced_subtree(ott_ids = ott_id(taxa), label_format = "name")

# extract tip names

tip_names <- tree$tip.label

tip_names <- str_replace_all(tip_names, "_", " ")

# check missing data

setdiff(species, tip_names)

# compute branch lengths

set.seed(123)

tree <- compute.brlen(tree, methid = "Grafen", power = 1)

# randomization for polytomy

tree <- multi2di(tree, random = TRUE)

# plot tree

plot(tree, type = "phylogram", cex = 0.5, label.offset = 0.01)
