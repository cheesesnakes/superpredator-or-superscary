# libraries
pacman::p_load(rotl, ape, phytools)

# load species names

species <- read.csv("data/populations.csv", header = TRUE, stringsAsFactors = FALSE)


# list of species names

spp <- unique(species$pop_sn)

# format species names as sentence case

spp <- stringr::str_trim(spp) #trim

spp <- stringr::str_to_sentence(spp)

length(spp)

# rename unmatched species

# Chromis wardi to Chromis

spp <- stringr::str_replace_all(spp, "Chromis wardi", "Chromis")

# Rhea pennata pennata to Rhea

spp <- stringr::str_replace_all(spp, "Rhea pennata pennata", "Rhea")

# get phylogenetic tree

taxa <- tnrs_match_names(spp, context_name = "Animals")

nrow(taxa)

head(taxa)

# retrieve tree

tree <- tol_induced_subtree(ott_ids = ott_id(taxa), label_format = "name")

# extract tip names

tip_names <- tree$tip.label

tip_names <- str_replace_all(tip_names, "_", " ")

# check missing data

setdiff(spp, tip_names)

# replace tip names for renamed species Rhea and Chromis

tip_names <- str_replace_all(tip_names, "Rhea", "Rhea pennata pennata")

tip_names <- str_replace_all(tip_names, "Chromis", "Chromis wardi")

# add _

tip_names <- str_replace_all(tip_names, " ", "_")

# apply to tree

tree$tip.label <- tip_names

# compute branch lengths

set.seed(123)

tree <- compute.brlen(tree, methid = "Grafen", power = 1)

# randomization for polytomy

tree <- multi2di(tree, random = TRUE)

# plot tree

png("figures/phylo.png", width = 800, height = 800)

plot(tree, type = "phylogram", cex = 1.25, label.offset = 0.01)

dev.off()
