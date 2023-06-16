# protocols for inclusion and exclusion of papers for review

## data from WOS database

papers_1 <- read.csv("review_search_1.csv") #data from first search string

str(papers_1)

# removing unused variables

library(tidyr)
library(dplyr)

papers_1 <- papers_1%>%
  select(Authors, Article.Title, Source.Title, Language, Document.Type, Keywords.Plus, Abstract, Publication.Year, DOI, Date.of.Export)

# Analysing keywords

## separating keywords into individual strings

library(stringr)

keywords <- list()

ind = 1

for (s in papers_1$Keywords.Plus) {
  
  keywords[ind] <- strsplit(s, ";")  
  
  ind = ind + 1
}

keywords <- unlist(keywords)

keywords <- str_trim(keywords) #removing white spaces

## frequency of keywords without filtering

key_freq <- data.frame(table(keywords))

## Filtering keywords based on relevance

## regex string

flter <- "cond*|trauma*|amygdala|disorder|aquisition|cort*|startle|antici*|anxi*|therap*|neuro*|phobias|nucle*|extin*|mem*|acquisition"

papers_1%>% # testing
  filter(grepl("acquisition", Keywords.Plus, ignore.case = T))%>%
  select(Article.Title)

## filtering and plotting

filt_key_freq <- key_freq%>%
  filter(Freq > 2,
         !grepl(flter, keywords, ignore.case = T))

filt_key_freq%>%
  ggplot(aes(reorder(keywords, Freq), Freq))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

## creating vector of keywords

filtered_keys <- as.character(filt_key_freq$keywords)

## turning vector into regex string

filtered_keys <- paste(filtered_keys, sep = "|", collapse = "|")

## filtering papers baased on final keyword list

papers_1_filtered <- papers_1%>%
  filter(grepl(filtered_keys, Keywords.Plus, ignore.case = T))

## writing output to csv

write.csv(papers_1_filtered, file = "review_search_1_filtered.csv")
