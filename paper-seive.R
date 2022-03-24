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


keywords[!grep("cond*", keywords, ignore.case = T)]
