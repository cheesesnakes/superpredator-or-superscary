papers <- read.csv("scope_final.csv")

## Clean data ----

library(dplyr)
library(tidyr)

papers <- papers%>%
  select(-c(contains(match = "conference", ignore.case = T), 
            contains(match = "Book", ignore.case = T), 
            Keywords.Plus, 
            contains(match = "addresses", ignore.case = T),
            contains(match = "funding", ignore.case = T),
            contains(match = "cite", ignore.case = T),
            contains(match = "publisher", ignore.case = T),
            contains(match = "abbreviation", ignore.case = T),
            Affiliations,
            Researcher.Ids,
            Research.Areas,
            ORCIDs,
            contains("usage", ignore.case = T),
            contains("Page"),
            Early.Access.Date,
            WoS.Categories,
            Web.of.Science.Index,
            IDS.Number,
            UT..Unique.WOS.ID.,
            Pubmed.Id,
            Open.Access.Designations,
            Hot.Paper.Status,
            Group.Authors,
            Part.Number,
            Authors,
            Supplement,
            X
  )
  )%>%
  mutate(Meeting.Abstract = as.character(Meeting.Abstract),
         Volume = as.character(Volume),
         Document.Type = ifelse(grepl("article*", Document.Type, ignore.case = T), "Article", Document.Type),
         Document.Type = ifelse(grepl("review*", Document.Type, ignore.case = T), "Review", Document.Type),
         Document.Type = ifelse(grepl("editorial*", Document.Type, ignore.case = T), "Editorial Material", Document.Type))

summary(papers)

## Cleaning for rayyan

names(papers) <- tolower(names(papers))

papers <- papers%>%
  mutate(key = 1:length(article.title))%>%
  rename(title = article.title,
         journal = source.title,
         keywords = author.keywords,
         authors = author.full.names,
         year = publication.year)%>%
  mutate(pages = NA,
         month = NA,
         day = NA,
         publisher = NA,
         location = NA,
         url = NA,
         notes = NA
         )%>%
  select(key, title, authors, journal, issn, volume, issue, pages, year, month, day, publisher, location, url, language, notes, abstract)

## subsampling

s <- sample(1:1000, 100)

pilot <- papers[s,]

## export

write.csv(pilot, "pilot_1.csv", row.names = F)
