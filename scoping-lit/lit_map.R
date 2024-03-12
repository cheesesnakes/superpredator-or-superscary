# Anthropogenic risk effects on animal behavior - systematic review

# mapping literature for review

## import data from WOS -----

setwd("C:/Users/shony/Nextcloud/Work/PhD/Thesis/review/Analysis/scoping-lit")

papers <- read.csv("scope_3.csv")

summary(papers)

## Clean data ----

library(dplyr)
library(tidyr)

papers <- papers%>%
  select(-c(contains(match = "conference", ignore.case = T), 
            contains(match = "book", ignore.case = T), 
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
            X
            )
         )%>%
  mutate(Meeting.Abstract = as.character(Meeting.Abstract),
         Volume = as.character(Volume),
         Document.Type = ifelse(grepl("article*", Document.Type, ignore.case = T), "Article", Document.Type),
         Document.Type = ifelse(grepl("review*", Document.Type, ignore.case = T), "Review", Document.Type),
         Document.Type = ifelse(grepl("editorial*", Document.Type, ignore.case = T), "Editorial Material", Document.Type))

summary(papers)

## Summaries ----

library(ggplot2)

### Number of publications by year

ggplot(papers, aes(Publication.Year))+
  geom_bar(col = "black")+
  labs(x = "Publication Year", y = "Count")+
  theme_bw()+
  theme(text = element_text(size = 20))

ggsave(last_plot(),  filename = "years.png", height = 4, width = 6)
### Article type

ggplot(papers, aes(Document.Type))+
  geom_bar(col = "black")+
  labs(x = "Document Type", y = "Count")+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 20))

ggsave(last_plot(),  filename = "type.png", height = 4, width = 6)

### Language

ggplot(papers, aes(Language))+
  geom_bar(col = "black")+
  labs(x = "Language", y = "Count")+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 20))


ggsave(last_plot(),  filename = "lang.png", height = 4, width = 6)

## sub-sampling ----

#sample <- 1:100

#papers <- papers[sample,]

## Analyse  keywords -----

## separating keywords into individual strings

library(stringr)

keywords <- list()

ind = 1

for (s in papers$Author.Keywords) {
  
  keywords[ind] <- strsplit(s, ";")  
  
  ind = ind + 1
}

keywords <- unlist(keywords)

keywords <- str_trim(keywords) #removing white spaces

keywords <- tolower(keywords)

### frequency

kw_freq <- data.frame(table(keywords))

### word cloud

library(wordcloud)

wordcloud(words = kw_freq$keywords, freq = kw_freq$Freq, colors = brewer.pal(8, "Dark2"), min.freq = 5, max.words = 500)

### grouping

### Exclusion - removing irrelavant key phrases and their cooccuring keywords

keys_in_str <- papers$Author.Keywords[!grepl("amygdala*|ptsd|anxiety|*exitinction|psych*|neuro*", papers$Author.Keywords, ignore.case = T)]

keys_in <- list()

ind = 1

for (s in keys_in_str) {
  
  keys_in[ind] <- strsplit(s, ";")  
  
  ind = ind + 1
}

keys_in <- unlist(keys_in)

keys_in <- str_trim(keys_in) #removing white spaces

keys_in <- tolower(keys_in)

### frequency

kw_in_freq <- data.frame(table(keys_in))

### word cloud

wordcloud(words = kw_in_freq$keys_in, freq = kw_in_freq$Freq, colors = brewer.pal(8, "Dark2"), min.freq = 5, max.words = 500)

## Analyse titles ----

## Separating words in the titles

title_words <- list()

ind = 1

for (w in papers$Article.Title) {
  
  title_words[ind] = strsplit(w, split = " ")
  
  ind = ind + 1

  }

title_words <- unlist(title_words)

title_words <- tolower(title_words)

title_words <- str_trim(title_words)


## Frequency

title_freq <- data.frame(table(title_words))

## removing stop words

library(tidytext)

title_freq <- title_freq%>%
  anti_join(stop_words, by = c("title_words" = "word"))

## Word Cloud

wordcloud(words = title_freq$title_words, freq = title_freq$Freq, colors = brewer.pal(8, "Dark2"), min.freq = 5, max.words = 500)

## Exclusion

## Grouping

## Analyse abstracts ----

## Separating words in the titles

abs_words <- list()

ind = 1

for (w in papers$Abstract) {
  
  abs_words[ind] = strsplit(w, split = " ")
  
  ind = ind + 1
  
}

abs_words <- unlist(abs_words)

abs_words <- tolower(abs_words)

abs_words <- str_trim(abs_words)

## Frequency

abs_freq <- data.frame(table(abs_words))

## removing stop words

abs_freq <- abs_freq%>%
  anti_join(stop_words, by = c("abs_words" = "word"))

## Word Cloud

wordcloud(words = abs_freq$abs_words, freq = abs_freq$Freq, colors = brewer.pal(8, "Dark2"), min.freq = 5, max.words = 500)

## Exclusion

abs_in_str <- papers$Abstract[!grepl("amygdala*|conditioning|ptsd|anxiety|*exitinction|psych*|neuro*", papers$Abstract, ignore.case = T)]

abs_in <- list()

ind = 1

for (s in abs_in_str) {
  
  abs_in[ind] <- strsplit(s, " ")  
  
  ind = ind + 1
}

abs_in <- unlist(abs_in)

abs_in <- str_trim(abs_in) #removing white spaces

abs_in <- tolower(abs_in)

### frequency

abs_in_freq <- data.frame(table(abs_in))

## removing stop words

abs_in_freq <- abs_in_freq%>%
  anti_join(stop_words, by = c("abs_in" = "word"))

### word cloud

wordcloud(words = abs_in_freq$abs_in, freq = abs_in_freq$Freq, colors = brewer.pal(8, "Dark2"), min.freq = 5, max.words = 500)

## Grouping

