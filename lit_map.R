# Anthropogenic risk effects on animal behavior - systematic review

# mapping literature for review

## import data from WOS -----

papers <- read.csv("scope_final.csv")

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
  theme_bw()+
  theme(text = element_text(size = 20))

### Article type

ggplot(papers, aes(Document.Type))+
  geom_bar(col = "black")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme_bw()+
  theme(text = element_text(size = 20))

### Language

ggplot(papers, aes(Language))+
  geom_bar(col = "black")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme_bw()+
  theme(text = element_text(size = 20))

## sub-sampling ----

sample <- 1:100

papers <- papers[sample,]

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

wordcloud(words = kw_in_freq$keys_in, freq = kw_freq$Freq, colors = brewer.pal(8, "Dark2"), min.freq = 5, max.words = 500, scale = c(1,1))

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

## Removing articles and conjugations

title_words <- title_words[!grepl("a|and|of|the|they're|their|because|for|on|in|to|but|by|not|is|between|stud*|from|with|effect*|diff*", title_words, ignore.case = T)]

## Frequency

title_freq <- data.frame(table(title_words))

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

## Removing articles and conjugations

abs_words <- abs_words[!grepl("a|and|of|the|they're|their|because|for|on|in|to|but|by|not|is|between|stud*|from|with|effect*|diff*|*ed|wh*|we|our*|or|be|such|did|it*|result|(c)", abs_words, ignore.case = T)]

## Frequency

abs_freq <- data.frame(table(abs_words))

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

## Removing articles and conjugations

abs_in <- abs_in[!grepl("a|and|of|the|they're|their|because|for|on|in|to|but|by|not|is|between|stud*|from|with|effect*|diff*|*ed|wh*|we|our*|or|be|such|did|it*|result|(c)", abs_in, ignore.case = T)]

### frequency

abs_in_freq <- data.frame(table(abs_in))

### word cloud

wordcloud(words = abs_in_freq$abs_in, freq = abs_in_freq$Freq, colors = brewer.pal(8, "Dark2"), min.freq = 5, max.words = 500)

## Grouping

