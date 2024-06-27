# formatting citations for papers in meta-analysis

# impor data

authors <- read.csv("data/authors.csv")

# split authors into list

authors <- authors %>%
  mutate(author = strsplit(Authors, ", ")) %>%
  unnest(author)

# if list of authors is more than 1 then shorten to First author et al.

authors <- authors %>%
    group_by(cite.key) %>%
    mutate(author = ifelse(n() > 1, paste(author[1], "et al."), author[1])) %>%
    ungroup()

# make citation with year

authors <- authors %>%
  mutate(cite = paste(author, year, sep = ", "))%>%
  distinct(cite.key, cite)

# remove everything before the first space or period in cite

authors <- authors %>%
  mutate(cite = str_replace(cite, "^[^ .]+[ .]", ""))%>%
  mutate(cite = str_trim(cite))

head(authors)
