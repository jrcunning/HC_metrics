library(scholar)
library(tidyverse)

## Get provided list of Haerther Center authors
hc_authors <- read_tsv("data/authors.txt") %>% drop_na()

## Get provided reference list of Haerther Center publications
hc_references <- read_tsv("data/publications.txt", col_names = "reference")

# Get publication list for individual author
#get_publications(hc_authors$scholar_id[14])

# Get full publication list for each author from Google Scholar
all_pubs <- hc_authors %>%
  mutate(all_pubs = map(scholar_id, ~ get_publications(.))) %>%
  unnest(all_pubs)

# Subset only Haerther Center publications using fuzzy text matching of titles to the provided reference list
hc_pubs <- all_pubs %>%
  mutate(hc_affil = map_lgl(title, ~ any(agrepl(., hc_references$reference)))) %>%
  filter(hc_affil)  # select only entries with titles that matched

# Remove duplicated publications based on title (e.g., multiple Haerther Center authors on same publication)
hc_pubs <- hc_pubs %>% 
  distinct(title, .keep_all = TRUE)

# Summarize citation counts with today's date
todays_totals <- hc_pubs %>%
  summarise(date = Sys.Date(),
            total_pubs = nrow(.),
            total_cites = sum(cites))

# Append today's totals to output file
write.table(todays_totals, file = "data/citation_counts.txt", sep = "\t", quote = F, 
            row.names = F, col.names = F, append = T)
