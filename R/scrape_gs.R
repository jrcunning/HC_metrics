library(scholar)
library(readxl)
library(tidyverse)

## Get provided list of Haerther Center authors
hc_authors <- read_tsv("data/authors.txt") %>% drop_na()

## Get provided reference list of pre-2019 Haerther Center publications
hc_oldrefs <- read_tsv("data/pre2019_publications.txt", col_names = "reference")

## Get newer pubs that have been input into research metrics form
hc_newrefs <- read_xlsx("data/Research_Metrics.xlsx") %>%
  filter(Category == "Publication") %>%
  select(reference = Citation)

## Create combined hc_references 
hc_references <- bind_rows(hc_oldrefs, hc_newrefs)

# Get publication list for individual author
#get_publications(hc_authors$scholar_id[14])

# Get full publication list for each author from Google Scholar
all_pubs <- hc_authors %>%
  mutate(all_pubs = map(scholar_id, ~ get_publications(.))) %>%
  unnest(all_pubs)

# Subset only Haerther Center publications (since 2012) using fuzzy text matching of titles to the provided reference list
hc_pubs <- all_pubs %>%
  mutate(hc_affil = map_lgl(title, ~ any(agrepl(., hc_references$reference)))) %>%
  filter(hc_affil)  # select only entries with titles that matched

# Remove duplicated publications based on title (e.g., multiple Haerther Center authors on same publication)
hc_pubs <- hc_pubs %>% 
  distinct(title, .keep_all = TRUE)

# Calculate departmental h-index
cites <- hc_pubs %>% 
  arrange(-cites) %>%
  pull(cites)

h.index <- tail(which(cites >= seq_along(cites)), 1)
i10.index <- sum(cites >= 10)

# Summarize citation counts with today's date
todays_totals <- hc_pubs %>%
  summarise(date = Sys.Date(),
            total_pubs = nrow(hc_references),
            total_cites = sum(cites),
            h.index = h.index,
            i10.index = i10.index)

# Append today's totals to output file
write.table(todays_totals, file = "data/citation_counts.txt", sep = "\t", quote = F, 
            row.names = F, col.names = F, append = T)
