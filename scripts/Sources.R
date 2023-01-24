# Exploring predicts based on source rather than study

# read in the data ----
predicts <- readRDS("data/diversity-2022-04-13-02-33-10-rds/diversity-2022-04-13-02-33-10.rds")

# Lets load the relevant package ----
library(tidyverse)

# Filter predicts for arthropods and then work out multiple group studies ----
multi <-
  predicts %>%
  filter(Phylum == "Arthropoda") %>% # filter only for arthropods
  select(Source_ID, Study_number, SS, SSS, SSB, SSBS, Taxon, 
         Rank, Kingdom, Phylum, Class, Order, Family, Genus, 
         Species, Higher_taxon,  Taxon_name_entered, Parsed_name, 
         Study_common_taxon, Rank_of_study_common_taxon, Predominant_habitat, Use_intensity) %>% # Subset the columns of interest
  droplevels() %>% # drop empty levels 
  mutate_all(na_if,"") %>% # add NAs to all empty cells
  group_by(SS) %>% # group by SS
  mutate(
    Multiple_groups = 
      case_when(
        (length(unique(Higher_taxon))) > 1 ~ "YES",
        (length(unique(Higher_taxon))) == 1 ~ "NO")) %>% # for each SS create a new column: multiple groups = YES/NO - depending on if there's 
  # more than one unique higher taxon within that study
  # Filter the data for studies with multiple groups
  filter(Multiple_groups == "YES") %>% # filter for the rows for studies with multiple groups
  ungroup() 

# Remove that fat df 
rm(predicts)

# For each source, how many rows do we have per taxonomic group? ---- 
  source_htg <- 
  multi %>%
  filter(Higher_taxon != "NA") %>% # some rows have unknown species of insects/arthropods
  group_by(Source_ID) %>% 
  # KEY DIFFERENCE HERE - grouping by Source rather than study to get a conservative estimate on the number of independent datasets
  count(Higher_taxon) %>%
  rename(number="n") %>%
  droplevels()

# HEAT MAP - Create a heat map showing how well represented taxonomic groups are across multi-group SOURCES (not studies) ---- 
source_htg %>%
  ggplot(aes(x = Higher_taxon, y = Source_ID, fill = number)) +
  geom_tile() +
  scale_fill_gradient(trans = 'log') + # log scale
  theme(axis.text.x = element_text(angle = 45, hjust= 1)) +
  theme(axis.text.y  = element_text(size = 7)) 
?rep#geom_text(aes(label = round(log(number), 1)))

