# Major groups within orders

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

# ants vs bees vs wasps ----

# Filter for hymenopterans
hymenoptera <- 
  multi %>% 
  filter(Order == "Hymenoptera") %>%
  droplevels()



