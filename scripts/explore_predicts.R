# Exploration of PREDICTS
# Justin Isip 
# Here is a script which does the following in order
# Explore the WHOLE predicts database
# Create a subset of predicts for arthropods
# Nice bit of code to filter arthropods for studies that look at multiple groups
# Exploring multiple group studies
# Heat map of how well represented taxonomic groups of arthropods are across studies on multiple groups


# read in the data ----
predicts <- readRDS("data/diversity-2022-04-13-02-33-10-rds/diversity-2022-04-13-02-33-10.rds")

# Lets load the relevant package ----
library(tidyverse)


# Exploration on all of predicts (not important) ----

# This shows me the number of rows per higher taxonomic group across all of predicts

higher_taxon <- predicts %>%
  group_by(Higher_taxon) %>%
  summarise(n()) %>%
  rename(number="n()") 
  
  #spread(Higher_taxon, number)

# This shows me the number of rows per study common taxon across all of predicts

study_taxon <- predicts %>%
  group_by(Study_common_taxon) %>%
  summarise(n()) %>%
  rename(number="n()")


# unique(predicts$Phylum) find out the levels of a column



# Subset predicts for arthropods and columns of interest ----

arthropods <-
    predicts %>%
    filter(Phylum == "Arthropoda") %>% # filter only for arthropods
    select(Source_ID, Study_number, SS, SSS, SSB, SSBS, Taxon, 
    Rank, Kingdom, Phylum, Class, Order, Family, Genus, 
    Species, Higher_taxon,  Taxon_name_entered, Parsed_name, 
    Study_common_taxon, Rank_of_study_common_taxon, Predominant_habitat, Use_intensity) %>% # Subset the columns of interest
    droplevels()

# Exploration of arthropods (not important) ----
# How many rows per higher taxonomic group for arthropods, every row is a study

higher_tax <- 
    arthropods %>%
    mutate_all(na_if,"") %>% 
    group_by(Higher_taxon) %>%
    summarise(n()) %>%
    rename(number="n()") %>%
    arrange(desc(number)) 

# Here's a plot showing the above

higher_tax %>%
    ggplot(aes(x = reorder(Higher_taxon, number),
    colour = Higher_taxon,
    fill = Higher_taxon,
    y = number)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    geom_text(aes(label = number), hjust = -0.3)

# How many rows for each taxon_name_entered across studies 

taxon_name <- 
    arthropods %>%
    mutate_all(na_if,"") %>% 
    group_by(Taxon_name_entered) %>%
    summarise(n()) %>%
    rename(number="n()") %>%
    arrange(desc(number)) %>% 
    droplevels()

# How many distinct species per higher taxonomic group?

distinct_sp <- 
    arthropods  %>% 
    mutate_all(na_if,"") %>%
    group_by(Higher_taxon) %>%
    summarise(n_distinct(Species)) %>%
    rename(Species="n_distinct(Species)") %>%
    arrange(desc(Species)) 

# Here's a plot showing the above

distinct_sp %>%
    ggplot(aes(x = reorder(Higher_taxon, Species), 
    colour = Higher_taxon,
    fill = Higher_taxon, 
    y = Species)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    geom_text(aes(label = Species), hjust = -0.3)

# How many distinct genera per higher taxonomic group?

distinct_genera <- 
    arthropods  %>% 
    mutate_all(na_if,"") %>%
    group_by(Higher_taxon) %>%
    summarise(n_distinct(Genus)) %>%
    rename(Genus="n_distinct(Genus)") %>%
    arrange(desc(Genus)) 

# Here's a plot showing the above

distinct_genera %>%
    ggplot(aes(x = Higher_taxon, colour = Higher_taxon, fill = Higher_taxon, y = Genus)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    geom_text(aes(label = Genus), hjust = -0.3)

# Look at how many studies (rather than multiple rows) there are for each study_common_taxon group?

study_common <- 
    arthropods %>%
    mutate_all(na_if,"") %>%
    select(SS, Study_common_taxon) %>%
    unique() %>% # what is the study common taxon for each unique study? 
    group_by(Study_common_taxon) %>% # Looks like there's 516 studies for arthropods
    summarise(n()) %>%
    rename(number="n()") %>%
    arrange(desc(number)) %>%
    droplevels() 
  
# Here's a plot showing the above

study_common %>%
    ggplot(aes(x = reorder(Study_common_taxon,number), 
    colour = Study_common_taxon, 
    fill = Study_common_taxon,
    y = number)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    geom_text(aes(label = number), hjust = -0.3)

# There are studies with an NA for study_common_taxon - is it because they've been identified to species level?

# Look at how many unique studies there are for each Rank_of_study_common_taxon group?

rank <- 
    arthropods %>%
    mutate_all(na_if,"") %>%
    select(SS, Rank_of_study_common_taxon) %>%
    unique() %>% # what is the study common taxon for each unique study? 
    droplevels() %>% # Looks like there's 516 studies for arthropods
    group_by(Rank_of_study_common_taxon) %>%
    summarise(n()) %>%
    rename(number="n()") %>%
    arrange(desc(number)) 

# Here's a plot showing the above

rank %>%
    ggplot(aes(x = Rank_of_study_common_taxon, 
    colour = Rank_of_study_common_taxon, 
    fill = Rank_of_study_common_taxon, 
    y = number)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    geom_text(aes(label = number), hjust = -0.3)

# Working out multiple groups studies? (create multi) ----

# arthropods %>% pull(Higher_taxon) same as arthropods$Higher_taxon

# arthropods %>%
# group_by(Higher_taxon) %>%
# tally() - same as summarise(n())

# distinct(arthropods, Higher_taxon) - gives me a list of the distinct factors of HT
# unique(arthropods$Higher_taxon) - similar but not dplyr

# Here's the magic to get a yes or no for each row depending on whether its part of a multi-group study

multi <-
    arthropods %>%
    mutate_all(na_if,"") %>%
    group_by(SS) %>% 
    mutate(
    Multiple_groups = 
    case_when(
    (length(unique(Higher_taxon))) > 1 ~ "YES",
    (length(unique(Higher_taxon))) == 1 ~ "NO")) %>% 
    
    # Filter the data for studies with multiple groups
    filter(Multiple_groups == "YES") %>%
    droplevels() %>%
    ungroup() 

# Exploration of multiple groups ----

# For those studies on multiple groups, how many rows do we have per HTG? 

how_many <- 
  multi %>%
  group_by(Higher_taxon) %>%
  count() %>%
  rename(number="n") %>%
  arrange(desc(number)) 

# Here's a plot showing the above

how_many %>%
  ggplot(aes(x = reorder(Higher_taxon, number),
             colour = Higher_taxon,
             fill = Higher_taxon,
             y = number)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = number), hjust = -0.3)

# How many distinct species per higher taxonomic group?
distinct_sp_htg <- 
  multi %>% 
  mutate_all(na_if,"") %>%
  group_by(Higher_taxon) %>%
  summarise(n_distinct(Species)) %>%
  rename(Species="n_distinct(Species)") %>%
  arrange(desc(Species)) 

# Here's a plot showing the above

distinct_sp_htg %>%
  ggplot(aes(x = reorder(Higher_taxon, Species),
             colour = Higher_taxon, 
             fill = Higher_taxon,
             y = Species)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = Species), hjust = -0.3)

# How many distinct species per higher taxonomic group per study? (for multiple groups)

distinct_sp_ss <- 
  multi  %>% 
  mutate_all(na_if,"") %>%
  filter(Higher_taxon != "NA") %>% 
  group_by(SS, Higher_taxon) %>%
  summarise(n_distinct(Species)) %>%
  rename(Species="n_distinct(Species)") 

# How many distinct parsed_names_entered per higher taxonomic group per study? (for multiple groups)

distinct_sp_parsed <- 
  multi  %>% 
  mutate_all(na_if,"") %>%
  filter(Higher_taxon != "NA") %>% 
  group_by(SS, Higher_taxon) %>%
  summarise(n_distinct(Parsed_name)) %>%
  rename(Parsed_name="n_distinct(Parsed_name)") 

# How many distinct taxon_name_entered per higher taxonomic group per study? (for multiple groups)

distinct_taxon <- 
  multi  %>% 
  mutate_all(na_if,"") %>%
  filter(Higher_taxon != "NA") %>% 
  group_by(Higher_taxon) %>%
  summarise(n_distinct(Taxon_name_entered)) %>%
  rename(N_taxon_name_entered="n_distinct(Taxon_name_entered)") 

# Here's a plot showing the above
distinct_taxon %>%
  ggplot(aes(x = reorder(Higher_taxon, N_taxon_name_entered),
             colour = Higher_taxon, 
             fill = Higher_taxon,         
             y = N_taxon_name_entered)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = N_taxon_name_entered), hjust = -0.3) +
  theme(legend.position = "none") 


# NROWS_HTG - For each study, how many rows do we have for each higher taxonomic group?  ---- 

nrows_htg <- 
    multi %>%
    filter(Higher_taxon != "NA") %>% # some rows have unknown species of insects/arthropods
    group_by(SS) %>% # there are 131 studies on multiple groups 
    count(Higher_taxon) %>%
    rename(number="n") %>%
    droplevels()


# 1ST HEAT MAP - Create a heat map showing how well represented taxonomic groups are across multi-group studies ---- 

nrows_htg %>%
  ggplot(aes(x = Higher_taxon, y = SS, fill = number)) +
  geom_tile() +
  scale_fill_gradient(trans = 'log') + # log scale
  theme(axis.text.x = element_text(angle = 45, hjust= 1)) +
  theme(axis.text.y  = element_text(size = 7)) 
?rep#geom_text(aes(label = round(log(number), 1)))




    





