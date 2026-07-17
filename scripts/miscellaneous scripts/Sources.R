# Exploring predicts based on source rather than study

# read in the data ----
predicts <- readRDS("data/diversity-2022-04-13-02-33-10.rds")

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

# Filter predicts for 5 orders and then work out multiple group studies ----
multi <-
  predicts %>%
  filter(Higher_taxon %in% c("Diptera", "Coleoptera", "Lepidoptera", "Hymenoptera", "Hemiptera")) %>% 
  droplevels() %>% # drop empty levels 
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

# subset multi for studies
plot_ss <- multi %>%
group_by(SS) %>% # there are 131 studies on multiple groups 
  count(Higher_taxon) %>% # For each study, how many rows do we have for each higher taxonomic group? 
  rename(number="n") %>%
  droplevels() 

# How many unique studies?
length(unique(plot_ss$SS)) # 106 unique studies

# Heatmap of 5 orders
plot_ss %>%
  ggplot(aes(x = Higher_taxon, y = SS, fill = number)) +
  geom_tile() +
  scale_fill_gradient(trans = 'log') + # log scale
  theme(axis.text.x = element_text(angle = 45, hjust= 1)) +
  theme(axis.text.y  = element_text(size = 7)) 

?rep#geom_text(aes(label = round(log(number), 1)))

# Remove that fat df 
rm(predicts)

# For each source, how many rows do we have per taxonomic group? ---- 
  plot_source <- 
  multi %>%
  #filter(Higher_taxon != "NA") %>% # some rows have unknown species of insects/arthropods
  group_by(Source_ID) %>% 
  # KEY DIFFERENCE HERE - grouping by Source rather than study to get a conservative estimate on the number of independent datasets
  count(Higher_taxon) %>%
  rename(number="n") %>%
  droplevels()



p1 <- clean %>% 
  group_by(Source_ID) %>% 
  count(Higher_taxon) %>%
  rename(number="n") %>%
  droplevels() 

p2 <- model_data_ab %>% 
  group_by(Source_ID) %>% 
  count(Predominant_land_use) %>%
  rename(number="n") %>%
  droplevels() 

p1 %>%
  ggplot(aes(x = Higher_taxon, y = Source_ID, fill = number)) +
  geom_tile() +
  #scale_fill_gradient(trans = 'log') + # log scale
  theme(axis.text.x = element_text(angle = 45, hjust= 1)) +
  theme(axis.text.y  = element_text(size = 7)) 
?rep#geom_text(aes(label = round(log(number), 1)))


p1 %>%
  ggplot(aes(x = Higher_taxon, y = Source_ID, fill = number)) +
  geom_tile(width = 0.8, height = 0.8) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Number of records") +
  geom_text(aes(label = round((number), 1)), color = "black", size = 3) + # Adjust label size and color as needed
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  labs(
    title = "",
    x = "Order",
    y = "Source"
  )

p2 %>%
  ggplot(aes(x = Predominant_land_use, y = Source_ID, fill = number)) +
  geom_tile(width = 0.8, height = 0.8) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Number of records") +
  geom_text(aes(label = round((number), 1)), color = "black", size = 3) + # Adjust label size and color as needed
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  labs(
    title = "",
    x = "Land-use category",
    y = "Source"
  )



# HEAT MAP - Create a heat map showing how well represented taxonomic groups are across multi-group SOURCES (not studies) ---- 
plot_source %>%
  ggplot(aes(x = Higher_taxon, y = Source_ID, fill = number)) +
  geom_tile() +
  scale_fill_gradient(trans = 'log') + # log scale
  theme(axis.text.x = element_text(angle = 45, hjust= 1)) +
  theme(axis.text.y  = element_text(size = 7)) 
?rep#geom_text(aes(label = round(log(number), 1)))



