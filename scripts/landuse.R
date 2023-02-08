# Work out how well represented land-use classes are for multi-group studies
# Justin Isip 6/6/22


# read in the data ----
predicts <- readRDS("data/diversity-2022-04-13-02-33-10.rds")

# Lets load the relevant packages ----
library(tidyverse)


# Filter predicts for multi group studies
multi <-
  predicts %>%
  filter(Phylum == "Arthropoda") %>%
  dplyr::select(Source_ID, Study_number, SS, SSS, SSB, SSBS, Taxon, 
         Rank, Kingdom, Phylum, Class, Order, Family, Genus, 
         Species, Higher_taxon, Taxon_name_entered, Parsed_name, 
         Study_common_taxon, Rank_of_study_common_taxon, Predominant_habitat, Use_intensity) %>% # Subset the columns of interest
  mutate_all(na_if,"") %>% # add NAs 
  group_by(SS) %>% 
  mutate(
    Multiple_groups = 
      case_when(
        (length(unique(Higher_taxon))) > 1 ~ "YES",
        (length(unique(Higher_taxon))) == 1 ~ "NO")) %>% 
  # Filter the data for studies with multiple groups
  filter(Multiple_groups == "YES") %>%
  ungroup() %>% 
  droplevels() 

# Remove predicts from the global environment
rm(predicts)


# For each study, which land-use classes do the taxonomic groups belong to?
habitat <- 
  multi %>%
  filter(Higher_taxon != "NA") %>% # some rows have unknown species of insects/arthropods
  filter(Higher_taxon %in% c("Arachnida", "Coleoptera", "Diptera", "Hemiptera", "Hymenoptera", "Lepidoptera")) %>%
  group_by(SS, Predominant_habitat) %>% 
  count(Higher_taxon) %>%
  rename(number="n") %>%
  filter(Predominant_habitat != "Cannot decide") %>%
  droplevels()

# Count the number of studies on each higher taxon for each land use class
habitat_studies <- habitat %>% 
  group_by(Predominant_habitat) %>% 
  count(Higher_taxon) %>% # count the number of higher_taxon rows, each row in habitat is an SS, so 1 HT row = 1 SS, so we get the number of studies
  rename(number_of_studies = "n")

# Create a heatmap showing the number of studies for each of the most studies-rich insect orders across land-use classes
habitat_studies %>%
  ggplot(aes(x = Predominant_habitat, y = Higher_taxon, fill = number_of_studies)) +
  geom_tile() +
  # scale_fill_gradient(trans = 'log') + # log scale
  theme(axis.text.x = element_text(angle = 45, hjust= 1)) +
  # geom_text(aes(label = round(log(number), 1)))
  geom_text(aes(label = number_of_studies), color = "white", size = 3) 

# We lose the multiple groups within a study when we pull distinct(SS) so we end up having less studies for each HT
a <- multi %>%
  distinct(SS, .keep_all = TRUE) %>%
  group_by(Higher_taxon) %>%
  count() %>%
  droplevels()


# Sensitivity check 
   b <- multi %>%
    filter(Higher_taxon != "NA") %>% # some rows have unknown species of insects/arthropods
    filter(Higher_taxon %in% c("Arachnida", "Coleoptera", "Diptera", "Hemiptera", "Hymenoptera", "Lepidoptera")) %>%
    filter(Predominant_habitat != "Cannot decide") %>%
    group_by(Predominant_habitat, Higher_taxon) %>%
    count(SS) %>%
    droplevels() %>%
    group_by(Predominant_habitat) %>%
    count(Higher_taxon)
  