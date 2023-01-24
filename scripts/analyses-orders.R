# Comparison of studies-rich orders

# modelling how Arachnida, Diptera, Coleoptera and Hymenoptera multi-group studies respond to land-use

# read in the data ----
diversity <- readRDS("data/diversity-2022-04-13-02-33-10-rds/diversity-2022-04-13-02-33-10.rds")

# Lets load the relevant package ----
library(tidyverse) # data processing
library(raster) # for dealing with spatial data
library(lme4) # for mixed effects models
library(car) # for getting anova tables with significance values
library(DHARMa) # for model criticism plots
library(MuMIn) # for checking explanatory power of mixed effects models
library(sjPlot) # for visualising results
library(effects) # for extracting model effects
library(merTools) # useful for a few things, but we're using it for extracting estimates for plotting
library(emmeans) # testing multiple comparisons (but also useful for plotting)


# correct sampling effort ----
  
studies_check <- diversity %>%
  # filter the rows where Sampling efforts are NA
  filter(is.na(Sampling_effort)) %>%
  # keep only unique studies
  distinct(SS) %>%
  # pull the vector
  pull(SS)

diversity %>%
  # filter the rows where studies are those that had missing sampling efforts (above)
  filter(SS %in% studies_check) %>%
  # drop missing levels
  droplevels() %>%
  # pull out the sampling efforts of these studies
  pull(Sampling_effort)%>%
  # summarise to check that ALL the data are NAs
  summary() 

# All the sampling effort data are NAs. 
# Let’s replace the missing sampling efforts with 1 assuming that the sampling efforts don’t vary within a study and correct those that need it.


diversity <- diversity %>%
  
  # replace missing sampling effort values with 1
  mutate(x = replace_na(Sampling_effort, 1)) %>%
  
  # group by Study
  group_by(SS) %>%
  
  # check how many sampling efforts there are in each study
  mutate(n_sample_effort = n_distinct(Sampling_effort),
         # get the maximum sampling effort for the studies
         
         max_sample_effort = max(Sampling_effort)
  ) %>%
  
  ungroup() %>%
  
  # if the study has more than one sampling effort, correct the abundance
  
  # so if there's only one sampling effort, then leave the dividing effort as empty because we don't want to change the abundances when we do the divisions. Otherwise, we give it the maximum sampling effort.
  mutate(DividingEffort = ifelse(n_sample_effort == 1, NA, max_sample_effort)) %>%
  
  # if the diversity metric isn't sensitive to the effort, then we'll change the value to NA too (so we won't end up changing the measurement), otherwise leave it as it is
  mutate(DividingEffort = ifelse(Diversity_metric_is_effort_sensitive == FALSE, NA, DividingEffort)) %>%
  
  # now let's create the effort corrected measurement by dividing the abundances by the sampling efforts
  # where the dividing effort isn't NA (i.e. when it is necessary)
  mutate(Corrected_sampling_effort = ifelse(is.na(DividingEffort), 
                                            Sampling_effort,
                                            Sampling_effort / DividingEffort),
         Effort_corrected_measurement = ifelse(is.na(DividingEffort),
                                               Measurement,
                                               Measurement * Corrected_sampling_effort))

# summarise the corrected sampling efforts
summary(diversity$Corrected_sampling_effort)

# Let’s test that this works using AD1_2001__Liow 1, which has multiple sampling efforts in the study.
diversity %>% 
  # filter out the test study
  filter(SS == "AD1_2001__Liow 1") %>% 
  # select out the columns to check
  dplyr::select(Measurement, Effort_corrected_measurement,
                Sampling_effort, Corrected_sampling_effort)

# Let’s now test with a study that only has one sampling effort
diversity %>% 
  filter(SS == "AD1_2004__Darvill 1") %>% 
  dplyr::select(Measurement, Effort_corrected_measurement,
                Sampling_effort, Corrected_sampling_effort)

# And finally, let’s test a study of species richness so we can make sure this works too.

diversity %>% 
  # filter out the test study
  filter(SS == "AD1_2011__Holzschuh 2") %>% 
  # select out the columns to check
  dplyr::select(Measurement, Effort_corrected_measurement,
                Sampling_effort, Corrected_sampling_effort)


# Merge sites ---- 

diversity <- diversity %>%
  
  # group by aspects of the sites that should be identical if we need to merge the abundances
  # I only want to merge abundances if they are within the same study and block
  # as I'm assuming that even if the locations and sampling times are the same, 
  # if the blocks or studies are different, then there is some good reason for this.
  group_by(Source_ID, Study_number, Study_name, Block,
           #diversity metric type
           Diversity_metric, Diversity_metric_type, Diversity_metric_unit,
           Diversity_metric_is_effort_sensitive,
           
           #details of the sites
           Predominant_habitat, Use_intensity, Years_since_fragmentation_or_conversion,
           
           #details of the sampling method
           Sampling_method, Sampling_effort_unit,
           
           #species identity
           Study_common_taxon, Rank_of_study_common_taxon,
           Taxon_number, Taxon_name_entered,
           Indication, Parsed_name,
           Best_guess_binomial, COL_ID, Taxon, Name_status,
           Rank, Kingdom, Phylum, Class, Order, Family, Genus, Species,
           Higher_taxon,
           
           #site location
           Longitude, Latitude,
           
           #sampling time
           Sample_start_earliest, Sample_end_latest, Sample_date_resolution) %>%
  
  # if the diversity metric is occurrence:
  #   if it is present at all, give it a 1, if it is always absent, give it a 0,
  # otherwise (if the metric is either abundance or species richness):
  #   calculate the weighted abundance/richness for each taxonomic group, weighted by sampling effort
  
  mutate(merged_diversity = 
           ifelse(Diversity_metric_type == "Occurrence",
                  # if any of the occurrence values are 1, `any` will return TRUE. If you sum a logical, TRUE becomes 1 and FALSE becomes 0
                  sum(any(Effort_corrected_measurement > 0)),
                  
                  # note that since we've already corrected the sampling effort, this is essentially a mean rather than a weighted mean for abundance measurements. It's a weighted mean for species richness though where sampling efforts vary.
                  stats::weighted.mean(x = Effort_corrected_measurement,
                                       w = Corrected_sampling_effort))
  )

# pull out the grouping data (so we can double check how many records we're merging for each)
group_dat <- diversity %>% 
  group_data() %>% # group_data() returns a data frame that defines the grouping structure
  mutate(nvals_merged = lengths(.rows),
         merge_ID = row_number())

# ungroup the diversity data for future use
diversity <- ungroup(diversity)

# create a dataset where we can extract just the merged data if we want to
diversity_merged <- diversity %>%
  left_join(group_dat)

# check that the merging has worked (row numbers should be equal right now)
nrow(diversity) == nrow(diversity_merged)

# Now let’s test that the sites have been merged correctly.
test_data <- diversity_merged %>%
  filter(nvals_merged > 1) %>%
  distinct(merge_ID, .keep_all = TRUE)

test1 <- diversity_merged %>%
  filter(merge_ID == test_data$merge_ID[1]) %>%
  dplyr::select(SS, SSB,
                Diversity_metric, Diversity_metric_type, Diversity_metric_unit,
                Predominant_habitat, Use_intensity, Years_since_fragmentation_or_conversion,
                Sampling_method, Sampling_effort_unit,
                Study_common_taxon, Rank_of_study_common_taxon,
                Taxon_name_entered,
                Best_guess_binomial,
                Longitude, Latitude,
                Sample_start_earliest, Sample_end_latest, Sample_date_resolution,
                Effort_corrected_measurement,
                Corrected_sampling_effort,
                merged_diversity,
                .rows,
                nvals_merged,
                merge_ID
  )

test1

paste(test_data$SS[8000])

test2 <- diversity_merged %>%
  filter(merge_ID == test_data$merge_ID[8000]) %>%
  dplyr::select(SS, SSB,
                Diversity_metric, Diversity_metric_type, Diversity_metric_unit,
                Predominant_habitat, Use_intensity, Years_since_fragmentation_or_conversion,
                Sampling_method, Sampling_effort_unit,
                Study_common_taxon, Rank_of_study_common_taxon,
                Taxon_name_entered,
                Best_guess_binomial,
                Longitude, Latitude,
                Sample_start_earliest, Sample_end_latest, Sample_date_resolution,
                Effort_corrected_measurement,
                Corrected_sampling_effort,
                merged_diversity,
                .rows,
                nvals_merged,
                merge_ID)

test2

# Now let’s check some studies that didn’t need merging.
test_data <- diversity_merged %>%
  filter(nvals_merged == 1) %>%
  distinct(merge_ID, .keep_all = TRUE)

paste(test_data$SS[1])

test3 <- diversity_merged %>%
  filter(merge_ID == test_data$merge_ID[1]) %>%
  dplyr::select(SS, SSB,
                Diversity_metric, Diversity_metric_type, Diversity_metric_unit,
                Predominant_habitat, Use_intensity, Years_since_fragmentation_or_conversion,
                Sampling_method, Sampling_effort_unit,
                Study_common_taxon, Rank_of_study_common_taxon,
                Taxon_name_entered,
                Best_guess_binomial,
                Longitude, Latitude,
                Sample_start_earliest, Sample_end_latest, Sample_date_resolution,
                Effort_corrected_measurement,
                Corrected_sampling_effort,
                merged_diversity,
                .rows,
                nvals_merged,
                merge_ID
  )

test3

# Let’s take a look at how often we’re merging sites. 
hist(diversity_merged$nvals_merged)

# So most of the time, we’re not merging any sites, because all their information is unique. 
# However, there are some cases where we’re merging many sites in the same study. 
# Let’s have a look at that case where we’re merging 90 sites.
diversity_merged %>% 
  filter(nvals_merged == 90)

 # Now I can calculate site-level diversity metrics ----

sites <- diversity_merged %>%
  
  # pull out only the merged diversity data
  distinct(merge_ID, .keep_all = TRUE) %>%
  
  # re-make SSB and SSBS values since we've now dropped a bunch of values
  mutate(SS = paste(Source_ID, Study_number),
         SSB = paste(SS, Block),
         SSBS = paste(SSB, Site_number)) %>%
  
  # group by SSBS (each unique value corresponds to a unique site)
  group_by(SSBS) %>%
  
  # now add up all the abundance measurements within each site
  mutate(TotalAbundance = ifelse(Diversity_metric_type == "Abundance",
                                 sum(merged_diversity),
                                 # if the diversity metric type isn't Abundance, then leave the TotalAbundance measurement as NA
                                 NA),
         
         # if the metric is already species richness
         SpeciesRichness = ifelse(Diversity_metric_type == "Species richness",
                                  # just use the value as given
                                  merged_diversity,
                                  # for abundance and occurrence measurements, count the number of unique species names that are present at the site 
                                  n_distinct(Taxon_name_entered[merged_diversity > 0])),
         
         # calculate Chao's Species Richness Estimator
         # if the diversity metric is suitable for this calculation
         ChaoRichness = ifelse(Diversity_metric_is_suitable_for_Chao == TRUE,
                               # calculate the Chao estimator
                               sum(merged_diversity > 0) + (((sum(merged_diversity == 1) * (sum(merged_diversity == 1)-1)) / (2*(sum(merged_diversity == 2)+1)))),
                               # otherwise give Chao NA
                               NA)
  ) %>%
  
  # ungroup
  ungroup() %>%
  
  # now group by Study ID
  group_by(SS) %>%
  
  # pull out some useful study-level numbers
  # maximum abundance for each study
  mutate(MaxAbundance = max(TotalAbundance),
         # minimum (non-zero) abundance for each study
         # we'll use this when we do species rarefaction
         MinNonZeroAbundance = ifelse(all(TotalAbundance == 0)| all(is.na(TotalAbundance)),
                                      NA,
                                      min(TotalAbundance[TotalAbundance > 0])),
         # number of species in the study
         SpeciesInStudy = n_distinct(Taxon_name_entered[merged_diversity > 0]),
         # assess whether the study is suitable for rarefaction
         SuitableForRarefaction = ifelse(
           # if all diversity measurements are integers
           # i.e. if you round down, it should be equal to the original number
           # and the diversity metric is suitable for Chao
           all(floor(merged_diversity) == merged_diversity) &
             Diversity_metric_is_suitable_for_Chao == TRUE,
           # then class the study as suitable for rarefaction
           TRUE,
           # otherwise it can't be used
           FALSE
         )
  ) %>%
  
  # ungroup
  ungroup()  %>%
  
  # now rescale total abundance, so that within each study, abundance varies from 0 to 1.
  mutate(RescaledAbundance = TotalAbundance/MaxAbundance,
         # for statistical modelling, we'll also calculate the square root of species abundance, although we might want to use log(x+1) transformation instead
         sqrtRescaledAbundance = sqrt(RescaledAbundance)
  )


# Let’s test that our information has been calculated correctly before we drop all the unnecessary data.

sites %>% 
  # filter out the test study
  filter(SS == "AD1_2001__Liow 1") %>% 
  # select out the columns to check
  dplyr::select(Measurement, Effort_corrected_measurement,
                Sampling_effort, Corrected_sampling_effort,
                merged_diversity,
                TotalAbundance, MaxAbundance, RescaledAbundance,
                SpeciesRichness, ChaoRichness)

sites %>% 
  filter(SS == "AD1_2004__Darvill 1") %>% 
  dplyr::select(Measurement, Effort_corrected_measurement,
                Sampling_effort, Corrected_sampling_effort,
                merged_diversity,
                TotalAbundance, MaxAbundance, RescaledAbundance,
                SpeciesRichness, ChaoRichness)

sites %>% 
  # filter out the test study
  filter(SS == "AD1_2011__Holzschuh 2") %>% 
  # select out the columns to check
  dplyr::select(Measurement, Effort_corrected_measurement,
                Sampling_effort, Corrected_sampling_effort,
                merged_diversity,
                TotalAbundance, MaxAbundance, RescaledAbundance,
                SpeciesRichness, ChaoRichness)

# pull out unique sites ----
# sites <- sites %>%
#  distinct(SSBS, .keep_all = TRUE) - don't do this because you only pick a distinct random row per SSBS which loses the multi group study data

# Fix up your explanatory variables ----

sites <- rename(sites,
                Predominant_land_use = Predominant_habitat)


# Relevel the land use and use intensity classes
sites <- sites %>%
  
  mutate(
    
    # collapse primary forest and non-forest together into primary vegetation as these aren't well distinguished
    Predominant_land_use = recode_factor(Predominant_land_use, 
                                         "Primary forest" = "Primary", 
                                         "Primary non-forest" = "Primary"),
    
    # indeterminate secondary veg and cannot decide get NA
    Predominant_land_use = na_if(Predominant_land_use, "Secondary vegetation (indeterminate age)"),
    Predominant_land_use = na_if(Predominant_land_use, "Cannot decide"),
    Use_intensity = na_if(Use_intensity, "Cannot decide"),
    
    # set reference levels
    Predominant_land_use = factor(Predominant_land_use),
    Predominant_land_use = relevel(Predominant_land_use, ref = "Primary"),
    Use_intensity = factor(Use_intensity),
    Use_intensity = relevel(Use_intensity, ref = "Minimal use")
  )

# take a look at the LandUse/Use intensity split
table(sites$Predominant_land_use, sites$Use_intensity)


# You might want to create a new factor where land use and intensity are pasted together. 
# This is often the easiest way to model effects and often we need to combine levels because the full factorial combination is sparse in areas or missing.

sites <- sites %>%
  mutate(LUI = interaction(Predominant_land_use, Use_intensity, sep = "_"),
         # as an example, let's collapse Mature Intense and Mature Light into a single category
         LUI = recode_factor(LUI, 
                             "Mature secondary vegetation_Intense use" = "Mature secondary vegetation_LightIntense", 
                             "Mature secondary vegetation_Light use" = "Mature secondary vegetation_LightIntense"),
         LUI = relevel(LUI, ref =  "Primary_Minimal use")
  )

table(sites$LUI)


# lets remove all the stuff we don't need from our global environment
rm(group_dat, test_data, test1, test2, test3, studies_check, diversity, diversity_merged)


# # Model site-level diversity, Lepidoptera vs Diptera vs Coleoptera vs Hymenoptera ----

# Subset predicts to multi-group studies that only include insects ----

orders <-
  sites %>%
  filter(Class == "Insecta") %>% # filter only for insects
  group_by(SS) %>% # group by SS
  mutate(
    Multiple_groups = 
      case_when(
        (length(unique(Higher_taxon))) > 1 ~ "YES",
        (length(unique(Higher_taxon))) == 1 ~ "NO")) %>% # for each SS create a new column: multiple groups = YES/NO - depending on if there's 
  # more than one unique higher taxon within that study
  # Filter the data for studies with multiple groups
  filter(Multiple_groups == "YES") %>% # filter for the rows for studies with multiple groups
  ungroup() %>% 
  droplevels()

# Step 1: collinearity ----

source("https://highstat.com/Books/Book2/HighstatLibV10.R")

# There doesn't seem to be too much collinearity between predominant land use and use intensity 
# There are different ‘rules of thumb’ about how high is too high when it comes to GVIFs. Under 3 is great and under 5 is ok. 
# Some people also say under 10 is acceptable, but I think that’s a bit too high personally.
corvif(orders[ , c("Predominant_land_use", "Use_intensity")])

# Step 2: complete cases ----
# Next, let’s make sure that every row of our dataset has known values for all of our explanatory variables.

model_data_ab <- drop_na(orders, 
                         RescaledAbundance, Predominant_land_use,
                         Use_intensity)

model_data_sr <- drop_na(orders, 
                         SpeciesRichness, Predominant_land_use,
                         Use_intensity)

# Step 3: starting/maximal model

# First, let’s transform RescaledAbundance.

model_data_ab <- mutate(model_data_ab, 
                        logAbundance = log(RescaledAbundance + 1),
                        sqrtAbundance = sqrt(RescaledAbundance)
)


