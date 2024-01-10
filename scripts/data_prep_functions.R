# data_prep_functions.R

# These functions help to prepare PREDICTS data for modelling


# Read latest diversity extract ------------------------------------------------

read_latest_diversity <- function(data_folder, verbose = TRUE){
  possibles <- list.files(data_folder, pattern = "diversity")
  latest <- rev(sort(possibles))[1]
  if (verbose == TRUE){
    cat(paste("Reading latest diversity extract (", 
              data_folder,
              "/",
              latest,
              ")\n",
              sep = ""))
  }
  to_return <- readRDS(file.path(data_folder, latest))
  attr(to_return, which = "diversity_extract") <- latest
  
  return(to_return)
}


# Summarise diversity data frame -----------------------------------------------

summarise_diversity <- function(data){
  data <- as.data.frame(data) # Avoiding tibbles
  nRows <- nrow(data)
  nSites <- length(unique(data$SSBS))
  nStudies <- length(unique(data$SS))
  nSources <- length(unique(data$Source_ID))
  
  to_return <- list(nRows = nRows,
                    nSites = nSites,
                    nStudies = nStudies,
                    nSources = nSources,
                    diversity_extract = 
                      attr(data, which = "diversity_extract"))
}


# Read latest sites extract ----------------------------------------------------

read_latest_sites <- function(data_folder, verbose = TRUE){
  possibles <- list.files(data_folder, pattern = "sites")
  latest <- rev(sort(possibles))[1]
  if (verbose == TRUE){
    cat(paste("Reading latest sites extract (", 
              data_folder,
              "/",
              latest,
              ")\n",
              sep = ""))
  }
  to_return <- readRDS(file.path(data_folder, latest))
  attr(to_return, which = "sites_extract") <- latest
  
  return(to_return)
}


# Summarise diversity data frame -----------------------------------------------

summarise_sites <- function(data){
  data <- as.data.frame(data) # Avoiding tibbles.
  nSites <- length(unique(data$SSBS))
  nStudies <- length(unique(data$SS))
  nSources <- length(unique(data$Source_ID))
  
  to_return <- list(nSites = nSites,
                    nStudies = nStudies,
                    nSources = nSources,
                    sites_extract = 
                      attr(data, which = "sites_extract"))
}


# Exclude data from particular contributors ------------------------------------

drop_contributors <- function(data, contributors){
  codes <- substr(data$Source_ID, 1, 3)
  to_drop <- which(codes %in% contributors)
  
  to_return <- data[-to_drop,]
}


# Check studies with *any* NAs for sampling effort have *all* NAs --------------

check_sampling_effort_nas <- function(data){
  # Returns TRUE if the condition is met, FALSE otherwise
  
  studies_check <- data %>%
    # filter the rows where Sampling efforts are NA
    filter(is.na(Sampling_effort)) %>%
    # keep only unique studies
    distinct(SS) %>%
    # pull the vector
    pull(SS)
  
  checks <- data %>%
    # filter rows whose studies had missing sampling efforts (above)
    filter(SS %in% studies_check) %>%
    # drop missing levels
    droplevels() %>%
    # pull out the sampling efforts of these studies
    pull(Sampling_effort)%>%
    # summarise to check that ALL the data are NAs
    summary() 
  
  # Return TRUE if and only if both min and max are NA
  pass <- is.na(checks[[1]]) & is.na(checks[[6]])
}


# Calculate a site's species coverage from its abundance values ----------------

get.SC2 <- function(within.site.abundances){
  # This function uses iNEXT to estimate coverage from the set of abundances.
  # The iNEXT function assumes data are integers. Where they are not, this
  # code first rescales so that the smallest non-zero abundance is 1 (a
  # conservative choice, which can only underestimate true coverage), and 
  # rounds all rescaled abundances to make them integers. If there are no
  # abundance data, or all the numbers are 0, coverage is 0.
  
  require(iNEXT)
  
  if (length(within.site.abundances) == 0) return(0)
  if (sum(within.site.abundances) == 0) return(0)
  if (all.equal(within.site.abundances, 
                as.integer(within.site.abundances)) == TRUE){
    answer <- DataInfo(within.site.abundances)$SC
  }else{
    smallest <- min(within.site.abundances[within.site.abundances > 0])
    within.site.abundances <- round(within.site.abundances/smallest)
    answer <- DataInfo(within.site.abundances)$SC
  }
}


# Fill and correct for sampling effort information -----------------------------

correct_for_sampling_effort <- function(data, verbose = TRUE){
  require(dplyr)
  require(magrittr)
  require(tidyr)
  
  # Record source of diversity data, to use later as an attribute
  div <- attr(data, which = "diversity_extract")
  
  data <- as.data.frame(diversity) # Avoiding tibbles.
  
  # Replace missing sampling effort values with 1
  data$Sampling_effort[is.na(data$Sampling_effort)] <- 1
  
  data <- data %>%

    # Group by Study.
    group_by(SS) %>%
    
    # Check how many sampling efforts there are in each study.
    mutate(n_sample_effort = n_distinct(Sampling_effort),
           # Get the maximum sampling effort for the studies
           max_sample_effort = max(Sampling_effort)
    ) %>%
    
    ungroup() %>%
    
    # If the study has more than one sampling effort, correct the abundance
    
    # So if there's only one sampling effort, then create a 'dummy sampling 
    # effort' of 1 so that we don't change the abundances when we do the 
    # divisions. Otherwise, we give it the maximum sampling effort.
    mutate(DividingEffort = ifelse(n_sample_effort == 1,
                                   1, max_sample_effort)) %>%
    
    # If the diversity metric isn't sensitive to the effort, then we'll change
    # the value to 1 too (so we won't end up changing the measurement), 
    # otherwise leave it as it is.
    mutate(DividingEffort = ifelse(Diversity_metric_is_effort_sensitive == 
                                     FALSE, 1, DividingEffort)) %>%
    
    # Now let's create the effort-corrected measurement by dividing the
    # abundances by the sampling efforts.
    mutate(Corrected_sampling_effort = Sampling_effort/max_sample_effort,
        Effort_corrected_measurement = Measurement/DividingEffort) 
  
  # Summarise the corrected sampling efforts.
  if (verbose == TRUE){
    cat("Summary of corrected sampling effort values:\n")
    print(summary(data$Corrected_sampling_effort))
  }
  
  attr(data, which = "diversity_extract") <- div
  attr(data, which = "when_created") <- Sys.time()
  
  return(data)
}


# Check studies with *any* NAs for sampling effort have *all* NAs --------------

check_sampling_effort_nas <- function(data){
  # Returns TRUE if the condition is met, FALSE otherwise
  
  studies_check <- data %>%
    # filter the rows where Sampling efforts are NA
    filter(is.na(Sampling_effort)) %>%
    # keep only unique studies
    distinct(SS) %>%
    # pull the vector
    pull(SS)
  
  checks <- data %>%
    # filter rows whose studies had missing sampling efforts (above)
    filter(SS %in% studies_check) %>%
    # drop missing levels
    droplevels() %>%
    # pull out the sampling efforts of these studies
    pull(Sampling_effort)%>%
    # summarise to check that ALL the data are NAs
    summary() 
  
  # Return TRUE if and only if both min and max are NA
  pass <- is.na(checks[[1]]) & is.na(checks[[6]])
}


# Add taxon_fold to split the diversity data into 10 exclusive groups ----------

add_taxon_fold <- function(data, folds = "default", verbose = TRUE){
  # By default (and nothing else is yet coded up), this puts each row into one
  # of ten taxonomic chunks (not all are proper taxa) that each have a decent
  # number of rows of data. This allows site-level values to be obtained within
  # each of the taxonomic folds in turn.
  
  # Set up taxon_fold, a factor that holds the fold identity.
  data$taxon_fold <- rep(NA, nrow(data))
  
  # Rows are assigned to folds based on the higher taxonomy information.
  if (folds == "default"){
    data$taxon_fold[data$Higher_taxon == "Aves"] <- "Birds"
    data$taxon_fold[data$Phylum=="Chordata" & 
                      data$Higher_taxon != "Aves"] <- "OtherVertebrates"
    data$taxon_fold[data$Higher_taxon == "Coleoptera"] <- "Beetles"
    data$taxon_fold[data$Higher_taxon == "Hymenoptera"] <- "Hymenoptera"
    data$taxon_fold[data$Class == "Insecta" & 
                      data$Higher_taxon != "Coleoptera" &
                      data$Higher_taxon != "Hymenoptera"] <- "OtherInsects"
    data$taxon_fold[data$Phylum == "Arthropoda" & 
                      data$Class != "Insecta"] <- "OtherArthropods"
    data$taxon_fold[data$Higher_taxon == "Magnoliopsida"] <- "Dicots"
    data$taxon_fold[data$Phylum == "Tracheophyta" & 
                      data$Higher_taxon != "Magnoliopsida"] <- "OtherTracheophytes"
    data$taxon_fold[data$Kingdom %in% c("Fungi", "Protozoa")] <- 
      "FungiProtistsLowerPlants"
    data$taxon_fold[data$Kingdom == "Animalia" & 
                      data$Phylum != "Arthropoda" & 
                      data$Phylum != "Chordata"] <- "OtherInvertebrates"
  }else{
    stop("Only the default folds have been coded up so far!")
  }
  
  data$taxon_fold <- as.factor(data$taxon_fold)
  
  if (verbose == TRUE){
    # By default, report on the breakdown of records per fold.
    cat("Numbers of rows withing each taxonomic fold are as follows:\n\n")
    print(table(data$taxon_fold))
  }
  
  return(data)
}


# Merge sites and calculate site metrics ---------------------------------------

merge_sites <- function(data, verbose = TRUE){
  require(dplyr)
  require(magrittr)
  require(tidyr)
  
  # Record source of diversity data, to use later as an attribute
  div <- attr(data, which = "diversity_extract")

  data <- data %>%
    # Group by aspects of the sites that should be identical if we need to merge
    # the abundances. Merge abundances if they are within the same study and 
    # block, because even if the locations, sampling times, crop etc are the
    # same, if the blocks or studies are different, then there is some good 
    # reason.
    
    group_by(Source_ID, Study_number, Study_name, Block,
             #diversity metric type
             Diversity_metric, Diversity_metric_type, Diversity_metric_unit,
             Diversity_metric_is_effort_sensitive,
             
             #details of the sites
             Predominant_habitat, Use_intensity, 
             Years_since_fragmentation_or_conversion,
             
             #details of any crops and farm management
             FF1, FF2, FF3, AES, Organic, Crop,
             
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
             Sample_start_earliest, Sample_end_latest, 
             Sample_date_resolution) %>%
    
    # if the diversity metric is occurrence:
    #   if it is present at all, give it a 1; if it is always absent, give a 0,
    # otherwise (if the metric is either abundance or species richness):
    #   calculate the weighted abundance/richness for each taxonomic group, 
    #   weighted by sampling effort
    
    mutate(merged_diversity = 
             ifelse(Diversity_metric_type == "Occurrence",
                    # if any of the occurrence values are 1, `any` will return 
                    # TRUE. If you sum a logical, TRUE becomes 1 and FALSE 
                    # becomes 0
                    sum(any(Effort_corrected_measurement > 0)),
                    
                    # note that since we've already corrected the sampling 
                    # effort, this is essentially a mean rather than a weighted 
                    # mean for abundance measurements. It's a weighted mean for 
                    # species richness though where sampling efforts vary.
                    stats::weighted.mean(x = Effort_corrected_measurement,
                                         w = Corrected_sampling_effort))
    )
  
  # pull out the grouping data (so we can double check how many records we're 
  # merging for each)
  group_dat <- data %>% 
    group_data() %>%
    mutate(nvals_merged = lengths(.rows),
           merge_ID = row_number())
  
  # Ungroup the diversity data for future use.
  data <- ungroup(data)
  
  # Create a dataset where we can extract just the merged data if we want to.
  data_merged <- data %>%
    left_join(group_dat)
  
  if (verbose == TRUE){
    # Summarise structure of data frame after merging
    cat("\nAfter merging, the sites data frame has:\n")
    cat(paste("   ", nrow(data_merged), "rows, from\n"))
    cat(paste("   ", length(unique(data_merged$SSBS)), "sites from\n"))
    cat(paste("   ", length(unique(data_merged$SS)), "studies and\n"))
    cat(paste("   ", length(unique(data_merged$Source_ID)), "sources.\n"))
  }

  # Set provenance attributes
  attr(data_merged, which = "diversity_extract") <- div
  attr(data_merged, which = "is_merged") <- TRUE
  attr(data_merged, which = "when_Created") <- Sys.time()
  
  return(data_merged)
}


# Calculate total abundance for each site --------------------------------------

get_site_abundances <- function(data, 
                                taxon_folds = FALSE,
                                verbose = TRUE){
  # This calculates site metrics for each site or, if desired, each taxon fold
  # within each site.
  
  # Make it a data frame to avoid having to deal with a tibble.
  data <- as.data.frame(data)
  
  # Set up a variable for uniquely identifying the site x taxon_fold combination
  # or, if taxon_folds == FALSE, the site. This is what grouping is then done
  # by.
  print(table(data$taxon_fold))
  
  if (taxon_folds == TRUE){
    data$the_taxon_folds <- data$taxon_fold
  }else{
    data$the_taxon_folds <- ""
  }

  # Just for debugging
  print(table(data$the_taxon_folds))
  
  # Start to calculate site-level metrics.
  sites <- data %>%
    
    # pull out only the merged diversity data
    distinct(merge_ID, .keep_all = TRUE) %>%
    
    # Re-make SSB and SSBS values since we've now dropped a bunch of values; and
    # also add SSBST which is that taxon x site combo. The trimws is because the
    # SSBST would otherwise end in a space if taxon_folds == FALSE.
    mutate(SS = paste(Source_ID, Study_number),
           SSB = paste(SS, Block),
           SSBS = paste(SSB, Site_number),
           SSBST = trimws(paste(SSBS, the_taxon_folds))) %>%
    
    # group by SSBST (each unique value corresponds to a unique site x fold)
    group_by(SSBST) %>%
    
    # Now add up all the abundance measurements within each site and also 
    # estimate coverage.
    mutate(TotalAbundance = ifelse(Diversity_metric_type == "Abundance",
                                   sum(merged_diversity, na.rm = TRUE),
                                   # If the diversity metric type isn't 
                                   # Abundance, then leave the TotalAbundance 
                                   # measurement as NA.
                                   NA),
           
           SpeciesRichness = ifelse(Diversity_metric_type == "Species richness",
                        merged_diversity,
                        # for abundance and occurrence measurements,
                        # count the number of unique species names 
                        # that are present at the site 
                        n_distinct(Best_guess_binomial[merged_diversity > 0])),
           
           coverage = get.SC2(merged_diversity)) %>%
    
    # ungroup
    ungroup() %>%
    
    # pull out unique site/fold combinations.
    distinct(SSBST, .keep_all = TRUE) %>%
    
    # now group by Study ID and fold.
    group_by(SS, the_taxon_folds) %>%
    
    # pull out the maximum abundance for each study/fold combination.
    mutate(MaxAbundance = max(TotalAbundance)) %>%
    
    # ungroup.
    ungroup() %>%
    
    # now rescale total abundance, so that within each study, the maximum is 1.
    mutate(RescaledAbundance = TotalAbundance/MaxAbundance) %>%
    
    mutate(sqrtRescaledAbundance = sqrt(RescaledAbundance))
  
  # Remove NA values and drop levels
  sites <- as.data.frame(sites)
  sites <- droplevels(subset(sites, !is.na(sqrtRescaledAbundance)))
  
  if (verbose == TRUE){
    # Summarise structure of site-level data frame
  cat("\nThere are abundance data for:\n")
  cat(paste("   ", sum(!is.na(sites$sqrtRescaledAbundance)), "sites, from\n"))
  cat(paste("   ", 
            length(unique(sites$SS[!is.na(sites$sqrtRescaledAbundance)])), 
            "studies and\n"))
  cat(paste("   ", 
          length(unique(sites$Source_ID[!is.na(sites$sqrtRescaledAbundance)])),
          "sources.\n"))
  }
  
  # Set provenance attributes
  attr(sites, which = "diversity_extract") <- 
    attr(data, which = "diversity_extract")
  attr(sites, which = "is_merged") <- attr(data, which = "is_merged")
  attr(sites, which = "taxon_folds") <- taxon_folds 
  attr(sites, which = "when_Created") <- Sys.time()
  
  return(sites)
}


# Make comparisons between pairs of sites --------------------------------------

make_all_cs_bal_comparisons <- function(data, verbose = TRUE){
  # Calculates 1 - bc_bal for every pair of different sites within each study, 
  # but does not attempt to characterise the comparison in any other way. The 
  # idea behind this function is that it will be called very seldom, producing a
  # large data frame that can then be selected from each time an analysis is
  # done.
  
  require(betapart) # For bray.part, which computes bc_bal
  require(reshape2) # For dcast
  
  data <- as.data.frame(data) # Getting rid of the tibble structure.
  data <- droplevels(subset(data, Diversity_metric_type == "Abundance"))
  
  studies <- unique(data$SS)
  if (verbose == TRUE) cat(paste("The data frame has", length(studies),
                                 "studies with abundance data.\n"))

  how_many <- length(studies) # Set to something small for testing purposes

  # Set up lists for the results
  all_res <- vector(mode = "list", length = how_many)
  species_count <- vector(mode = "list", length = how_many)
  
  counter <- 0 # position in lists of results
  
  # Loop through the studies; for each one, calculate 1 - bc_bal
  for (i in studies[1: how_many]){
    # Increment counter
    counter <- counter + 1
    
    # Subset to the focal study.
    tempdat <- subset(data, SS == i)
    
    # Reshape to a site by taxa matrix.
    site.sp <- reshape2::dcast(tempdat, SSBS ~ Taxon_name_entered, 
                               value.var='Measurement', fun.aggregate=mean, 
                               na.rm = TRUE)
    rownames(site.sp) <- site.sp$SSBS
    site.sp$SSBS <- NULL
    
    if (verbose == TRUE){
      cat(paste(counter, " - ", i, ": ", nrow(site.sp), " sites\n", sep = ""))
    }
    
    # Get totals for the sites.
    site_totals <- site.sp
    site_totals$sum_spp <- rowSums(site_totals)
    site_totals$SSBS <- row.names(site_totals)
    site_totals <- site_totals[,c("sum_spp", "SSBS")]
    
    # Keep site x species matrix.
    names(species_count)[[counter]] <- as.character(i)
    species_count[[counter]] <- site_totals
    
    # Calculate the two components of Bray-Curtis dissimilarity.
    res <- bray.part(site.sp)
    bcbal <- 1 - (res$bray.bal)
    
    # Record results.
    names(all_res)[[counter]] <- as.character(i)
    all_res[[counter]] <- bcbal
    
  }
  
  # Melt each study's comparisons into a data frame.
  tt <- lapply(all_res, function(x) reshape2::melt(as.matrix(x), 
                                                   varnames = c("row", "col")))
  
  # Stick the data frames together and rename the columns.
  tt2 <- do.call(rbind.data.frame, tt)
  names(tt2) <- c("site1", "site2", "cs_bal")
  
  if (verbose == TRUE){
    cat(paste("Data frame has", nrow(tt2), "pairwise comparisons.\n"))
  }

  # Set provenance attributes
  attr(tt2, which = "diversity_extract") <- 
    attr(data, which = "diversity_extract")
  attr(tt2, which = "is_merged") <- attr(data, which = "is_merged")
  attr(tt2, which = "when_Created") <- Sys.time()
  
  return(tt2)
}


# Tidy site-level data frame ---------------------------------------------------

tidy_sites <- function(sites, shorten = TRUE, verbose = TRUE){
  require(dplyr)
  require(tidyr)
  require(magrittr)

  sites <- rename(sites,
                LUH1 = Predominant_habitat)

  sites <- sites %>%
  
  mutate(
    # Collapse primary forest and non-forest together into primary as these 
    # aren't well distinguished.
    LUH1 = recode_factor(LUH1, 
                         "Primary forest" = "Primary", 
                         "Primary non-forest" = "Primary"),
    
    # cannot decide get NA
    LUH1 = na_if(LUH1, "Cannot decide"),
    Use_intensity = na_if(Use_intensity, "Cannot decide"),
    
    # set reference levels
    LUH1 = relevel(factor(LUH1), ref = "Primary"),
    Use_intensity = factor(Use_intensity),
    Use_intensity = relevel(Use_intensity, ref = "Minimal use")
  )
  
  if (shorten == TRUE){
    sites <- sites %>%
      mutate(
        Use_intensity = recode_factor(Use_intensity,
                          "Minimal use" = "Minimal",
                          "Light use" = "Light",
                          "Intense use" = "Intense"),
        LUH1 = recode_factor(LUH1,
                             "Plantation Forest" = "PlantationForest",
                             "Young secondary vegetation" = "YSV",
                             "Intermediate secondary vegetation" = "ISV",
                             "Mature secondary vegetation" = "MSV",
                             "Secondary vegetation (indeterminate age)" = "SVU")
      )
  }
  
  if (verbose == TRUE){
    cat("Numbers of sites in each LUH1 class:\n")
    print(table(sites$LUH1))
  }
  
  to.drop <- c("Insightly_category", "Taxon_number", "Taxon_name_entered", 
               "Resolution_entered", "Indication", "Status", "Parsed_name", 
               "COL_ID", "Taxon", "Name_status", "Rank", "Kingdom", "Phylum", 
               "Class", "Order", "Family", "Genus", "Species",
               "Higher_taxon","Best_guess_binomial", ".rows")
  to.keep <- setdiff(names(sites), to.drop)
  sites <- subset(sites, select = to.keep)
  
  sites <- droplevels(sites)
}


# Make empty comparisons data frame to hold bc_bal and explanatory variables ---

identify_comparisons <- function(abundance, basis, verbose = TRUE){
  # TO ADD - A check that all the levels of the factor are in one of the 3 sets

  abundance <- as.data.frame(abundance) # I am less adept with tibbles
  
  # Drop sites whose levels of the key factor are not required.
  keep <- c(basis$are_baseline, basis$are_not_baseline)
  sites <- abundance[abundance[, key] %in% keep, ]
  
  if (verbose == TRUE){
    # Report on what has been done and the effect it has had.
    cat(paste("Original site-level data frame had", 
              nrow(abundance), "sites from", length(unique(abundance$SS)),
              "studies.\n"))
    cat(paste("Factor used to determine retention and use as baseline:",
              basis$key, "\n"))
    cat(paste("Dropping the following levels left", nrow(sites), "sites from", 
              length(unique(sites$SS)), "studies:\n  "))
    cat(paste(basis$to_drop, collapse = "\n  "))
  }
  
  # Set up empty data frame for pairwise comparisons.
  comparisons <- data.frame(
    Source_ID = NULL,
    SS = NULL,
    site1 = NULL,
    site1_key = NULL,
    site2 = NULL,
    site2_key = NULL)
    
  # Vector of the studies to loop through.
  studies <- unique(sites$SS)
  
  # Within each study, compare every baseline site with every site
  for (i in studies){
    this_study <- subset(sites, SS == i)
    baseline_sites <- this_study$SSBS[this_study[, key] %in% basis$are_baseline]
    baseline_key <- this_study[, key][this_study[, key] %in% basis$are_baseline]
    
    to_add <- data.frame(
      Source_ID = rep(this_study$Source_ID[1], 
                      length(baseline_sites) * nrow(this_study)),
      SS = rep(this_study$SS[1], 
                      length(baseline_sites) * nrow(this_study)),
      site1 = rep(baseline_sites, each = nrow(this_study)),
      site1_key = rep(baseline_key, each = nrow(this_study)),
      site2 = rep(this_study$SSBS, times = length(baseline_sites)),
      site2_key = rep(this_study[, key], times = length(baseline_sites)))
    
    if (verbose == TRUE){
      cat(paste(i, ": ", (nrow(this_study) - 1) * length(baseline_sites), 
                " comparisons (from ", nrow(this_study), " sites, ",
                length(baseline_sites), "of them baseline).\n"), sep = "")
    }
    comparisons <- rbind(comparisons, to_add)
  }
  
  if (verbose == TRUE){
    cat(paste("\nThere are", sum(comparisons$site1 != comparisons$site2),
              "comparisons in all, from", length(unique(comparisons$SS)),
              "studies."))
  }
  
  # Remove self-comparisons and return the skeleton data frame
  comparisons <- droplevels(subset(comparisons, site1 != site2))
}


# Get great-circle distances and Gower environmental distances -----------------

add_distances <- function(comparisons, all_pairs, verbose = TRUE){
  matches <- match(paste(comparisons$site1, comparisons$site2),
                   paste(all_pairs$site1, all_pairs$site2))
  comparisons$distance <- all_pairs$distance[matches]
  comparisons$gowerEnvDist <- all_pairs$gowerEnvDist[matches]
  
  # Warn if any NAs in what's been added.
  if(sum(is.na(comparisons$distance) > 0)){
    warning("Warning: some distance values are NA!")
  }
  if(sum(is.na(comparisons$gowerEnvDist) > 0)){
    warning("Warning: some gowerEnvDist values are NA!")
  }
  
  return(comparisons)
}

