# site_comparison_functions.R

# These functions are intended to be run only when the database has new data.

# They help to quantify differences between pairs of sites within each study.


# Make every pairwise comparison between sites within each study ---------------

compare_all_pairs <- function(sites, verbose = TRUE){
  # This function sets up a data frame in which each pair of different sites 
  # within each study produces two rows - a vs b and b vs a. The only difference
  # that this function actually calculates itself is the distance between them,
  # in units of metres (NOT in terms of how many Maximum_linear_extents). Other
  # differences get filled in by later functions. This function does not make
  # any attempt to be efficient in terms of computation or memory - the aim is 
  # clarity.
  
  require(geosphere) # For calculating Haversine distance between paired sites.
  
  sites <- as.data.frame(sites) # Getting rid of any tibble structure.
  
  studies <- unique(sites$SS)
  if (verbose == TRUE) cat(paste("The data frame has", nrow(sites), 
                                 "sites from", length(studies),
                                 "studies and", length(unique(sites$Source_ID)),
                                 "sources.\n"))
  
  how_many <- length(studies) # Set to something small for testing purposes.

  # Set up structure of data frame to hold the information on the pair of sites.
  all_pairs <- data.frame(Source_ID = NULL,
                          SS = NULL,
                          site1 = NULL,
                          site1_Latitude = NULL,
                          site1_Longitude = NULL,
                          site1_when = NULL,
                          site2 = NULL,
                          site2_Latitude = NULL,
                          site2_Longitude = NULL,
                          site2_when = NULL)
  
  for (i in studies[1:how_many]){
    s <- sites[sites$SS == i, ]
    n_s <- nrow(s) # Number of sites within the study
    this_study <- data.frame(Source_ID = rep(s$Source_ID[1], n_s ^ 2),
                             SS = rep(i, n_s ^ 2),
                             site1 = rep(s$SSBS, each = n_s),
                             site1_Latitude = rep(s$Latitude, each = n_s),
                             site1_Longitude = rep(s$Longitude, each = n_s),
                             site1_when = rep(median(c(
                               s$Sample_start_earliest, s$Sample_end_latest)),
                               each = n_s),
                             site2 = rep(s$SSBS, times = n_s),
                             site2_Latitude = rep(s$Latitude, times = n_s),
                             site2_Longitude = rep(s$Longitude, times = n_s),
                             site2_when = rep(median(c(
                               s$Sample_start_earliest, s$Sample_end_latest)),
                               times = n_s))
    if (verbose == TRUE){
      cat(paste(i, ": ", n_s * (n_s - 1), " comparisons...\n"))
    }
    all_pairs <- rbind(all_pairs, this_study)
  }
  
  # Remove self-comparisons.
  all_pairs <- subset(all_pairs, site1 != site2)
  
  if (verbose == TRUE){
    cat(paste("\nThe resulting data frame has",
              nrow(all_pairs), "pairs of sites.\n"))
  }
  
  # Calculate Haversine distance between each pair of sites.
  # First pull out the longitudes and latitudes for each site combination
  s1LongLat <- as.matrix(all_pairs[, c("site1_Longitude", "site1_Latitude")])
  s2LongLat <- as.matrix(all_pairs[, c("site2_Longitude", "site2_Latitude")])
  
  # Then calculate the distance between sites.
  all_pairs$distance <- distHaversine(s1LongLat, s2LongLat)
  
  if(verbose == TRUE){
    cat(paste("Haversine distances calculated for", 
              sum(!is.na(all_pairs$distance)), "pairs of sites.\n"))
  }
  
  # Add provenance attribute.
  attr(all_pairs, which = "sites_extract") <- attr(sites, 
                                                   which = "sites_extract")
  
  return(all_pairs)
}




