# Function for predicting abundance or species richness percentage differences
# From baseline of primary veg based on model coefficients

predict_this_model <- function(model, order, percentage_diff, baseline, add_primary) {
  
  # gather the effects and confidence intervals using simulation. Simulate 1000 times
  effects <- FEsim(model, n.sims = 1000)
  
  # make the term column a factor so it can be recoded
  effects$term <- as.factor(effects$term)
  
  # pull out effects
  effects <- effects %>%
    slice(1:6) 
  
  # rename factors for ease of visualisation
  effects$term <- recode_factor(effects$term, "(Intercept)" = "Primary",
                                #"Predominant_land_useYoung secondary vegetation" = "YSV",
                                #"Predominant_land_useIntermediate secondary vegetation" = "ISV",
                                #"Predominant_land_useMature secondary vegetation" = "MSV",
                                "Predominant_land_useSecondary vegetation" = "Secondary vegetation",
                                "Predominant_land_usePlantation forest" = "Plantation",
                                "Predominant_land_usePasture" = "Pasture",
                                "Predominant_land_useCropland" = "Cropland",
                                "Predominant_land_useUrban" = "Urban")
  
  
  # Add in upper and lower confidence intervals 
  effects <- effects %>%
    mutate(Upper_ci = (median + 1.96*sd)) %>%
    mutate(Lower_ci = (median - 1.96*sd))

  

  # if I don't supply percentage_diff as an argument then do nothing
  if(missing(percentage_diff)) {
    NULL
  }
  
  else {
    # This calculates the percentage difference based on each of the coefficients relative to primary
    if(percentage_diff == "YES") {
      effects <- effects %>%
        mutate(
          Percent_diff = (((exp(median[1] + median) - 1) / (exp(median[1]) - 1))*100)-100) %>%
        mutate(
          Percent_upper = (((exp(median[1] + Upper_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100) %>%
        mutate(
          Percent_lower = (((exp(median[1] + Lower_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100)
    }}

  
  if(missing(baseline)) {
    NULL
  }
  
  else {
    
    if(baseline == "median"){
      # Use this when you've set primary to zero but are looking on the raw/absolute values scale in log space
      effects[1,3] <- 0 #shift median
      effects[1,5] <- 0 #shift upper
      effects[1,6] <- 0 #shift lower 
    }
    if(baseline == "percentage"){
      # Use this when you've set primary to zero and are looking on the percentage difference scale
      effects[1,7] <- 0 #shift median
      effects[1,8] <- 0 #shift upper
      effects[1,9] <- 0 #shift lower 
    }}
 
  

  # Rename the sim eff term column name for joining
  colnames(effects)[1] <- "Predominant_land_use"
  
  # Add in a column of Order to make sure there is a grouping variable
  if(order == "Diptera"){
   effects <- effects %>% mutate(Order = "Diptera")
  }
  if(order == "Coleoptera"){
    effects <- effects %>% mutate(Order = "Coleoptera")
  }
  if(order == "Hymenoptera"){
    effects <- effects %>% mutate(Order = "Hymenoptera")
  }
  if(order == "Hemiptera"){
    effects <- effects %>% mutate(Order = "Hemiptera")
  }
  if(order == "Lepidoptera"){
    effects <- effects %>% mutate(Order = "Lepidoptera")
  }
  
  # make the order column a factor so it can be recoded
  effects$Order <- as.factor(effects$Order)
  
  return(effects)
}

