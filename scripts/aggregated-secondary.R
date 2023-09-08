# All five with secondary aggregated

collapsed_ab <- model_data_ab %>%
    mutate(
      Predominant_land_use = recode_factor(Predominant_land_use,
                                    "Young secondary vegetation" = "Aggregated secondary vegetation",
                                    "Intermediate secondary vegetation" = "Aggregated secondary vegetation",
                                    "Mature secondary vegetation" = "Aggregated secondary vegetation"),
      # reset reference levels
      Predominant_land_use = factor(Predominant_land_use),
      Predominant_land_use = relevel(Predominant_land_use, ref = "Primary"),
      Use_intensity = factor(Use_intensity),
      Use_intensity = relevel(Use_intensity, ref = "Minimal use")
    )

table(collapsed_ab$Predominant_land_use)
table(model_data_ab$Predominant_land_use)


# maximal model
ab1 <-lmer(logAbundance ~ Predominant_land_use * Higher_taxon + (1 | Source_ID) + (1 | SS) + (1 | SSB),
          data = collapsed)
# remove block
ab2 <-lmer(logAbundance ~ Predominant_land_use * Higher_taxon + (1 | Source_ID) + (1 | SS),
          data = collapsed)

# remove study
ab3 <-lmer(logAbundance ~ Predominant_land_use * Higher_taxon + (1 | Source_ID),
          data = collapsed)

# Compare models 
AIC(ab1, ab2, ab3) # ab2 slightly better

Anova(ab1)

## Backwards stepwise selection of fixed effects 

## Model with the interaction removed but both predictors still present
ab4 <- lmer(logAbundance ~ Predominant_land_use + Higher_taxon + (1 | Source_ID) + (1|SS), data = collapsed)

## Just land use as predictor
ab5 <- lmer(logAbundance ~ Predominant_land_use + (1 | Source_ID) + (1|SS), data = collapsed)

## Just Higher_taxon as the predictor
ab6 <- lmer(logAbundance ~ Higher_taxon + (1 | Source_ID) + (1|SS), data = collapsed)

## Make a null, intercept only model
ab7 <- lmer(logAbundance ~ (1 | Source_ID) + (1|SS), data = collapsed)

## Compare the models
AIC(ab1, ab2, ab3, ab4, ab5, ab6, ab7)
model.sel(ab1, ab2, ab3, ab4, ab5, ab6, ab7)

# m2 best

# model estimates
summary(ab2)
anova(ab2)

rm(ab1,ab3,ab4,ab5,ab6,ab7)

### Species richness model

# Log transform species richness
model_data_sr$logSR <- log(model_data_sr$SpeciesRichness + 1)

# Have a look at sample sizes
table(model_data_sr$Predominant_land_use, model_data_sr$Use_intensity) 

# Check the distributions
hist(model_data_sr$SpeciesRichness) ; hist(model_data_sr$logSR) ; hist(model_data_ab$logAbundance) ; hist(log(model_data_ab$TotalAbundance + 1))

# Collapse secondary and intense and light land use intensities to have a more even spread of the data

collapsed_sr <- model_data_sr %>%
  mutate(
    Predominant_land_use = recode_factor(Predominant_land_use,
                                         "Young secondary vegetation" = "Aggregated secondary vegetation",
                                         "Intermediate secondary vegetation" = "Aggregated secondary vegetation",
                                         "Mature secondary vegetation" = "Aggregated secondary vegetation"),
    # reset reference levels
    Predominant_land_use = factor(Predominant_land_use),
    Predominant_land_use = relevel(Predominant_land_use, ref = "Primary"),
    Use_intensity = recode_factor(Use_intensity,
                                  "Intense use" = "High_use",
                                  "Light use" = "High_use"),
    Use_intensity = factor(Use_intensity),
    Use_intensity = relevel(Use_intensity, ref = "Minimal use")
  )

table(collapsed_sr$Predominant_land_use, collapsed_sr$Use_intensity)
table(model_data_sr$Predominant_land_use, model_data_sr$Use_intensity)

# maximal model
sr1 <-lmer(logSR ~ Predominant_land_use * Higher_taxon + (1 | Source_ID) + (1 | SS) + (1 | SSB),
           data = collapsed_sr)

# remove block
sr2 <-lmer(logSR ~ Predominant_land_use * Higher_taxon + (1 | Source_ID) + (1 | SS),
           data = collapsed_sr)

# remove study
sr3 <-lmer(logSR ~ Predominant_land_use * Higher_taxon + (1 | Source_ID),
           data = collapsed_sr)

# Compare models 
AIC(sr1, sr2, sr3) # sr2 slightly better

Anova(sr2)

## Backwards stepwise selection of fixed effects 

## Model with the interaction removed but both predictors still present
sr4 <- lmer(logSR ~ Predominant_land_use + Higher_taxon + (1 | Source_ID) + (1|SS), data = collapsed_sr)

## Just land use as predictor
sr5 <- lmer(logSR ~ Predominant_land_use + (1 | Source_ID) + (1|SS), data = collapsed_sr)

## Just Higher_taxon as the predictor
sr6 <- lmer(logSR ~ Higher_taxon + (1 | Source_ID) + (1|SS), data = collapsed_sr)

## Make a null, intercept only model
sr7 <- lmer(logSR ~ (1 | Source_ID) + (1|SS), data = collapsed_sr)

## Compare the models
AIC(sr1, sr2, sr3, sr4, sr5, sr6, sr7)
model.sel(sr1, sr2, sr3, sr4, sr5, sr6, sr7)

# sr2 just slightly more optimal than sr1

# model estimates
summary(sr2)
anova(sr2)

rm(sr1,sr3, sr4, sr5, sr6, sr7, sr8)

# BE CLEAR TO SIMULATE THE EFFECTS OF EITHER AB OR SR BELOW!

# gather the effects and confidence intervals using simulation. Simulate 1000 times
effects <- FEsim(m2, n.sims = 1000)

# make the term column a factor so it can be recoded
effects$term <- as.factor(effects$term)

col <- effects %>% slice(1:6)
dip <- effects %>% slice(7,11:15)
hemi<- effects %>% slice(8,16:20)
hym <- effects %>% slice(9,21:25)
lep <- effects %>% slice(10,26:30)

# rename factors for ease of visualisation
col$term <- recode_factor(col$term, "(Intercept)" = "Primary",
                          "Predominant_land_useAggregated secondary vegetation" = "Aggregated SV",
                          "Predominant_land_usePlantation forest" = "Plantation",
                          "Predominant_land_usePasture" = "Pasture",
                          "Predominant_land_useCropland" = "Cropland",
                          "Predominant_land_useUrban" = "Urban")

# rename factors for ease of visualisation
dip$term <- recode_factor(dip$term, "Higher_taxonDiptera" = "Primary",
                          "Predominant_land_useAggregated secondary vegetation:Higher_taxonDiptera" = "Aggregated SV",
                          "Predominant_land_usePlantation forest:Higher_taxonDiptera" = "Plantation",
                          "Predominant_land_usePasture:Higher_taxonDiptera" = "Pasture",
                          "Predominant_land_useCropland:Higher_taxonDiptera" = "Cropland",
                          "Predominant_land_useUrban:Higher_taxonDiptera" = "Urban")

# rename factors for ease of visualisation
hemi$term <- recode_factor(hemi$term, "Higher_taxonHemiptera" = "Primary",
                           "Predominant_land_useAggregated secondary vegetation:Higher_taxonHemiptera" = "Aggregated SV",
                           "Predominant_land_usePlantation forest:Higher_taxonHemiptera" = "Plantation",
                           "Predominant_land_usePasture:Higher_taxonHemiptera" = "Pasture",
                           "Predominant_land_useCropland:Higher_taxonHemiptera" = "Cropland",
                           "Predominant_land_useUrban:Higher_taxonHemiptera" = "Urban")

# rename factors for ease of visualisation
hym$term <- recode_factor(hym$term, "Higher_taxonHymenoptera" = "Primary",
                          "Predominant_land_useAggregated secondary vegetation:Higher_taxonHymenoptera" = "Aggregated SV",
                          "Predominant_land_usePlantation forest:Higher_taxonHymenoptera" = "Plantation",
                          "Predominant_land_usePasture:Higher_taxonHymenoptera" = "Pasture",
                          "Predominant_land_useCropland:Higher_taxonHymenoptera" = "Cropland",
                          "Predominant_land_useUrban:Higher_taxonHymenoptera" = "Urban")

# rename factors for ease of visualisation
lep$term <- recode_factor(lep$term, "Higher_taxonLepidoptera" = "Primary",
                          "Predominant_land_useAggregated secondary vegetation:Higher_taxonLepidoptera" = "Aggregated SV",
                          "Predominant_land_useIntermediate secondary vegetation:Higher_taxonLepidoptera" = "ISV",
                          "Predominant_land_useMature secondary vegetation:Higher_taxonLepidoptera" = "MSV",
                          "Predominant_land_usePlantation forest:Higher_taxonLepidoptera" = "Plantation",
                          "Predominant_land_usePasture:Higher_taxonLepidoptera" = "Pasture",
                          "Predominant_land_useCropland:Higher_taxonLepidoptera" = "Cropland",
                          "Predominant_land_useUrban:Higher_taxonLepidoptera" = "Urban")

# Add in upper and lower confidence intervals 

col <- col %>%
  mutate(Upper_ci = (median + 1.96*sd)) %>%
  mutate(Lower_ci = (median - 1.96*sd))

dip <- dip %>%
  mutate(Upper_ci = (median + 1.96*sd)) %>%
  mutate(Lower_ci = (median - 1.96*sd))

hemi <- hemi %>%
  mutate(Upper_ci = (median + 1.96*sd)) %>%
  mutate(Lower_ci = (median - 1.96*sd))

hym <- hym %>%
  mutate(Upper_ci = (median + 1.96*sd)) %>%
  mutate(Lower_ci = (median - 1.96*sd))

lep <- lep %>%
  mutate(Upper_ci = (median + 1.96*sd)) %>%
  mutate(Lower_ci = (median - 1.96*sd))


# Back transform the log abundance/species richness estimates
col <- col %>%
  mutate(
    Percent_diff = (((exp(median[1] + median) - 1) / (exp(median[1]) - 1))*100)-100) %>%
  mutate(
    Percent_upper = (((exp(median[1] + Upper_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100) %>%
  mutate(
    Percent_lower = (((exp(median[1] + Lower_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100)

dip <- dip %>%
  mutate(
    Percent_diff = (((exp(median[1] + median) - 1) / (exp(median[1]) - 1))*100)-100) %>%
  mutate(
    Percent_upper = (((exp(median[1] + Upper_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100) %>%
  mutate(
    Percent_lower = (((exp(median[1] + Lower_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100)

hemi <- hemi %>%
  mutate(
    Percent_diff = (((exp(median[1] + median) - 1) / (exp(median[1]) - 1))*100)-100) %>%
  mutate(
    Percent_upper = (((exp(median[1] + Upper_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100) %>%
  mutate(
    Percent_lower = (((exp(median[1] + Lower_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100)

hym <- hym %>%
  mutate(
    Percent_diff = (((exp(median[1] + median) - 1) / (exp(median[1]) - 1))*100)-100) %>%
  mutate(
    Percent_upper = (((exp(median[1] + Upper_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100) %>%
  mutate(
    Percent_lower = (((exp(median[1] + Lower_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100)

lep <- lep %>%
  mutate(
    Percent_diff = (((exp(median[1] + median) - 1) / (exp(median[1]) - 1))*100)-100) %>%
  mutate(
    Percent_upper = (((exp(median[1] + Upper_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100) %>%
  mutate(
    Percent_lower = (((exp(median[1] + Lower_ci) - 1) / (exp(median[1]) - 1)) * 100) - 100)


# Shift the baseline down to 0

col[1,7] <- 0
col[1,8] <- 0
col[1,9] <- 0

dip[1,7] <- 0
dip[1,8] <- 0
dip[1,9] <- 0

hemi[1,7] <- 0
hemi[1,8] <- 0
hemi[1,9] <- 0

hym[1,7] <- 0
hym[1,8] <- 0
hym[1,9] <- 0

lep[1,7] <- 0
lep[1,8] <- 0
lep[1,9] <- 0


# Rename the sim eff term column name for joining
colnames(col)[1] <- "Predominant_land_use"
colnames(dip)[1] <- "Predominant_land_use"
colnames(hemi)[1] <- "Predominant_land_use"
colnames(hym)[1] <- "Predominant_land_use"
colnames(lep)[1] <- "Predominant_land_use"


# Add in an order column for grouping
col <- col %>% mutate(Order = "Coleoptera")
dip <- dip %>% mutate(Order = "Diptera")
hym <- hym %>% mutate(Order = "Hymenoptera")
hemi <- hemi %>% mutate(Order = "Hemiptera")
lep <- lep %>% mutate(Order = "Lepidoptera")


# Combine all the predictions
combined <- rbind(col, dip, hemi, hym, lep)

# Add land-use factors for plotting
comb_plot_limits <- c("Primary", "Aggregated SV", "Plantation", "Pasture", "Cropland", "Urban") 

# Add specific colours for each order
group_colours <- c(Diptera = "#FFC300", Lepidoptera = "#ff5733", Hymenoptera = "#c70039", Hemiptera = "#900c3f", Coleoptera = "#581845")


plot <- combined %>%
  ggplot()+
  aes(x = Predominant_land_use, y = Percent_diff, colour = Order, group = Order)+
  geom_hline(yintercept = 0, linewidth = 0.5, color = ("black"), linetype = 1)+
  geom_point(size = 3, position = position_dodge(width = 0.5))+
  geom_linerange(aes(ymin = Percent_lower, ymax = Percent_upper), position = position_dodge(width = 0.5), linewidth = 1)+
  theme_classic()+
  scale_x_discrete(limits=comb_plot_limits)+
  geom_vline(xintercept = 1.5, linetype = 2)+
  geom_vline(xintercept = 2.5, linetype = 2)+
  geom_vline(xintercept = 3.5, linetype = 2)+
  geom_vline(xintercept = 4.5, linetype = 2)+
  geom_vline(xintercept = 5.5, linetype = 2)+
  theme(axis.text.x = element_text(face= "bold", angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold"),
        panel.border = element_rect(colour = "black",  fill=NA))+
  xlab("Land use intensity class")+
  ylab("Total abundance difference (%)") +
  scale_colour_manual(values=group_colours) + # this overrides the colours for the groups as above
  guides(color = guide_legend(
    override.aes=list(shape = 15, size = 8)))



flies <- model_data_ab %>% dplyr::select(SS, Order, Predominant_land_use, logAbundance) %>% filter(Order == "Diptera")

table(flies$Predominant_land_use) 

flies %>% filter(Predominant_land_use == "Young secondary vegetation") %>% View()
flies %>% filter(Predominant_land_use == "Intermediate secondary vegetation") %>% View()
flies %>% filter(Predominant_land_use == "Mature secondary vegetation") %>% View()

flies %>% dplyr::select(SS, Order, Predominant_land_use, logAbundance) %>% 
  filter(Predominant_land_use == c("Young secondary vegetation","Intermediate secondary vegetation","Mature secondary vegetation")) %>% View()


flies %>% filter(Predominant_land_use == c("Young secondary vegetation","Intermediate secondary vegetation","Mature secondary vegetation")) %>% View()

flies %>% dplyr::select(Predominant_land_use, logAbundance) %>% filter(Predominant_land_use %in% c("Young secondary vegetation", 
                                                                                                 "Intermediate secondary vegetation",
                                                                                                  "Mature secondary vegetation")) %>% View()








hist(flies$logAbundance)



# Extract f ratio, df and p value
anova(dhy_m2)

# Check residuals
hist(residuals(dhy_m2))

# Check r2
r.squaredGLMM(dhy_m2)

# Extract correlation coefficients
corr_dip_hym <- as.matrix(summary(dhy_m2)$coefficients)

# Split the coefficients into the two orders
diptera <- corr_dip_hym[2:8, 1]
hymenoptera <- corr_dip_hym[10:nrow(corr_dip_hym), 1] 

# Plot the coefficients
plot(diptera, hymenoptera)
cor(diptera, hymenoptera)


rm(dhy_m2, dhy_m4, dhy_m5, dhy_m6, dhy_m7, hym_1, hym_eff, diptera_hymenoptera, dip_eff, corr_dip_hym, diptera, hymenoptera)