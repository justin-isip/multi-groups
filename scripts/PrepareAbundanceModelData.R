#' Prepare Data for Abundance Modelling
#'
#' This function reads species abundance data, performs extensive preprocessing
#' (including transformations, filtering, and feature engineering), and saves
#' the final dataset along with scaling values for use in abundance modelling.
#'
#' The input data is expected in a `.qs` format, either locally or from an S3 bucket.
#' The output includes a transformed and cleaned dataset ready for modeling,
#' as well as a separate file containing the scaling parameters for numeric variables.
#'
#' @param file_abundance_data Character. Path to the input `.qs` file containing species abundance data.
#' @param dir_out Character. Directory where the output files will be saved.
#' @param loc_abundance_data Character. Location of the input file. Either `"local"` or `"s3"`. Default is `"s3|local"`.
#' @param loc_out Character. Location to save the output files. Either `"local"` or `"s3"`. Default is `"s3|local"`.
#'
#' @return Character. The full path (as a string) to the saved model-ready dataset.
#'
#' @export
#' @importFrom lubridate interval
PrepareAbundanceModelData <- function(
    file_abundance_data,
    dir_out,
    loc_abundance_data = "s3|local",
    loc_out = "s3|local"
) {
  .SetLoggerLayout()

  logger::log_info("check the inputs")
  .CheckFileExists(file_abundance_data)
  .CheckDirExists(dir_out)

  logger::log_info("specify the file path to hold the results")
  file_out <- file.path(dir_out, "abundance-model-data.qs")

  logger::log_info("read in the data input and check the attributes")
  df_abundance <- .ReadQSFile(
    path = file_abundance_data,
    location = loc_abundance_data
  )

  # check the attributes
  .CheckDiversityAttributes(df_abundance)

  logger::log_info("transform the data to prepare for modelling")
  df_model_data <- df_abundance |>
    # Rescale the abundance data (previously this was in pipe-predicts)
    .RescaleAbundance() |>
    # transform the abundance measurement
    .LogTransformAbundance() |>
    # Timber has a limited number of sites and we're not modelling it
    # so drop these data
    dplyr::filter(
      LUH2 != "Timber",
      # remove data with unusual metrics (the values are on different scales)
      !Diversity_metric %in% c("relative abundance", "sign density", "biovolume"),
      # remove a bacteria study that hasn't had its taxonomy resolved
      Source_ID != "YL1_2021__Ohigashi",
      # remove any other studies where the taxonomy hasn't been resolved
      !is.na(Taxon_fold_ss)
    ) |>
    # collapse other levels that have limited data e.g.
    # Mature_secondary_vegetation_Intense = 30 sites
    # and combine N-fixing crops with Annual
    dplyr::mutate(
      LUH2 = dplyr::recode_factor(LUH2, "Nitrogen_fixing" = "Annual"),
      LUH2 = ifelse(
        LUH2_UI == "Primary_Minimal",
        "Primary_Minimal",
        paste(LUH2)
      ),
      LUH2 = factor(LUH2),
      LUH2_UI = dplyr::recode_factor(
        LUH2_UI,
        "Mature_secondary_vegetation_Intense" = "Mature_secondary_vegetation_Light_Intense",
        "Mature_secondary_vegetation_Light" = "Mature_secondary_vegetation_Light_Intense",
        "Nitrogen_fixing_Minimal" = "Annual_Minimal",
        "Nitrogen_fixing_Light" = "Annual_Light",
        "Nitrogen_fixing_Intense" = "Annual_Intense",
      ),
      Baseline_v_not = ifelse(
        LUH2 == "Primary_Minimal",
        "Baseline",
        "Not_baseline"
      ),
      Baseline_v_not = factor(Baseline_v_not),
      Baseline_v_not = relevel(Baseline_v_not, ref = "Baseline"),
      # transform the variables
      log_hpd = log1p(hpd),
      log_hpd_scaled = scale(log_hpd),
      log_t30 = log1p(t30),
      log_t30_scaled = scale(log_t30),
      nathab_scaled = scale(Landscape_Natural_Habitat),
      study_hpd_scaled = scale(study_hpd),

      # pull out the means and standard deviations so we can scale the rasters
      mean_log_hpd = mean(log_hpd, na.rm = TRUE),
      sd_log_hpd = sd(log_hpd, na.rm = TRUE),
      max_log_hpd_scaled = max(log_hpd_scaled, na.rm = TRUE),
      mean_log_t30 = mean(log_t30, na.rm = TRUE),
      sd_log_t30 = sd(log_t30, na.rm = TRUE),
      max_log_t30_scaled = max(log_t30_scaled, na.rm = TRUE),
      mean_nathab = mean(Landscape_Natural_Habitat, na.rm = TRUE),
      sd_nathab = sd(Landscape_Natural_Habitat, na.rm = TRUE),
      max_nathab_scaled = max(nathab_scaled, na.rm = TRUE)
    ) |>
    # drop missing values
    tidyr::drop_na(
      Log_abundance_pX,
      log_hpd_scaled,
      study_hpd_scaled,
      log_t30_scaled,
      nathab_scaled,
      LUH2,
      LUH2_UI
    ) |>
    # add polynomials
    dplyr::mutate(
      log_hpd_scaled_p1 = poly(log_hpd_scaled, 2)[, 1],
      log_hpd_scaled_p2 = poly(log_hpd_scaled, 2)[, 2],
      LUH2 = relevel(LUH2, ref = "Primary_Minimal"),
      LUH2_UI = relevel(LUH2_UI, ref = "Primary_Minimal")
    ) |>
    dplyr::select(
      Log_abundance_pX,
      LUH2,
      Use_intensity,
      LUH2_UI,
      Baseline_v_not,
      log_hpd_scaled,
      log_hpd_scaled_p1, log_hpd_scaled_p2,
      log_t30_scaled,
      nathab_scaled,
      study_hpd_scaled,
      Taxon_fold_ss, SS, SSB, SSBS,
      mean_log_hpd,
      sd_log_hpd,
      max_log_hpd_scaled,
      mean_log_t30,
      sd_log_t30,
      max_log_t30_scaled,
      mean_nathab,
      sd_nathab,
      max_nathab_scaled
    ) |>
    droplevels()

  logger::log_info("save out the scaled values separately for later")
  df_scaled <- df_model_data |>
    dplyr::select(
      mean_log_hpd,
      sd_log_hpd,
      max_log_hpd_scaled,
      mean_log_t30,
      sd_log_t30,
      max_log_t30_scaled,
      mean_nathab,
      sd_nathab,
      max_nathab_scaled
    ) |>
    dplyr::distinct()

  logger::log_info("save out the scaled values")
  .WriteQSFile(
    object = df_scaled,
    path = file.path(dir_out, "abundance-data-scaling-values.qs"),
    location = loc_out
  )

  logger::log_info("save out the final output model data ({file_out})")
  .WriteQSFile(
    object = df_model_data,
    path = file_out,
    location = loc_out
  )

  # return the file path
  return(file_out)

}

# Rescale the abundance by the study-level maxima; add columns for
# `Max_abundance` and `Rescaled_abundance`.
.RescaleAbundance <- function(df_abundance) {
  result <- df_abundance |>
    dplyr::mutate(
      Max_abundance = max(Total_abundance, na.rm = TRUE),
      .by = SS
    ) |>
    dplyr::mutate(
      Rescaled_abundance = Total_abundance / Max_abundance
    )

  return(result)
}

# Log-transform abundance; add in a column for the minimum study-level abundance
.LogTransformAbundance <- function(df_abundance) {
  result <- df_abundance |>
    dplyr::mutate(
      Min_non_zero_ss = min(Min_non_zero_abundance),
      .by = SS
    ) |>
    dplyr::mutate(
      Log_plus_what = ifelse(
        Diversity_metric_unit %in% c("individuals", "times observed"),
        1,
        Min_non_zero_ss / 2
      ),
      Log_abundance_pX = log(Total_abundance + Log_plus_what)
    )

  return(result)
}
