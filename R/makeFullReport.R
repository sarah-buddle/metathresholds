#' Wrapper function to use provided thresholds to filter the raw classifier outputs provided in the samplesheet.
#'
#'
#'
#' @param samplesheet_filepath Path to samplesheet. See https://github.com/sarah-buddle/metathresholds for a template.
#' @param taxonomizr_sql Path to nameNode.sqlite file for taxonomizr. If this doesn't exist, a new one will be created in this location. Generating this file may take some time.
#' @param db_filepath Path to taxonomy database used by metaThresholds. If this doesn't exist, a new one will be created in this location. If no filepath is provided, a new database will be generated for each analysis.
#' @param thresholds_filepath Path to file containing thresholds. See https://github.com/sarah-buddle/metathresholds for a template.
#' @param reads_thres_bac
#' @param reads_thres_vir
#' @param reads_thres_fun
#' @param reads_thres_euk
#' @param rpm_ratio_thres_bac
#' @param rpm_ratio_thres_vir
#' @param rpm_ratio_thres_fun
#' @param rpm_ratio_thres_euk
#' @param proportion_thres_bas
#' @param proportion_thres_vir
#' @param proportion_thres_fun
#' @param proportion_thres_euk
#' @param low_level_bac
#' @param low_level_vir
#' @param low_level_fun
#' @param low_level_euk
#' @param collapse_species if true, any reads assigned to sub-species, strains etc will be assigned to the relevant species for analysis
#' @param keep_species_only
#' @param virus_only
#' @return Report with taxons classified as positive and negative according to thresholds.
#' @export

makeFullReport <- function(samplesheet_filepath = "./samplesheet.csv",
                           taxonomizr_sql = "./nameNode.sqlite",
                           db_filepath = NA,
                           thresholds_filepath = "./thresholds.csv",
                           proportion = "proportion_nonhuman_classified",
                           positive_species_filepath = NULL,
                           reads_thres_bac = 0,
                           reads_thres_vir = 0,
                           reads_thres_fun = 0,
                           reads_thres_euk = 0,
                           rpm_ratio_thres_bac = 0,
                           rpm_ratio_thres_vir = 0,
                           rpm_ratio_thres_fun = 0,
                           rpm_ratio_thres_euk = 0,
                           proportion_thres_bac = 0,
                           proportion_thres_vir = 0,
                           proportion_thres_fun = 0,
                           proportion_thres_euk = 0,
                           low_level_bac = FALSE,
                           low_level_vir = FALSE,
                           low_level_fun = FALSE,
                           low_level_euk = FALSE,
                           collapse_species = TRUE,
                           keep_species_only = TRUE,
                           virus_only = FALSE) {

  # Import samplesheet
  samplesheet <- importSamplesheet(samplesheet_filepath)

  # Import all files in samplesheet
  reports <- importAllReports(samplesheet)

  # Make database
  db <- makeTaxDBReports(reports, taxonomizr_sql, db_filepath)

  # Combine
  report_full <- reports %>%
    dplyr::mutate(taxid = as.character(taxid)) %>%
    dplyr::left_join(db) %>%
    dplyr::left_join(samplesheet) %>%
    dplyr::select(-filepath)

  if (collapse_species) {

    report_full <- collapseSpecies(report_full, db)

  }

  if (keep_species_only) {

    report_full <- report_full %>%
      dplyr::filter(rank == "species" & !is.na(type))

  }

  # Link controls
  report_controls <- linkControls(report_full)

  # Calculate PMR
  report_stats <- readProportions(report_controls)

  if (virus_only) {

    report_stats <- report_stats %>%
      dplyr::filter(type == "Virus")

  }

  # Identify positive species using thresholds
  report_thresholds <- applyThresholdsReport(report_stats,
                                             thresholds_filepath = thresholds_filepath,
                                             proportion,
                                             reads_thres_bac,
                                             reads_thres_vir,
                                             reads_thres_fun,
                                             reads_thres_euk,
                                             rpm_ratio_thres_bac,
                                             rpm_ratio_thres_vir,
                                             rpm_ratio_thres_fun,
                                             rpm_ratio_thres_euk,
                                             proportion_thres_bac,
                                             proportion_thres_vir,
                                             proportion_thres_fun,
                                             proportion_thres_euk,
                                             low_level_bac,
                                             low_level_vir,
                                             low_level_fun,
                                             low_level_euk)

  # Determine if results are correct
  if (!is.null(positive_species_filepath)) {

    positive_species_df <- read.csv(positive_species_filepath, header = FALSE)

    report_results <- report_thresholds %>%
      dplyr::mutate(result_category = isCorrect(taxid, rank, result, positive_species_df$V1))

  }


}
