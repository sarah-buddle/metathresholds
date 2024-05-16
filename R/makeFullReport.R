#' Wrapper function to calculate reads per million ratio and proportion of micorbial reads from raw classifier output
#'
#'
#'
#' @param samplesheet_filepath path to samplesheet
#' @param taxonomizr_sql sql for taxonomizr
#' @param db_filepath path to taxonomy database - provide this to avoid reproducing teh database for every analysis
#' @param collapse_species if true, any reads assigned to sub-species, strains etc will be assigned to the relevant species for analysis
#' @return A full report containing RPMR and PMR
#' @export

makeFullReport <- function(samplesheet_filepath, taxonomizr_sql, db_filepath = NULL, thresholds_filepath, collapse_species = TRUE) {

  # Import samplesheet
  samplesheet <- importSamplesheet(samplesheet_filepath)

  # Import all files in samplesheet
  reports <- importAllReports(samplesheet)

  # Make database
  db <- makeTaxDBReports(reports, taxonomizr_sql, db_filepath)

  # Combine
  report_full <- reports %>%
    dplyr::left_join(db) %>%
    dplyr::left_join(samplesheet) %>%
    dplyr::select(-filepath)

  if (collapse_species) {

    report_full <- collapseSpecies(report_full, db)

  }

  # Link controls
  report_controls <- linkControls(report_full)

  # Calculate PMR
  report_stats <- readProportions(report_controls)

  # Identify positive species using thresholds
  report_thresholds <- applyThresholdsReport(report_stats, thresholds_filepath = thresholds_filepath)



}
