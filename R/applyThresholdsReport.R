#' Link sample data to corresponding controls
#'
#'
#'
#' @param report_full reports linked with relevant control IDs
#' @return A report with control read counts linked to their relevant sample
#' @export


applyThresholdsReport <- function(report,
                            thresholds_filepath = NULL,
                            proportion = "proportion_nonhuman_classified",
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
                            low_level_euk = FALSE) {

  if (!is.null(thresholds_filepath)) {

    thresholds <- read.csv(thresholds_filepath, header = TRUE)

  }

  print(proportion)

  # report_results <- report %>%
  #   dplyr::mutate(result = applyThresholds(type, rpm_ratio, reads, control_reads, !!(rlang::ensym(proportion)),
  #                                          thresholds))

  report_results <- report %>%
    dplyr::mutate(result = applyThresholds(type, rpm_ratio, reads, control_reads, !!(rlang::ensym(proportion)),
                               thresholds,
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
                               low_level_euk))

}
