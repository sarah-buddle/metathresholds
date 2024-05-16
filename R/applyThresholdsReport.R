#' Link sample data to corresponding controls
#'
#'
#'
#' @param report_full reports linked with relevant control IDs
#' @return A report with control read counts linke dto their relevant sample
#' @export


applyThresholdsReport <- function(report,
                            thresholds_filepath = NULL,
                            proportion = "proportion_nonhuman_classified",
                            rpm_ratio_thres_bac = 0,
                            rpm_ratio_thres_vir = 0,
                            rpm_ratio_thres_fun = 0,
                            rpm_ratio_thres_euk = 0,
                            proportion_thres_bac = 0,
                            proportion_thres_vir = 0,
                            proportion_thres_fun = 0,
                            proportion_thres_euk = 0,
                            reads_thres_vir = 0,
                            reads_thres_fun = 0,
                            control_reads_thres_vir = 0) {

  thresholds <- read.csv(thresholds_filepath)

  report_results <- report %>%
    dplyr::mutate(result = applyThresholds(type, rpm_ratio, reads, control_reads, proportion,
                               thresholds,
                               rpm_ratio_thres_bac,
                               rpm_ratio_thres_vir,
                               rpm_ratio_thres_fun,
                               rpm_ratio_thres_euk,
                               proportion_thres_bac,
                               proportion_thres_vir,
                               proportion_thres_fun,
                               proportion_thres_euk,
                               reads_thres_vir,
                               reads_thres_fun,
                               control_reads_thres_vir))

}
