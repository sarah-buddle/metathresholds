#' Combine DNA and RNA results, retaining only whichever method gives a higher rpmr
#'
#'
#'
#' @param report Output of makeFullReport
#' @return Combine DNA and RNA report
#' @export


combineDNARNA <- function(report) {

  report_combined_na <- report %>%
    dplyr::arrange(dnarna_pair, tool, taxid, desc(result), desc(rpm_ratio)) %>%
    dplyr::distinct(dnarna_pair, tool, taxid, .keep_all = TRUE)

}
