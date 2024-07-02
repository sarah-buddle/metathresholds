#' Link sample data to corresponding controls
#'
#'
#'
#' @param report_full reports linked with relevant control IDs - see makeFullReport
#' @return A report with control read counts linked to their relevant sample
#' @export


linkControls <- function(report_full) {

  # Calculate rpm
  report_rpm <- report_full %>%
    dplyr::mutate(rpm = reads * 10^6 / total_raw_reads_sample)

  # Get just control info
  controls <- report_rpm %>%
    dplyr::filter(corresponding_control_id == "control") %>%
    dplyr::select(-corresponding_control_id, -dnarna_pair) %>%
    dplyr::rename(corresponding_control_id = sample_id,
                  control_reads = reads,
                  control_rpm = rpm,
                  total_raw_reads_control = total_raw_reads_sample)

  # Get raw control read numbers
  control_raw <- controls %>%
    dplyr::select(corresponding_control_id, total_raw_reads_control) %>%
    dplyr::distinct()


  # Combine everything and use calculate rpmr
  report_controls <- report_rpm %>%
    dplyr::filter(corresponding_control_id != "control") %>%
    dplyr::left_join(dplyr::select(controls, -total_raw_reads_control)) %>%
    dplyr::left_join(control_raw) %>%
    dplyr::mutate(control_reads = tidyr::replace_na(control_reads, 0),
                  control_rpm = tidyr::replace_na(control_rpm, 0)) %>%
    dplyr::mutate(rpm_ratio = rpm / control_rpm) %>%
    dplyr::mutate(rpm_ratio = ifelse(control_reads == 0, reads, rpm_ratio))

}
