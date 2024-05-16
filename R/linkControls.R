#' Link sample data to corresponding controls
#'
#'
#'
#' @param report_full reports linked with relevant control IDs
#' @return A report with control read counts linke dto their relevant sample
#' @export


linkControls <- function(report_full) {

  report_rpm <- report_full %>%
    dplyr::mutate(rpm = reads * 10^6 / total_raw_reads_sample)

  control_raw <- report_full %>%
    dplyr::filter(corresponding_control_id == "control") %>%
    dplyr::select(sample_id, total_raw_reads_sample) %>%
    dplyr::rename(corresponding_control_id = sample_id,
                  total_raw_reads_control = total_raw_reads_sample) %>%
    dplyr::distinct()

  controls <- report_rpm %>%
    dplyr::filter(corresponding_control_id == "control") %>%
    dplyr::select(-corresponding_control_id) %>%
    dplyr::rename(corresponding_control_id = sample_id,
                  control_reads = reads,
                  control_rpm = rpm,
                  total_raw_reads_control = total_raw_reads_sample)

  control_raw <- controls %>%
    dplyr::select(corresponding_control_id, total_raw_reads_control) %>%
    dplyr::distinct()


  report_controls <- report_rpm %>%
    dplyr::filter(corresponding_control_id != "control") %>%
    dplyr::left_join(dplyr::select(controls, -total_raw_reads_control)) %>%
    dplyr::left_join(control_raw) %>%
    dplyr::mutate(control_reads = tidyr::replace_na(control_reads, 0),
                  control_rpm = tidyr::replace_na(control_reads, 0)) %>%
    dplyr::mutate(rpm_ratio = rpm / control_rpm) %>%
    dplyr::mutate(rpm_ratio = ifelse(control_reads == 0, reads, rpm_ratio))

}
