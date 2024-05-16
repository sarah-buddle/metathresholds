#' Create species-level database
#'
#'
#'
#' @param taxid Taxon ID
#' @param taxonomizr_sql sql fro taxonomizr
#' @return A named list containing fileds associated with the input taxon ID
#' @export

compareControls <- function(report_full) {

  control_report <- report_full %>%
    dplyr::mutate(rpm = reads * 10^6 / raw_read_count)




# control_report <- report %>%
#   dplyr::left_join(controls, by = c("run", "sample")) %>%
#   dplyr::filter(is.na(control)) %>% # just keep the controls
#   dplyr::select(-control, dnarna) %>%
#   dplyr::group_by(sample, run, db, species, species_taxid, type, tool) %>%
#   dplyr::summarise(control_reads = sum(reads)) %>%
#   dplyr::left_join(read_counts, by = c("sample", "run")) %>%
#   dplyr::mutate(control_rpm = control_reads*1000000/raw_reads) %>%
#   dplyr::rename(control = sample, control_raw_reads = raw_reads)
#
# report_controls_rm <- report %>%
#   dplyr::left_join(controls, by = c("run", "sample")) %>%
#   dplyr::filter(!is.na(control)) %>%
#   dplyr::rename(sample_reads = reads) %>%
#   dplyr::left_join(read_counts, by = c("sample", "run")) %>%
#   dplyr::mutate(sample_rpm = sample_reads*1000000/raw_reads) %>%
#   dplyr::rename(sample_raw_reads = raw_reads) %>%
#   dplyr::left_join(control_report, by = c("run", "db", "species", "species_taxid", "type", "tool", "control")) %>%
#   dplyr::mutate(control_rpm = ifelse(is.na(control_reads) | control_reads == 0, 1000000/sample_raw_reads, control_rpm)) %>%  # set control rpm to equivalent of 1 read in the sample
#   dplyr::mutate(rpm_ratio = sample_rpm / control_rpm)

}
