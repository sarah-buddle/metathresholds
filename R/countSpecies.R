#' Count true and false positive species present in samples and calculate sensitivity and precision
#'
#'
#'
#' @param combined_dnarna_report output of combineDNARNA
#' @param positive_species_filepath Path to csv file with list of expected positive taxon IDs
#' @param by_type if true, calculates stas per type (e.g. bacteria, virus) within each sample. Default false.
#' @return A report with a row for each sample with stats calculated
#' @export
#'

countSpecies <- function(combined_dnarna_report, positive_species_filepath, by_type = FALSE) {

  n_expected <- read.csv(positive_species_filepath, header = FALSE) %>%
    nrow(.) %>%
    as.numeric(.)

  species <- combined_dnarna_report %>%
    dplyr::filter(rank == "species") %>%
    dplyr::filter(!(taxid %in% exclude_taxids)) %>%
    dplyr::mutate(count = 1)

  if (by_type) {

    grouped_species <- species %>%
      dplyr::group_by(dnarna_pair, tool, result_category, type)

  } else {

    grouped_species <- species %>%
      dplyr::group_by(dnarna_pair, tool, result_category)

  }

  counts <- grouped_species %>%
    dplyr::summarise(total_species = sum(count)) %>%
    dplyr::filter(!is.na(result_category)) %>%
    tidyr::pivot_wider(names_from = "result_category", values_from = "total_species", values_fill = 0) %>%
    dplyr::mutate(sensitivity = true_positive / n_expected,
                  precision = true_positive / (true_positive + false_positive))

}
