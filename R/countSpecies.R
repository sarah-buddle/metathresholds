#' Count true and false positive species present in samples
#'
#'
#'
#' @param taxid Taxon ID
#' @param result
#' @param positive_taxids list of true positive taxids
#' @param by_type Should the results be grouped into viruses, bacteria, fungi and other eukaryotes
#' @return A named list which will become the taxonomy database entry associated with the input taxon ID.
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
