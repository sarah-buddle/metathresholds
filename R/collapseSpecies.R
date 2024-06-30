#' Wrapper function to calculate reads per million ratio and proportion of micorbial reads from raw classifier output
#'
#'
#'
#' @param report_full reports combined with db and sample info
#' @param db output of makeTaxDBReports
#' @return Report collapsed by species
#' @export

collapseSpecies <- function(report_full, db) {

  names_ranks <- db %>%
    dplyr::filter(is.na(species_taxid) | taxid == species_taxid) %>%
    dplyr::select(name, name_speciesorhigher, taxid, rank) %>%
    dplyr::distinct()

  report_collapsed <- report_full %>%
    dplyr::group_by(sample_id, tool, type, name_speciesorhigher, species_taxid,
                    corresponding_control_id, dnarna, dnarna_pair, total_raw_reads_sample) %>%
    dplyr::summarise(reads = sum(reads)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(names_ranks)

  return(report_collapsed)

}
