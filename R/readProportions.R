#' Calculate proportions of total, nonhuman classified and reads of that type
#'
#'
#'
#' @param report_controls output of linkControls
#' @param contaminant_taxids list of contaminant taxids to remove
#' @return A named list containing fileds associated with the input taxon ID
#' @export


readProportions <- function(report_controls, contaminant_taxids = NULL) {

  human_taxids <- c(131567, 2759, 33154, 33208, 6072, 33213, 33511, 7711, 89593, 7742, 7776, 117570, 117571,
                    8287, 1338369, 32523, 32524, 40674, 32525, 9347, 1437010, 314146, 9443, 376913, 9526, 314295, 9604, 207598, 9605, 9606)

  human_taxons <- c("cellular organisms", "Eukaryota", "Opisthokonta", "Metazoa", "Eumetazoa", "Bilateria", "Deuterostomia", "Chordata",
                    "Craniata", "Vertebrata", "Gnathostomata", "Teleostomi", "Euteleostomi", "Sarcopterygii", "Dipnotetrapodomorpha", "Tetrapoda",
                    "Amniota", "Mammalia", "Theria", "Eutheria", "Boreoeutheria", "Euarchontoglires", "Primates", "Haplorrhini", "Simiiformes", "Catarrhini",
                    "Hominoidea", "Hominidae", "Homininae", "Homo", "Homo sapiens")

  unclassified_taxons <- c("unclassified", "root", "unidentified", "uncultured organism")

totals_nonhuman_classified <- report_controls %>%
  dplyr::filter(!(name_speciesorhigher %in% unclassified_taxons)
                & !(name_speciesorhigher %in% human_taxons)
                & !(taxid %in% contaminant_taxids)
                 & !(rank %in% unclassified_taxons)
                & !is.na(type)) %>%
  dplyr::group_by(sample_id, tool) %>%
  dplyr::summarise(total_nonhuman_classified_reads_sample = sum(reads))

totals_type <- report_controls %>%
  dplyr::filter(!is.na(type) & !(taxid %in% contaminant_taxids)) %>%
  dplyr::group_by(sample_id, tool, type) %>%
  dplyr::summarise(total_type_reads_sample = sum(reads))

report_final <- report_controls %>%
  dplyr::left_join(totals_nonhuman_classified, by = c("sample_id", "tool")) %>%
  dplyr::left_join(totals_type, by = c("sample_id", "tool", "type")) %>%
  dplyr::mutate(proportion_raw = reads / total_raw_reads_sample,
                proportion_nonhuman_classified = reads / total_nonhuman_classified_reads_sample,
                proportion_type = reads / total_type_reads_sample) %>%
  dplyr::mutate(proportion_nonhuman_classified = ifelse(name_speciesorhigher %in% c(unclassified_taxons, human_taxons) |
                                                        taxid %in% contaminant_taxids,
                                                        NA, proportion_nonhuman_classified),
                proportion_type = ifelse(taxid %in% contaminant_taxids, NA, proportion_type)) %>%
  dplyr::select(sample_id, dnarna, dnarna_pair, tool, name, taxid, type, rank, name_speciesorhigher, species_taxid, reads, total_raw_reads_sample, rpm,
                corresponding_control_id, control_reads, total_raw_reads_control, control_rpm,
                rpm_ratio, proportion_raw, proportion_nonhuman_classified, total_nonhuman_classified_reads_sample, proportion_type, total_type_reads_sample)

}
