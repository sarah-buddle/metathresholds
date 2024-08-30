#' import outputs of taxonomic classifiers
#'
#'
#'
#' @param sample_id name of your sample
#' @param tool program used to generate report
#' @param input_filepath filepath to report corresponding to sample_id and tool
#' @return A report containing just the taxids and read counts
#' @export

importReport <- function(sample_id, tool, input_filepath) {

  print(paste(sample_id, tool, input_filepath))

  if (!is.na(input_filepath)) {

    # Import report from taxonomic classifier. Just keep the taxon ID and the number of reads assigned
    if (tool %in% c("kraken2", "dragen", "epi2me_kraken", "megan_lr")) {

      report <- read.delim(input_filepath, sep = "\t", header = FALSE, row.names = NULL, strip.white = TRUE) %>%
        dplyr::mutate_if(is.character, trimws) %>%
        dplyr::select(V5, V3) %>%
        dplyr::rename(reads = V3, taxid = V5) %>%
        dplyr::filter(reads != 0 & !is.na(taxid)) %>%
        dplyr::mutate(sample_id = sample_id,
                      tool = tool)

    } else if (tool %in% c("bracken", "epi2me_bracken")) {

      report <- read.delim(input_filepath, sep = "\t", header = TRUE, row.names = NULL, strip.white = TRUE) %>%
        dplyr::mutate_if(is.character, trimws) %>%
        dplyr::select(taxonomy_id, new_est_reads) %>%
        dplyr::rename(reads = new_est_reads, taxid = taxonomy_id) %>%
        dplyr::filter(reads != 0 & !is.na(taxid)) %>%
        dplyr::mutate(sample_id = sample_id,
                      tool = tool)

    } else if (tool == "metamix") {

      report <- read.delim(input_filepath, sep = "\t", header = TRUE, row.names = NULL, strip.white = TRUE) %>%
        dplyr::mutate_if(is.character, trimws) %>%
        dplyr::select(taxonID, finalAssignments) %>%
        dplyr::rename(taxid = taxonID, reads = finalAssignments) %>%
        dplyr::filter(reads != 0 & !is.na(taxid)) %>%
        dplyr::mutate(sample_id = sample_id,
                      tool = tool)

    } else if (tool == "metamix_fast") {

      report <- read.delim(input_filepath, sep = "\t", header = TRUE, row.names = NULL, strip.white = TRUE) %>%
        dplyr::mutate_if(is.character, trimws) %>%
        dplyr::select(taxonID, countReads) %>%
        dplyr::rename(taxid = taxonID, reads = countReads) %>%
        dplyr::filter(reads != 0 & !is.na(taxid)) %>%
        dplyr::mutate(sample_id = sample_id,
                      tool = tool)

    } else if (tool == "czid") {

      report <- read.delim(input_filepath, sep = ",", header = TRUE, row.names = NULL, strip.white = TRUE) %>%
        dplyr::mutate_if(is.character, trimws) %>%
        dplyr::select(tax_id, nt_count) %>%
        dplyr::rename(taxid = tax_id, reads = nt_count) %>%
        dplyr::filter(reads != 0 & !is.na(taxid)) %>%
        dplyr::mutate(sample_id = sample_id,
                      tool = tool)

    } else if (tool == "onecodex") {

      report <- read.delim(input_filepath, sep = ",", header = TRUE, row.names = NULL, strip.white = TRUE) %>%
        dplyr::mutate_if(is.character, trimws) %>%
        dplyr::filter(grepl(sample_id, Sample.Name)) %>%
        dplyr::select(Tax.ID, Reads) %>%
        dplyr::rename(reads = Reads, taxid = Tax.ID) %>%
        dplyr::filter(reads != 0 & !is.na(taxid)) %>%
        dplyr::mutate(sample_id = sample_id,
                      tool = tool)

    }

  } else {

    warning("No ", tool, " report provided for ", sample_id)

    report <- data.frame(matrix(nrow=0, ncol = 4))
    colnames(report) <- c("taxid", "reads", "sample_id", "tool")

  }

  return(report)

}
