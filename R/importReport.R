#' Create species-level database
#'
#'
#'
#' @param sample_id name of your sample
#' @param tool program used to generate report - can be kraken2
#' @param input_filepath filepath to report corresponding to sample_id and tool
#' @return A report containing just the taxids and read counts
#' @export

importReport <- function(sample_id, tool, input_filepath) {

  # Import report from taxomic classifier. Just keep the taxon ID and the number of reads assigned
  if (tool == "kraken2") {

    report <- read.delim(input_filepath, sep = "\t", header = FALSE, row.names = NULL, strip.white = TRUE) %>%
      dplyr::mutate_if(is.character, trimws) %>%
      dplyr::select(V5, V3) %>%
      dplyr::rename(reads = V3, taxid = V5) %>%
      dplyr::filter(reads != 0 & !is.na(taxid)) %>%
      dplyr::mutate(sample_id = sample_id,
                    tool = tool)

  }

}
