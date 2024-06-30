#' Import samplesheet
#'
#'
#'
#' @param samplesheet_filepath filepath to samplesheet
#' @return A named list containing fieds associated with the input taxon ID
#' @export


importSamplesheet <- function(samplesheet_filepath) {

  samplesheet_colnames <- c("sample_id", "corresponding_control_id", "dnarna", "dnarna_pair", "raw_read_count_1", "raw_read_count_2",
                            "kraken2_filepath", "bracken_filepath", "dragen_filepath", "epi2me_kraken_filepath", "epi2me_bracken_filepath",
                            "megan_lr_filepath","metamix_filepath", "metamix_fast_filepath", "czid_filepath", "onecodex_filepath")

  samplesheet <- read.csv(samplesheet_filepath, strip.white = TRUE)

  if (!setequal(colnames(samplesheet), samplesheet_colnames)) {

    stop("Error: incorrect filenames in samplesheet")

  }

 # Should add some more conditions here

  samplesheet_clean <- samplesheet %>%
    dplyr::mutate(raw_read_count_1 = tidyr::replace_na(raw_read_count_1, 0),
                  raw_read_count_2 = tidyr::replace_na(raw_read_count_2, 0)) %>%
    dplyr::mutate(total_raw_reads_sample = raw_read_count_1 + raw_read_count_2) %>%
    dplyr::select(-c(raw_read_count_1, raw_read_count_2)) %>%
    tidyr::pivot_longer(contains("_filepath"), names_to = "tool", values_to = "filepath") %>%
    dplyr::mutate(tool = gsub("_filepath", "", tool))

  return(samplesheet_clean)

}
